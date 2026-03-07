

aggregate_temporal <- function(tdl, to_freq){

  known_fqs <- c("month", "quarter", "halfyear", "year")

  fq <- frequency.tdf_long(tdl)

  rnk <- which(known_fqs == fq)

  can_be_aggregated_to <- known_fqs[rnk:length(known_fqs)] %>% setdiff(fq)

  if(length(can_be_aggregated_to)==0){
    warning("Data is already at the annual frequency (the coarsest level). Further temporal reduction/aggregation or disaggregation is not possible.", call. = FALSE)
    return(NULL)
  }

  if(missing(to_freq)){
    to_freq <- can_be_aggregated_to[1]
    message(paste0("Since to_freq not supplied taking to_freq = ", to_freq," !"))
  }

  if(!(to_freq %in% can_be_aggregated_to)){
    # also say about supplied to_freq
    stop(
      paste0(
        "Data of frequency ", fq,
        " can only be aggregated to these frequencies: ",
        paste0(can_be_aggregated_to, collapse = ", "),
        ". Supplied to_freq is ", to_freq, " !"),
      call. = FALSE)
  }

  time_conversion_fn <- switch(
    to_freq,
    "month" = fiscal_month,
    "quarter" = fiscal_quarter,
    "halfyear" = fiscal_halfyear,
    "year" = fiscal_year)

  dat <- tdl$data

  dat <- dat %>%
    rename(time_old = time) %>%
    dplyr::mutate(
      time = time_conversion_fn(time_old)
    )

  chk <- dat %>%
    group_by(time, meta.release_tag, meta.price_basis, meta.name, meta.disaggregation_group) %>%
    cols_causing_group_variation()

  chk2 <- ("time_old" %in% chk) && (!any(stringr::str_detect( chk, "^meta\\.")))

  if(!chk2){
    stop("There are multiple rows for some combinations of primary key type columns.", call. = FALSE)
  }

  time_agg_fn <- sum

  # if it exsis "meta.temporal_accumulation_rule"
  if("meta.temporal_accumulation_rule" %in% colnames(dat)){

    time_agg_fn_name <- dat$meta.temporal_accumulation_rule[1]

    time_agg_fn <- switch(
      time_agg_fn_name,
      "sum" = sum,
      "mean" = mean,
      "max" = max,
      "min" = min,
      stop("Unknown meta.temporal_accumulation_rule found in data. Supported rules are: sum, mean, max, min.", call. = FALSE)
    )

  }


  dat2 <- dat %>%
    dplyr::group_by(time, meta.release_tag, meta.price_basis, meta.name, meta.disaggregation_group) %>%
    dplyr::summarise(
      dplyr::across(dplyr::starts_with("value"), time_agg_fn),
      dplyr::across(dplyr::starts_with("meta"), ~ dplyr::first(.x)),
      .groups = "drop"
    )

  # safety checks
  tdf_long_check_structure(dat2, hmap = tdl$hmap)

  tdl$data <- dat2

  tdl


}
