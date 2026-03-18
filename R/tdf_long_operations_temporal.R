

aggregate_temporal <- function(tdl, to_freq, silent = FALSE){

  tdf_long_check_structure(tdl)

  known_fqs <- c("month", "quarter", "halfyear", "year")

  fq <- frequency.tdf_long(tdl)

  rnk <- which(known_fqs == fq)

  can_be_aggregated_to <- known_fqs[rnk:length(known_fqs)] %>% setdiff(fq)

  if(length(can_be_aggregated_to)==0){
    warning("Data is already at the annual frequency (the coarsest level). Further temporal reduction/aggregation or disaggregation is not possible.", call. = FALSE)
    return(invisible(tdl))
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

  fi_this <- freq_info_for_two_freqs(fq, to_freq)

  time_conversion_fn <- fi_this$low_freq_info$converter

  dat <- tdl$data

  dat <- dat %>% mutate(
    meta.low_freq_time = time_conversion_fn(time)
  )

  # Check for completeness (init)
  c_inf <- complete_time_sequence_from_benchmark(
    dat$time, to_freq
  )

  colnames(c_inf) <- c("time", "meta.low_freq_time")

  full_grid_part <- dat %>%
    distinct(meta.release_tag, meta.price_basis, meta.name, meta.disaggregation_group)

  full_grid <- dplyr::cross_join(
    c_inf,
    full_grid_part
  )

  # We populate NA in missing high freq points (so that the sum becomes NA)
  dat_na_populated <- dat %>%
    full_join(
      full_grid,
      by = colnames(full_grid)
    )


  # meta.low_freq_time created earlier
  dat_na_populated <- dat_na_populated %>%
    rename(
      time = meta.low_freq_time,
      time_old = time
    )

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

  dat2 <- dat_na_populated %>%
    dplyr::group_by(time, meta.release_tag, meta.price_basis, meta.name, meta.disaggregation_group) %>%
    dplyr::summarise(
      dplyr::across(dplyr::starts_with("value"), time_agg_fn),
      dplyr::across(dplyr::starts_with("meta"), ~ dplyr::first(.x)),
      .groups = "drop"
    )

  # This is very key step. So NA plays a vital role here. na.rm should not be turned on.
  dat2_no_na <- dat2 %>% filter(!is.na(value.level))

  if(NROW(dat2_no_na)!=NROW(dat2) && !silent){
    message("Some primay key combinations has incomplete temporal coverage and ",
            "hence not considered for aggregation.")
  }

  # safety checks
  tdf_long_check_structure(dat2_no_na, hmap = tdl$hmap)

  tdl$data <- dat2_no_na

  tdl

}


