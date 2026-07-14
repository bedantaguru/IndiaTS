

aggregate_temporal <- function(tdl, to_freq = NULL, silent = FALSE,
                               aggregate_function = sum,
                               aggregate_on_incomplete = FALSE){

  tdf_long_check_structure(tdl)

  known_fqs <- c("month", "quarter", "halfyear", "year")

  fq <- frequency.tdf_long_list(tdl)

  if(!(fq %in% known_fqs)){
    stop(
      sprintf(
        "Temporal aggregation is only supported for the following frequencies: %s. Received: '%s'.",
        paste(known_fqs, collapse = ", "),
        fq[1]
      ),
      call. = FALSE
    )
  }

  rnk <- which(known_fqs == fq)

  can_be_aggregated_to <- known_fqs[rnk:length(known_fqs)] %>% setdiff(fq)

  if(length(can_be_aggregated_to)==0){
    warning("Data is already at the annual frequency (the coarsest level). Further temporal reduction/aggregation or disaggregation is not possible.", call. = FALSE)
    return(invisible(tdl))
  }

  if(missing(to_freq) || is.null(to_freq)){
    to_freq <- can_be_aggregated_to[1]
    if(!silent){
      message(paste0("Since to_freq not supplied taking to_freq = ", to_freq," !"))
    }
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

  time_agg_fn <- aggregate_function

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

  # Incomplete Period Summaries
  d_na_chk <- dat_na_populated |>
    dplyr::filter(
      dplyr::if_any(
        dplyr::starts_with("value"),
        ~ is.na(.x)
      )
    )

  # Special Operations on incomplete cases
  if(NROW(d_na_chk)>0 && aggregate_on_incomplete) {
    # By this time dat should have meta.low_freq_time already attached (from prior steps)
    dat_no_na <- dat |>
      dplyr::filter(
        dplyr::if_all(
          dplyr::starts_with("value"),
          ~ !is.na(.x)
        )
      )

    dat_no_na <- dat_no_na |>
      dplyr::mutate(high_freq_pt = fi_this$high_freq_info$converters_period( .data$time)) |>
      dplyr::group_by(.data$meta.name, .data$meta.disaggregation_group,
                      .data$meta.price_basis, .data$meta.release_tag,
                      .data$meta.low_freq_time) |>
      dplyr::mutate(high_freq_pt_norm = .data$high_freq_pt - min(.data$high_freq_pt)) |>
      dplyr::ungroup()

    common_len <- dat_no_na$high_freq_pt_norm |>
      split(dat_no_na$meta.low_freq_time) |>
      purrr::reduce(intersect)

    if(length(common_len)==0) stop("No common low-frequency periods found across the data range.", call. = FALSE)

    dat_no_na_filtered <- dat_no_na |> dplyr::filter(.data$high_freq_pt_norm %in% common_len)

    # Override dat_na_populated with intermediate dat_no_na_filtered
    dat_na_populated <- dat_no_na_filtered |>
      dplyr::rename(time_old = "time") |>
      dplyr::rename(time = "meta.low_freq_time")
  }


  dat2 <- dat_na_populated %>%
    dplyr::group_by(time, meta.release_tag, meta.price_basis, meta.name, meta.disaggregation_group) %>%
    dplyr::summarise(
      dplyr::across(dplyr::starts_with("value"), time_agg_fn),
      dplyr::across(dplyr::starts_with("meta"), ~ dplyr::first(.x)),
      .groups = "drop"
    )

  # This is very key step. So NA plays a vital role here. na.rm should not be turned on.
  dat2_no_na <- dat2 |>
    dplyr::filter(
      dplyr::if_all(
        dplyr::starts_with("value"),
        ~ !is.na(.x)
      )
    )

  if(NROW(dat2_no_na)!=NROW(dat2) && !silent && !aggregate_on_incomplete){
    message("Some primay key combinations has incomplete temporal coverage and ",
            "hence not considered for aggregation.")
  }

  # safety checks
  tdf_long_check_structure(dat2_no_na, hmap = tdl$hmap)

  tdl$data <- dat2_no_na

  tdl

}


