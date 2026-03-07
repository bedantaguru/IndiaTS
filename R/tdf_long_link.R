

tdf_long_temporal_link <- function(tdl1 , tdl2){

  known_fq_dat <- tibble::tibble(
    known_fqs = c("month", "quarter", "halfyear", "year"),
    units = c(12,4,2,1)
  )

  fq1 <- frequency.tdf_long(tdl1)
  fq2 <- frequency.tdf_long(tdl2)

  if(fq1==fq2){
    stop("Both are of same frequency, no linking needed", call. = FALSE)
  }

  rnk1 <- which(known_fq_dat$known_fqs==fq1)
  rnk2 <- which(known_fq_dat$known_fqs==fq2)

  unit1 <- known_fq_dat$units[rnk1]
  unit2 <- known_fq_dat$units[rnk2]

  linked_tdl <- list()

  if(rnk1 > rnk2){
    linked_tdl$low_freq <- tdl1
    linked_tdl$high_freq <- tdl2
    linked_tdl$high_to_low_ratio <- unit2/unit1
  } else {
    linked_tdl$low_freq <- tdl2
    linked_tdl$high_freq <- tdl1
    linked_tdl$high_to_low_ratio <- unit1/unit2
  }

  low_freq <- frequency.tdf_long(linked_tdl$low_freq)

  # Add a column to the high frequency data that indicates which low frequency period it belongs to
  hf <- linked_tdl$high_freq$data

  hf <- hf %>%
    dplyr::mutate(
      meta.low_freq_time =
        if (low_freq == "month") as.character(fiscal_month(time))
      else if (low_freq == "quarter") as.character(fiscal_quarter(time))
      else if (low_freq == "halfyear") as.character(fiscal_halfyear(time))
      else if (low_freq == "year") as.character(fiscal_year(time))
      else NA_character_
    )

  linked_tdl$high_freq$data <- hf

  linked_tdl

}

liked_tdf_long_implied_figures <- function(linked_tdl){

  hf <- linked_tdl$high_freq$data
  lf <- linked_tdl$low_freq$data

  hf_orig_cols <- colnames(hf)

  high_freq <- frequency.tdf_long(linked_tdl$high_freq)
  low_freq <- frequency.tdf_long(linked_tdl$low_freq)

  if(low_freq!="year"){
    message("Implied calculation currently only implemented for linking to yearly data. Returning linked TDL without any implied calculations.")
    return(linked_tdl)
  }

  time_summary <- hf %>%
    group_by(
      meta.low_freq_time, meta.release_tag, meta.price_basis,
      meta.name, meta.disaggregation_group) %>%
    summarise(n = n(), .groups = "drop")

  applicable_for_implied_calculation <- time_summary %>%
    filter(n == linked_tdl$high_to_low_ratio - 1)

  if(NROW(applicable_for_implied_calculation) == 0) {
    message("No implied calculation possible as there are no low frequency periods with exactly one high frequency period missing.")
    return(linked_tdl)
  }

  hf_unit_function <- switch(
    high_freq,
    month   = function(x) fiscal_month(x, with_year = FALSE),
    quarter = function(x) fiscal_quarter(x, with_year = FALSE),
    halfyear= function(x) fiscal_halfyear(x, with_year = FALSE),
    function(x) NA_character_
  )

  hf_whole_function <- switch(
    high_freq,
    month   = function(x) fiscal_month(x, with_year = TRUE),
    quarter = function(x) fiscal_quarter(x, with_year = TRUE),
    halfyear= function(x) fiscal_halfyear(x, with_year = TRUE),
    function(x) NA_character_
  )

  hf <- hf %>%
    mutate(meta.unit_time = as.character(hf_unit_function(time)))

  for_implied_calc_hf <- hf %>%
    inner_join(
      applicable_for_implied_calculation %>% select(-n),
      by = c("meta.low_freq_time", "meta.release_tag", "meta.price_basis",
             "meta.name", "meta.disaggregation_group"))

  # Here it is not generalized to other low_freq (this is tuned for year only)
  all_hfs_per_lf <- hf_unit_function(.current_fixed_date+1:12*31) %>% unique()


  full_grid <- tidyr::expand_grid(
    meta.unit_time = all_hfs_per_lf,
    meta.low_freq_time = unique(for_implied_calc_hf$meta.low_freq_time),
    meta.release_tag = unique(for_implied_calc_hf$meta.release_tag),
    meta.price_basis = unique(for_implied_calc_hf$meta.price_basis),
    meta.name = unique(for_implied_calc_hf$meta.name),
    meta.disaggregation_group = unique(for_implied_calc_hf$meta.disaggregation_group)
  )

  deficiency_map  <- full_grid %>%
    full_join(
      for_implied_calc_hf,
      by = colnames(full_grid)
    )

  deficiency_map_missing <- deficiency_map %>%
    filter(is.na(value.level)) %>%
    mutate(time = paste0(meta.unit_time," ", meta.low_freq_time) %>%
             hf_whole_function) %>%
    select(time, meta.release_tag, meta.price_basis, meta.name,
           meta.disaggregation_group, meta.low_freq_time)

  deficiency_map_present_total <- deficiency_map %>%
    filter(!is.na(value.level)) %>%
    group_by(meta.low_freq_time, meta.release_tag, meta.price_basis,
             meta.name, meta.disaggregation_group) %>%
    summarise(value.partial_year_total = sum(value.level), .groups = "drop") %>%
    rename(time = meta.low_freq_time) %>%
    mutate(time = time %>% as_fiscal_period())

  deficiency_map_present_total <- deficiency_map_present_total %>%
    left_join(lf, c("time", "meta.release_tag", "meta.price_basis",
                    "meta.name", "meta.disaggregation_group"))
  deficiency_map_present_total <- deficiency_map_present_total %>%
    mutate(value.implied = value.level - value.partial_year_total)

  deficiency_map_impl <- deficiency_map_present_total %>%
    select(-value.level, -value.partial_year_total) %>%
    rename(value.level = value.implied,
           meta.low_freq_time = time) %>%
    mutate(meta.low_freq_time = as.character(meta.low_freq_time))

  deficiency_map_impl <- deficiency_map_impl %>%
    inner_join(
      deficiency_map_missing,
      by = c("meta.low_freq_time", "meta.release_tag", "meta.price_basis",
             "meta.name", "meta.disaggregation_group")
    )

  hf_part <- deficiency_map_impl[hf_orig_cols]

  hf_pre <- hf[hf_orig_cols]

  hf_pre <- hf_pre %>% rows_append_distinct(
    hf_part,
    primary_key = c("time", "meta.release_tag", "meta.price_basis",
                    "meta.name", "meta.disaggregation_group")
  )

  linked_tdl$high_freq$data <- hf_pre

  linked_tdl

}

liked_tdf_long_tally <- function(linked_tdl) {

}


