

tdf_long_temporal_link <- function(tdl1 , tdl2){

  fq1 <- frequency.tdf_long(tdl1)
  fq2 <- frequency.tdf_long(tdl2)

  if(fq1==fq2){
    stop("Both are of same frequency, no linking possible/needed!", call. = FALSE)
  }

  fi <- freq_info_for_two_freqs(fq1, fq2)

  linked_tdl <- list()

  if(fi$first_more_aggregated_than_second){
    linked_tdl$low_freq <- tdl1
    linked_tdl$high_freq <- tdl2
  } else {
    linked_tdl$low_freq <- tdl2
    linked_tdl$high_freq <- tdl1
  }

  # Add a column to the high frequency data that indicates which low frequency period it belongs to
  hf <- linked_tdl$high_freq$data

  hf <- hf %>%
    dplyr::mutate(
      meta.low_freq_time = fi$low_freq_info$converter(time)
    )

  linked_tdl$high_freq$data <- hf

  linked_tdl

}

linked_tdf_long_implied_figures <- function(linked_tdl){

  # linked_tdl is expected to be a list with elements:
  #  - high_freq: a tdf_long object containing high-frequency data
  #  - low_freq:  a tdf_long object containing low-frequency data
  #
  # This function attempts to compute "implied" high-frequency observations
  # when exactly one high-frequency period is missing inside a low-frequency
  # aggregation (currently implemented only for low_freq == "year").
  #
  # The algorithm:
  # 1. Identify low-frequency periods that have exactly one missing high-frequency row.
  # 2. For those low-frequency periods, compute the partial-year total from present high-frequency rows.
  # 3. Subtract that partial total from the low-frequency reported total to derive the implied value.
  # 4. Insert the implied high-frequency rows into the high-frequency dataset.
  #
  # NOTE: The function intentionally returns the original linked_tdl unchanged
  #       (with a message) if the low frequency is not "year" or if no suitable
  #       low-frequency periods are found.

  hf <- linked_tdl$high_freq$data
  lf <- linked_tdl$low_freq$data

  # Save original high-frequency column order for later reassembly
  hf_orig_cols <- colnames(hf)

  high_freq <- frequency.tdf_long(linked_tdl$high_freq)
  low_freq <- frequency.tdf_long(linked_tdl$low_freq)

  # Only implemented for linking to yearly low-frequency data
  if(low_freq!="year"){
    message("Implied calculation currently only implemented for linking to yearly data. Returning linked TDL without any implied calculations.")
    return(linked_tdl)
  }

  # Summarise how many high-frequency rows exist per low-frequency bucket
  time_summary <- hf %>%
    group_by(
      meta.low_freq_time, meta.release_tag, meta.price_basis,
      meta.name, meta.disaggregation_group) %>%
    summarise(n = n(), .groups = "drop")

  # Identify low-frequency groups that are missing exactly one high-frequency period
  applicable_for_implied_calculation <- time_summary %>%
    filter(n == linked_tdl$high_to_low_ratio - 1)

  # If no groups qualify, inform and return unchanged
  if(NROW(applicable_for_implied_calculation) == 0) {
    message("No implied calculation possible as there are no low frequency periods with exactly one high frequency period missing.")
    return(linked_tdl)
  }

  # Build helper functions that map time -> unit-level labels.
  # hf_unit_function returns the unit label without year (e.g. "Jan", "Q1").
  # hf_whole_function returns the full fiscal unit label with year attached.
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

  # Annotate high-frequency rows with a unit-level time identifier (meta.unit_time)
  hf <- hf %>%
    mutate(meta.unit_time = as.character(hf_unit_function(time)))

  # Keep only high-frequency rows that belong to the low-frequency groups we will attempt to fill
  for_implied_calc_hf <- hf %>%
    inner_join(
      applicable_for_implied_calculation %>% select(-n),
      by = c("meta.low_freq_time", "meta.release_tag", "meta.price_basis",
             "meta.name", "meta.disaggregation_group"))

  # Construct the universe of possible unit-level labels for the low-frequency period.
  # NOTE: This is tuned to the "year" low_freq case and uses an ad-hoc sequence
  # (.current_fixed_date+1:12*31) to obtain candidate dates which are then converted
  # to unit labels. The expression is left intact as-is.
  all_hfs_per_lf <- hf_unit_function(.current_fixed_date+1:12*31) %>% unique()

  # Build a full grid of expected high-frequency rows for each low-frequency group
  full_grid <- tidyr::expand_grid(
    meta.unit_time = all_hfs_per_lf,
    meta.low_freq_time = unique(for_implied_calc_hf$meta.low_freq_time),
    meta.release_tag = unique(for_implied_calc_hf$meta.release_tag),
    meta.price_basis = unique(for_implied_calc_hf$meta.price_basis),
    meta.name = unique(for_implied_calc_hf$meta.name),
    meta.disaggregation_group = unique(for_implied_calc_hf$meta.disaggregation_group)
  )

  # Join the full grid with existing high-frequency rows to detect missing unit rows
  deficiency_map  <- full_grid %>%
    full_join(
      for_implied_calc_hf,
      by = colnames(full_grid)
    )

  # Rows that are missing in the high-frequency data (i.e., NA value.level) are candidates for implied values
  deficiency_map_missing <- deficiency_map %>%
    filter(is.na(value.level)) %>%
    mutate(time = paste0(meta.unit_time," ", meta.low_freq_time) %>%
             hf_whole_function) %>%
    select(time, meta.release_tag, meta.price_basis, meta.name,
           meta.disaggregation_group, meta.low_freq_time)

  # For present (non-missing) high-frequency rows compute the partial-year total per low-frequency group
  deficiency_map_present_total <- deficiency_map %>%
    filter(!is.na(value.level)) %>%
    group_by(meta.low_freq_time, meta.release_tag, meta.price_basis,
             meta.name, meta.disaggregation_group) %>%
    summarise(value.partial_year_total = sum(value.level), .groups = "drop") %>%
    rename(time = meta.low_freq_time) %>%
    mutate(time = time %>% as_fiscal_period())

  # Attach the low-frequency reported totals so we can compute implied = reported - partial
  deficiency_map_present_total <- deficiency_map_present_total %>%
    left_join(lf, c("time", "meta.release_tag", "meta.price_basis",
                    "meta.name", "meta.disaggregation_group"))

  # Compute the implied value for the missing unit as low-frequency total minus observed partial-year total
  deficiency_map_present_total <- deficiency_map_present_total %>%
    mutate(value.implied = value.level - value.partial_year_total)

  # Reformat implied-value results into the same shape as deficiency_map_missing and prepare for joining
  deficiency_map_impl <- deficiency_map_present_total %>%
    select(-value.level, -value.partial_year_total) %>%
    rename(value.level = value.implied,
           meta.low_freq_time = time) %>%
    mutate(meta.low_freq_time = as.character(meta.low_freq_time))

  # Keep only the implied rows that correspond to actually missing unit rows
  deficiency_map_impl <- deficiency_map_impl %>%
    inner_join(
      deficiency_map_missing,
      by = c("meta.low_freq_time", "meta.release_tag", "meta.price_basis",
             "meta.name", "meta.disaggregation_group")
    )

  # Extract the implied high-frequency rows matching original high-frequency column order
  hf_part <- deficiency_map_impl[hf_orig_cols]

  # Preserve the original high-frequency data as a baseline
  hf_pre <- hf[hf_orig_cols]

  # Append implied rows to the original high-frequency dataset while preserving uniqueness
  hf_pre <- hf_pre %>% rows_append_distinct(
    hf_part,
    primary_key = c("time", "meta.release_tag", "meta.price_basis",
                    "meta.name", "meta.disaggregation_group")
  )

  # Replace the high-frequency data in the linked structure with the augmented dataset
  linked_tdl$high_freq$data <- hf_pre

  # Return the modified linked structure
  linked_tdl

}

linked_tdf_long_tally <- function(linked_tdl) {

  high_freq <- frequency.tdf_long(linked_tdl$high_freq)
  low_freq <- frequency.tdf_long(linked_tdl$low_freq)

  from_high_to_low <- aggregate_temporal(linked_tdl$high_freq, to_freq = low_freq)

  common <- from_high_to_low$data %>%
    select(time, meta.release_tag, meta.price_basis, meta.name, meta.disaggregation_group, value.level) %>%
    inner_join(
      linked_tdl$low_freq$data %>%
        select(time, meta.release_tag, meta.price_basis, meta.name, meta.disaggregation_group, value.level) ,
      by = c("time", "meta.release_tag", "meta.price_basis",
             "meta.name", "meta.disaggregation_group"),
      suffix = c("_from_high", "_from_low")
    )

  common <- common %>%
    mutate(
      value.level_ratio = value.level_from_high / value.level_from_low,
      value.divergence = (value.level_ratio - 1) %>% abs() %>% round(2))


}


