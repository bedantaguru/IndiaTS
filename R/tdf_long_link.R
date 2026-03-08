

# This functions adds meta.low_freq_time to High Frequency tdl
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

  # Fill a missing high-frequency observation using the corresponding
  # low-frequency benchmark total.

  # For each low-frequency period, if exactly one high-frequency
  # observation is missing, the value is implied as:
  # low_frequency_total − sum(existing_high_frequency_values).

  # The function identifies such cases, computes the implied value,
  # creates the missing high-frequency row, and appends it to the
  # dataset while preserving primary-key uniqueness.

  # If no such case exists, the input object is returned unchanged.

  hf <- linked_tdl$high_freq$data
  lf <- linked_tdl$low_freq$data

  # Preserve original high-frequency column order so appended rows match structure
  hf_orig_cols <- colnames(hf)

  high_freq <- frequency.tdf_long(linked_tdl$high_freq)
  low_freq <- frequency.tdf_long(linked_tdl$low_freq)

  fi_this <- freq_info_for_two_freqs(low_freq, high_freq)

  # Count how many high-frequency observations exist inside each low-frequency benchmark group
  time_summary <- hf %>%
    group_by(
      meta.low_freq_time, meta.release_tag, meta.price_basis,
      meta.name, meta.disaggregation_group) %>%
    summarise(n = n(), .groups = "drop")

  # Identify groups where exactly one high-frequency observation is missing
  applicable_for_implied_calculation <- time_summary %>%
    filter(n == fi_this$high_to_low_ratio - 1)

  # If none qualify, no implied calculation can be performed
  if(NROW(applicable_for_implied_calculation) == 0) {
    message("No implied calculation possible as there are no low frequency periods with exactly one high frequency period missing.")
    return(linked_tdl)
  }

  # Keep only high-frequency rows belonging to groups eligible for implied calculation
  for_implied_calc_hf <- hf %>%
    inner_join(
      applicable_for_implied_calculation %>% select(-n),
      by = c("meta.low_freq_time", "meta.release_tag", "meta.price_basis",
             "meta.name", "meta.disaggregation_group"))

  # Generate the complete sequence of expected high-frequency times within each low-frequency benchmark period
  full_grid_time  <- complete_time_sequence_from_benchmark(
    for_implied_calc_hf$time,
    for_implied_calc_hf$meta.low_freq_time)
  colnames(full_grid_time) <- c("time", "meta.low_freq_time")

  # Build the full combination of metadata dimensions expected for those time points
  full_grid_part <- tidyr::expand_grid(
    meta.release_tag = unique(for_implied_calc_hf$meta.release_tag),
    meta.price_basis = unique(for_implied_calc_hf$meta.price_basis),
    meta.name = unique(for_implied_calc_hf$meta.name),
    meta.disaggregation_group = unique(for_implied_calc_hf$meta.disaggregation_group)
  )

  full_grid <- dplyr::cross_join(
    full_grid_time,
    full_grid_part
  )

  # Join full grid with existing high-frequency data to identify missing rows
  deficiency_map  <- full_grid %>%
    full_join(
      for_implied_calc_hf,
      by = colnames(full_grid)
    )

  # Rows with missing value.level correspond to the single missing high-frequency observation
  deficiency_map_missing <- deficiency_map %>%
    filter(is.na(value.level)) %>%
    select(time, meta.release_tag, meta.price_basis, meta.name,
           meta.disaggregation_group, meta.low_freq_time)

  # Compute observed partial totals of high-frequency values within each low-frequency benchmark
  deficiency_map_present_total <- deficiency_map %>%
    filter(!is.na(value.level)) %>%
    group_by(meta.low_freq_time, meta.release_tag, meta.price_basis,
             meta.name, meta.disaggregation_group) %>%
    summarise(value.partial_year_total = sum(value.level), .groups = "drop") %>%
    rename(time = meta.low_freq_time)

  # Attach corresponding low-frequency benchmark values
  deficiency_map_present_total <- deficiency_map_present_total %>%
    left_join(
      lf,
      by = c("time", "meta.release_tag", "meta.price_basis",
             "meta.name", "meta.disaggregation_group")
    )

  # Calculate implied value as: benchmark total minus observed partial high-frequency sum
  deficiency_map_present_total <- deficiency_map_present_total %>%
    mutate(value.implied = value.level - value.partial_year_total)

  # Prepare implied rows to match the high-frequency data structure
  deficiency_map_impl <- deficiency_map_present_total %>%
    select(-value.level, -value.partial_year_total) %>%
    rename(value.level = value.implied,
           meta.low_freq_time = time)

  # Retain only rows corresponding to the actual missing high-frequency observation
  deficiency_map_impl <- deficiency_map_impl %>%
    inner_join(
      deficiency_map_missing,
      by = c("meta.low_freq_time", "meta.release_tag", "meta.price_basis",
             "meta.name", "meta.disaggregation_group")
    )

  # Extract implied rows with the same column order as the original high-frequency dataset
  hf_part <- deficiency_map_impl[hf_orig_cols]

  # Preserve the original high-frequency data as base
  hf_pre <- hf[hf_orig_cols]

  # Append implied rows while ensuring primary-key uniqueness
  hf_pre <- hf_pre %>% rows_append_distinct(
    hf_part,
    primary_key = c("time", "meta.release_tag", "meta.price_basis",
                    "meta.name", "meta.disaggregation_group")
  )

  # Replace high-frequency data inside the linked structure with the augmented dataset
  linked_tdl$high_freq$data <- hf_pre

  # Return updated linked tdf_long object
  linked_tdl

}

linked_tdf_long_tally <- function(linked_tdl) {

  high_freq <- frequency.tdf_long(linked_tdl$high_freq)
  low_freq <- frequency.tdf_long(linked_tdl$low_freq)

  from_high_to_low <- aggregate_temporal(
    linked_tdl$high_freq,
    to_freq = low_freq,
    silent = TRUE)

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
      value.divergence = (value.level_ratio - 1) %>% abs() %>% round(2)) %>%
    rename(value.level = value.level_from_low)

  lft <- linked_tdl$low_freq

  tdf_long_check_structure(common, lft$hmap)

  lft$data <- common

  lft

}


