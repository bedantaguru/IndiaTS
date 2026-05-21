
#' Temporally Link Time Series Data of Different Frequencies
#'
#' @description
#' Connects two \code{tdf_long} objects that contain the same data at different
#' frequencies (e.g., quarterly and annual series).
#'
#' @details
#' This function creates a temporal link between a high-frequency and a
#' low-frequency dataset. It is primarily used to prepare datasets for further
#' operations such as benchmarking, implied figure (Implicit) calculation, and other
#' mixed-frequency tasks.
#'
#' For example, it can be used to link quarterly and annual GDP/GVA series.
#' The function automatically identifies which input is high-frequency and which
#' is low-frequency, and adds a \code{meta.low_freq_time} column to the
#' high-frequency dataset to map it to the corresponding low-frequency period.
#'
#' @param tl1 A \code{tdf_long} object containing time series data.
#' @param tl2 A \code{tdf_long} object containing the same data at a different frequency.
#'
#' @return A list containing the two linked \code{tdf_long} objects, structurally
#' separated into \code{low_freq} and \code{high_freq} components.
#'
#' @export
temporal_link <- function(tl1, tl2){
  UseMethod("temporal_link")
}

#' @export
temporal_link.tdf_long <- function(tl1, tl2){
  if(!inherits(tl1,"tdf_long") || !inherits(tl2,"tdf_long")) {
    stop("Both inputs are required to be of <tdf_long> class", call. = FALSE)
  }
  tl1l <- to_tdf_long_list(tl1)
  tl2l <- to_tdf_long_list(tl2)
  tlll <- tdf_long_temporal_link(tl1l, tl2l)
  tlll |> purrr::map(as_tdf_long)
}

#' @export
temporal_link.tdf_long_list <- function(tl1, tl2){
  if(!inherits(tl1,"tdf_long_list") || !inherits(tl2,"tdf_long_list")) {
    stop("Both inputs are required to be of <tdf_long_list> class", call. = FALSE)
  }
  tdf_long_temporal_link(tl1, tl2)
}


# This functions adds meta.low_freq_time to High Frequency tdl
tdf_long_temporal_link <- function(tdl1 , tdl2){

  g1 <- tdl1$data |> dplyr::distinct(meta.name, meta.disaggregation_group)
  g2 <- tdl2$data |> dplyr::distinct(meta.name, meta.disaggregation_group)
  chk <- g1 |> dplyr::inner_join(g2, by = c("meta.name", "meta.disaggregation_group"))

  if(NROW(chk)==0){
    stop("Temporal link not possible as no common name present!", call. = FALSE)
  }

  fq1 <- frequency.tdf_long_list(tdl1)
  fq2 <- frequency.tdf_long_list(tdl2)

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

  high_freq <- frequency.tdf_long_list(linked_tdl$high_freq)
  low_freq <- frequency.tdf_long_list(linked_tdl$low_freq)

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

  # Preserve the original high-frequency data
  hf_pre <- hf[hf_orig_cols]

  # Append implied rows while ensuring primary-key uniqueness
  hf_pre <- hf_pre %>% rows_append_distinct(
    hf_part,
    primary_key = c("time", "meta.release_tag", "meta.price_basis",
                    "meta.name", "meta.disaggregation_group")
  )

  # Replace high-frequency data inside the linked structure with the augmented dataset
  linked_tdl$high_freq$data <- hf_pre

  # Return updated linked tdf_long_list object
  linked_tdl

}

linked_tdf_long_tally <- function(linked_tdl) {

  high_freq <- frequency.tdf_long_list(linked_tdl$high_freq)
  low_freq <- frequency.tdf_long_list(linked_tdl$low_freq)

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


#' Compute Implied High-Frequency Estimates
#'
#' @description
#' Calculates missing high-frequency observations (e.g., a Q4 residual) by
#' subtracting existing high-frequency periods (e.g., Q1-Q3) from the
#' corresponding low-frequency benchmark total (e.g., Annual First Advance Estimates).
#'
#' @details
#' This function operates on a temporally linked pair of \code{tdf_long} objects.
#' It scans the data for low-frequency benchmark periods where exactly one
#' high-frequency observation is missing. When it finds such a case, it computes
#' the implied (residual) value for that missing period and appends it to the
#' high-frequency dataset while preserving primary-key uniqueness.
#'
#' If no such cases exist (i.e., high-frequency periods are fully complete or
#' missing more than one observation per benchmark group), the dataset is
#' returned unchanged.
#'
#' @param linked_tdf_long A named list containing two \code{tdf_long} objects.
#' Must specifically contain nodes named \code{"low_freq"} and \code{"high_freq"}.
#' This is typically the direct output of \code{temporal_link()}.
#'
#' @return A list identical in structure to the input \code{linked_tdf_long},
#' but with the newly calculated implied figures appended to the \code{high_freq}
#' \code{tdf_long} data frame.
#'
#' @export
compute_implied <- function(linked_tdf_long){

  # Validation: Ensure the input is a list with the exact required structure
  if (!is.list(linked_tdf_long) || !all(c("low_freq", "high_freq") %in% names(linked_tdf_long))) {
    stop("`linked_tdf_long` must be a named list containing exactly 'low_freq' and 'high_freq' elements. Expected output from `temporal_link()`.", call. = FALSE)
  }

  # Validation: Ensure both nodes are specifically of the public <tdf_long> class
  if (!inherits(linked_tdf_long$low_freq, "tdf_long") || !inherits(linked_tdf_long$high_freq, "tdf_long")) {
    stop("Both the 'low_freq' and 'high_freq' elements of the list must be of class <tdf_long>.", call. = FALSE)
  }

  # Convert public tdf_long objects to internal tdf_long_list format
  ltdl <- linked_tdf_long |> purrr::map(to_tdf_long_list)

  # Perform the implied figure calculation
  ltdlo <- linked_tdf_long_implied_figures(ltdl)

  # Convert back to public tdf_long objects before returning
  ltdlo |> purrr::map(as_tdf_long)

}

