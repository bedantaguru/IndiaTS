

# Internal Engine for Reconstructing Levels from Rates or Shares
convert_to_level_core <- function(tdl, present_metric_col, req_metric_flag, recreate_fn) {

  req_col <- paste0("value.", req_metric_flag)

  # ---------------------------------------------------------------------------
  # 1. Basic Validation Checks
  # ---------------------------------------------------------------------------
  if (!inherits(tdl, "tdf_long")) {
    stop("`tdl` must inherit from the `tdf_long` class.", call. = FALSE)
  }

  if (!all(c("value.level", present_metric_col) %in% colnames(tdl))) {
    stop(sprintf("`tdl` must contain both `value.level` and `%s`.", present_metric_col), call. = FALSE)
  }

  # EARLY EXIT: If no levels are missing, there is nothing to reconstruct
  if (!anyNA(tdl$value.level)) {
    return(tdl)
  }

  if (!all(!is.na(tdl$value.level) | !is.na(tdl[[present_metric_col]]))) {
    stop(
      sprintf("Missing data conflict: There are rows where both `value.level` and `%s` are NA. ", present_metric_col),
      "At least one must be present in every row to reconstruct the series.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # 2. Categorize and Stash Columns
  # ---------------------------------------------------------------------------
  original_cols <- colnames(tdl)
  value_cols <- stringr::str_subset(original_cols, "^value\\.")

  registry_cols     <- paste0("value.", names(metric_registry))
  recomputable_cols <- intersect(value_cols, registry_cols)

  id_cols  <- c("meta.release_tag", "meta.price_basis", "meta.name", "meta.disaggregation_group")
  key_cols <- intersect(c("time", id_cols), original_cols)

  passthrough_cols <- setdiff(
    original_cols,
    c(key_cols, "value.level", recomputable_cols)
  )

  passthrough_dat <- tdl |>
    dplyr::select(dplyr::all_of(c(key_cols, passthrough_cols)))

  # ---------------------------------------------------------------------------
  # 3. Iterative Reconstruction Loop
  # ---------------------------------------------------------------------------
  step1 <- tdl
  max_iter <- NROW(tdl) + 1L
  n_iter <- 0L

  repeat {
    n_iter <- n_iter + 1L
    n_missing_before <- sum(is.na(step1$value.level))

    # 3a: Dynamically trigger the required prerequisite metric
    metric_args <- stats::setNames(list(TRUE), req_metric_flag)
    step1 <- do.call(compute_metric, c(list(dat = step1), metric_args))

    # 3b: Reconstruct the level using the injected math function
    step2 <- step1 |> compute_metric(
      .custom_function = list(
        level_recreate = recreate_fn
      ),
      .options = list(.custom_function = "data")
    )

    # 3c: Fill NAs and dynamically drop intermediate scratch columns
    step3 <- step2 |> compute_metric(
      .custom_function = list(
        level_fix = \(x) dplyr::if_else(is.na(x$value.level), x$value.level_recreate, x$value.level)
      ),
      .options = list(.custom_function = "data")
    ) |>
      dplyr::select(-value.level, -dplyr::all_of(req_col), -value.level_recreate) |>
      dplyr::rename(value.level = value.level_fix)

    step1 <- step3
    n_missing_after <- sum(is.na(step1$value.level))

    if (n_missing_after == 0L) break

    if (n_missing_after >= n_missing_before || n_iter > max_iter) {
      stop(
        sprintf("Unable to reconstruct all missing values of `value.level`. "),
        sprintf("Some observations lack a valid base metric (`%s`) to project from.", req_col),
        call. = FALSE
      )
    }
  }

  # ---------------------------------------------------------------------------
  # 4. Clean up, Recalculate, and Restore
  # ---------------------------------------------------------------------------
  filled_tdl <- step1 |>
    dplyr::select(dplyr::all_of(c(key_cols, "value.level")))

  recompute_names <- stringr::str_remove(recomputable_cols, "^value\\.")

  if (length(recompute_names) > 0) {
    recompute_flags <- stats::setNames(as.list(rep(TRUE, length(recompute_names))), recompute_names)
    filled_tdl <- do.call(compute_metric, c(list(dat = filled_tdl), recompute_flags))
  }

  final_tdl <- filled_tdl |>
    dplyr::left_join(passthrough_dat, by = key_cols) |>
    dplyr::select(dplyr::all_of(original_cols)) |>
    dplyr::distinct()

  return(final_tdl)
}


# =============================================================================
# REVERSAL REGISTRY
# =============================================================================
# A list containing all available functions for reconstructing `value.level`
# from derived metrics.

metric_reversal_registry <- list(

  # Reverse YoY Growth Rate to Level
  value.growth_rate = function(tdl) {
    convert_to_level_core(
      tdl = tdl,
      present_metric_col = "value.growth_rate",
      req_metric_flag = "level_prev_year",
      recreate_fn = \(x) x$value.level_prev_year * (1 + x$value.growth_rate / 100)
    )
  },

  # Reverse PoP Momentum to Level
  value.momentum = function(tdl) {
    convert_to_level_core(
      tdl = tdl,
      present_metric_col = "value.momentum",
      req_metric_flag = "level_prev_prd",
      recreate_fn = \(x) x$value.level_prev_prd * (1 + x$value.momentum / 100)
    )
  },

  # Reverse YoY Log Growth Rate to Level
  value.ln_growth_rate = function(tdl) {
    convert_to_level_core(
      tdl = tdl,
      present_metric_col = "value.ln_growth_rate",
      req_metric_flag = "level_prev_year",
      recreate_fn = \(x) x$value.level_prev_year * exp(x$value.ln_growth_rate / 100)
    )
  },

  # Reverse PoP Log Momentum to Level
  value.ln_momentum = function(tdl) {
    convert_to_level_core(
      tdl = tdl,
      present_metric_col = "value.ln_momentum",
      req_metric_flag = "level_prev_prd",
      recreate_fn = \(x) x$value.level_prev_prd * exp(x$value.ln_momentum / 100)
    )
  },

  # Reverse Structural Share to Level
  value.share_pct = function(tdl) {

    if (!all(c("meta.parent", "meta.parent_disaggregation_group") %in% colnames(tdl))) {
      stop(
        "This reversal of share, requires the `meta.parent` and `meta.parent_disaggregation_group` ",
        "columns to locate the parent levels.",
        call. = FALSE
      )
    }

    convert_to_level_core(
      tdl = tdl,
      present_metric_col = "value.share_pct",
      req_metric_flag = "level_parent",
      recreate_fn = \(x) x$value.level_parent * (x$value.share_pct / 100)
    )
  }
)


detect_metric_to_reverse_from <- function(tdl){
  tdl |>
    # 1. Isolate rows where level is missing
    dplyr::filter(is.na(value.level)) |>
    # 2. Select only the value.* columns (excluding value.level itself)
    dplyr::select(dplyr::starts_with("value."), -value.level) |>
    # 3. Keep columns that have AT LEAST ONE non-NA value
    purrr::keep(~ any(!is.na(.x))) |>
    # 4. Extract the column names as a character vector
    names()
}



#' Reverse Derived Metrics to Base Level
#'
#' @description
#' Automatically detects which rate or share metric is populated when `value.level`
#' is missing, and routes the data to the appropriate reconstruction engine.
#'
#' @param tdl A `tdf_long` object.
#' @param verbose Logical. If `TRUE` (default), prints a message indicating which
#'   metric was detected and is being used for the reconstruction.
#' @return A `tdf_long` object with `value.level` fully reconstructed.
#' @export
convert_to_level <- function(tdl, verbose = TRUE) {

  from_this <- detect_metric_to_reverse_from(tdl)

  # Scenario 1: Nothing to reconstruct (or no metrics available to reconstruct from)
  if (length(from_this) == 0) {
    if (isTRUE(verbose)) {
      message("No missing levels to reconstruct, or no valid metrics found to project from. Returning original data.")
    }
    return(tdl)
  }

  # Scenario 2: Ambiguity (Too many target metrics)
  if (length(from_this) > 1) {
    stop(
      sprintf(
        "Ambiguous reconstruction: Found multiple candidate metrics to reverse from (%s). ",
        paste(from_this, collapse = ", ")
      ),
      "The engine currently supports reversing from exactly one metric at a time to prevent conflicting math.",
      call. = FALSE
    )
  }

  # Scenario 3: Single candidate found. Validate against registry.
  supported_metrics <- names(metric_reversal_registry)

  if (!from_this %in% supported_metrics) {
    stop(
      sprintf(
        "Unsupported reversal metric: Cannot dynamically reconstruct level from `%s`. ",
        from_this
      ),
      sprintf(
        "Currently supported metrics are: %s.",
        paste(supported_metrics, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Scenario 4: All clear. Alert the user and execute the specific reversal function.
  if (isTRUE(verbose)) {
    message(sprintf("Reconstructing missing `value.level` using detected metric: '%s'", from_this))
  }

  f_this <- metric_reversal_registry[[from_this]]

  f_this(tdl)
}
