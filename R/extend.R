

#' Extend a Dataset with New Incremental Data
#'
#' @description
#' `extend()` is an S3 generic function used to merge or append new incremental
#' data (`y`) into a primary dataset (`x`). It ensures that rows remain distinct,
#' resolving duplicates by prioritizing either the old data ("left") or the new
#' data ("right").
#'
#' While `extend()` works with standard data frames and lists, its primary
#' intent is to seamlessly update long-format time-series (`tdf_long`) objects
#' with incoming raw data of varying shapes.
#'
#' @param x The primary dataset (e.g., `tibble`, `data.frame`, `list`, or `tdf_long`).
#' @param y The incremental data to append. For time-series data (`tdf_long`),
#'   `y` is highly flexible and will be automatically normalized. It can be an atomic
#'   vector, a wide data frame with a `seq` column, a wide `tdf`, or another `tdf_long`.
#' @param primary_key *(For `tibble`/`data.frame`/`list`)* A character vector of column
#'   names that uniquely identify a row. Not used for `tdf_long`, which inherently
#'   derives its primary keys from the standard schema.
#' @param type Character string indicating duplicate resolution direction.
#'   `"left"` keeps the original row in `x`; `"right"` overwrites it with the new row from `y`.
#' @param value_def *(For `tdf_long`)* Character string denoting the metric (e.g., "value.level").
#'   Strictly required when `y` is an atomic vector or a wide data frame.
#' @param var_def *(For `tdf_long`)* Character string denoting the series name (`meta.name`).
#'   Required when `y` is an atomic vector and `x` contains multiple series, to explicitly
#'   specify which series `y` belongs to.
#' @param auto_level_reversal *(For `tdf_long`)* Logical. If `TRUE` (default), the function
#'   automatically attempts to reconstruct missing `value.level` observations using the
#'   newly appended derived metrics (e.g., growth rates or momentum).
#' @param ... Additional arguments passed to specific methods.
#'
#' @details
#' **Time-Series Extension (`tdf_long`):**
#' Extending a `tdf_long` object is a specialized operation. When you pass new data
#' (`y`), the `extend()` method automatically normalizes it to match the strict
#' structural and hierarchical requirements of `x`. This means:
#' \itemize{
#'   \item **Automatic Normalization:** If `y` is just a vector of values or a wide
#'     table, `extend()` reshapes it, anchors the relative timestamps, and translates
#'     the series names (falling back to short names or case-insensitive matching if needed).
#'   \item **Metadata Inference:** Missing structural columns in `y` (like
#'     `meta.release_tag` or `meta.price_basis`) are securely inherited from `x`.
#'   \item **Hierarchy Preservation:** The hierarchy map (`hmap`) attached to `x` is
#'     automatically applied to the normalized `y`.
#'   \item **No Primary Key Needed:** Because `tdf_long` follows a strict schema,
#'     `extend()` automatically uses standard identifiers (`time`, `meta.name`,
#'     `meta.release_tag`, `meta.price_basis`, `meta.disaggregation_group`) to safely
#'     append data without requiring you to manually specify a `primary_key`.
#' }
#'
#' @return An object of the same class as `x`, extended with the data from `y`.
#' @export
extend <- function(x, y, ...) {
  UseMethod("extend")
}

#' @rdname extend
#' @export
extend.tibble <- function(x, y, primary_key, type = c("left", "right"), ...) {
  rows_append_distinct(dmain = x, dincre = y, primary_key, type = type)
}

#' @rdname extend
#' @export
extend.list <- function(x, y = NULL, primary_key, type = c("left", "right"), ...) {
  rows_append_distinct(dmain = x, dincre = NULL, primary_key, type = type)
}

#' @rdname extend
#' @export
extend.data.frame <- extend.tibble

#' @rdname extend
#' @export
extend.tdf_long <- function(x, y, value_def, var_def = NULL, auto_level_reversal = TRUE, type = c("left", "right"), ...) {

  # Set standard primary keys (internal to tdf_long, users don't provide this)
  primary_key <- c(
    "time",
    "meta.name",
    "meta.release_tag",
    "meta.price_basis",
    "meta.disaggregation_group"
  )

  # Normalize the flexible incoming data to perfectly align with the x structure
  y_norm <- ex_tdf_input_normalization(x, y, value_def = value_def, var_def = var_def)

  # Safely append the normalized data
  to1 <- rows_append_distinct(dmain = x, dincre = y_norm, primary_key = primary_key, type = type)

  # Attempt automatic level reconstruction if requested
  if (isTRUE(auto_level_reversal)) {
    to1 <- convert_to_level_after_extend(to1)
  }

  to1
}


#' Normalize Flexible Input Data to tdf_long Format
#'
#' @description
#' `ex_tdf_input_normalization()` is an internal helper function that takes
#' inconsistently shaped incoming data (`y`) and coerces it into a standardized
#' `tdf_long` format that structurally matches a reference dataset (`x`).
#' It handles atomic vectors, relative time sequences, and wide-format data.
#'
#' @param x The reference `tdf_long` object to draw metadata and hierarchy rules from.
#' @param y The raw data to normalize.
#' @param value_def Character string denoting the metric (e.g., "value.level").
#'   Strictly required when `y` is an atomic vector or a wide data frame.
#' @param var_def Character string denoting the series name (`meta.name`).
#'   Required when `y` is an atomic vector and `x` contains multiple series.
#'
#' @details
#' The function performs intelligent name matching between `y` and `x` by falling
#' back to case-insensitive matching or `meta.short_name` matching if exact names
#' are not found. It also automatically injects missing primary metadata columns
#' (like `meta.release_tag`) and attaches the `hmap` from `x`.
#'
#' @return A strictly formatted `tdf_long` object ready for row-binding.
#' @keywords internal
ex_tdf_input_normalization <- function(x, y, value_def, var_def = NULL) {

  if (!inherits(x, "tdf_long")) {
    stop(
      "The first argument `x` must inherit from the `tdf_long` class.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Helper: validate and normalize value_def
  # ---------------------------------------------------------------------------
  clean_value_def <- function(vd) {

    if (missing(vd) || is.null(vd)) {
      stop(
        "Argument `value_def` is missing. It is strictly required when `y` ",
        "is a vector or wide data frame.",
        call. = FALSE
      )
    }

    vd_clean <- vd |>
      tolower() |>
      stringr::str_squish() |>
      stringr::str_remove_all("value\\.") |>
      stringr::str_replace_all(" ", "_")

    metric_names <- names(metric_registry)

    vd_match <- intersect(metric_names, vd_clean)

    if (length(vd_match) != 1L) {
      stop(
        sprintf(
          "Invalid `value_def` ('%s'). It must resolve to exactly one ",
          "registered metric in `metric_registry`.",
          vd
        ),
        call. = FALSE
      )
    }

    vd_match
  }

  # ---------------------------------------------------------------------------
  # Helper: remap meta.name in y to the corresponding meta.name in x
  #
  # Matching priority:
  #   1. Exact match
  #   2. Cleaned-name match
  #   3. Cleaned short-name match, if available
  # ---------------------------------------------------------------------------
  match_series_names <- function(yt) {

    x_names <- unique(x$meta.name)
    y_names <- unique(yt$meta.name)

    # 1. Exact match
    unmatched <- setdiff(y_names, x_names)

    if (length(unmatched) == 0L) {
      return(yt)
    }

    # 2. Cleaned-name match
    x_names_clean <- str_clean(x_names)
    y_names_clean <- str_clean(y_names)

    unmatched_clean <- setdiff(y_names_clean, x_names_clean)

    if (length(unmatched_clean) == 0L) {

      name_map <- dplyr::tibble(
        meta.internal_map_name = x_names_clean,
        meta.name = x_names
      )

      return(
        yt |>
          dplyr::mutate(
            meta.internal_map_name = str_clean(meta.name)
          ) |>
          dplyr::select(-meta.name) |>
          dplyr::left_join(
            name_map,
            by = "meta.internal_map_name"
          ) |>
          dplyr::select(-meta.internal_map_name)
      )
    }

    # 3. Short-name match
    if (!"meta.short_name" %in% colnames(x)) {
      stop(
        paste0(
          "Series names in `y` do not match `meta.name` in `x`, and ",
          "`x` does not contain `meta.short_name` for fallback matching. ",
          "Unmatched series: ",
          paste(unmatched, collapse = ", "),
          "."
        ),
        call. = FALSE
      )
    }

    x_short_names <- unique(x$meta.short_name)
    x_short_names_clean <- str_clean(x_short_names)

    unmatched_short <- setdiff(
      y_names_clean,
      x_short_names_clean
    )

    if (length(unmatched_short) > 0L) {
      stop(
        paste0(
          "Could not match the series names in `y` against either ",
          "`meta.name` or `meta.short_name` in `x`. ",
          "Unmatched series: ",
          paste(unmatched, collapse = ", "),
          ". Please verify the series names."
        ),
        call. = FALSE
      )
    }

    name_map <- dplyr::tibble(
      meta.internal_map_name = x_short_names_clean,
      meta.short_name = x_short_names
    )

    yt |>
      dplyr::mutate(
        meta.internal_map_name = str_clean(meta.name)
      ) |>
      dplyr::select(-meta.name) |>
      dplyr::left_join(
        name_map,
        by = "meta.internal_map_name"
      ) |>
      dplyr::select(-meta.internal_map_name) |>
      dplyr::left_join(
        x |> dplyr::distinct(meta.name, meta.short_name),
        by = "meta.short_name"
      ) |>
      dplyr::select(-meta.short_name)
  }

  # ---------------------------------------------------------------------------
  # Case 1: y is an atomic vector
  # ---------------------------------------------------------------------------
  if (is.atomic(y) && is.null(dim(y))) {

    nv <- x |>
      dplyr::distinct(
        meta.name,
        meta.disaggregation_group
      )

    # Use var_def if supplied; otherwise infer it if only 1 series exists
    if (!missing(var_def) && !is.null(var_def)) {
      y_name <- as.character(var_def)
    } else if (NROW(nv) == 1L) {
      y_name <- nv$meta.name[[1L]]
    } else {
      stop(
        paste0(
          "When `y` is an atomic vector and `x` contains multiple series, ",
          "you must explicitly supply `var_def` to specify which series `y` belongs to. ",
          "Found ", NROW(nv), " distinct combinations in `x`."
        ),
        call. = FALSE
      )
    }

    y_vals <- y
    y <- dplyr::tibble(seq = seq_along(y_vals))
    y[[y_name]] <- y_vals
  }

  # ---------------------------------------------------------------------------
  # Case 2: wide data frame with seq
  # ---------------------------------------------------------------------------
  if (
    is.data.frame(y) &&
    "seq" %in% colnames(y) &&
    NCOL(y) >= 2L
  ) {

    if (anyNA(y$seq) || !is.numeric(y$seq)) {
      stop(
        "When `y` is a wide data frame with a `seq` column, `seq` must be numeric and contain no missing values.",
        call. = FALSE
      )
    }

    xt_max <- max(x$time, na.rm = TRUE)

    y <- y |>
      dplyr::mutate(time = xt_max + seq) |>
      dplyr::select(-seq) |>
      as_tdf()
  }

  # ---------------------------------------------------------------------------
  # Case 3: y is a wide tdf
  # ---------------------------------------------------------------------------
  if (is_tdf(y)) {

    value_def <- clean_value_def(value_def)

    y <- as_tdf_long.tdf(
      y,
      value_col_name = value_def
    ) |>
      dplyr::select(
        time,
        meta.name,
        dplyr::starts_with("value.")
      )
  }

  # ---------------------------------------------------------------------------
  # Final supported form: y must now be tdf_long
  # ---------------------------------------------------------------------------
  if (inherits(y, "tdf_long")) {

    yt <- match_series_names(y)

    # -------------------------------------------------------------------------
    # Match primary metadata columns from x when omitted in y
    # -------------------------------------------------------------------------
    primary_cols <- c(
      "meta.disaggregation_group",
      "meta.release_tag",
      "meta.price_basis",
      intersect(colnames(x), "meta.short_name")
    )

    missing_primary_cols <- setdiff(
      primary_cols,
      colnames(yt)
    )

    if (length(missing_primary_cols) > 0L) {

      x_reference <- x |>
        dplyr::select(
          meta.name,
          dplyr::all_of(missing_primary_cols)
        ) |>
        dplyr::distinct()

      yt <- yt |>
        dplyr::distinct() |>
        dplyr::left_join(
          x_reference,
          by = "meta.name"
        )
    }

    # -------------------------------------------------------------------------
    # Attach hierarchy map from x
    # -------------------------------------------------------------------------
    yt <- hmap(
      yt,
      set = hmap(x)
    )

    return(yt)
  }

  # ---------------------------------------------------------------------------
  # Unsupported input
  # ---------------------------------------------------------------------------
  stop(
    paste0(
      "Input `y` is of an unsupported format. It must be one of the following: ",
      "an atomic vector, a wide data frame containing `seq`, a wide `tdf`, ",
      "or a `tdf_long` object."
    ),
    call. = FALSE
  )
}


#' Auto-Reconstruct Levels After Extension
#'
#' @description
#' An internal helper that attempts to reconstruct `value.level` from derived metrics
#' after new data has been appended via `extend()`. It wraps `convert_to_level()` in a
#' safe `tryCatch` block so that an extension operation does not fail completely if
#' the mathematical reversal cannot be resolved.
#'
#' @param x A `tdf_long` object freshly extended with new data.
#' @return A `tdf_long` object, ideally with missing levels reconstructed. If the
#'   reconstruction fails, it returns the original un-reversed `x` and issues a message.
#' @keywords internal
convert_to_level_after_extend <- function(x) {

  if (!inherits(x, "tdf_long")) {
    stop(
      "The first argument `x` must inherit from the `tdf_long` class.",
      call. = FALSE
    )
  }

  # Safely attempt to reverse metrics back to levels
  res <- tryCatch(
    {
      # Assuming convert_to_level has verbose = FALSE available to keep console clean
      convert_to_level(x, verbose = FALSE)
    },
    error = function(e) {
      # Catch the error, alert the user, and return NULL so the fallback triggers
      message(
        "Auto-reconstruction of missing levels failed during extension: ", e$message,
        "\nProceeding with extended data without level reconstruction."
      )
      return(NULL)
    }
  )

  # If the tryCatch failed or didn't return a proper tdf_long, fallback to the original data
  if (is.null(res) || !inherits(res, "tdf_long")) {
    return(x)
  }

  return(res)
}
