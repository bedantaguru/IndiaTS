

is_date_type <- function(x){
  lubridate::is.Date(x) || lubridate::is.POSIXt(x) || lubridate::is.POSIXlt(x) || lubridate::is.POSIXct(x)
}


is_time <- function(x){
  is_date_type(x) || inherits(x, calendar_period_class[1]) || inherits(x, fiscal_period_class[1])
}

is_period_type <- function(x){
  inherits(x, calendar_period_class[1]) || inherits(x, fiscal_period_class[1])
}


str_clean <- function(x,
                       allow_numbers = TRUE,
                       allow_specials = FALSE,
                       no_space = FALSE,
                       special_chars = "_%&@") {

  # 1. Validation & Setup
  if (length(x) == 0) return(character(0))
  x <- as.character(x)
  x <- stringr::str_to_lower(x)

  # 2. Number Handling (Pre-processing)
  if (allow_numbers) {
    # Collapse space between sign and digit: "+  10" -> "+10"
    x <- stringr::str_replace_all(x, "([+-])\\s+(?=\\d)", "\\1")

    # Remove dangling signs (signs not followed by digits)
    x <- stringr::str_replace_all(x, "([+-])(?!\\d)", " ")
  }

  # 3. Build Allowed Character Pattern
  # Start with standard letters
  allowed_class <- "a-z"

  if (allow_numbers) {
    # Add digits and literal + or -
    allowed_class <- paste0(allowed_class, "0-9+\\-")
  }

  if (allow_specials && nzchar(special_chars)) {
    # FIX: Split the special_chars string into individual characters,
    # escape them using the package (handles [], $, ., etc. safely),
    # then collapse them back together.
    chars_vec <- unlist(stringr::str_split(special_chars, ""))
    safe_specials <- paste0(stringr::str_escape(chars_vec), collapse = "")

    allowed_class <- paste0(allowed_class, safe_specials)
  }

  # Create a "Negative Character Class" (match anything NOT allowed)
  # We keep \\s to preserve spaces during this phase
  pattern <- paste0("[^", allowed_class, "\\s]")

  # 4. Clean and Standardize
  # Remove forbidden characters
  x <- stringr::str_replace_all(x, pattern, " ")

  # squish() is optimized: it trims whitespace AND collapses internal spaces
  x <- stringr::str_squish(x)

  # 5. Handle underscores if requested
  if (no_space) {
    x <- stringr::str_replace_all(x, "\\s+", "_")
  }

  return(x)
}


get_extdata_path <- function(file) {
  system.file("extdata", file,
              package = utils::packageName())
}




cols_breaking_group_uniqueness <- function(d, group_colnames) {

  if (!inherits(d, "data.frame")) {
    stop("`d` must be a data.frame or tibble.")
  }

  # If group columns not supplied
  if (missing(group_colnames)) {

    if (dplyr::is_grouped_df(d)) {
      group_colnames <- dplyr::group_vars(d)
    } else {
      return(colnames(d))
    }

  }

  group_colnames <- unique(group_colnames)

  # If grouping uses all columns, nothing can break uniqueness
  if (length(group_colnames) == ncol(d)) {
    return(character(0))
  }

  rest_cols <- setdiff(colnames(d), group_colnames)

  # Compute within-group distinct counts
  d2 <- d %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_colnames))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(rest_cols), dplyr::n_distinct),
      .groups = "drop"
    )

  # Max distinct count for each column across groups
  max_counts <- d2 %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(rest_cols), max))

  max_counts <- as.matrix(max_counts)

  # Columns with variation (>1)
  colnames(max_counts)[max_counts > 1]

}


rows_append_distinct_df <- function(dmain, dincre, primary_key) {

  if (!is.data.frame(dmain)) {
    stop("`dmain` must be a data.frame or tibble.", call. = FALSE)
  }

  if (!is.data.frame(dincre)) {
    stop("`dincre` must be a data.frame or tibble.", call. = FALSE)
  }

  if (!is.character(primary_key)) {
    stop("`primary_key` must be a character vector of column names.", call. = FALSE)
  }

  if (!all(primary_key %in% names(dmain))) {
    stop("All `primary_key` columns must exist in `dmain`.", call. = FALSE)
  }

  if (!all(primary_key %in% names(dincre))) {
    stop("All `primary_key` columns must exist in `dincre`.", call. = FALSE)
  }

  dincre_excl <- dplyr::anti_join(
    dincre,
    dmain,
    by = primary_key
  )

  dplyr::bind_rows(
    dmain,
    dincre_excl
  )
}


rows_append_distinct_lst <- function(df_list, primary_key) {

  if (!is.list(df_list)) {
    stop("Input must be a list of data.frames.", call. = FALSE)
  }

  if (length(df_list) == 0) {
    return(tibble::tibble())
  }

  purrr::reduce(
    df_list,
    function(dmain, dincre) {
      rows_append_distinct_df(dmain, dincre, primary_key)
    }
  )
}


rows_append_distinct <- function(dmain, dincre = NULL, primary_key) {

  if (is.list(dmain) && is.null(dincre)) {

    return(
      rows_append_distinct_lst(
        df_list = dmain,
        primary_key = primary_key
      )
    )

  } else {

    return(
      rows_append_distinct_df(
        dmain = dmain,
        dincre = dincre,
        primary_key = primary_key
      )
    )

  }
}

