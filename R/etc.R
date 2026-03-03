

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
