
#' Extract Indian fiscal Month
#'
#' Determines the Indian fiscal month corresponding to a date or a
#' textual representation of a fiscal reporting period. The function
#' supports both date-like inputs and character strings commonly encountered
#' in economic data, corporate disclosures, and administrative records.
#'
#' The Indian fiscal year follows the April–March convention. fiscal
#' months are interpreted relative to this fiscal calendar; however, when
#' year information is included in the output, it reflects the **calendar
#' year associated with the month**, not the fiscal year label.
#'
#' For character inputs, the function recognises month names and month-based
#' expressions embedded in free-form text. When possible, the corresponding
#' calendar year is also extracted and included in the output.
#'
#' Invalid or unrelated text results in \code{NA}.
#'
#' @param x A vector representing time. Can be a date-like object
#'   (\code{Date}, \code{POSIXct}) or a character vector describing a month or
#'   reporting period.
#'
#' @param with_year Logical. If \code{TRUE} (default), the **calendar year**
#'   associated with the month is included in the output when available.
#'   If \code{FALSE}, only the month identifier is returned.
#'
#' @return A character vector representing the fiscal month. Depending on
#'   the input and \code{with_year}, values may include a month label with
#'   calendar year information or only the month component. Elements that
#'   cannot be parsed return \code{NA}.
#'
#' @examples
#' ## Date inputs
#' fiscal_month(as.Date("2024-04-15"))
#' fiscal_month(as.Date("2025-01-10"), with_year = FALSE)
#'
#' ## Character inputs with month information
#' fiscal_month("April 2024")
#' fiscal_month("Jan 2023")
#'
#' ## Mixed real-world inputs
#' x <- c(
#'   "April 2024",
#'   "Dec 2022",
#'   "Results for July 2023",
#'   "random note"
#' )
#' fiscal_month(x)
#'
#' ## Vectorised usage
#' dates <- seq(as.Date("2024-04-01"), by = "month", length.out = 6)
#' fiscal_month(dates)
#'
#' @seealso
#' \code{\link{fiscal_year}},
#' \code{\link{fiscal_quarter}}
#'
#' @export
fiscal_month <- function(x, with_year = TRUE){
  if(is_date_type(x)){
    res <- fiscal_month_for_date(as.Date(x), with_year = with_year)
  } else {
    if(is.character(x)){
      res <- fiscal_month_for_txt(x, with_year = with_year)
    } else {
      stop("Input must be either a date or character vector.", call. = FALSE)
    }
  }
  class(res) <- fiscal_period_class
  res
}




#' Extract Indian fiscal Quarter
#'
#' Determines the Indian fiscal quarter corresponding to a date or a
#' textual representation of a fiscal reporting period. The function
#' supports both date-like inputs and free-form character strings commonly
#' found in economic, corporate, and administrative datasets.
#'
#' The fiscal year follows the April–March convention:
#' \itemize{
#'   \item Q1: April–June
#'   \item Q2: July–September
#'   \item Q3: October–December
#'   \item Q4: January–March
#' }
#'
#' For character inputs, the function recognises quarter labels (e.g. \code{Q1}),
#' month ranges (e.g. \code{Apr-Jun}, \code{October to December}), and fiscal
#' year formats such as \code{FY24}, \code{FY2024}, or \code{2023-24}.
#'
#' A full quarter–fiscal-year label is returned only when both components
#' can be identified. Invalid or unrelated text results in \code{NA}.
#'
#' @param x A vector representing time. Can be a date-like object
#'   (\code{Date}, \code{POSIXct}) or a character vector describing a fiscal
#'   quarter, month range, or fiscal year.
#'
#' @param with_year Logical. If \code{TRUE} (default), the fiscal year is
#'   included in the output when available. If \code{FALSE}, only the quarter
#'   identifier is returned.
#'
#' @return A character vector of fiscal quarter labels.
#'   Possible values include \code{"Q1:2024-25"}, \code{"Q3"}, or \code{NA}.
#'
#' @examples
#' ## Date inputs
#' fiscal_quarter(as.Date("2024-06-30"))
#' fiscal_quarter(as.Date("2025-02-10"), with_year = FALSE)
#'
#' ## Character inputs with quarter labels
#' fiscal_quarter("Q1 FY2024")
#' fiscal_quarter("q4 2022-23")
#'
#' ## Month-range based inputs
#' fiscal_quarter("Apr-Jun FY24")
#' fiscal_quarter("October to December 2023-24")
#'
#' ## Mixed and messy real-world inputs
#' x <- c(
#'   "Q1 FY2024",
#'   "Jul-Sep 2023-24",
#'   "January to March FY2022",
#'   "random note"
#' )
#' fiscal_quarter(x)
#'
#' ## Vectorised usage in data workflows
#' dates <- seq(as.Date("2024-04-01"), by = "month", length.out = 6)
#' fiscal_quarter(dates)
#'
#' @seealso
#' \code{\link{fiscal_year}}
#'
#' @export
fiscal_quarter <- function(x, with_year = TRUE, auto_convert_calendar_quarter = TRUE){
  if(is_date_type(x)){
    res <- fiscal_quarter_for_date(as.Date(x), with_year = with_year)
  } else {
    if (inherits(x , calendar_period_class[1])) {
      res <- fiscal_quarter_for_date(calendar_quarter_to_date(x, anchor = "mid"))
    } else {
      if(is.character(x)){
        res <- fiscal_quarter_for_txt(x, with_year = with_year)
        if(auto_convert_calendar_quarter) {
          res_alt <- calendar_quarter_for_txt(x, with_year = with_year)
          res_alt <- fiscal_quarter_for_date(calendar_quarter_to_date(res_alt, anchor = "mid"))
          res <- ifelse(is.na(res), res_alt, res)
        }
      } else {
        stop("Input must be either a date, character or calendar_period vector.", call. = FALSE)
      }
    }
  }
  class(res) <- fiscal_period_class
  res
}


#' Calendar quarter extractor
#'
#' Extract calendar quarter labels from text or Date inputs.
#'
#' @param x A character vector or Date vector
#' @param with_year Logical; whether to include year (Qx:YYYY) or only Qx
#'
#' @return A character vector of calendar quarters
#'
#' @examples
#' calendar_quarter("GDP grew in Q2:2025")
#' calendar_quarter(as.Date("2025-08-15"))
#' calendar_quarter(as.Date("2025-08-15"), with_year = FALSE)
#'
#' @export
calendar_quarter <- function(x, with_year = TRUE, auto_convert_fiscal_quarter = TRUE) {

  if (is_date_type(x)) {

    res <- calendar_quarter_for_date(
      as.Date(x),
      with_year = with_year
    )

  } else {

    if (inherits(x, fiscal_period_class[1])) {

      res <- calendar_quarter_for_date(
        fiscal_quarter_to_date(x, anchor = "mid"),
        with_year = with_year
      )

    } else {

      if (is.character(x)) {

        res <- calendar_quarter_for_txt(
          x,
          with_year = with_year
        )

        # similar to above add auto conversion from fiscal calender
        if(auto_convert_fiscal_quarter) {
          res_alt <- fiscal_quarter_for_txt(x, with_year = with_year)
          res_alt <- calendar_quarter_for_date(fiscal_quarter_to_date(res_alt, anchor = "mid"))
          res <- ifelse(is.na(res), res_alt, res)
        }

      } else {

        stop(
          "Input must be either a date, character or fiscal_period vector.",
          call. = FALSE
        )
      }
    }
  }

  class(res) <- calendar_period_class
  res
}






#' Extract Indian fiscal Year
#'
#' Determines the Indian fiscal year corresponding to a date or a
#' textual representation of a fiscal reporting period. The function
#' supports both date-like inputs and character strings commonly found in
#' macroeconomic data, corporate disclosures, and administrative records.
#'
#' The Indian fiscal year follows the April–March convention and is
#' returned in the standard \code{"YYYY-YY"} format (e.g. \code{"2023-24"}).
#'
#' For character inputs, the function recognises explicit fiscal year
#' ranges (e.g. \code{"2023-24"}) as well as shorthand notations such as
#' \code{"FY24"} or \code{"FY2024"}. Two-digit fiscal years are interpreted
#' relative to the current century using conservative disambiguation rules.
#'
#' Invalid or unrelated text results in \code{NA}.
#'
#' @param x A vector representing time. Can be a date-like object
#'   (\code{Date}, \code{POSIXct}) or a character vector describing a
#'   fiscal year.
#'
#' @return A character vector of fiscal years in \code{"YYYY-YY"} format,
#'   or \code{NA} when the fiscal year cannot be determined.
#'
#' @examples
#' ## Date inputs
#' fiscal_year(as.Date("2024-06-30"))
#' fiscal_year(as.Date("2025-01-15"))
#'
#' ## Character inputs: explicit ranges
#' fiscal_year("2023-24")
#' fiscal_year("fiscal Year 2022–23")
#'
#' ## Character inputs: FY notation
#' fiscal_year("FY2024")
#' fiscal_year("FY24")
#'
#' ## Mixed real-world inputs
#' x <- c(
#'   "FY2023",
#'   "2021-22",
#'   "Results for FY24",
#'   "random text"
#' )
#' fiscal_year(x)
#'
#' ## Vectorised usage
#' dates <- seq(as.Date("2023-01-01"), by = "quarter", length.out = 6)
#' fiscal_year(dates)
#'
#' @seealso
#' \code{\link{fiscal_quarter}}
#'
#' @export
fiscal_year <- function(x){
  if(is_date_type(x)){
    res <- fiscal_year_for_date(as.Date(x))
  } else {
    if(is.character(x)){
      res <- extract_fy(x)
    } else {
      stop("Input must be either a date or character vector.", call. = FALSE)
    }
  }
  class(res) <- fiscal_period_class
  res
}




#' Parse character input as fiscal periods
#'
#' Coerces a character vector representing fiscal time periods into a
#' standardized fiscal period format.
#'
#' Parsing is attempted in order of decreasing temporal granularity:
#' \itemize{
#'   \item fiscal month (finest resolution)
#'   \item fiscal quarter
#'   \item fiscal year (coarsest resolution, optional)
#' }
#'
#' For each element, the first successful match is retained. Elements that
#' cannot be parsed at any supported resolution are returned as \code{NA}.
#'
#' @param x A character vector representing fiscal periods.
#' @param with_year Logical. If \code{TRUE}, fiscal year detection is used
#'   as a fallback when month and quarter detection fail.
#'
#' @return
#' A character vector of the same length as \code{x}, containing standardized
#' fiscal period labels. The result is assigned class
#' \code{fiscal_period_class}.
#'
#' @details
#' Detection follows a greedy strategy, always preferring the finest
#' available temporal resolution:
#' \enumerate{
#'   \item Month detection is applied to all elements.
#'   \item Quarter detection is applied only to unresolved elements.
#'   \item fiscal year detection is applied last (if enabled).
#' }
#'
#' This ensures consistent frequency preference across mixed inputs.
#'
#' @examples
#' x <- c(
#'   "Jan:2026",
#'   "Q2:2024-25",
#'   "2018-19",
#'   "invalid"
#' )
#'
#' as_fiscal_period(x)
#'
#' @export
as_fiscal_period <- function(x, with_year = TRUE) {

  if(is_date_type(x)){
    res <- as_fiscal_period_for_date(as.Date(x), with_year = with_year)
  } else {
    if(inherits(x, calendar_period_class[1])){
      res <- as_fiscal_period_for_calendar_period(x, with_year = with_year)
    } else {
      if(is.character(x)){
        res <- as_fiscal_period_for_txt(x, with_year = with_year)
      } else {
        stop("Input must be a date, calendar_period, or character vector.", call. = FALSE)
      }
    }
  }
  class(res) <- fiscal_period_class
  res
}


#' Coerce input to a calendar period
#'
#' Converts dates, fiscal periods, or character representations into a
#' standardized calendar period object. The function resolves the finest
#' possible calendar granularity supported by the input, subject to the
#' \code{with_year} flag.
#'
#' Dispatch is performed in the following order:
#' \enumerate{
#'   \item Date-like inputs are interpreted using calendar date rules
#'   \item Objects inheriting from \code{fiscal_period_class} are converted
#'         using fiscal-to-calendar mappings
#'   \item Character inputs are parsed heuristically for calendar periods
#' }
#'
#' @param x A vector of dates, character strings, or objects inheriting from
#'   \code{fiscal_period_class}.
#' @param with_year Logical. If \code{TRUE}, year information is included in the
#'   resulting calendar period where applicable. If \code{FALSE}, only
#'   sub-annual periods (such as months or quarters) are returned.
#'
#' @return A character vector of class \code{calendar_period_class}, with one
#'   element per input. Unparseable inputs return \code{NA}.
#'
#' @details
#' This function is a high-level coercion wrapper. It delegates parsing and
#' conversion to the following internal helpers depending on input type:
#' \itemize{
#'   \item \code{as_calendar_period_for_date()}
#'   \item \code{as_calendar_period_for_fiscal_period()}
#'   \item \code{as_calendar_period_for_txt()}
#' }
#'
#' Character inputs may represent months, quarters, or years using common
#' calendar conventions. When multiple interpretations are possible, the
#' finest granularity is preferred.
#'
#' @examples
#' as_calendar_period(as.Date("2023-06-15"))
#'
#' as_calendar_period("Q2 2023")
#'
#' as_calendar_period("2021")
#'
#' fp <- as_fiscal_period("Q4 2022-23")
#' as_calendar_period(fp)
#'
#' @seealso
#' \code{\link{as_fiscal_period}},
#' \code{\link{as_calendar_period_for_txt}}
#'
#' @export
as_calendar_period <- function(x, with_year = TRUE) {

  if(is_date_type(x)){
    res <- as_calendar_period_for_date(as.Date(x), with_year = with_year)
  } else {
    if(inherits(x, fiscal_period_class[1])){
      res <- as_calendar_period_for_fiscal_period(x, with_year = with_year)
    } else {
      if(is.character(x)){
        res <- as_calendar_period_for_txt(x, with_year = with_year)
      } else {
        stop("Input must be a date, fiscal_period, or character vector.", call. = FALSE)
      }
    }
  }
  class(res) <- calendar_period_class
  res
}


#' Extract frequency from fiscal periods
#'
#' Determines the temporal frequency of each element in a
#' \code{fiscal_period} object based on its textual format.
#'
#' @param x A character vector of class \code{fiscal_period}.
#' @param singular Logical. If \code{TRUE}, returns a single frequency label if
#'  all elements share the same frequency, or "mixed" if they differ. Default
#'  is \code{FALSE} to return individual frequencies.
#'
#' @return
#' A character vector of the same length as \code{x}, with values
#' \code{"month"}, \code{"quarter"}, \code{"year"}, or \code{NA}.
#'
#' @export
frequency.fiscal_period <- function(x, singular = FALSE) {

  n <- length(x)
  out <- rep(NA_character_, n)

  # Month: Jan:YYYY
  is_month <- grepl("^[A-Za-z]{3}:\\d{4}$", x)
  out[is_month] <- "month"

  # Quarter: Q1:YYYY-YY
  is_quarter <- grepl("^Q[1-4]:\\d{4}-\\d{2}$", x)
  out[is_quarter] <- "quarter"

  # Half-year: H1:YYYY-YY or H2:YYYY-YY
  is_halfyear <- grepl("^H[12]:\\d{4}-\\d{2}$", x)
  out[is_halfyear] <- "halfyear"

  # Year: YYYY-YY
  is_year <- grepl("^\\d{4}-\\d{2}$", x)
  out[is_year] <- "year"

  if (singular) {
    unique_freq <- unique(na.omit(out))
    if (length(unique_freq) == 1) {
      return(unique_freq)
    } else {
      return("mixed")
    }
  }

  out
}


#' Extract frequency from calendar periods
#'
#' Determines the temporal frequency of each element in a \code{calendar_period}
#' object based on its textual format.
#'
#' @param x A character vector of class \code{calendar_period}.
#' @param singular Logical. If \code{TRUE}, returns a single frequency label if
#'   all elements share the same frequency, or "mixed" if they differ. Default
#'   is \code{FALSE} to return individual frequencies.
#'
#' @return A character vector of the same length as \code{x}, with values
#' \code{"month"}, \code{"quarter"}, \code{"year"}, or \code{NA}.
#'
#' @export
frequency.calendar_period <- function(x, singular = FALSE) {

  n <- length(x)
  out <- rep(NA_character_, n)

  # Month: Jan:YYYY
  is_month <- grepl("^[A-Za-z]{3}:\\d{4}$", x)
  out[is_month] <- "month"

  # Quarter: Q1:YYYY
  is_quarter <- grepl("^Q[1-4]:\\d{4}$", x)
  out[is_quarter] <- "quarter"

  # Year: YYYY
  is_year <- grepl("^\\d{4}$", x)
  out[is_year] <- "year"

  if(singular){
    # If singular is TRUE, convert to singular form (month -> month, quarter -> quarter, year -> year) (if all are same) otherwise "mixed"
    unique_freq <- unique(na.omit(out))
    if(length(unique_freq) == 1){
      return(unique_freq)
    } else {
      return("mixed")
    }
  }

  out
}


#' Detect Frequency Pattern in Date Vectors
#'
#' Automatically detects the temporal frequency of a Date vector by analyzing
#' the intervals between consecutive dates. Uses the median interval and
#' coefficient of variation to classify the frequency pattern.
#'
#' @param x A vector of Date objects to analyze
#' @param ... Additional arguments (currently unused, for S3 method compatibility)
#'
#' @return A character string indicating the detected frequency:
#' \describe{
#'   \item{\code{"day"}}{Daily data (1-2 day intervals)}
#'   \item{\code{"week"}}{Weekly data (6-8 day intervals)}
#'   \item{\code{"biweek"}}{Bi-weekly data (13-16 day intervals)}
#'   \item{\code{"month"}}{Monthly data (25-35 day intervals)}
#'   \item{\code{"bimonth"}}{Bi-monthly data (60-67 day intervals)}
#'   \item{\code{"quarter"}}{Quarterly data (85-95 day intervals)}
#'   \item{\code{"halfyear"}}{Half-yearly data (175-190 day intervals)}
#'   \item{\code{"year"}}{Annual data (350-380 day intervals)}
#'   \item{\code{"mixed"}}{Irregular or mixed frequency (CV > 0.3)}
#'   \item{\code{NA_character_}}{Unable to determine (insufficient data or no clear pattern)}
#' }
#'
#' @details
#' The function works by:
#' \enumerate{
#'   \item Removing NA values and sorting dates
#'   \item Calculating differences between consecutive dates
#'   \item Computing the median interval (robust to outliers)
#'   \item Checking coefficient of variation (CV = SD/mean) to detect irregular patterns
#'   \item Classifying into standard frequency categories
#' }
#'
#' A CV threshold of 0.3 (30\%) is used to identify mixed/irregular frequencies.
#' The classification ranges are intentionally wide to accommodate real-world
#' variations such as weekends, holidays, month-end adjustments, and leap years.
#'
#' @section Edge Cases:
#' \itemize{
#'   \item Empty vectors or vectors with only NA values return \code{NA_character_}
#'   \item Single date observations return \code{NA_character_} (frequency cannot be determined)
#'   \item All identical dates return \code{NA_character_}
#'   \item Highly variable intervals (CV > 0.3) return \code{"mixed"}
#' }
#'
#' @examples
#' # Monthly data
#' monthly_dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = 12)
#' frequency.Date(monthly_dates)
#' # Returns: "month"
#'
#' # Quarterly data
#' quarterly_dates <- seq.Date(as.Date("2020-01-01"), by = "quarter", length.out = 8)
#' frequency.Date(quarterly_dates)
#' # Returns: "quarter"
#'
#' # Annual data
#' yearly_dates <- seq.Date(as.Date("2015-01-01"), by = "year", length.out = 10)
#' frequency.Date(yearly_dates)
#' # Returns: "year"
#'
#' # Mixed/irregular frequency
#' mixed_dates <- c(
#'   as.Date("2020-01-01"),
#'   as.Date("2020-02-01"),
#'   as.Date("2020-05-01"),
#'   as.Date("2021-01-01")
#' )
#' frequency.Date(mixed_dates)
#' # Returns: "mixed"
#'
#' # Edge case: single observation
#' frequency.Date(as.Date("2020-01-01"))
#' # Returns: NA_character_
#'
#' # Edge case: with NA values (automatically removed)
#' dates_with_na <- c(as.Date("2020-01-01"), NA, as.Date("2020-02-01"),
#'                    as.Date("2020-03-01"), NA)
#' frequency.Date(dates_with_na)
#' # Returns: "month"
#'
#' @seealso \code{\link{diff}}, \code{\link{seq.Date}}
#'
#' @export
frequency.Date <- function(x, ...) {
  # Remove NA values
  x_cl <- x[!is.na(x)]

  # Handle edge cases
  if (length(x_cl) == 0) {
    return(NA_character_)
  }

  if (length(x_cl) == 1) {
    return(NA_character_)  # Can't determine frequency with single observation
  }

  # Sort and calculate differences
  freq_chk <- x_cl %>% sort() %>% diff() %>% as.numeric()

  # Handle case where all dates are identical
  if (all(freq_chk == 0)) {
    return(NA_character_)
  }

  # Check for mixed frequency (only if we have 3+ observations)
  # With only 2 observations (1 difference), we can't assess variability
  if (length(freq_chk) > 1) {
    sd_chk <- sd(freq_chk)
    mean_chk <- mean(freq_chk)
    cv <- sd_chk / mean_chk

    if (cv > 0.3) {  # 30% coefficient of variation threshold
      return("mixed")
    }
  }

  # Use median for classification (more robust to outliers)
  primary_metric <- median(freq_chk)

  # Classify frequency based on median interval
  # Using wider ranges to account for real-world variations
  # (weekends, holidays, month-end variations, leap years)
  if (primary_metric >= 1 && primary_metric <= 2) {
    return("day")
  } else if (primary_metric >= 6 && primary_metric <= 8) {
    return("week")
  } else if (primary_metric >= 13 && primary_metric <= 16) {
    return("biweek")
  } else if (primary_metric >= 25 && primary_metric <= 35) {
    return("month")
  } else if (primary_metric >= 60 && primary_metric <= 67) {
    return("bimonth")
  } else if (primary_metric >= 85 && primary_metric <= 95) {
    return("quarter")
  } else if (primary_metric >= 175 && primary_metric <= 190) {
    return("halfyear")
  } else if (primary_metric >= 350 && primary_metric <= 380) {
    return("year")
  } else {
    return(NA_character_)
  }
}

#' Coerce fiscal_period to Date
#'
#' Converts a \code{fiscal_period} object to a Date using
#' the specified anchor.
#'
#' @param x A character vector of class \code{fiscal_period}.
#' @param anchor One of "first", "mid", or "last".
#' @param ... Unused.
#'
#' @return A Date vector of the same length as \code{x}.
#'
#' @export
as.Date.fiscal_period <- function(
    x,
    anchor = c("last", "first", "mid"),
    ...) {

  anchor <- match.arg(anchor)

  fq <- frequency.fiscal_period(x)

  out <- rep(as.Date(NA), length(x))

  # month
  is_month <- fq == "month"
  if (any(is_month)) {
    out[is_month] <- fiscal_month_to_date(
      x[is_month],
      anchor = anchor
    )
  }

  # quarter
  is_quarter <- fq == "quarter"
  if (any(is_quarter)) {
    out[is_quarter] <- fiscal_quarter_to_date(
      x[is_quarter],
      anchor = anchor
    )
  }

  # half-year
  is_halfyear <- fq == "halfyear"
  if (any(is_halfyear)) {
    out[is_halfyear] <- fiscal_halfyear_to_date(
      x[is_halfyear],
      anchor = anchor
    )
  }

  # year
  is_year <- fq == "year"
  if (any(is_year)) {
    out[is_year] <- fiscal_year_to_date(
      x[is_year],
      anchor = anchor
    )
  }

  out
}



#' Coerce calendar_period to Date
#'
#' Converts a \code{calendar_period} object to a Date using
#' the specified anchor.
#'
#' @param x A character vector of class \code{calendar_period}.
#' @param anchor One of "first", "mid", or "last".
#' @param ... Unused.
#'
#' @return A Date vector of the same length as \code{x}.
#'
#' @export
as.Date.calendar_period <- function(
    x,
    anchor = c("last", "first", "mid"),
    ...
) {

  anchor <- match.arg(anchor)

  fq <- frequency.calendar_period(x)

  out <- rep(as.Date(NA), length(x))

  ## month: reuse fiscal_month_to_date (grammar identical)
  is_month <- fq == "month"
  if (any(is_month)) {
    out[is_month] <- fiscal_month_to_date(
      x[is_month],
      anchor = anchor
    )
  }

  ## quarter
  is_quarter <- fq == "quarter"
  if (any(is_quarter)) {
    out[is_quarter] <- calendar_quarter_to_date(
      x[is_quarter],
      anchor = anchor
    )
  }

  ## year: YYYY (inline, no helper)
  is_year <- fq == "year"
  if (any(is_year)) {

    year <- as.integer(x[is_year])

    start_date <- as.Date(sprintf("%04d-01-01", year))
    end_date   <- as.Date(sprintf("%04d-12-31", year))

    out[is_year] <- switch(
      anchor,
      first = start_date,
      last  = end_date,
      mid   = start_date + as.integer((end_date - start_date) / 2)
    )
  }

  out
}

#' Previous period conversion
#'
#' Returns the immediately preceding period of the same frequency for each
#' element of a fiscal or calendar period vector.
#'
#' For example:
#' \itemize{
#'   \item The previous period of \code{"Q2:2024-25"} is \code{"Q1:2024-25"}
#'   \item The previous period of \code{"Q1:2024-25"} is \code{"Q4:2023-24"}
#'   \item The previous period of \code{"Jan:2014"} is \code{"Dec:2013"}
#' }
#'
#' The function is an S3-style dispatcher and selects the appropriate internal
#' method based on the class of \code{x}. Supported classes are
#' \code{fiscal_period} and \code{calendar_period}. Supplying any other
#' input will result in an error.
#'
#' @param x A character vector of class \code{fiscal_period} or
#'   \code{calendar_period}.
#'
#' @return
#' A character vector of the same length and class as \code{x}, containing the
#' immediately preceding periods.
#'
#' @examples
#' # Monthly fiscal periods
#' x <- as_fiscal_period(c("Jan:2014", "Feb:2014", "Mar:2014"))
#' previous_period(x)
#'
#' # Quarterly fiscal periods
#' q <- as_fiscal_period(c("Q1:2024-25", "Q2:2024-25"))
#' previous_period(q)
#'
#' # Calendar periods
#' y <- as_calendar_period(c("2022", "2023"))
#' previous_period(y)
#'
#' @export
previous_period <- function(x) {
  if(inherits(x, fiscal_period_class[1])){
    res <- previous_period_for_fiscal_period(x)
  } else if (inherits(x, calendar_period_class[1])) {
    res <- previous_period_for_calendar_period(x)
  } else {
    stop("Input must be either a fiscal_period or calendar_period vector.", call. = FALSE)
  }
  res
}


#' Previous year conversion
#'
#' Returns the period corresponding to the same frequency in the previous year
#' for each element of a fiscal or calendar period vector.
#'
#' Unlike \code{\link{previous_period}}, which moves back by one period unit
#' (for example, one month or one quarter), \code{previous_year} moves back
#' approximately one year while preserving the original frequency.
#'
#' For example:
#' \itemize{
#'   \item \code{"Jan:2014"} becomes \code{"Jan:2013"}
#'   \item \code{"Q2:2024-25"} becomes \code{"Q2:2023-24"}
#'   \item \code{"2023"} becomes \code{"2022"}
#' }
#'
#' Internally, this is implemented by shifting the underlying date
#' representation by a one-year lag (365 days) and re-mapping it back to the
#' appropriate period format.
#'
#' @param x A character vector of class \code{fiscal_period} or
#'   \code{calendar_period}.
#'
#' @return
#' A character vector of the same length and class as \code{x}, containing the
#' corresponding periods from the previous year.
#'
#' @examples
#' # Monthly fiscal periods
#' x <- as_fiscal_period(c("Jan:2014", "Feb:2014"))
#' previous_year(x)
#'
#' # Quarterly fiscal periods
#' q <- as_fiscal_period("Q3:2024-25")
#' previous_year(q)
#'
#' # Calendar years
#' y <- as_calendar_period(c("2022", "2023"))
#' previous_year(y)
#'
#' @seealso
#' \code{\link{previous_period}}
#'
#' @export
previous_year <- function(x) {
  if(inherits(x, fiscal_period_class[1])){
    res <- previous_period_for_fiscal_period(x, lag_len = c("month" = 365, "quarter" = 365, "halfyear" = 365, "year" = 365))
  } else if (inherits(x, calendar_period_class[1])) {
    res <- previous_period_for_calendar_period(x, lag_len = c("month" = 365, "quarter" = 365, "year" = 365))
  } else {
    stop("Input must be either a fiscal_period or calendar_period vector.", call. = FALSE)
  }
  res
}


#' Check whether a period vector is continuous
#'
#' Determines whether a vector of calendar or fiscal periods represents a
#' continuous sequence with no missing gaps, allowing for reasonable variation
#' in period lengths.
#'
#' Continuity is evaluated by converting periods to mid-point dates and checking
#' whether the gaps between consecutive periods are consistent with the expected
#' spacing implied by the period frequency.
#'
#' Supported frequencies are month, quarter, half-year, and year. Mixed
#' frequencies are treated as non-continuous.
#'
#' @param fp A vector inheriting from a recognized period class
#'   (calendar or fiscal).
#'
#' @return Logical scalar. \code{TRUE} if the periods form a continuous sequence,
#'   otherwise \code{FALSE}.
#'
#' @details
#' A tolerance of 20 percent is allowed when comparing observed gaps to expected
#' gaps in order to account for varying month lengths, leap years, and calendar
#' irregularities.
#'
#' The input does not need to be ordered; periods are sorted internally before
#' continuity is assessed.
#'
#' @examples
#' x <- as_calendar_period(c("Jan 2020", "Feb 2020", "Mar 2020"))
#' is_continuous(x)
#'
#' @export
is_continuous <- function(fp) {

  if(!is_period_type(fp)){
    stop("Input is not a recognized period type (fiscal or calendar).", call. = FALSE)
  }

  fp_date <- as.Date(fp, anchor = "mid")
  freq <- stats::frequency(fp, singular = TRUE)

  if(freq == "mixed"){
    return(FALSE)
  }

  expected_gap <- dplyr::case_when(
    freq == "month" ~ 30,
    freq == "quarter" ~ 90,
    freq == "halfyear" ~ 182,
    freq == "year" ~ 365,
    TRUE ~ NA_real_
  )

  if( is.na(expected_gap))  return(FALSE)

  # Check if gaps between consecutive dates are consistent with expected gap

  gap_chk <- fp_date %>% sort() %>% diff()
  gap_pct <- gap_chk / expected_gap

  if(any(abs(gap_pct-1) > 0.2)){
    # allow 20% deviation in gap length to account for varying month lengths, leap years, etc.
    return(FALSE)
  } else {
    return(TRUE)
  }


}

