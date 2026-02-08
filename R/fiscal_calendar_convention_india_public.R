
#' Extract Indian Financial Month
#'
#' Determines the Indian financial month corresponding to a date or a
#' textual representation of a financial reporting period. The function
#' supports both date-like inputs and character strings commonly encountered
#' in economic data, corporate disclosures, and administrative records.
#'
#' The Indian financial year follows the April–March convention. Financial
#' months are interpreted relative to this fiscal calendar; however, when
#' year information is included in the output, it reflects the **calendar
#' year associated with the month**, not the financial year label.
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
#' @return A character vector representing the financial month. Depending on
#'   the input and \code{with_year}, values may include a month label with
#'   calendar year information or only the month component. Elements that
#'   cannot be parsed return \code{NA}.
#'
#' @examples
#' ## Date inputs
#' financial_month(as.Date("2024-04-15"))
#' financial_month(as.Date("2025-01-10"), with_year = FALSE)
#'
#' ## Character inputs with month information
#' financial_month("April 2024")
#' financial_month("Jan 2023")
#'
#' ## Mixed real-world inputs
#' x <- c(
#'   "April 2024",
#'   "Dec 2022",
#'   "Results for July 2023",
#'   "random note"
#' )
#' financial_month(x)
#'
#' ## Vectorised usage
#' dates <- seq(as.Date("2024-04-01"), by = "month", length.out = 6)
#' financial_month(dates)
#'
#' @seealso
#' \code{\link{financial_year}},
#' \code{\link{financial_quarter}}
#'
#' @export
financial_month <- function(x, with_year = TRUE){
  if(is_date_type(x)){
    res <- financial_month_for_date(as.Date(x), with_year = with_year)
  } else {
    if(is.character(x)){
      res <- financial_month_for_txt(x, with_year = with_year)
    } else {
      stop("Input must be either a date or character vector.", call. = FALSE)
    }
  }
  class(res) <- financial_period_class
  res
}




#' Extract Indian Financial Quarter
#'
#' Determines the Indian financial quarter corresponding to a date or a
#' textual representation of a financial reporting period. The function
#' supports both date-like inputs and free-form character strings commonly
#' found in economic, corporate, and administrative datasets.
#'
#' The financial year follows the April–March convention:
#' \itemize{
#'   \item Q1: April–June
#'   \item Q2: July–September
#'   \item Q3: October–December
#'   \item Q4: January–March
#' }
#'
#' For character inputs, the function recognises quarter labels (e.g. \code{Q1}),
#' month ranges (e.g. \code{Apr-Jun}, \code{October to December}), and financial
#' year formats such as \code{FY24}, \code{FY2024}, or \code{2023-24}.
#'
#' A full quarter–financial-year label is returned only when both components
#' can be identified. Invalid or unrelated text results in \code{NA}.
#'
#' @param x A vector representing time. Can be a date-like object
#'   (\code{Date}, \code{POSIXct}) or a character vector describing a financial
#'   quarter, month range, or financial year.
#'
#' @param with_year Logical. If \code{TRUE} (default), the financial year is
#'   included in the output when available. If \code{FALSE}, only the quarter
#'   identifier is returned.
#'
#' @return A character vector of financial quarter labels.
#'   Possible values include \code{"Q1:2024-25"}, \code{"Q3"}, or \code{NA}.
#'
#' @examples
#' ## Date inputs
#' financial_quarter(as.Date("2024-06-30"))
#' financial_quarter(as.Date("2025-02-10"), with_year = FALSE)
#'
#' ## Character inputs with quarter labels
#' financial_quarter("Q1 FY2024")
#' financial_quarter("q4 2022-23")
#'
#' ## Month-range based inputs
#' financial_quarter("Apr-Jun FY24")
#' financial_quarter("October to December 2023-24")
#'
#' ## Mixed and messy real-world inputs
#' x <- c(
#'   "Q1 FY2024",
#'   "Jul-Sep 2023-24",
#'   "January to March FY2022",
#'   "random note"
#' )
#' financial_quarter(x)
#'
#' ## Vectorised usage in data workflows
#' dates <- seq(as.Date("2024-04-01"), by = "month", length.out = 6)
#' financial_quarter(dates)
#'
#' @seealso
#' \code{\link{financial_year}}
#'
#' @export
financial_quarter <- function(x, with_year = TRUE, auto_convert_calendar_quarter = TRUE){
  if(is_date_type(x)){
    res <- financial_quarter_for_date(as.Date.(x), with_year = with_year)
  } else {
    if (inherits(x , calendar_period_class)) {
      res <- financial_quarter_for_date(calendar_quarter_to_date(x, anchor = "mid"))
    } else {
      if(is.character(x)){
        res <- financial_quarter_for_txt(x, with_year = with_year)
        if(auto_convert_calendar_quarter) {
          res_alt <- calendar_quarter_for_txt(x, with_year = with_year)
          res_alt <- financial_quarter_for_date(calendar_quarter_to_date(res_alt, anchor = "mid"))
          res <- ifelse(is.na(res), res_alt, res)
        }
      } else {
        stop("Input must be either a date, character or calendar_period vector.", call. = FALSE)
      }
    }
  }
  class(res) <- financial_period_class
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
calendar_quarter <- function(x, with_year = TRUE, auto_convert_financial_quarter = TRUE) {

  if (is_date_type(x)) {

    res <- calendar_quarter_for_date(
      as.Date(x),
      with_year = with_year
    )

  } else {

    if (inherits(x, financial_period_class)) {

      res <- calendar_quarter_for_date(
        financial_quarter_to_date(x, anchor = "mid"),
        with_year = with_year
      )

    } else {

      if (is.character(x)) {

        res <- calendar_quarter_for_txt(
          x,
          with_year = with_year
        )

        # similar to above add auto conversion from fiscal calender
        if(auto_convert_financial_quarter) {
          res_alt <- financial_quarter_for_txt(x, with_year = with_year)
          res_alt <- calendar_quarter_for_date(financial_quarter_to_date(res_alt, anchor = "mid"))
          res <- ifelse(is.na(res), res_alt, res)
        }

      } else {

        stop(
          "Input must be either a date, character or financial_period vector.",
          call. = FALSE
        )
      }
    }
  }

  class(res) <- calendar_period_class
  res
}






#' Extract Indian Financial Year
#'
#' Determines the Indian financial year corresponding to a date or a
#' textual representation of a financial reporting period. The function
#' supports both date-like inputs and character strings commonly found in
#' macroeconomic data, corporate disclosures, and administrative records.
#'
#' The Indian financial year follows the April–March convention and is
#' returned in the standard \code{"YYYY-YY"} format (e.g. \code{"2023-24"}).
#'
#' For character inputs, the function recognises explicit financial year
#' ranges (e.g. \code{"2023-24"}) as well as shorthand notations such as
#' \code{"FY24"} or \code{"FY2024"}. Two-digit financial years are interpreted
#' relative to the current century using conservative disambiguation rules.
#'
#' Invalid or unrelated text results in \code{NA}.
#'
#' @param x A vector representing time. Can be a date-like object
#'   (\code{Date}, \code{POSIXct}) or a character vector describing a
#'   financial year.
#'
#' @return A character vector of financial years in \code{"YYYY-YY"} format,
#'   or \code{NA} when the financial year cannot be determined.
#'
#' @examples
#' ## Date inputs
#' financial_year(as.Date("2024-06-30"))
#' financial_year(as.Date("2025-01-15"))
#'
#' ## Character inputs: explicit ranges
#' financial_year("2023-24")
#' financial_year("Financial Year 2022–23")
#'
#' ## Character inputs: FY notation
#' financial_year("FY2024")
#' financial_year("FY24")
#'
#' ## Mixed real-world inputs
#' x <- c(
#'   "FY2023",
#'   "2021-22",
#'   "Results for FY24",
#'   "random text"
#' )
#' financial_year(x)
#'
#' ## Vectorised usage
#' dates <- seq(as.Date("2023-01-01"), by = "quarter", length.out = 6)
#' financial_year(dates)
#'
#' @seealso
#' \code{\link{financial_quarter}}
#'
#' @export
financial_year <- function(x){
  if(is_date_type(x)){
    res <- financial_year_for_date(as.Date(x))
  } else {
    if(is.character(x)){
      res <- extract_fy(x)
    } else {
      stop("Input must be either a date or character vector.", call. = FALSE)
    }
  }
  class(res) <- financial_period_class
  res
}




#' Parse character input as financial periods
#'
#' Coerces a character vector representing financial time periods into a
#' standardized financial period format.
#'
#' Parsing is attempted in order of decreasing temporal granularity:
#' \itemize{
#'   \item Financial month (finest resolution)
#'   \item Financial quarter
#'   \item Financial year (coarsest resolution, optional)
#' }
#'
#' For each element, the first successful match is retained. Elements that
#' cannot be parsed at any supported resolution are returned as \code{NA}.
#'
#' @param x A character vector representing financial periods.
#' @param with_year Logical. If \code{TRUE}, financial year detection is used
#'   as a fallback when month and quarter detection fail.
#'
#' @return
#' A character vector of the same length as \code{x}, containing standardized
#' financial period labels. The result is assigned class
#' \code{financial_period_class}.
#'
#' @details
#' Detection follows a greedy strategy, always preferring the finest
#' available temporal resolution:
#' \enumerate{
#'   \item Month detection is applied to all elements.
#'   \item Quarter detection is applied only to unresolved elements.
#'   \item Financial year detection is applied last (if enabled).
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
#' as_financial_period(x)
#'
#' @export
as_financial_period <- function(x, with_year = TRUE) {

  if (!is.character(x)) {
    stop("Input must be a character vector.", call. = FALSE)
  }

  n <- length(x)
  out <- rep(NA_character_, n)

  # Try month first (finest granularity)
  month_result <- financial_month_for_txt(x, with_year = with_year)
  out <- ifelse(is.na(out) & !is.na(month_result), month_result, out)

  # Try quarter if month failed
  still_na <- is.na(out)
  if (any(still_na)) {
    quarter_result <- financial_quarter_for_txt(x[still_na], with_year = with_year)
    out[still_na] <- ifelse(!is.na(quarter_result), quarter_result, out[still_na])
  }

  # Try year if both month and quarter failed
  still_na <- is.na(out)
  if (any(still_na) && with_year) {
    year_result <- extract_fy(x[still_na])
    out[still_na] <- ifelse(!is.na(year_result), year_result, out[still_na])
  }

  class(out) <- financial_period_class
  out
}



#' Extract frequency from financial periods
#'
#' Determines the temporal frequency of each element in a
#' \code{financial_period} object based on its textual format.
#'
#' @param x A character vector of class \code{financial_period}.
#' @param singular Logical. If \code{TRUE}, returns a single frequency label if
#'  all elements share the same frequency, or "mixed" if they differ. Default
#'  is \code{FALSE} to return individual frequencies.
#'
#' @return
#' A character vector of the same length as \code{x}, with values
#' \code{"month"}, \code{"quarter"}, \code{"year"}, or \code{NA}.
#'
#' @export
frequency.financial_period <- function(x, singular = FALSE) {

  n <- length(x)
  out <- rep(NA_character_, n)

  # Month: Jan:YYYY
  is_month <- grepl("^[A-Za-z]{3}:\\d{4}$", x)
  out[is_month] <- "month"

  # Quarter: Q1:YYYY-YY
  is_quarter <- grepl("^Q[1-4]:\\d{4}-\\d{2}$", x)
  out[is_quarter] <- "quarter"

  # Year: YYYY-YY
  is_year <- grepl("^\\d{4}-\\d{2}$", x)
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

  return(out)

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



#' Coerce financial_period to Date
#'
#' Converts a \code{financial_period} object to a Date using
#' the specified anchor.
#'
#' @param x A character vector of class \code{financial_period}.
#' @param anchor One of "first", "mid", or "last".
#' @param ... Unused.
#'
#' @return A Date vector of the same length as \code{x}.
#'
#' @export
as.Date.financial_period <- function(
    x,
    anchor = c("last", "first", "mid"),
    ...) {

  anchor <- match.arg(anchor)

  fq <- frequency.financial_period(x)

  out <- rep(as.Date(NA), length(x))

  # month
  is_month <- fq == "month"
  if (any(is_month)) {
    out[is_month] <- financial_month_to_date(
      x[is_month],
      anchor = anchor
    )
  }

  # quarter
  is_quarter <- fq == "quarter"
  if (any(is_quarter)) {
    out[is_quarter] <- financial_quarter_to_date(
      x[is_quarter],
      anchor = anchor
    )
  }

  # year
  is_year <- fq == "year"
  if (any(is_year)) {
    out[is_year] <- financial_year_to_date(
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

  ## month: reuse financial_month_to_date (grammar identical)
  is_month <- fq == "month"
  if (any(is_month)) {
    out[is_month] <- financial_month_to_date(
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
#' element of a financial or calendar period vector.
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
#' \code{financial_period} and \code{calendar_period}. Supplying any other
#' input will result in an error.
#'
#' @param x A character vector of class \code{financial_period} or
#'   \code{calendar_period}.
#'
#' @return
#' A character vector of the same length and class as \code{x}, containing the
#' immediately preceding periods.
#'
#' @examples
#' # Monthly financial periods
#' x <- as_financial_period(c("Jan:2014", "Feb:2014", "Mar:2014"))
#' previous_period(x)
#'
#' # Quarterly financial periods
#' q <- as_financial_period(c("Q1:2024-25", "Q2:2024-25"))
#' previous_period(q)
#'
#' # Calendar periods
#' y <- as_calendar_period(c("2022", "2023"))
#' previous_period(y)
#'
#' @export
previous_period <- function(x) {
  if(inherits(x, financial_period_class)){
    res <- previous_period_for_financial_period(x)
  } else if (inherits(x, calendar_period_class)) {
    res <- previous_period_for_calendar_period(x)
  } else {
    stop("Input must be either a financial_period or calendar_period vector.", call. = FALSE)
  }
  res
}


#' Previous year conversion
#'
#' Returns the period corresponding to the same frequency in the previous year
#' for each element of a financial or calendar period vector.
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
#' @param x A character vector of class \code{financial_period} or
#'   \code{calendar_period}.
#'
#' @return
#' A character vector of the same length and class as \code{x}, containing the
#' corresponding periods from the previous year.
#'
#' @examples
#' # Monthly financial periods
#' x <- as_financial_period(c("Jan:2014", "Feb:2014"))
#' previous_year(x)
#'
#' # Quarterly financial periods
#' q <- as_financial_period("Q3:2024-25")
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
  if(inherits(x, financial_period_class)){
    res <- previous_period_for_financial_period(x, lag_len = c("month" = 365, "quarter" = 365, "year" = 365))
  } else if (inherits(x, calendar_period_class)) {
    res <- previous_period_for_calendar_period(x, lag_len = c("month" = 365, "quarter" = 365, "year" = 365))
  } else {
    stop("Input must be either a financial_period or calendar_period vector.", call. = FALSE)
  }
  res
}
