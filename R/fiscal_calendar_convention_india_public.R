
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
financial_quarter <- function(x, with_year = TRUE){
  if(is_date_type(x)){
    res <- financial_quarter_for_date(as.Date(x), with_year = with_year)
  } else {
    if(is.character(x)){
      res <- financial_quarter_for_txt(x, with_year = with_year)
    } else {
      stop("Input must be either a date or character vector.", call. = FALSE)
    }
  }
  class(res) <- financial_period_class
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
#'
#' @return
#' A character vector of the same length as \code{x}, with values
#' \code{"month"}, \code{"quarter"}, \code{"year"}, or \code{NA}.
#'
#' @export
frequency.financial_period <- function(x) {

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

  out
}

