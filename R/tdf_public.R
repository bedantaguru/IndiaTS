
#' Coerce an object to a time-indexed data frame (tdf)
#'
#' Converts a \code{data.frame} or \code{ts} object into a standardized
#' time-indexed data frame (\code{tdf}), where one column represents time
#' (date or period) and remaining columns represent numeric variables.
#'
#' The function detects or infers the time column, converts it to either
#' a calendar or fiscal period when possible, and attaches metadata
#' describing time type and continuity.
#'
#' @param df A \code{data.frame}, \code{tibble}, or \code{ts} object.
#'   If a \code{ts} object is supplied, it is first converted using
#'   \code{ts_to_tdf_part()}.
#' @param allow_discontinuous Logical. If \code{FALSE}, a warning is issued
#'   when the detected time index is not continuous.
#' @param fiscal_type Logical. If \code{TRUE}, time conversion attempts are
#'   performed using fiscal period rules; otherwise calendar period rules
#'   are used.
#'
#' @return A \code{tdf} object (a tibble inheriting from \code{tdf_class})
#'   with:
#'   \itemize{
#'     \item a single \code{time} column (date or period),
#'     \item one or more numeric value columns,
#'     \item attributes describing time type and continuity.
#'   }
#'
#' @details
#' The function follows these steps:
#' \enumerate{
#'   \item If the input is a \code{ts} object, it is converted to a data frame.
#'   \item If the input is already a \code{tdf}, it is returned unchanged.
#'   \item The input is coerced to a tibble and scanned for time-like columns.
#'   \item If no explicit time column is found, the first column is assumed
#'         to represent time and is converted accordingly.
#'   \item Non-numeric, non-time columns are discarded with a warning.
#'   \item The time column is converted to a fiscal or calendar period when
#'         possible; otherwise it is converted to a \code{Date}.
#'   \item Continuity of the time index is checked and recorded.
#' }
#'
#' The input does not need to be ordered; continuity is assessed internally.
#'
#' @section Attributes:
#' The returned object includes the following attributes:
#' \describe{
#'   \item{time_type}{One of \code{"period"} or \code{"date"}, indicating how
#'   the time column is represented.}
#'   \item{continuity}{Logical scalar indicating whether the time index is
#'   continuous.}
#' }
#'
#' @examples
#' df <- data.frame(
#'   period = c("Jan 2023", "Feb 2023", "Mar 2023"),
#'   value  = c(10, 12, 15)
#' )
#'
#' as_tdf(df)
#'
#' ts_obj <- ts(c(100, 105, 110), start = 2022, frequency = 1)
#' as_tdf(ts_obj)
#'
#'
#' @export
as_tdf <- function(df, allow_discontinuous = TRUE, fiscal_type = TRUE){

  if(is.ts(df)){
    df <- ts_to_tdf_part(df, fiscal_type = fiscal_type)
  } else {
    if(!is.data.frame(df)){
      stop("Input must be a data.frame!!")
    }

  }

  if(is_tdf(df)){
    cat("Input is already a tdf. Returning the input.\n")
    return(df)
  }

  df <- tibble::as_tibble(df)

  dt_cols <- df %>% purrr::map_lgl(is_time)


  if(sum(dt_cols)>1){
    stop("More than one date-column found!", call. = FALSE)
  }

  # read/conversion mode
  if(sum(dt_cols)==0){
    cat("No date/time column found. Converting into tdf assuming first column is date/period type.\n")
    df <- as_tdf_convert_time(df, fiscal = fiscal_type)
    dt_cols <- df %>% purrr::map_lgl(is_time)
  }

  var_cols <- df %>% purrr::map_lgl(is.numeric)

  cns <- colnames(df)
  cns_dt <- cns[which(dt_cols)]
  cns_var <- cns[which(var_cols)]
  cns_rest <- cns %>% setdiff(c(cns_dt, cns_var))

  if(length(cns_rest)>0){
    warning(paste0("These columns are non-numerical. Discarding them: ",
                   paste0(cns_rest, collapse = ", ")))
  }

  df <- df[c(cns_dt, cns_var)]

  colnames(df)[1] <- "time"

  tdf_time_type <- "period"

  if(!is_period_type(df$time)){
    # try to convert to period (based on fiscal_type) if fail convert to date
    tdf_time_type <-
      tryCatch(
        {
          df$time <- if(fiscal_type) as_fiscal_period(df$time) else as_calendar_period(df$time)
          "period"
        },
        error = function(e){
          df$time <- lubridate::as_date(df$time)
          "date"
        }
      )
  }

  attr(df, "time_type") <- if(exists("tdf_time_type")) tdf_time_type else "unknown"

  attr(df, "shape") <- "wide"

  cont_chk <- tryCatch({
       is_continuous_time(df$time)},
    error = function(e){
      # warning("Continuity check failed. Setting continuity attribute to NA", call. = FALSE)
      NA
    }
  )
  attr(df, "continuity") <- cont_chk

  if(!allow_discontinuous  & !cont_chk){
    warning("The time column is not continuous. Consider setting allow_discontinuous = TRUE to suppress this warning.", call. = FALSE)
  }

  class(df) <- c(tdf_class, class(df))

  df

}


#' @export
frequency.tdf <- function(x, ...){
  tm <- x$time
  if(is_period_type(tm)){
    stats::frequency(tm, singular = TRUE)
  } else {
    stats::frequency(tm)
  }
}


#' @export
sort.tdf <- function(x, decreasing = FALSE, ...){
  x$time_as_date <- as.Date(x$time)
  x <- x[order(x$time_as_date, decreasing = decreasing), ]
  x$time_as_date <- NULL
  x
}


#' Convert a tdf object to a base R ts
#'
#' Converts a \code{tdf} object into a base R \code{ts} by inferring the
#' data frequency from its time index and computing the appropriate
#' start value. Calendar and fiscal periods are supported.
#'
#' Supported frequencies are monthly, quarterly, halfyearly, and yearly.
#' Period-based indices are internally anchored using
#' \code{as.Date(..., anchor = "mid")}.
#'
#' Fiscal years and halfyears adjust the start year to align with
#' base R \code{ts} conventions.
#'
#' @param tdf_obj
#' A \code{tdf} object with a \code{time} column and one or more numeric
#' series.
#'
#' @return
#' A base R \code{ts} object.
#'
#' @examples
#' ts(1:40, start = c(2010, 1), frequency = 12) %>%
#'   as_tdf() %>%
#'   to_ts()
#'
#' x <- data.frame(
#'   time  = c("H1:2020-21", "H2:2020-21"),
#'   value = c(100, 120)
#' ) %>%
#'   as_tdf()
#'
#' to_ts(x)
#'
#' @name to_ts
#' @export
to_ts <- function(tdf_obj){

  tdf_obj <- sort(tdf_obj)

  freq <- stats::frequency(tdf_obj)
  tdf_obj <- sort(tdf_obj)

  tdf_obj_rest <- tdf_obj
  tdf_obj_rest$time <- NULL

  if(is_period_type(tdf_obj$time)){
    tm_as_dt <- as.Date(tdf_obj$time, anchor = "mid")
  } else {
    tm_as_dt <- as.Date(tdf_obj$time[1])
  }
  start_time <- tm_as_dt[1]

  is_fiscal   <- inherits(tdf_obj$time, fiscal_period_class[1])

  if (freq == "month") {

    this_ts <- ts(
      tdf_obj_rest,
      start = c(lubridate::year(start_time),
                lubridate::month(start_time)),
      frequency = 12
    )

  } else if (freq == "quarter") {

    this_ts <- ts(
      tdf_obj_rest,
      start = c(lubridate::year(start_time),
                lubridate::quarter(start_time)),
      frequency = 4
    )

  } else if (freq == "halfyear") {


    if(!is_fiscal){
      this_ts <- ts(
        tdf_obj_rest,
        start = c(
          lubridate::year(start_time),
          ifelse(lubridate::month(start_time) <= 6, 1, 2)
        ),
        frequency = 2
      )
    } else {
      this_ts <- ts(
        tdf_obj_rest,
        start = c(
          # If as.Date is anchored to mid, then H1:2020-21 will be anchored to 2020-07-01 and H2:2020-21 will be anchored to 2020-12-01. So we need to add 1 to the year.
          lubridate::year(start_time) + 1,
          ifelse(
            fiscal_halfyear_for_date(start_time, with_year = FALSE) == "H1",
            1, 2
          )
        ),
        frequency = 2
      )
    }


  } else if (freq == "year") {

    this_ts <- ts(
      tdf_obj_rest,
      start = lubridate::year(start_time) + ifelse(is_fiscal, 1, 0),
      frequency = 1
    )

  } else {

    stop(
      "Unsupported frequency detected. Supported: monthly, quarterly, halfyearly, yearly.",
      call. = FALSE
    )
  }

  this_ts
}


#' @rdname to_ts
#' @export
as.ts.tdf <- function(x, ...) {
  to_ts(x)
}
