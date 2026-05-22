
#' Coerce an object to a time-indexed data frame (tdf)
#'
#' Converts a \code{data.frame}, \code{ts}, or \code{tdf_long} object into a
#' standardized time-indexed data frame (\code{tdf}), where one column
#' represents time (date or period) and remaining columns represent numeric
#' variables.
#'
#' The function detects or infers the time column, converts it to either
#' a calendar or fiscal period when possible, and attaches metadata
#' describing time type and continuity.
#'
#' @param df A \code{data.frame}, \code{tibble}, \code{ts}, or \code{tdf_long}
#'   object. If a \code{ts} object is supplied, it is first converted using
#'   \code{ts_to_tdf_part()}. If a \code{tdf_long} object is supplied, it is
#'   filtered, de-duplicated, and pivoted to wide form before conversion.
#' @param allow_discontinuous Logical. If \code{FALSE}, a warning is issued
#'   when the detected time index is not continuous. Used by the
#'   \code{data.frame} and \code{ts} methods.
#' @param fiscal_type Logical. If \code{TRUE}, time conversion attempts are
#'   performed using fiscal period rules; otherwise calendar period rules
#'   are used. Used by the \code{data.frame} and \code{ts} methods.
#' @param value_name Character scalar. Name of the value column to extract
#'   from a \code{tdf_long} input, with or without the \code{"value."}
#'   prefix (e.g. \code{"gdp"} or \code{"value.gdp"}). Matching is
#'   case-insensitive. If \code{NULL} (default), the first column whose name
#'   begins with \code{"value."} is used. If more than one name is supplied,
#'   only the first is used and a warning is issued. Used by the
#'   \code{tdf_long} method.
#' @param names Character vector. Series names (\code{meta.name} values) to
#'   retain from a \code{tdf_long} input. If \code{NULL} (default), all names
#'   within the selected \code{disaggregation_group}(s) are retained; if
#'   \code{disaggregation_group} is also \code{NULL}, every name in the input
#'   is retained. Used by the \code{tdf_long} method.
#' @param disaggregation_group Character vector. Disaggregation group(s)
#'   (\code{meta.disaggregation_group} values) to retain from a
#'   \code{tdf_long} input. If \code{NULL} (default) and \code{names} is also
#'   \code{NULL}, all disaggregation groups are retained. Used by the
#'   \code{tdf_long} method.
#' @param divergence_tol_pct Numeric scalar. Tolerance, in percent, for
#'   within-\code{(time, meta.name)} value divergence when collapsing
#'   duplicate observations in a \code{tdf_long} input. Divergence is
#'   computed as \code{(max - min) / max * 100}. Groups exceeding this
#'   threshold trigger the variation-reduction strategy (latest release tag
#'   where orderable, otherwise an averaging fallback with a warning).
#'   Defaults to \code{5}. Used by the \code{tdf_long} method.
#' @param ... Additional arguments passed to methods.
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
#' For \code{data.frame} and \code{ts} inputs, the function follows these
#' steps:
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
#' For \code{tdf_long} inputs, the chosen value column is renamed internally,
#' rows are filtered by \code{names} and \code{disaggregation_group}, any
#' residual within-\code{(time, meta.name)} variation is resolved using
#' release-tag ordering (or averaging as a fallback), and the result is
#' pivoted to wide form before being passed through the standard conversion
#' path.
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
as_tdf <- function(df, allow_discontinuous = TRUE, fiscal_type = TRUE, value_name = NULL, names = NULL, disaggregation_group = NULL, divergence_tol_pct = 5, ...){
  UseMethod("as_tdf")
}

#' @export
as_tdf.tdf_long <- function(df, value_name = NULL, names = NULL, disaggregation_group = NULL, divergence_tol_pct = 5, ...){

  if(is.null(value_name)){
    value_name <- colnames(df)[stringr::str_detect(colnames(df), "value\\.")]
    value_name <- value_name[1]
  }

  if(length(value_name)>1){
    warning("More than one value name not supported! Taking First one.", call. = FALSE)
    value_name <- value_name[1]
  }

  value_name <- value_name |>
    tolower() |>
    stringr::str_remove_all("value\\.")

  value_col <- paste0("value.", value_name)

  if(!(value_col %in% colnames(df))){
    stop("Value column not found!", call. = FALSE)
  }

  df <- df |>
    dplyr::rename(val_col := dplyr::all_of(value_col))

  dgs <- disaggregation_group

  if(is.null(names)){
    if(is.null(disaggregation_group)){
      names <- df$meta.name |> unique()
      dgs <- df$meta.disaggregation_group |> unique()
    } else {
      names <- df |>
        dplyr::filter(meta.disaggregation_group %in% disaggregation_group) |>
        pull(meta.name) |>
        unique()
    }
  }

  if(length(names)==0) {
    stop("Either no name fetched based on input combination!", call. = FALSE)
  }

  df <- df |>
    dplyr::filter(meta.name %in% names, meta.disaggregation_group %in% dgs)

  uchk <- df |>
    dplyr::group_by(time, meta.name) |> cols_causing_group_variation()

  # If variation is due to something else apart from names then variation
  # reduction strategy is to be implemented
  if(length(uchk)>0){
    d1 <- df |>
      dplyr::group_by(time, meta.name) |>
      dplyr::summarise(
        divergance = round(
          (max(val_col) - min(val_col))/max(val_col)*100),
        remarks = paste0(
          ifelse(
            dplyr::n_distinct(meta.release_tag)>1,
            paste0(dplyr::n_distinct(meta.release_tag), " release_tags;"), ""),
          ifelse(
            dplyr::n_distinct(meta.price_basis)>1,
            paste0(dplyr::n_distinct(meta.price_basis), " price_basis;"), ""),
          ifelse(
            dplyr::n_distinct(meta.disaggregation_group)>1,
            paste0(dplyr::n_distinct(meta.disaggregation_group), " disaggregation_group;"), "")
        ),
        .groups = "drop") |>
      dplyr::filter(divergance>divergence_tol_pct)

    info <- list(
      rtag = sum(stringr::str_count(d1$remarks, "release_tags")),
      dgs = sum(stringr::str_count(d1$remarks, "disaggregation_group")),
      pbs = sum(stringr::str_count(d1$remarks, "price_basis"))
    )

    # In this case it will start variation reduction strategy
    if(NROW(d1)>0 && (info$rtag > max(info$dgs, info$pbs))){

      # if release_tag's are sort-able then sort and take latest one
      rel_tag_flt_cols <- c("meta.release_order","meta.release_date")
      rel_tag_flt_cols <- colnames(df) |> intersect(rel_tag_flt_cols)

      if(length(rel_tag_flt_cols)>0){
        # Case when there is order able info for release tags
        if("meta.release_order" %in% rel_tag_flt_cols){
          df <- df |>
            group_by(time, meta.name) |>
            slice_max(order_by = meta.release_order, n = 1, with_ties = FALSE) %>%
            ungroup()
        } else {
          df <- df |>
            group_by(time, meta.name) |>
            slice_max(order_by = meta.release_date, n = 1, with_ties = FALSE) %>%
            ungroup()
        }
      } else {
        # if release_tag is Non-sort-able then use avg with warning
        warning("Averaging across variations of the same name. Results may be unexpected. To suppress this warning, use proper inputs instead of mass conversion.", call. = FALSE)
        df <- df |> group_by(time, meta.name) |> summarise(val_col = mean(val_col, na.rm= TRUE), .groups = "drop")
      }
    } else {
      stop("Unexpected variation found in the data unable to proceed further!", call. = FALSE)
    }

  }

  # After this there should be no variation
  uchk <- df |>
    dplyr::group_by(time, meta.name) |> cols_causing_group_variation()

  # This is never supposed to hit. Kept for safety
  if(length(uchk)>0) stop("Unexpected variation found!", call. = FALSE)

  df <- dplyr::as_tibble(df)

  df <- df |> dplyr::select(time, meta.name, val_col) |> dplyr::distinct()

  df_wide <- df |> tidyr::pivot_wider(id_cols = time, names_from = meta.name, values_from = val_col)

  as_tdf_for_ts_and_df(df_wide)

}

#' @export
as_tdf.data.frame <- function(df, allow_discontinuous = TRUE, fiscal_type = TRUE, ...){
  as_tdf_for_ts_and_df(df)
}

#' @export
as_tdf.ts <- function(df, allow_discontinuous = TRUE, fiscal_type = TRUE, ...){
  as_tdf_for_ts_and_df(df)
}


as_tdf_for_ts_and_df <- function(df, allow_discontinuous = TRUE, fiscal_type = TRUE, ...){

  if(is.ts(df)){
    df <- ts_to_tdf_part(df, fiscal_type = fiscal_type)
  } else {
    if(!is.data.frame(df)){
      stop("Input must be a data.frame!!", call. = FALSE)
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
    if(sum(dt_cols)==0){
      stop("Failed to convert date/time!", call. = FALSE)
    }
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
    is_continuous(df$time)},
    error = function(e){
      # warning("Continuity check failed. Setting continuity attribute to NA", call. = FALSE)
      NA
    }
  )
  attr(df, "continuity") <- cont_chk

  if(!allow_discontinuous  & !cont_chk){
    warning("The time column is not continuous. Consider setting allow_discontinuous = TRUE to suppress this warning.", call. = FALSE)
  }

  class(df) <- tdf_class

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
