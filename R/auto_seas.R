
get_Diwali_dates_src_wt <- function(search_year) {

  yr_chk <- (search_year-1960) %% 20

  if(yr_chk != 0) {
    stop("The search_year should be 1960,1980 etc. (multiple of 20)", call. = FALSE)
  }

  # Else proceed
  src_url <- sprintf("http://www.world-timedate.com/holidays/kali_puja_deepavali_date_list.php?year_range=%d", search_year)

  wp <- rvest::read_html(src_url)

  tabs <- wp %>% rvest::html_table()

  target_tab <- tabs %>% purrr::map_lgl(~{
    .x %>% as.character() %>% tolower() %>%  stringr::str_detect("tithi") %>% any()
  })

  tabs <- tabs[target_tab]

  target_tab <- tabs %>% purrr::map_int(~{
    .x %>% dim() %>% prod()
  })

  final_tab <- tabs[[which.min(target_tab)]]

  d <- final_tab[-1,c(1,2)]

  colnames(d) <- c("year","date_txt")

  d <- d %>% dplyr::mutate(date = lubridate::mdy(date_txt))

  d

}


get_Diwali_dates <- function(
    from_year = 1994, to_year = 2027,
    cache_location = file.path(pkg_user_dir("cache"), "diwali")){

  if(!exists(cache_location)) {
    dir.create(cache_location, recursive = TRUE, showWarnings = FALSE)
  }

  #Alternative source may be defined here
  # Use get_Diwali_dates_src_wt

  get_Diwali_dates_src_wt_m <-  memoise::memoise(
    get_Diwali_dates_src_wt,
    cache = memoise::cache_filesystem(path = cache_location))

  start_yr <- (from_year %/% 20)*20
  yrs <- seq(from = start_yr, to = to_year, by = 20)

  all_yrs <- yrs %>% purrr::map(get_Diwali_dates_src_wt_m) %>% purrr::list_rbind()

  # May need further development

  all_yrs

}

to_month_date <- function(date, year, month){
  if(missing(year)|| missing(month)){
    year <- lubridate::year(date)
    month <- lubridate::month(date)
  }
  as.Date(
    paste0(
      year, "-", month, "-01"
    )
  )
}


# Continuous/Fractional Dummy for Diwali/etc
# fractional_dummy_for_monthly_series_from_point_holidays
to_monthly_holiday_dummy <- function(
    point_holidays,
    duration,
    wts_fn = function(x) rep(1, times = length(x))){

  # Only odd numbers are allowed
  stopifnot(duration %% 2 == 1)

  # Continuous/Fractional Dummy for Diwali/etc
  # fractional_dummy_for_monthly_series_from_point_holidays

  durs <- seq(duration)
  durs <- durs - mean(durs)
  wts_d <- wts_fn(durs)
  wts_d <- wts_d/mean(wts_d)
  tot_wt <- sum(wts_d)

  expand_holidays <- purrr::map2(
    durs, wts_d,
    function(d, w){
      tibble::tibble(
        date = point_holidays + d,
        wt = w
      )
    }) %>% purrr::list_rbind()

  expand_holidays <- expand_holidays %>%
    dplyr::mutate(
      month = lubridate::month(date),
      year = lubridate::year(date))

  expand_holidays_full <- tidyr::expand_grid(
    year = unique(expand_holidays$year),
    # Efficient way might be : month = unique(expand_holidays$month) and then
    # add missing months later with 0 dummy value
    month = 1:12
  )

  expand_holidays_full <- expand_holidays_full %>%
    dplyr::mutate(wt = 0, date = to_month_date(year = year, month = month))

  expand_holidays <- expand_holidays %>%
    dplyr::bind_rows(
      expand_holidays_full
    )

  holiday_dat <- expand_holidays %>%
    dplyr::group_by(year, month) %>%
    dplyr::summarise(avg_holiday_day_wt = sum(wt)/tot_wt, .groups = "drop")

  holiday_dat %>%
    dplyr::group_by(month) %>%
    dplyr::mutate(avg_holiday_day_wt_month = mean(avg_holiday_day_wt)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(year_month = to_month_date(year = year, month = month)) %>%
    dplyr::mutate(dummy = avg_holiday_day_wt - avg_holiday_day_wt_month) %>%
    dplyr::select(year_month, dummy)

}



num_sundays_in_month <- function(d) {
  d <- as.Date(d)

  first_day <- lubridate::floor_date(d, "month")
  last_day  <- lubridate::ceiling_date(d, "month") - lubridate::days(1)

  purrr::map2_int(first_day, last_day, ~{
    sum(weekdays(seq(.x, .y, by = "day")) == "Sunday")
  })
}

# as per current code
generate_SA_dummies <- function(from, to){

  dat_dummy <- tibble::tibble(
    date = seq(to_month_date(from), to_month_date(to), by = "month") |>
      to_month_date()
  )

  change_day <- function(x, to_this){
    lubridate::day(x) <- to_this
    x
  }

  dat_dummy <- dat_dummy %>%
    dplyr::mutate(
      num_days = lubridate::days_in_month(date),
      num_sundays = num_sundays_in_month(date),
      # Specific Adjustments
      republic_day = (weekdays(change_day(date,26)) != "Sunday") &
        (lubridate::month(date)==1),
      independence_day = (weekdays(change_day(date,15)) != "Sunday") &
        (lubridate::month(date)==8),
      gandhi_jayanti_day = (weekdays(change_day(date,2)) != "Sunday") &
        (lubridate::month(date)==10),
      # Trading days
      trading_days = num_days -
        num_sundays - republic_day - independence_day - gandhi_jayanti_day
    )

  diwali_days <- get_Diwali_dates(
    from_year = lubridate::year(from),
    to_year = lubridate::year(to))

  diwali_days <- diwali_days %>%
    dplyr::select(date) %>%
    dplyr::mutate(is_diwali = TRUE)

  diwali_days_fl <- diwali_days %>%
    dplyr::mutate(date = to_month_date(date))


  dat_dummy <- dat_dummy %>%
    dplyr::left_join(diwali_days_fl, by = "date") %>%
    dplyr::mutate(is_diwali = ifelse(is.na(is_diwali), FALSE, is_diwali))


  cont_diwali_dummy <- to_monthly_holiday_dummy(diwali_days$date, duration = 5)
  cont_diwali_dummy <- cont_diwali_dummy %>%
    dplyr::rename(date = year_month, D_cont = dummy)

  # see 7.39 The working-days effect of IMF SA (https://www.imf.org/external/pubs/ft/qna/pdf/2017/chapterv7.pdf)
  dat_dummy <- dat_dummy %>%
    dplyr::mutate(
      TD = trading_days - 6/1*num_sundays,
      D = as.numeric(is_diwali))

  dat_dummy <- dat_dummy %>%
    dplyr::left_join(cont_diwali_dummy, by = "date")


  dat_dummy %>%
    dplyr::select(date, TD, D, D_cont) %>%
    as_tdf()

}


#' Internal helper: run X-13ARIMA-SEATS with consistent settings
#'
#' Wraps \code{seasonal::seas()} with a fixed set of options used across
#' the package's automatic seasonal adjustment routines. The decomposition
#' engine (X-11 or SEATS) is selected via \code{seats}.
#'
#' @param ts_in A univariate \code{ts} object to be seasonally adjusted.
#' @param xreg Optional regression matrix passed to \code{seasonal::seas()}.
#' @param usertype Optional character vector specifying the type of each
#'   user-defined regressor (e.g. \code{"td"}, \code{"holiday"}).
#' @param seats Logical. If \code{TRUE}, use the SEATS decomposition engine;
#'   if \code{FALSE} (default), use X-11.
#'
#' @return A \code{seas} object as returned by \code{seasonal::seas()}.
#'
#' @keywords internal
#' @noRd
run_seas <- function(ts_in, xreg = NULL, usertype = NULL, seats = FALSE) {

  # Build argument list dynamically so X-11 / SEATS args are mutually exclusive
  args <- list(
    x                   = ts_in,
    transform.function  = "none",
    transform.power     = 0,
    xreg                = xreg,
    regression.usertype = usertype,
    regression.aictest  = NULL,
    automdl.savelog     = "amd",
    check.print         = "all",
    outlier.types       = "all"
  )

  if (seats) {
    # SEATS is the default engine; no x11 spec → SEATS runs
    # (no x11.sigmalim, since that's an X-11-only argument)
  } else {
    args$x11          <- ""
    args$x11.sigmalim <- c(1.5, 2.5)
  }

  do.call(seasonal::seas, args)
}

auto_seas_monthly_series <- function(tdf, alpha = 0.05, attach_diagnostic = FALSE, SEATS = FALSE) {

  # Prepare data
  ahead_num <- if(SEATS) 3 else 1
  years_ahead <- max(tdf$time) %>% as.Date(anchor = "last")
  lubridate::year(years_ahead) <- lubridate::year(years_ahead) + ahead_num

  sa_dummy <- generate_SA_dummies(
    from = min(tdf$time) %>% as.Date(),
    to = years_ahead)

  sa_dummy_ts <- sa_dummy %>% to_ts()

  for_single_var <- function(var_name){
    this_ts <- tdf[c("time", var_name)] %>% to_ts
    sa_dummy_this <- sa_dummy_ts[,c("TD", "D")]
    # Alternative form usertype_this <- c("td","holiday")
    usertype_this <- c("td","td")
    s_fit <- run_seas(
      this_ts,
      xreg = sa_dummy_this,
      usertype = usertype_this,
      seats = SEATS)
    dummy_state <- c("both")
    coeffs <- summary(s_fit)$coefficients
    coeffs <- tibble::as_tibble(coeffs) %>%
      dplyr::bind_cols(tibble::tibble(var = rownames(coeffs)),.)
    colnames(coeffs)[5] <- "p_value"
    coeffs_r1 <- coeffs %>% dplyr::filter(var == "xreg1")
    coeffs_r2 <- coeffs %>% dplyr::filter(var == "xreg2")
    # Refit if required
    if (coeffs_r1$p_value > alpha && coeffs_r2$p_value > alpha){
      s_fit <- run_seas(this_ts, seats = SEATS)
      dummy_state <- c("none")
    } else if (coeffs_r1$p_value > alpha && coeffs_r2$p_value < alpha) {
      s_fit <- run_seas(
        this_ts,
        xreg = sa_dummy_this[,2],
        usertype = usertype_this[2],
        seats = SEATS)
      dummy_state <- c("trading_day")
    } else if (coeffs_r1$p_value < alpha && coeffs_r2$p_value > alpha) {
      s_fit <- run_seas(
        this_ts,
        xreg = sa_dummy_this[,1],
        usertype = usertype_this[1],
        seats = SEATS)
      dummy_state <- c("diwali")
    }
    # Output: seasonally adjusted series (SEATS s11 or X-11 d11)
    sa_series <- if (SEATS) s_fit$series$s11 else s_fit$series$d11
    dout <- sa_series %>% as_tdf()
    colnames(dout)[2] <- var_name
    list(
      data = dout,
      info = if (attach_diagnostic) {
        list(fit = s_fit, reg = dummy_state, engine = if (SEATS) "SEATS" else "X-11")
      } else NULL
    )
  }

  for_single_var_safe <- function(var_name){
    tryCatch(
      for_single_var(var_name = var_name),
      error = function(e) {
        this_ts <- tdf[c("time", var_name)]
        this_ts[[2]] <- NA
        e_msg <- substr(e$message, 1, 20) |>
          stringr::str_replace_all("\n+|\r+"," ") |>
          paste0(collapse = "; ") |> paste0("...")
        message(
          paste0(
            "Seasonal adjustment failed for variable <",var_name,
            ">. The corresponding values in the adjusted table have been filled with NA.\n",
            "Error message snippet: ", e_msg)
        )
        return(
          list(
            data = this_ts,
            info = NULL
          )
        )
      }
    )
  }

  # Gather Results
  results <- colnames(tdf)[-1] %>% purrr::map(for_single_var_safe)

  out_tdf <- results %>%
    purrr::map("data") %>%
    purrr::reduce(function(x, y){
      dplyr::left_join(x, y, by = "time")
    })

  if (attach_diagnostic) {
    attr(out_tdf, "diagnostics") <- results %>%
      purrr::map("info") %>%
      rlang::set_names(colnames(tdf)[-1])
  }

  out_tdf
}

auto_seas_quarterly_series <- function(tdf, attach_diagnostic = FALSE, SEATS = FALSE) {

  for_single_var <- function(var_name){
    this_ts <- tdf[c("time", var_name)] %>% to_ts
    s_fit <- run_seas(this_ts, seats = SEATS)
    dummy_state <- c("none")
    # Output: seasonally adjusted series (SEATS s11 or X-11 d11)
    sa_series <- if (SEATS) s_fit$series$s11 else s_fit$series$d11
    dout <- sa_series %>% as_tdf()
    colnames(dout)[2] <- var_name
    list(
      data = dout,
      info = if (attach_diagnostic) {
        list(fit = s_fit, reg = dummy_state, engine = if (SEATS) "SEATS" else "X-11")
      } else NULL
    )
  }

  # Gather Results
  results <- colnames(tdf)[-1] %>% purrr::map(for_single_var)

  out_tdf <- results %>%
    purrr::map("data") %>%
    purrr::reduce(function(x, y){
      dplyr::left_join(x, y, by = "time")
    })

  if (attach_diagnostic) {
    attr(out_tdf, "diagnostics") <- results %>%
      purrr::map("info") %>%
      rlang::set_names(colnames(tdf)[-1])
  }

  out_tdf
}


#' Automatic seasonal adjustment of a time-indexed data frame
#'
#' Dispatches to the appropriate seasonal adjustment routine based on the
#' frequency of the input \code{tdf}. Quarterly series are passed to
#' \code{auto_seas_quarterly_series()} (no user regressors); monthly series
#' are passed to \code{auto_seas_monthly_series()} (with trading-day and
#' Diwali regressors, retained or dropped via a coefficient-significance
#' refit step).
#'
#' The underlying decomposition is performed by X-13ARIMA-SEATS via
#' \code{seasonal::seas()}. The decomposition engine (X-11 or SEATS) is
#' selected via the \code{SEATS} argument and applies uniformly to all
#' series in \code{tdf}.
#'
#' @param tdf A time-indexed data frame (\code{tdf}) with a \code{time}
#'   column and one or more numeric value columns. Frequency must be either
#'   monthly or quarterly; other frequencies are rejected.
#' @param alpha Numeric scalar in (0, 1). Significance threshold used by
#'   \code{auto_seas_monthly_series()} when deciding whether to retain
#'   trading-day and Diwali regressors after the initial fit. Ignored for
#'   quarterly inputs. Defaults to \code{0.05}.
#' @param attach_diagnostic Logical. If \code{TRUE}, the returned object
#'   carries a \code{"diagnostics"} attribute: a named list (keyed by
#'   variable name) of per-series diagnostic lists, each containing the
#'   full \code{seas} fit object (\code{fit}), the regressor state retained
#'   (\code{reg}: one of \code{"both"}, \code{"trading_day"},
#'   \code{"diwali"}, or \code{"none"}), and the engine used
#'   (\code{engine}: \code{"X-11"} or \code{"SEATS"}). Defaults to
#'   \code{FALSE}.
#' @param SEATS Logical. If \code{TRUE}, use the SEATS decomposition engine
#'   (extracting \code{s11} as the seasonally adjusted series). If
#'   \code{FALSE} (default), use the X-11 engine (extracting \code{d11}).
#'   See \emph{Engine choice} below.
#'
#' @return A wide \code{tdf} containing the seasonally adjusted series for
#'   each non-time column of the input, joined on \code{time}. When
#'   \code{attach_diagnostic = TRUE}, the result additionally carries a
#'   \code{"diagnostics"} attribute as described above.
#'
#' @section Engine choice:
#' X-11 is a filter-based decomposition; SEATS is a model-based
#' decomposition derived from the fitted ARIMA model. Both engines share
#' the same RegARIMA preprocessing stage, so user regressors (trading-day,
#' Diwali) behave identically across engines. MoSPI publishes quarterly
#' GDP/GVA seasonally adjusted series using SEATS; most central bank
#' communications on monthly indicators use X-11. When in doubt, run both
#' and compare residual seasonality diagnostics on the output series.
#'
#' @section Frequency dispatch:
#' The frequency of \code{tdf} is determined via \code{frequency(tdf)} and
#' must resolve to either \code{"month"} or \code{"quarter"}. Any other
#' value (including \code{NA} or unrecognised strings) triggers an error.
#'
#' @export
auto_seas <- function(tdf, alpha = 0.05, attach_diagnostic = FALSE, SEATS = FALSE) {
  fq <- frequency(tdf)

  if (fq == "quarter") {
    auto_seas_quarterly_series(
      tdf, attach_diagnostic = attach_diagnostic, SEATS = SEATS)
  } else if (fq == "month") {
    auto_seas_monthly_series(
      tdf, alpha = alpha, attach_diagnostic = attach_diagnostic, SEATS = SEATS)
  } else {
    stop(paste0("This frequency <", fq,"> not supported!"), call. = FALSE)
  }
}


