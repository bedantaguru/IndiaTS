
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
    date = seq(from, to, by = "month")
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
    dplyr::mutate(date = lubridate::floor_date(date, unit = "month") %>% as.Date())


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


auto_seas_monthly_series <- function(tdf, alpha = 0.05) {

  # Prepare data
  one_yr_ahead <- max(tdf$time) %>% as.Date(anchor = "last")
  lubridate::year(one_yr_ahead) <- lubridate::year(one_yr_ahead) + 1
  sa_dummy <- generate_SA_dummies(
    from = min(tdf$time) %>% as.Date(),
    to = one_yr_ahead)
  sa_dummy_ts <- sa_dummy %>% to_ts()

  # Common seas code section
  seas_spec <- function(ts_in, xreg = NULL, usertype = NULL) {
    seasonal::seas(
      ts_in,
      transform.function = "none",
      transform.power    = 0,
      xreg               = xreg,
      regression.usertype = usertype,
      regression.aictest = NULL,
      x11                = "",
      automdl.savelog    = "amd",
      check.print        = "all",
      outlier.types      = "all",
      x11.sigmalim       = c(1.5, 2.5)
    )
  }


  for_single_var <- function(var_name){

    this_ts <- tdf[c("time", var_name)] %>% to_ts

    sa_dummy_this <- sa_dummy_ts[,c("TD", "D")]
    # Alternative form usertype_this <- c("td","holiday")
    usertype_this <- c("td","td")

    s_fit <- seas_spec(
      this_ts,
      xreg = sa_dummy_this,
      usertype = usertype_this)

    coeffs <- summary(s_fit)$coefficients
    coeffs <- tibble::as_tibble(coeffs) %>%
      dplyr::bind_cols(tibble::tibble(var = rownames(coeffs)),.)
    colnames(coeffs)[5] <- "p_value"

    coeffs_r1 <- coeffs %>% filter(var == "xreg1")
    coeffs_r2 <- coeffs %>% filter(var == "xreg2")
    # Refit if required
    if (coeffs_r1$p_value > alpha && coeffs_r2$p_value > alpha){
      s_fit <- seas_spec(this_ts)
    } else if (coeffs_r1$p_value > alpha && coeffs_r2$p_value < alpha) {
      s_fit <- seas_spec(
        this_ts,
        xreg = sa_dummy_this[,2],
        usertype = usertype_this[2])
    } else if (coeffs_r1$p_value < alpha && coeffs_r2$p_value > alpha) {
      s_fit <- seas_spec(
        this_ts,
        xreg = sa_dummy_this[,1],
        usertype = usertype_this[1])
    }

    # Output: seasonally adjusted series (X-11 D11)
    dout <- s_fit$series$d11 %>% as_tdf()
    colnames(dout)[2] <- var_name
    dout

  }

  out_tdf <- colnames(tdf)[-1] %>% map(for_single_var) %>%
    purrr::reduce(function(x, y){
      left_join(x,y , by = "time")
    })

  out_tdf

}

