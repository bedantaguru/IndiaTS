

is_tdf <- function(x) {
  inherits(x, tdf_class[1])
}


as_tdf_convert_time <- function(df, fiscal = TRUE){
  if(fiscal){
    df[[1]] <- as_fiscal_period(df[[1]])
  } else {
    df[[1]] <- as_calendar_period(df[[1]])
  }
  df

}


ts_to_tdf_part <- function(ts_obj, fiscal_type = TRUE) {

  freq <- stats::frequency(ts_obj)
  tt   <- stats::time(ts_obj)

  # Year and subperiod index
  yr  <- as.numeric(floor(tt))
  sub <- as.numeric(round((tt - yr) * freq) + 1)

  if (freq == 12) {
    # Monthly: first day of month
    date <- lubridate::ymd(sprintf("%d-%02d-01", yr, sub))
    if(fiscal_type){
      f_date <- fiscal_month_for_date(date)
    }

  } else if (freq == 4) {
    # Quarterly: first day of quarter
    month <- (sub - 1) * 3 + 1
    date  <- lubridate::ymd(sprintf("%d-%02d-01", yr, month))

    if(fiscal_type){
      f_date <- fiscal_quarter_for_date(date)
    }

  } else if (freq == 2) {
    # Half-yearly: first day of half-year
    month <- ifelse(sub == 1, 1, 7)
    date  <- lubridate::ymd(sprintf("%d-%02d-01", yr, month))

    if(fiscal_type){
      cat("Converting half-yearly dates to fiscal half-yearly periods.\nThis will translate (2022, 2) to H2:2021-22; (2020,1) to H1:2019-20 etc.\n")
      f_date <- fiscal_halfyear_for_date(date) %>% previous_period_for_fiscal_period()
    }

  } else if (freq == 1) {
    # Yearly: first day of year
    date <- lubridate::ymd(sprintf("%d-01-01", yr))

    if(fiscal_type){
      cat("Converting yearly dates to fiscal yearly periods.\nThis will translate 2022 to 2021-22; 2020 to 2019-20 etc.\n")
      f_date <- fiscal_year_for_date(date)
    }

  } else {
    stop(
      "Only yearly (1), half-yearly (2), quarterly (4), and monthly (12) ts objects are supported",
      call. = FALSE
    )
  }

  # Convert ts / mts to data.frame cleanly
  val <- as.data.frame(ts_obj)


  this_date <- date

  if(fiscal_type){
    class(f_date) <- fiscal_period_class
    this_date <- f_date
  }


  dfo <- data.frame(
    date = this_date,
    val,
    row.names = NULL
  )

  dfo
}
