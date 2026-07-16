



viz_unified <- function(
    td, xl_file,
    na_omit =  TRUE,
    single_layer_monthly = TRUE,
    second_layer_monthly = c("fy","calender"),
    num_digits_for_round = 1, type = c("echarts","excel"),
    title = NULL, subtitle = NULL,
    ...){

  type <- match.arg(type)
  second_layer_monthly <- match.arg(second_layer_monthly)

  if(type == "excel" && missing(xl_file)){
    stop(
      "'xl_file' must be supplied when 'type = \"excel\"'.",
      call. = FALSE
    )
  }

  if(!is_tdf(td)){
    stop("TDF-expected!", call. = FALSE)
  }

  # Simplify TDF
  pd <- dplyr::as_tibble(td)

  if(na_omit){
    pd <- stats::na.omit(pd)
  }

  if(NROW(pd)==0){
    stop("No complete observation!", call. = FALSE)
  }

  fq <- frequency.tdf(pd)

  supported_freqs <- c("month","quarter","halfyear","year")

  if (!(fq %in% supported_freqs)) {
    stop(
      sprintf(
        "Unsupported frequency '%s'. Supported frequencies are: %s.",
        fq,
        paste(supported_freqs, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Note: single layer for month and year and double layer for quarter and half year
  # with monthly can be made double layer if chosen

  # Double Layer charts
  if((fq %in% c("quarter", "halfyear")) || (!single_layer_monthly && fq == "month")) {

    period_func <- switch(
      fq,
      "quarter"  = fiscal_quarter,
      "halfyear" = fiscal_halfyear,
      "month"    = fiscal_month
    )

    yr_fun <- fiscal_year

    if(fq == "month" && second_layer_monthly == "calender") {
      yr_fun <- function(x){
        as.Date(x) |> lubridate::year()
      }
    }

    pd1 <- pd |>
      dplyr::mutate(
        year = yr_fun(.data$time) |> as.character(),
        prd = period_func(.data$time, with_year = FALSE) |> as.character()
      ) |>
      dplyr::select(-"time") |>
      dplyr::arrange(.data$year, .data$prd) |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::where(is.numeric),
          ~ base::round(.x, num_digits_for_round)
        )
      )

    if(type == "echarts"){
      return(
        viz_echarts_time_series(
          pd1,
          single_layered = FALSE,
          year_col = "year", period_col = "prd",
          title_text = title, subtitle_text = subtitle,
          ...)
      )
    }

    if(type == "excel"){
      return(
        viz_excel_chart_layered_axis(
          pd1, file = xl_file,
          single_layered = FALSE,
          year_col = "year", period_col = "prd",
          title = title, subtitle = subtitle,
          ...)
      )
    }
  }

  if((fq %in% c("year")) || (single_layer_monthly && fq == "month")) {

    pd1 <- pd |>
      dplyr::arrange(as.Date(.data$time)) |>
      dplyr::mutate(
        time = as.character(.data$time)
      ) |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::where(is.numeric),
          ~ base::round(.x, num_digits_for_round)
        )
      )

    if(type == "echarts"){
      return(
        viz_echarts_time_series(
          pd1,
          single_layered = TRUE,
          single_layer_col = "time",
          title_text = title, subtitle_text = subtitle,
          ...)
      )
    }

    if(type == "excel"){
      return(
        viz_excel_chart_layered_axis(
          pd1, file = xl_file,
          single_layered = TRUE,
          single_layer_col = "time",
          title = title, subtitle = subtitle,
          ...)
      )
    }
  }

}


