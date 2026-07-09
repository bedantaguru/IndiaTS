
#' Create a Layered ECharts Time Series Visualization (Internal)
#'
#' Writes an interactive time-series line chart using `echarts4r` from a tidy dataframe.
#' Supports standard single-layered axes (e.g., monthly or continuous dates) as well as
#' two-layered category axes (e.g., quarterly or semi-annual periods grouped within financial years).
#'
#' @param data A dataframe containing the time series data.
#' @param single_layered Logical; if `TRUE`, uses a single-level category axis based on `single_layer_col`. If `FALSE` (default), uses a two-level year/period axis.
#' @param single_layer_col Character string. Name of the column used when `single_layered = TRUE`. Defaults to `"time"`.
#' @param year_col Character string. Name of the outer (year/macro-period) column used when `single_layered = FALSE`. Defaults to `"year"`.
#' @param period_col Character string. Name of the inner (sub-period, e.g., Q1..Q4, Jan..Dec) column used when `single_layered = FALSE`. Defaults to `"period"`.
#' @param legend_type_to_col_map A list mapping series types (`"type1"`, `"type2"`, `"no_legend"`) to column names. Defaults to an empty list, which automatically maps all numeric columns to `"type1"` (solid lines).
#' @param annual_col_name Character string or `NULL`. Optional column name for annual reference lines (rendered as dotted markLines).
#' @param halyearly_col_name Character string or `NULL`. Optional column name for half-yearly reference lines (rendered as dotted markLines).
#' @param ylim Numeric vector of length 2 or `NULL`. Optional Y-axis limits (e.g., `c(4, 10)`).
#' @param title_text Character string. Chart main title.
#' @param subtitle_text Character string. Chart subtitle.
#' @param colors Character vector of hex colors for the series palette.
#' @param add_zoom Logical. Whether to enable the top-right toolbox zoom and reset icons. Defaults to `FALSE`.
#' @param add_vertical_scrollbar Logical. Whether to show the vertical Y-axis dataZoom scrollbar. Defaults to `FALSE`.
#' @param add_horizontal_scrollbar Logical. Whether to show the horizontal X-axis dataZoom scrollbar. Defaults to `TRUE`.
#'
#' @return An `echarts4r` htmlwidget object.
#' @keywords internal
#' @examples
#' \dontrun{
#' # Example 1: Two-layered quarterly category axis
#' df_quarterly <- tibble::tibble(
#'   year = c(
#'     rep("2023-24", 4), rep("2024-25", 4),
#'     rep("2025-26", 4), rep("2026-27", 4)
#'   ),
#'   period = rep(c("Q1", "Q2", "Q3", "Q4"), 4),
#'
#'   `Public Estimates` = c(
#'     9.7, 9.3, 9.5, 8.4,
#'     6.5, 5.6, 6.4, 7.4,
#'     7.8, 8.2, NA, NA,
#'     NA, NA, NA, NA
#'   ),
#'   `Projections December 2025` = c(
#'     rep(NA, 10),
#'     7.2, 6.6, 6.8, 6.8, 7.0, 6.9
#'   ),
#'   `Projections October 2025` = c(
#'     rep(NA, 10),
#'     7.1, 6.8, 6.8, 6.8, 6.9, 6.9
#'   ),
#'   `Annual Figures` = c(
#'     9.225, 9.225, 9.225, 9.225,
#'     6.475, 6.475, 6.475, 6.475,
#'     7.45,  7.45,  7.45,  7.45,
#'     6.875, 6.875, 6.875, 6.875
#'   ),
#'   `Halfyearly Figures` = c(
#'     9.5,  9.5,  8.95, 8.95,
#'     6.05, 6.05, 6.9,  6.9,
#'     8.0,  8.0,  6.9,  6.9,
#'     6.8,  6.8,  6.95, 6.95
#'   )
#' )
#'
#' my_quarterly_chart <- viz_echarts_time_series(
#'   data = df_quarterly,
#'   single_layered = FALSE,
#'   year_col = "year",
#'   period_col = "period",
#'   legend_type_to_col_map = list(
#'     type1 = c("Public Estimates"),
#'     type2 = c("Projections December 2025", "Projections October 2025"),
#'     no_legend = NULL
#'   ),
#'   annual_col_name = "Annual Figures",
#'   halyearly_col_name = "Halfyearly Figures",
#'   ylim = c(4, 10),
#'   title_text = "Projections of GDP: Quarterly Path",
#'   subtitle_text = "(Y-o-Y growth in per cent)",
#'   colors = c("#4472C4", "#00B050", "red")
#' )
#'
#' my_quarterly_chart
#'
#' # Example 2: Single-layered monthly trajectory
#' df_monthly <- tibble::tibble(
#'   month = c("Jan 2025", "Feb 2025", "Mar 2025", "Apr 2025", "May 2025", "Jun 2025"),
#'   `Actual Sales` = c(120, 135, 140, 155, 160, 175),
#'   `Target Sales` = c(115, 130, 145, 150, 165, 180)
#' )
#'
#' my_monthly_chart <- viz_echarts_time_series(
#'   data = df_monthly,
#'   single_layered = TRUE,
#'   single_layer_col = "month",
#'   legend_type_to_col_map = list(
#'     type1 = c("Actual Sales"),
#'     type2 = c("Target Sales")
#'   ),
#'   annual_col_name = NULL,
#'   halyearly_col_name = NULL,
#'   title_text = "Monthly Sales Trajectory vs Target",
#'   subtitle_text = "(in USD Thousands)",
#'   colors = c("#255E91", "#ED7D31")
#' )
#'
#' my_monthly_chart
#'
#' # Example 3: Two-layered semi-annual trajectory
#' df_semi_annual <- tibble::tibble(
#'   year = c("2024-25", "2024-25", "2025-26", "2025-26"),
#'   half = c("H1", "H2", "H1", "H2"),
#'   `Revenue` = c(45.2, 48.7, 52.1, 55.4)
#' )
#'
#' my_semi_chart <- viz_echarts_time_series(
#'   data = df_semi_annual,
#'   single_layered = FALSE,
#'   year_col = "year",
#'   period_col = "half",
#'   annual_col_name = NULL,
#'   halyearly_col_name = NULL,
#'   title_text = "Semi-Annual Revenue Growth",
#'   subtitle_text = "(in USD Millions)",
#'   colors = c("#70AD47")
#' )
#'
#' my_semi_chart
#' }
viz_echarts_time_series <- function(
    data,
    single_layered = FALSE,
    single_layer_col = "time",
    year_col = "year",
    period_col = "period",
    legend_type_to_col_map = list(),
    annual_col_name = "annual_vals",
    halyearly_col_name = "halyearly_vals",
    ylim = NULL,
    title_text = "<Chart Title>",
    subtitle_text = "<Subtitle>",
    colors = c("#4472C4", "#00B050", "red", "#E69F00", "#56B4E9"),
    add_zoom = FALSE,
    add_vertical_scrollbar = FALSE,
    add_horizontal_scrollbar = TRUE
) {

  # ---------------------------------------------------------
  # 0. Input Validation & Legend Mapping Setup
  # ---------------------------------------------------------
  if (isTRUE(single_layered)) {
    if (!single_layer_col %in% names(data)) {
      stop(sprintf("`data` must contain column '%s' when single_layered = TRUE.", single_layer_col))
    }
    time_col <- single_layer_col
  } else {
    if (!all(c(year_col, period_col) %in% names(data))) {
      stop(sprintf("`data` must contain columns '%s' and '%s' when single_layered = FALSE.", year_col, period_col))
    }
    time_col <- ".composite_time"
  }

  # Default fallback: map unassigned numeric series to solid lines (type1)
  if (is.null(legend_type_to_col_map) || length(legend_type_to_col_map) == 0) {
    ignore_cols <- c(single_layer_col, year_col, period_col, time_col, annual_col_name, halyearly_col_name)
    all_series_cols <- setdiff(names(data), ignore_cols[!is.na(ignore_cols)])
    legend_type_to_col_map <- list(type1 = all_series_cols)
  }

  # ---------------------------------------------------------
  # 1. Grid Construction & Chronological Alignment
  # ---------------------------------------------------------
  df_parsed <- data

  if (isTRUE(single_layered)) {
    plot_df <- df_parsed
    plot_df$.time <- as.character(plot_df[[single_layer_col]])
  } else {
    ordered_years <- unique(as.character(df_parsed[[year_col]]))
    ordered_periods <- unique(as.character(df_parsed[[period_col]]))

    full_grid <- expand.grid(
      year = factor(ordered_years, levels = ordered_years),
      period = factor(ordered_periods, levels = ordered_periods)
    ) |>
      dplyr::arrange(.data$year, .data$period) |>
      dplyr::mutate(
        year = as.character(.data$year),
        period = as.character(.data$period)
      )

    full_grid[[time_col]] <- paste(full_grid$year, full_grid$period)

    df_join <- df_parsed
    df_join[[time_col]] <- paste(df_join[[year_col]], df_join[[period_col]])
    df_join <- df_join[, setdiff(names(df_join), c(year_col, period_col)), drop = FALSE]

    plot_df <- full_grid |>
      dplyr::left_join(df_join, by = time_col)

    plot_df$.time <- plot_df[[time_col]]
  }

  # ---------------------------------------------------------
  # 2. Series Bridging Logic
  # ---------------------------------------------------------
  last_obs_idx <- -1
  if (!is.null(legend_type_to_col_map$type1) && length(legend_type_to_col_map$type1) > 0) {
    ref_col <- legend_type_to_col_map$type1[1]
    if (ref_col %in% names(plot_df)) {
      last_obs_idx <- max(which(!is.na(plot_df[[ref_col]])))

      if (is.finite(last_obs_idx) && !is.null(legend_type_to_col_map$type2)) {
        for (proj_col in legend_type_to_col_map$type2) {
          if (proj_col %in% names(plot_df)) {
            plot_df[[proj_col]] <- replace(
              plot_df[[proj_col]],
              last_obs_idx,
              plot_df[[ref_col]][last_obs_idx]
            )
          }
        }
      }
    }
  }

  # ---------------------------------------------------------
  # 3. Dynamic Tooltip Configuration
  # ---------------------------------------------------------
  label_fmt <- htmlwidgets::JS(sprintf("function(p) {
    return (p.dataIndex === %d ? '{startBox|' : '{usualBox|') + p.value[1] + '}';
  }", max(0, last_obs_idx - 1)))

  ann_label <- ifelse(!is.null(annual_col_name), annual_col_name, "")
  half_label <- ifelse(!is.null(halyearly_col_name), halyearly_col_name, "")

  tooltip_fmt <- htmlwidgets::JS(sprintf("function(p) {
    var res = p[0].name + '<br/>';
    for (var i = 0; i < p.length; i++) {
      if(p[i].value[1] != null && p[i].seriesName !== 'Phantom') {
        if(p[i].seriesName === '%s') {
          res += p[i].marker + '<b>Annual Growth Rate:</b> ' + p[i].value[1] + '%%<br/>';
        } else if (p[i].seriesName === '%s') {
          res += p[i].marker + '<b>Semi-Annual Growth Rate:</b> ' + p[i].value[1] + '%%<br/>';
        } else {
          res += p[i].marker + p[i].seriesName + ': ' + p[i].value[1] + '<br/>';
        }
      }
    }
    return res;
  }", ann_label, half_label))

  style_box <- function(c) list(borderColor = c, borderWidth = 1.5, backgroundColor = "white", padding = c(4,4), color = "black", fontSize = 12)
  style_inv <- list(borderColor = "transparent", color = "transparent", fontSize = 0)

  # ---------------------------------------------------------
  # 4. Base Chart Initialization & Legend Mapping
  # ---------------------------------------------------------
  my_chart <- plot_df |>
    echarts4r::e_charts(.time)

  color_idx <- 1

  # --- Solid Line Series ---
  if (!is.null(legend_type_to_col_map$type1)) {
    for (col in legend_type_to_col_map$type1) {
      if (col %in% names(plot_df)) {
        col_color <- colors[((color_idx - 1) %% length(colors)) + 1]
        my_chart <- my_chart |>
          echarts4r::e_line_(
            serie = col, name = col, color = col_color, symbolSize = 8,
            lineStyle = list(width = 3, type = "solid"),
            label = list(show = TRUE, position = "bottom", color = "black", fontWeight = "bold")
          )
        color_idx <- color_idx + 1
      }
    }
  }

  # --- Dashed Line Series with Boxed Labels ---
  if (!is.null(legend_type_to_col_map$type2)) {
    pos_toggle <- TRUE
    for (col in legend_type_to_col_map$type2) {
      if (col %in% names(plot_df)) {
        col_color <- colors[((color_idx - 1) %% length(colors)) + 1]
        lbl_pos <- ifelse(pos_toggle, "top", "bottom")

        my_chart <- my_chart |>
          echarts4r::e_line_(
            serie = col, name = col, color = col_color, symbolSize = 8,
            lineStyle = list(type = "dashed", width = 3),
            label = list(show = TRUE, formatter = label_fmt, position = lbl_pos,
                         rich = list(startBox = style_inv, usualBox = style_box(col_color)))
          )
        color_idx <- color_idx + 1
        pos_toggle <- !pos_toggle
      }
    }
  }

  # --- Dotted Line Series (No Legend) ---
  if (!is.null(legend_type_to_col_map$no_legend)) {
    for (col in legend_type_to_col_map$no_legend) {
      if (col %in% names(plot_df)) {
        col_color <- colors[((color_idx - 1) %% length(colors)) + 1]
        my_chart <- my_chart |>
          echarts4r::e_line_(
            serie = col, name = col, color = col_color, symbolSize = 6,
            legend = FALSE, lineStyle = list(width = 2, type = "dotted")
          )
        color_idx <- color_idx + 1
      }
    }
  }

  # ---------------------------------------------------------
  # 5A. Optional Reference Layer: Annual MarkLines
  # ---------------------------------------------------------
  if (!is.null(annual_col_name) && annual_col_name %in% names(plot_df) && any(!is.na(plot_df[[annual_col_name]]))) {
    my_chart <- my_chart |>
      echarts4r::e_line_(serie = annual_col_name, name = annual_col_name, color = "#9B0056", symbol = "none", lineStyle = list(opacity = 0))

    if (isTRUE(single_layered)) {
      annual_segments <- plot_df |>
        dplyr::filter(!is.na(.data[[annual_col_name]])) |>
        dplyr::summarise(
          start_period = dplyr::first(.data$.time),
          end_period = dplyr::last(.data$.time),
          y_val = dplyr::first(.data[[annual_col_name]]),
          .groups = "drop"
        )
    } else {
      annual_segments <- plot_df |>
        dplyr::filter(!is.na(.data[[annual_col_name]])) |>
        dplyr::group_by(.data$year) |>
        dplyr::summarise(
          start_period = dplyr::first(.data$.time),
          end_period = dplyr::last(.data$.time),
          y_val = dplyr::first(.data[[annual_col_name]]),
          .groups = "drop"
        )
    }

    annual_mark_lines_data <- lapply(1:nrow(annual_segments), function(i) {
      list(
        list(xAxis = annual_segments$start_period[i], yAxis = annual_segments$y_val[i]),
        list(xAxis = annual_segments$end_period[i], yAxis = annual_segments$y_val[i])
      )
    })

    series_names <- sapply(my_chart$x$opts$series, function(s) s$name)
    ann_idx <- which(series_names == annual_col_name)

    if (length(ann_idx) > 0) {
      my_chart$x$opts$series[[ann_idx[1]]]$markLine <- list(
        data = annual_mark_lines_data,
        symbol = c("none", "none"),
        lineStyle = list(color = "#9B0056", width = 2, opacity = 0.6, type = "dotted"),
        label = list(show = FALSE),
        tooltip = list(show = FALSE)
      )
    }
  }

  # ---------------------------------------------------------
  # 5B. Optional Reference Layer: Half-Yearly MarkLines
  # ---------------------------------------------------------
  if (!is.null(halyearly_col_name) && halyearly_col_name %in% names(plot_df) && any(!is.na(plot_df[[halyearly_col_name]]))) {
    my_chart <- my_chart |>
      echarts4r::e_line_(serie = halyearly_col_name, name = halyearly_col_name, color = "#D66011", symbol = "none", lineStyle = list(opacity = 0))

    if (isTRUE(single_layered)) {
      half_segments <- plot_df |>
        dplyr::filter(!is.na(.data[[halyearly_col_name]])) |>
        dplyr::summarise(
          start_period = dplyr::first(.data$.time),
          end_period = dplyr::last(.data$.time),
          y_val = dplyr::first(.data[[halyearly_col_name]]),
          .groups = "drop"
        )
    } else {
      half_point <- ceiling(length(unique(plot_df$period)) / 2)
      first_half_periods <- unique(plot_df$period)[1:half_point]

      half_segments <- plot_df |>
        dplyr::filter(!is.na(.data[[halyearly_col_name]])) |>
        dplyr::mutate(half = ifelse(.data$period %in% first_half_periods, "H1", "H2")) |>
        dplyr::group_by(.data$year, .data$half) |>
        dplyr::summarise(
          start_period = dplyr::first(.data$.time),
          end_period = dplyr::last(.data$.time),
          y_val = dplyr::first(.data[[halyearly_col_name]]),
          .groups = "drop"
        )
    }

    half_mark_lines_data <- lapply(1:nrow(half_segments), function(i) {
      list(
        list(xAxis = half_segments$start_period[i], yAxis = half_segments$y_val[i]),
        list(xAxis = half_segments$end_period[i], yAxis = half_segments$y_val[i])
      )
    })

    series_names <- sapply(my_chart$x$opts$series, function(s) s$name)
    half_idx <- which(series_names == halyearly_col_name)

    if (length(half_idx) > 0) {
      my_chart$x$opts$series[[half_idx[1]]]$markLine <- list(
        data = half_mark_lines_data,
        symbol = c("none", "none"),
        lineStyle = list(color = "#D66011", width = 2, opacity = 0.6, type = "dotted"),
        label = list(show = FALSE),
        tooltip = list(show = FALSE)
      )
    }
  }

  # ---------------------------------------------------------
  # 6A. Layout, Formatting, and Axis Configuration
  # ---------------------------------------------------------
  my_chart <- my_chart |>
    echarts4r::e_title(title_text, subtitle_text, left = "center",
                       textStyle = list(fontWeight = "bold", fontSize = 16),
                       subtextStyle = list(color = "black", fontWeight = "bold", fontSize = 14)) |>
    echarts4r::e_legend(top = "15%", itemWidth = 30) |>
    echarts4r::e_grid(top = "25%", bottom = "15%") |>
    echarts4r::e_tooltip(trigger = "axis", formatter = tooltip_fmt)

  if (!is.null(ylim) && length(ylim) == 2) {
    my_chart <- my_chart |>
      echarts4r::e_y_axis(min = ylim[1], max = ylim[2], splitLine = list(show = FALSE))
  } else {
    my_chart <- my_chart |>
      echarts4r::e_y_axis(splitLine = list(show = FALSE))
  }

  if (isTRUE(single_layered)) {
    my_chart$x$opts$xAxis <- list(
      type = "category",
      data = plot_df$.time,
      axisTick = list(alignWithLabel = TRUE),
      axisLabel = list(
        fontWeight = "bold",
        color = "black",
        margin = 8
      )
    )
  } else {
    my_chart$x$opts$xAxis <- list(
      list(
        type = "category",
        data = plot_df$.time,
        axisTick = list(alignWithLabel = TRUE),
        axisLabel = list(
          fontWeight = "bold",
          color = "black",
          margin = 8,
          formatter = htmlwidgets::JS("function(value) {
            var parts = value.split(' ');
            return parts.length > 1 ? parts.slice(1).join(' ') : value;
          }")
        )
      ),
      list(
        type = "category",
        position = "bottom",
        offset = 25,
        axisLine = list(show = FALSE),
        axisTick = list(
          show = TRUE,
          length = 25,
          inside = TRUE,
          alignWithLabel = FALSE,
          lineStyle = list(color = "black", width = 1)
        ),
        splitLine = list(
          show = TRUE,
          lineStyle = list(color = "grey", width = 1, type = "dashed", opacity = 0.8)
        ),
        axisLabel = list(fontWeight = "bold", color = "black", interval = 0),
        data = unique(plot_df$year)
      )
    )
  }

  # ---------------------------------------------------------
  # 6B. Zoom & Scrollbar Controls
  # ---------------------------------------------------------
  if (isTRUE(add_horizontal_scrollbar)) {
    my_chart <- my_chart |>
      echarts4r::e_datazoom(
        type = "slider",
        x_index = if (isTRUE(single_layered)) 0 else c(0, 1),
        bottom = "2%",
        start = 0,
        end = 100,
        toolbox = add_zoom
      )
  }

  if (isTRUE(add_vertical_scrollbar)) {
    my_chart <- my_chart |>
      echarts4r::e_datazoom(
        type = "slider",
        y_index = 0,
        right = "2%",
        start = 0,
        end = 100,
        toolbox = add_zoom
      )
  }

  if (isTRUE(add_zoom) && !isTRUE(add_horizontal_scrollbar) && !isTRUE(add_vertical_scrollbar)) {
    my_chart <- my_chart |>
      echarts4r::e_datazoom(
        type = "inside",
        x_index = if (isTRUE(single_layered)) 0 else c(0, 1),
        toolbox = TRUE
      )
  }

  return(my_chart)
}
