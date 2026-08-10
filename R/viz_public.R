
#' Plot Time-Series Data Frame (tdf) Objects
#'
#' A unified S3 plotting method for tdf objects. This function generates either
#' interactive HTML visualizations or layered, native charts exported directly
#' to an Excel file.
#'
#' @param x A tdf (time-series data frame) object to be plotted.
#' @param xl_file Character string. The path to the output Excel file. Must be provided if type = "excel".
#' @param na_omit Logical. If TRUE (default), omits missing values from the dataset before plotting.
#' @param single_layer_monthly Logical. If TRUE (default), uses a single-layer category axis for monthly data. If FALSE, uses a two-level year/period axis.
#' @param second_layer_monthly Character string. Specifies the macro-period layer for monthly data when single_layer_monthly = FALSE. One of "fy" (fiscal year) or "calender" (calendar year).
#' @param num_digits_for_round Numeric. Number of decimal places to round numeric columns to. Defaults to 1.
#' @param type Character string. The output format for the chart. Either "echarts" (interactive HTML widget) or "excel" (native Excel chart). Defaults to "echarts".
#' @param title Character string or NULL. The main title of the chart.
#' @param subtitle Character string or NULL. The subtitle of the chart.
#' @param ... Additional arguments passed down to the underlying rendering engines.
#'
#'   \strong{Shared / Appearance Options:}
#'   \itemize{
#'     \item \code{colors}: Optional character vector of hex colors (e.g., "#4472C4") recycled to the number of series.
#'     \item \code{ylim} (ECharts) / \code{ymin}, \code{ymax} (Excel): Numeric value-axis limits.#'
#'   }
#'
#'   \strong{Excel-Specific Options:}
#'   \itemize{
#'     \item \code{sheet}: Character; the worksheet name.
#'     \item \code{chart_type}: Character; "line" (default) or "bar" (clustered column chart).
#'     \item \code{digits}: Numeric; controls exact decimal formatting for cells and end-point labels.
#'     \item \code{show_avg}: Logical; whether to draw the rounded-rectangle average box. Default TRUE.
#'     \item \code{avg_label}: Character; heading shown inside the average box.
#'     \item \code{avg_window}: Numeric; number of trailing complete periods averaged per series.
#'     \item \code{bullet_type}: Character; average-box glyph style ("option1", "option2", "option3", or "custom").
#'     \item \code{custom_bullet_hex}: Character; Unicode hex (e.g., "29BF") used if bullet_type = "custom".
#'     \item \code{add_footnote}: Logical; triggers a footnote if incomplete periods (*) exist.
#'     \item \code{footnote_msg}: Character; text used when the footnote is shown.
#'     \item \code{enable_gridline}: Logical; draws faint major gridlines. Default TRUE.
#'     \item \code{if_single_series_disable_legend}: Logical; hides the legend if only one series exists.
#'     \item \code{legend_type}: Character; end-point data label style ("type1" for colored text, "type2" for boxed).
#'     \item \code{data_label_till}: Numeric; number of trailing observations from the right to label. Default 1.
#'     \item \code{add_points}: Logical; adds hollow data point markers to line charts. Default FALSE.
#'     \item \code{unit_for_summary}: Character; unit string (e.g., "%" or " (Lakhs)") for average values.
#'   }
#'
#'   \strong{ECharts-Specific Options:}
#'   \itemize{
#'     \item \code{legend_type_to_col_map}: A list mapping series types ("type1", "type2", "no_legend") to column names.
#'     \item \code{annual_col_name}: Character; column for annual reference dotted markLines.
#'     \item \code{halyearly_col_name}: Character; column for half-yearly reference dotted markLines.
#'     \item \code{add_zoom}: Logical; enables toolbox zoom/reset icons. Default FALSE.
#'     \item \code{add_vertical_scrollbar}: Logical; shows a vertical Y-axis dataZoom. Default FALSE.
#'     \item \code{add_horizontal_scrollbar}: Logical; shows a horizontal X-axis dataZoom. Default TRUE.
#'     \item \code{add_data_label}: Logical; shows data labels on the line series. Default FALSE.
#'   }
#'
#' @return For type = "echarts", returns an echarts4r htmlwidget object. For type = "excel", invisibly returns the normalized path to the written file.
#'
#' @details Requires the openxlsx2 package and a system zip program on the PATH for utils::zip() (bundled with Rtools on Windows) for Excel outputs. Note that LibreOffice under-renders dotted line segments, whereas Microsoft Excel renders them correctly.
#'
#' @export
plot.tdf <- function(
    x,
    type = c("echarts","excel"),
    xl_file,
    na_omit = TRUE,
    single_layer_monthly = TRUE,
    second_layer_monthly = c("fy","calender"),
    num_digits_for_round = 1,
    asterisk_data_till = 0,
    asterisk_data_time_points = NULL,
    title = NULL,
    subtitle = NULL,
    ...) {

  viz_unified(
    td = x,
    xl_file = xl_file,
    na_omit = na_omit,
    single_layer_monthly = single_layer_monthly,
    second_layer_monthly = second_layer_monthly,
    num_digits_for_round = num_digits_for_round,
    type = type,
    title = title,
    subtitle = subtitle,
    asterisk_data_time_points = asterisk_data_time_points,
    asterisk_data_till = asterisk_data_till,
    ...
  )
}
