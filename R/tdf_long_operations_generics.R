
#' Aggregate a `tdf_long` Object
#'
#' @description
#' Aggregates a `tdf_long` object either across time (temporal aggregation)
#' or across components using the embedded hierarchical map.
#'
#' @details
#' The function supports two modes of aggregation:
#'
#' \itemize{
#'   \item \strong{\code{type = "temporal"}} aggregates a higher-frequency series to
#'   a lower frequency (e.g., monthly to quarterly or quarterly to yearly). The target
#'   frequency can be specified using \code{to_freq}; if omitted, the next lower
#'   frequency is chosen automatically.
#'
#'   \item \strong{\code{type = "component"}} aggregates series across the hierarchy
#'   defined in the embedded \code{hmap}. This is useful for rolling up detailed
#'   classifications into higher-level aggregates (e.g., industry groups into total GVA).
#' }
#'
#' @param x A `tdf_long` object.
#' @param type Type of aggregation. One of `"temporal"` (default) or `"component"`.
#' @param to_freq Target frequency for temporal aggregation (e.g., `"quarter"`,
#'   `"halfyear"` or `"year"`). If `NULL`, the next lower frequency is selected
#'   automatically. Ignored when `type = "component"`.
#' @param aggregate_function Function used to aggregate observations during temporal
#'   aggregation. Defaults to [base::sum].
#' @param silent Logical. If `TRUE`, suppresses informational messages.
#' @param ... Additional arguments passed to internal aggregation methods.
#'
#' @return
#' A `tdf_long` object containing the aggregated series.
#'
#' @export
#' @method aggregate tdf_long
#' @importFrom stats aggregate
aggregate.tdf_long <- function(
    x,
    type = c("temporal", "component"),
    to_freq = NULL,
    aggregate_function = sum,
    on_incomplete_periods = FALSE,
    silent = FALSE, ...){

  type <- match.arg(type)

  xl <- to_tdf_long_list(x)

  if(type == "component"){
    lo <- aggregate_component(xl, silent = silent)
  } else {
    lo <- aggregate_temporal(xl, to_freq = to_freq, silent = silent,
                             aggregate_function = aggregate_function,
                             aggregate_on_incomplete = on_incomplete_periods)
  }

  as_tdf_long(lo)

}




#' @export
#' @importFrom dplyr group_by
group_by.tdf_long <- function(.data, ..., .add = FALSE, .drop = dplyr::group_by_drop_default(.data)) {
  # 1. Check if ... is empty
  if (...length() == 0) {

    # 2. Pass predefined unquoted column names directly to NextMethod
    NextMethod("group_by", .data,
               .data$time, .data$meta.release_tag, .data$meta.price_basis,
               .data$meta.name, .data$meta.disaggregation_group,
               .add = .add, .drop = .drop)

  } else {

    # 3. If the user provided their own groups, proceed normally
    NextMethod()

  }
}
