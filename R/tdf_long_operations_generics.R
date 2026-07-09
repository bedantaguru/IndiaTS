
#' Aggregate a tdf_long Object Temporally or by Component
#'
#' @description
#' A unified method to aggregate time-series data within a \code{tdf_long} object.
#' It supports both dimensional aggregation (rolling up components via the embedded
#' hierarchical map) and temporal aggregation (converting to a lower frequency).
#'
#' @details
#' The function operates in one of two modes depending on the \code{type} argument:
#'
#' \itemize{
#'   \item \strong{\code{type = "component"}}: Aggregates data dimensionally based on the
#'   embedded hierarchical map (\code{hmap}). This is a generic framework used to roll up
#'   granular classifications into higher-level aggregates. For example, aggregating
#'   sectoral data (Agriculture, Industry, Services) into headline Gross Value Added (GVA).
#'
#'   \item \strong{\code{type = "temporal"}}: Aggregates data over time to convert a
#'   high-frequency series to a lower frequency. For example, aggregating quarterly
#'   figures into half-yearly or annual figures. This mode requires the \code{to_freq}
#'   argument to be specified.
#' }
#'
#' @param x A \code{tdf_long} object containing the time-series data.
#' @param type A character string specifying the type of aggregation.
#' Must be either \code{"component"} (default) or \code{"temporal"}.
#' @param to_freq The target frequency for temporal aggregation (e.g., \code{"annual"}).
#' If left as \code{NULL} (the default) when \code{type = "temporal"}, the function
#' automatically selects the next lower frequency (e.g., monthly becomes quarterly,
#' quarterly becomes half-yearly). Ignored if \code{type = "component"}.
#' @param silent Logical. If \code{TRUE}, suppresses informational messages during
#' the aggregation process. Default is \code{FALSE}.
#' @param ... Additional arguments passed to the underlying internal aggregation functions.
#'
#' @return An aggregated \code{tdf_long} object.
#'
#' @export
#' @importFrom stats aggregate
aggregate.tdf_long <- function(
    x,
    type = c("component","temporal"),
    to_freq = NULL, silent = FALSE, ...){

  type <- match.arg(type)

  xl <- to_tdf_long_list(x)

  if(type == "component"){
    lo <- aggregate_component(xl, silent = silent)
  } else {
    lo <- aggregate_temporal(xl, to_freq = to_freq, silent = silent)
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
