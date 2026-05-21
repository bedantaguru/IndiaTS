methods::setOldClass("tibble_with_attrs")

#' Create a tibble that preserves attributes
#'
#' @param x A data frame or tibble.
#' @param ... Name-value pairs of attributes to attach.
#' @return A `tibble_with_attrs`.
#' @examples
#' d <- tibble_with_attrs(iris, source = "datasets", built = Sys.time())
#' attr(dplyr::filter(d, Sepal.Length > 5), "source")
tibble_with_attrs <- function(x, ...) {
  x <- tibble::as_tibble(x)
  dots <- rlang::list2(...)
  for (nm in names(dots)) attr(x, nm) <- dots[[nm]]

  # Safely inject "tibble_with_attrs" without disrupting class order
  if (!inherits(x, "tibble_with_attrs")) {
    base_classes <- c("tbl_df", "tbl", "data.frame")
    front_classes <- setdiff(class(x), base_classes)
    tail_classes  <- intersect(base_classes, class(x))
    class(x) <- c(front_classes, "tibble_with_attrs", tail_classes)
  }
  x
}

#' @rdname tibble_with_attrs
as_tibble_with_attrs <- function(x) {
  if (inherits(x, "tibble_with_attrs")) return(x)
  tibble_with_attrs(x)
}

# attributes that belong to the data-frame machinery, not the user
.protected_attrs <- c("names", "row.names", "class", "groups")

sticky_attrs <- function(x) {
  a <- attributes(x)
  a[setdiff(names(a), .protected_attrs)]
}

# =============================================================================
# Core Restoration Helper
# =============================================================================

# helper: re-attach sticky attrs and strictly assert original subclass hierarchy
.restore_tibble_attrs <- function(out, template) {
  # 1. Restore attributes
  # (Note: sticky_attrs already protects "groups" from being overwritten)
  saved <- sticky_attrs(template)
  for (nm in names(saved)) attr(out, nm) <- saved[[nm]]

  # 2. Identify custom classes dplyr accidentally dropped
  dropped_classes <- setdiff(
    c("tbl_df", "tbl", "data.frame", class(template)),
    class(out)
  )

  # 3. Exclude dynamic classes from restoration.
  # If dplyr removed them from 'out' (e.g., via ungroup), let them stay dead.
  classes_to_restore <- setdiff(dropped_classes, c("grouped_df", "rowwise_df"))

  # 4. Combine everything.
  # By using class(out) here, we natively keep "grouped_df" if dplyr added/kept it!
  combined_classes <- c(classes_to_restore, setdiff(class(out), classes_to_restore))

  # 5. Force standard data frame classes strictly to the end
  base_classes <- c("tbl_df", "tbl", "data.frame")

  front_classes <- setdiff(combined_classes, base_classes)
  tail_classes  <- intersect(base_classes, combined_classes)

  class(out) <- c(front_classes, tail_classes)

  out
}

# =============================================================================
# Base / Vctrs / Dplyr Reconstructors
# =============================================================================

#' Reconstruct method for dplyr
#'
#' Re-attaches user attributes from `template` onto `data` after any
#' dplyr verb. Group structure and row names come from `NextMethod()`.
#'
#' @param data The new data returned by the verb.
#' @param template The original input.
#' @export
#' @keywords internal
#' @importFrom dplyr dplyr_reconstruct
dplyr_reconstruct.tibble_with_attrs <- function(data, template) {
  .restore_tibble_attrs(NextMethod(), template)
}

#' @export
#' @importFrom vctrs vec_restore
vec_restore.tibble_with_attrs <- function(x, to, ...) {
  .restore_tibble_attrs(NextMethod(), to)
}

#' @export
`[.tibble_with_attrs` <- function(x, ...) {
  .restore_tibble_attrs(NextMethod(), x)
}

#' @export
print.tibble_with_attrs <- function(x, ...) {
  NextMethod()
  extras <- names(sticky_attrs(x))
  if (length(extras)) {
    cat("# sticky attrs: ", paste(extras, collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}

# =============================================================================
# Explicit Dplyr Verb Overrides
# =============================================================================

#' @export
#' @importFrom dplyr mutate
mutate.tibble_with_attrs <- function(.data, ...) {
  .restore_tibble_attrs(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr filter
filter.tibble_with_attrs <- function(.data, ...) {
  .restore_tibble_attrs(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr arrange
arrange.tibble_with_attrs <- function(.data, ...) {
  .restore_tibble_attrs(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr select
select.tibble_with_attrs <- function(.data, ...) {
  .restore_tibble_attrs(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr rename
rename.tibble_with_attrs <- function(.data, ...) {
  .restore_tibble_attrs(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr rename_with
rename_with.tibble_with_attrs <- function(.data, ...) {
  .restore_tibble_attrs(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr relocate
relocate.tibble_with_attrs <- function(.data, ...) {
  .restore_tibble_attrs(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr distinct
distinct.tibble_with_attrs <- function(.data, ...) {
  .restore_tibble_attrs(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr transmute
transmute.tibble_with_attrs <- function(.data, ...) {
  .restore_tibble_attrs(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr slice
slice.tibble_with_attrs <- function(.data, ...) {
  .restore_tibble_attrs(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr summarise
summarise.tibble_with_attrs <- function(.data, ...) {
  .restore_tibble_attrs(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr summarize
summarize.tibble_with_attrs <- summarise.tibble_with_attrs

#' @export
#' @importFrom dplyr reframe
reframe.tibble_with_attrs <- function(.data, ...) {
  .restore_tibble_attrs(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr ungroup
ungroup.tibble_with_attrs <- function(x, ...) {
  .restore_tibble_attrs(NextMethod(), x)
}

#' @export
#' @importFrom dplyr group_by
group_by.tibble_with_attrs <- function(.data, ...) {
  .restore_tibble_attrs(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr rowwise
rowwise.tibble_with_attrs <- function(data, ...) {
  .restore_tibble_attrs(NextMethod(), data)
}

#' @export
#' @importFrom dplyr count
count.tibble_with_attrs <- function(x, ...) {
  .restore_tibble_attrs(NextMethod(), x)
}

#' @export
#' @importFrom dplyr tally
tally.tibble_with_attrs <- function(x, ...) {
  .restore_tibble_attrs(NextMethod(), x)
}

#' @export
#' @importFrom dplyr add_count
add_count.tibble_with_attrs <- function(x, ...) {
  .restore_tibble_attrs(NextMethod(), x)
}

#' @export
#' @importFrom dplyr nest_by
nest_by.tibble_with_attrs <- function(.data, ...) {
  .restore_tibble_attrs(NextMethod(), .data)
}
