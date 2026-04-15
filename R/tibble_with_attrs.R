

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
  class(x) <- union("tibble_with_attrs", class(x))
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


#' Reconstruct method for dplyr
#'
#' Re-attaches user attributes from `template` onto `data` after any
#' dplyr verb. Group structure and row names come from `NextMethod()`.
#'
#' @param data The new data returned by the verb.
#' @param template The original input.
#' @export
#' @importFrom dplyr dplyr_reconstruct
dplyr_reconstruct.tibble_with_attrs <- function(data, template) {
  out <- NextMethod()
  saved <- sticky_attrs(template)
  for (nm in names(saved)) attr(out, nm) <- saved[[nm]]
  if (!inherits(out, "tibble_with_attrs")) {
    class(out) <- union("tibble_with_attrs", class(out))
  }
  out
}

#' @export
#' @importFrom vctrs vec_restore
vec_restore.tibble_with_attrs <- function(x, to, ...) {
  out <- NextMethod()
  saved <- sticky_attrs(to)
  for (nm in names(saved)) attr(out, nm) <- saved[[nm]]
  if (!inherits(out, "tibble_with_attrs")) {
    class(out) <- union("tibble_with_attrs", class(out))
  }
  out
}


#' @export
`[.tibble_with_attrs` <- function(x, ...) {
  saved <- sticky_attrs(x)
  out <- NextMethod()
  for (nm in names(saved)) attr(out, nm) <- saved[[nm]]
  if (!inherits(out, "tibble_with_attrs")) {
    class(out) <- union("tibble_with_attrs", class(out))
  }
  out
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
