


fiscal_period_class <- c("fiscal_period")
calendar_period_class <- c("calendar_period")

tdf_class <- c("tdf", "tbl_df", "tbl", "data.frame")

tdf_long_class <- c("tdf_long", "list")

# Set the old class for S4 methods compatibility
methods::setOldClass(fiscal_period_class)
methods::setOldClass(tdf_class)

#' @export
print.fiscal_period <- function(x, ...){
  print(as.character(x))
}


#' @export
print.calendar_period <- function(x, ...){
  print(as.character(x))
}


#' @export
print.tdf <- function(x, ...){
  cat("A time-data-frame (tdf) object\n")
  print(tibble::as_tibble(x))
}
