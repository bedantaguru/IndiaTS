


fiscal_period_class <- c("fiscal_period", "india_period")
calendar_period_class <- c("calendar_period", "india_period")

# Set the old class for S4 methods compatibility
methods::setOldClass(fiscal_period_class)

#' @export
print.fiscal_period <- function(x, ...){
  print(as.character(x))
}


#' @export
print.calendar_period <- function(x, ...){
  print(as.character(x))
}
