


financial_period_class <- c("financial_period")
calendar_period_class <- c("calendar_period")

# Set the old class for S4 methods compatibility
methods::setOldClass(financial_period_class)

#' @export
print.financial_period <- function(x, ...){
  print(as.character(x))
}


#' @export
print.calendar_period <- function(x, ...){
  print(as.character(x))
}
