


financial_period_class <- c("financial_period")

# Set the old class for S4 methods compatibility
methods::setOldClass(financial_period_class)

#' @export
print.financial_period <- function(x, ...){
  print(as.character(x))
}


read_as_financial_period <- function(x) {

}
