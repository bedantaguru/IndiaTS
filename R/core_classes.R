


fiscal_period_class <- c("fiscal_period")
calendar_period_class <- c("calendar_period")

tdf_class <- c("tdf", "tbl_df", "tbl", "data.frame")

tdf_long_class <- c("tdf_long", "list")

# Set the old class for S4 methods compatibility
methods::setOldClass(fiscal_period_class)
methods::setOldClass(tdf_class)
methods::setOldClass(tdf_long_class)

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
  cat("A Time-Data-Frame (tdf) object\n\n")
  print(tibble::as_tibble(x))
}

#' @export
print.tdf_long <- function(x, ...){
  cat(paste0("A Time-Data-Frame (tdf) - object in Long form, with ",
             ifelse(NCOL(x$hmap)>0, NCOL(x$hmap), "no"), " disaggregation layers.\n\n"))

  print(tibble::as_tibble(x$data))
}
