


fiscal_period_class <- c("fiscal_period")
calendar_period_class <- c("calendar_period")

tdf_class <- c("tdf", "tbl_df", "tbl", "data.frame")

tdf_long_list_class <- c("tdf_long_list", "list")

tdf_long_class <- c("tdf_long", "tibble_with_attrs", "tbl", "data.frame")

# Set the old class for S4 methods compatibility
methods::setOldClass(fiscal_period_class)
methods::setOldClass(tdf_class)
methods::setOldClass(tdf_long_list_class)
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
print.tdf_long_list <- function(x, ...){
  cat(paste0("(Internal) A Time-Data-Frame (tdf) List - object in Long form, with ",
             ifelse(NCOL(x$hmap)>1, NCOL(x$hmap), "no"), " disaggregation layers.\n\n"))

  print(tibble::as_tibble(x$data))
}


#' @export
frequency.tdf_long_list <- function(tdl){
  tdf_long_check_shallow(tdl)
  tdl$data %>% pull(time) %>% frequency.fiscal_period(singular = TRUE)
}

#' @export
print.tdf_long <- function(x, ...){
  cat(paste0("A Time-Data-Frame (tdf) - object in Long form, with ",
             ifelse(NCOL(attr(x,"hmap"))>1, NCOL(attr(x,"hmap")), "no"), " disaggregation layers.\n\n"))

  print(tibble::as_tibble(x), n = 5)
}

