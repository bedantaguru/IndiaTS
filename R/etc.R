

is_date_type <- function(x){
  lubridate::is.Date(x) || lubridate::is.POSIXt(x) || lubridate::is.POSIXlt(x) || lubridate::is.POSIXct(x)
}
