

is_date_type <- function(x){
  lubridate::is.Date(x) || lubridate::is.POSIXt(x) || lubridate::is.POSIXlt(x) || lubridate::is.POSIXct(x)
}


is_time <- function(x){
  is_date_type(x) || inherits(x, calendar_period_class[1]) || inherits(x, fiscal_period_class[1])
}

is_period_type <- function(x){
  inherits(x, calendar_period_class[1]) || inherits(x, fiscal_period_class[1])
}
