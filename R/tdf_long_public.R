


#' @export
frequency.tdf_long <- function(tdl){
  tdf_long_check_shallow(tdl)
  tdl$data %>% pull(time) %>% frequency.fiscal_period(singular = TRUE)
}
