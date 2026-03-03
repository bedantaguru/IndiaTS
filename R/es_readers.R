
# es:eSankhyiki Read data from https://www.esankhyiki.gov.in/
# eSankhyiki is a software for data entry and management of statistical data. It is widely used in India for various statistical surveys and data collection activities.

es_load_predefined_hmaps <- function(which_hmap) {
  fn <- get_extdata_path(paste0("hmap_", str_clean(which_hmap, no_space = TRUE)))
  if(file.exists(fn)){
    readRDS(fn)
  } else {
    stop("No predefined hmap found for the specified type: ", which_hmap)
  }
}

es_convert_gva <- function(d, hmap) {

}



