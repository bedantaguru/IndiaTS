


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
print.tdf_long <- function(x, ...) {
  cat("A Time-Data-Frame (tdf) - object in Long form, with:\n")

  # 1. Disaggregation layers
  hmap_cols <- NCOL(attr(x, "hmap"))
  layer_text <- if (!is.null(hmap_cols) && hmap_cols > 1) hmap_cols else "no"
  cat(paste0(" * ", layer_text, " disaggregation layers\n"))

  # 2. Time periods
  if ("time" %in% names(x)) {
    n_time <- length(unique(x$time))
    cat(paste0(" * ", n_time, " unique time periods\n"))
  }

  # 3. Release tags (Only prints if > 1)
  if ("meta.release_tag" %in% names(x)) {
    n_tags <- length(unique(x$meta.release_tag))
    if (n_tags > 1) {
      cat(paste0(" * ", n_tags, " distinct release tags\n"))
    }
  }

  # 4. Price basis and Real/Nominal check
  if ("meta.price_basis" %in% names(x)) {
    unique_pb <- unique(x$meta.price_basis)
    n_pb <- length(unique_pb)

    if (n_pb > 0) {
      pb_lower <- tolower(unique_pb)
      has_real <- any(stringr::str_detect(pb_lower, "real"))
      has_nom  <- any(stringr::str_detect(pb_lower, "nominal"))

      rn_flags <- c()
      if (has_real) rn_flags <- c(rn_flags, "Real")
      if (has_nom)  rn_flags <- c(rn_flags, "Nominal")

      rn_text <- ""
      if (length(rn_flags) > 0) {
        rn_text <- paste0(" (containing: ", paste(rn_flags, collapse = " & "), ")")
      }

      cat(paste0(" * ", n_pb, " price basis type", ifelse(n_pb > 1, "s", ""), rn_text, "\n"))
    }
  }

  # 5. Column counts for meta.* and value.*
  n_meta <- sum(stringr::str_detect(names(x), "^meta\\."))
  n_val  <- sum(stringr::str_detect(names(x), "^value\\."))
  cat(paste0(" * Columns: ", n_meta, " metadata, ", n_val, " value\n\n"))

  # Finally, print the underlying tibble
  print(tibble::as_tibble(x), n = 5)
}

