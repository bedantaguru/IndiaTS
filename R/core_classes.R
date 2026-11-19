


fiscal_period_class <- c("fiscal_period")
calendar_period_class <- c("calendar_period")

tdf_class <- c("tdf", "tibble_with_attrs", "tbl_df", "tbl", "data.frame")

tdf_long_list_class <- c("tdf_long_list", "list")

tdf_long_class <- c("tdf_long", "tibble_with_attrs", "tbl", "data.frame")

temporally_linked_tdf_long_class <- c("temporally_linked_tdf_long","list")

# Set the old class for S4 methods compatibility
methods::setOldClass(fiscal_period_class)
methods::setOldClass(tdf_class)
methods::setOldClass(tdf_long_list_class)
methods::setOldClass(tdf_long_class)
methods::setOldClass(temporally_linked_tdf_long_class)

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
print.temporally_linked_tdf_long <- function(x, ...){
  cat("A 'temporally linked tdf long' object\n")
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
  cat("A Time-Data-Frame (tdf) - object in Long form, with:\n\n")

  # Grouped info
  if(dplyr::is_grouped_df(x)){
    gvars <- dplyr::group_vars(x)
    cat(paste0(" * Grouped with gropued vars: ", paste0(gvars, collapse = ", "), "\n\n"))
  }

  # 0. Completeness
  req_cols <- c("time", "meta.release_tag", "meta.price_basis", "meta.name",
                "meta.disaggregation_group")

  def_cols <- setdiff(req_cols, names(x))
  if(length(def_cols)>0) {
    cat(paste0(" * Note: ", paste0(def_cols, collapse = ", "), " - cols not present. \n\n"))
  }

  cat("Meta-Info:-\n")


  # 1. Disaggregation layers
  hmap_cols <- NCOL(attr(x, "hmap"))
  n_active_dgs <- 0
  n_active_tag <- ""
  layer_text <- if (!is.null(hmap_cols) && hmap_cols > 1) hmap_cols else "no"
  if(hmap_cols>0 && ("meta.disaggregation_group" %in% names(x))){
    n_active_dgs <- x$meta.disaggregation_group |> intersect(colnames(attr(x, "hmap"))) |> length()
    n_active_tag <- ifelse(n_active_dgs<hmap_cols, paste0(" (",n_active_dgs, "-active)"), "")
  }
  cat(paste0(" * ", layer_text, " disaggregation layers", n_active_tag, "\n"))

  # 2. Time periods
  if ("time" %in% names(x)) {
    n_time <- length(unique(x$time))
    continuous_tag <- ifelse(is_continuous(x$time), "continuous", "< DISCONTINUOUS >")
    to_from_tag <- paste0(min(x$time), " to ", max(x$time))
    cat(paste0(" * ", n_time, " unique ", continuous_tag," time periods; (",to_from_tag,")\n"))
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
      has_real <- any(stringr::str_detect(pb_lower, "real"), na.rm = TRUE)
      has_nom  <- any(stringr::str_detect(pb_lower, "nominal"), na.rm = TRUE)

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

