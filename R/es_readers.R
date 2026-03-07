
# es:eSankhyiki Read data from https://www.esankhyiki.gov.in/
# eSankhyiki is a software for data entry and management of statistical data. It is widely used in India for various statistical surveys and data collection activities.

es_load_predefined_data <- function(which_data) {
  fn <- get_extdata_path(paste0("es_", str_clean(which_data, no_space = TRUE)))
  if(file.exists(fn)){
    readRDS(fn)
  } else {
    stop("No predefined hmap found for the specified type: ", which_data)
  }
}

es_convert_gva_proc1 <- function(dn, hmap) {
  if(NROW(dn) == 0) return(tibble::tibble())

  dout <- es_convert_gva_proc1_part(dn, hmap)

  d_not_done <- dn %>% anti_join(dout, by = "row_id")

  if(NROW(d_not_done)>0){
    # TODO # Start it need to check
    # Kept old copy in "IndiaTS Accumulate"
    stop("[Not Developed] If you see this please contact developers. ",
         "It means there are some rows that could not be mapped to any disaggregation group for time: ",
         unique(d_not_done$time)[1], " and revision: ",
         unique(d_not_done$revision)[1],
         ". Please check the input data and the hmap.", call. = FALSE)
  }

  dout

}

es_convert_gva_proc1_part <- function(dn, hmap){
  if(NROW(dn) == 0) return(tibble::tibble())

  dout <- tibble::tibble()

  if("industry" %in% colnames(dn)){
    if("subindustry" %in% colnames(dn)){
      dn_ind <- dn %>% filter(is.na(subindustry))
    } else {
      dn_ind <- dn
    }

    chk <- dn_ind %>% group_by(industry, price_basis) %>%
      cols_causing_group_variation()

    if(length(chk)>0) {
      stop(
        "The combination of industry and price_basis is not unique. ",
        "Please check the input data for industry category (one example): ",
        paste0(unique(dn_ind$industry)[1], collapse = ", "), " and price basis: ",
        paste0(unique(dn_ind$price_basis)[1], collapse = ", "), call. = FALSE)
    }

    dgs <- hmap_which_disaggregation_group(dn_ind$industry, hmap)

    if(all(!is.na(dgs)) && length(dgs)>0){

      dn_ind_part <- dgs %>% map(
        function(dg){
          d0 <- dn_ind %>%
            filter(str_clean(industry) %in% str_clean(hmap[[dg]]))

          d0 <- d0 %>%
            mutate(meta.name = industry,
                   meta.disaggregation_group = dg) %>%
            select(-industry)

          not_preset <- str_clean(hmap[[dg]]) %>% setdiff(str_clean(d0$meta.name))

          if(length(not_preset)>0){
            stop("These industry categories are not present in the input data but are expected based on the hmap for disaggregation group:- ", dg, ":- ",
                 paste0(not_preset, collapse = ", "), call. = FALSE)
          }

          d0

        }
      ) %>% bind_rows()

      dn_ind_part <- dn_ind_part %>% select(time, revision, price_basis, meta.name, meta.disaggregation_group, value, row_id, unit)
      dout <- dout %>% bind_rows(dn_ind_part)

    }

  }

  if(("subindustry" %in% colnames(dn)) && ("industry" %in% colnames(dn))){
    dn_sub <- dn %>% filter(!is.na(subindustry))
    dgs <- hmap_which_disaggregation_group(dn_sub$subindustry, hmap)

    if(all(!is.na(dgs)) && length(dgs)>0){
      dn_sub_part <- dgs %>% map(
        function(dg){
          d0 <- dn_sub %>%
            filter(str_clean(subindustry) %in% str_clean(hmap[[dg]]))

          d0 <- d0 %>%
            mutate(meta.name = subindustry,
                   meta.disaggregation_group = dg) %>%
            select(-subindustry, -industry)

          not_preset <- str_clean(hmap[[dg]]) %>% setdiff(str_clean(d0$meta.name))

          if(length(not_preset)>0){
            # Search in industry level if not found in subindustry
            d1 <- dn_ind %>%
              filter(str_clean(industry) %in% str_clean(hmap[[dg]]))

            d1 <- d1 %>%
              mutate(meta.name = industry,
                     meta.disaggregation_group = dg) %>%
              select(-subindustry, -industry)

            d0 <- d0 %>%
              bind_rows(d1)

            not_preset <- str_clean(hmap[[dg]]) %>% setdiff(str_clean(d0$meta.name))

            # Now raise error as no way to get those
            if(length(not_preset)>0){
              stop("These subindustry categories are not present in the input data but are expected based on the hmap for disaggregation group:- ", dg, ":- ",
                   paste0(not_preset, collapse = ", "), call. = FALSE)

            }

          }

          d0


        }
      ) %>% bind_rows()

      dn_sub_part <- dn_sub_part %>% select(time, revision, price_basis, meta.name, meta.disaggregation_group, value, row_id, unit)
      dout <- dout %>% bind_rows(dn_sub_part)

    }

  }

  # institutional_sector processing can be added similarly if needed
  # TODO
  if("institutional_sector" %in% colnames(dn)){
    dn_inst <- dn %>%
      filter(is.na(industry) & is.na(subindustry) & !is.na(institutional_sector))

    dgs <- hmap_which_disaggregation_group(dn_sec$institutional_sector, hmap)

    if(!is.na(dgs) && length(dgs)>0){

      dn_inst_part <- dgs %>% map(
        function(dg){
          d0 <- dn_inst %>%
            filter(str_clean(institutional_sector) %in% str_clean(hmap[[dg]]))

          d0 <- d0 %>%
            mutate(meta.name = institutional_sector,
                   meta.disaggregation_group = dg) %>%
            select(-institutional_sector)

          not_preset <- str_clean(hmap[[dg]]) %>% setdiff(str_clean(d0$meta.name))

          if(length(not_preset)>0){
            stop("These institutional sector categories are not present in the input data but are expected based on the hmap for disaggregation group:- ", dg, ":- ",
                 paste0(not_preset, collapse = ", "), call. = FALSE)
          }

          d0

        }
      ) %>% bind_rows()

      dn_inst_part <- dn_inst_part %>%
        select(time, revision, price_basis, meta.name, meta.disaggregation_group, value, row_id, unit)
      dout <- dout %>% bind_rows(dn_inst_part)
    }

  }

  dout

}

es_gva_annual_revision_summary <- function(dat) {

  ds <- dat %>% distinct(year, base_year, revision, industry, subindustry, institutional_sector)
  ds <- ds %>% mutate(
    industry = str_clean(industry),
    subindustry = str_clean(subindustry),
    institutional_sector = str_clean(institutional_sector)
  )

  f_str <- function(x){
    x <- x[!is.na(x)] %>% unique()
    p1 <- x %>% sort() %>% paste0(collapse = "; ")
    paste0("(", length(x), ") : ", p1)
  }

  ds1 <- ds %>% group_by(year, base_year, revision) %>%
    summarise_all(f_str)

  ds2 <- ds1 %>%
    group_by(industry, subindustry, institutional_sector) %>%
    summarise_all(f_str)

  ds2

}






es_convert_gva_common <- function(
    dat,
    hmap = NULL,
    expected_cols,
    required_cols,
    can_keep_cols = character(0),
    cat_cols,
    freq_to_check = NULL,
    time_fn,                       # function(dat) returns vector 'time'
    remove_empty_cat_cols = FALSE, # logical: do annual-style rem_cats pruning
    use_rev_meta = FALSE,          # logical: attach rev_meta (annual)
    static_revision_tag = NULL     # used for quarterly to set revision
) {
  # load hmap if needed
  if (missing(hmap) || is.null(hmap)) {
    hmap <- es_load_predefined_data("hmap_gva")
  }

  # column name cleaning
  colnames(dat) <- str_clean(colnames(dat), no_space = TRUE)

  # Init rev_meta
  rev_meta <- NULL

  # simple required/expected checks (fail early)
  if (!all(required_cols %in% colnames(dat))) {
    stop("Input data is missing some required columns.",
         "\nMissing columns are: ",
         paste0(setdiff(required_cols, colnames(dat)), collapse = ", "),
         call. = FALSE)
  }

  if (!all(colnames(dat) %in% expected_cols)) {
    stop("Input data has some unexpected columns.",
         "\nUnexpected columns are: ",
         paste0(setdiff(colnames(dat), expected_cols), collapse = ", "),
         call. = FALSE)
  }

  if (!is.null(freq_to_check) && ("frequency" %in% colnames(dat))) {
    freqs <- dat$frequency %>% unique
    if (length(freqs) > 1 || !all(tolower(freqs) == freq_to_check)) {
      stop(paste0("Frequency column found but it is not ", freq_to_check,
                  " for all rows."), call. = FALSE)
    }
  }

  # optional revision metadata validation (annual)
  if (use_rev_meta) {

    if(!("revision" %in% colnames(dat))) {
      stop("Revision metadata validation requested but no 'revision' column found in the data.", call. = FALSE)
    }
    rev_meta <- es_load_predefined_data("revision_meta") %>%
      mutate(revision_clean = str_clean(revision))

    non_present_revs <- dat$revision %>% str_clean %>% unique %>% setdiff(rev_meta$revision_clean)
    if (length(non_present_revs) > 0) {
      stop("These revisions found in the data are not present in the revision metadata. Please check the input data and the revision metadata. Revisions found: ",
           paste0(non_present_revs, collapse = ", "), call. = FALSE)
    }
  }

  # replacement "gross value added" -> "Gross Value Added"
  if (length(cat_cols) > 0) {
    dat <- dat %>%
      mutate(
        across(
          .cols = any_of(cat_cols),
          .fns = ~ ifelse(str_detect(tolower(.x), "gross value added"), "Gross Value Added", .x)
        )
      )
  } else {
    stop("At least one category column must be specified in cat_cols.", call. = FALSE)
  }

  # categories from hmap
  all_cats <- hmap %>% as.matrix %>% as.character %>% unique %>% str_clean %>% unique

  # filter rows that contain any category not in hmap (same as you had)
  d_filt <- dat %>%
    filter(
      if_any(
        .cols = any_of(cat_cols),
        .fns = ~ str_clean(.x) %in% all_cats
      )
    )

  # optional: remove category columns that are only NA/total (annual logic)
  rem_cats <- NULL
  if (remove_empty_cat_cols && length(cat_cols) > 0) {
    cat_state <- d_filt[cat_cols] %>% map_lgl(function(cl) {
      cls <- cl %>% str_clean %>% unique
      cls <- cls %>% setdiff("gross value added")
      cls <- cls[!is.na(cls)]
      length(cls) > 0
    })
    if (any(!cat_state)) {
      rem_cats <- names(which(!cat_state))
      d_filt <- d_filt %>% filter(if_all(any_of(rem_cats), is.na))
    }
  }

  final_cols <- intersect(c(required_cols, can_keep_cols), colnames(dat))
  if (!is.null(rem_cats)) final_cols <- setdiff(final_cols, rem_cats)

  d_final <- d_filt[final_cols] %>% distinct()

  key_cols <- c("base_year", "year", "quarter", "revision", cat_cols)
  key_cols_this <- intersect(key_cols, colnames(d_final))

  chk <- d_final %>% cols_causing_group_variation(group_colnames = key_cols_this)

  if(length(chk) > 0) {
    stop("There are columns that have multiple values for the same combination of key columns:",
         paste0(chk, collapse = ", "), call. = FALSE)
  }

  d_final <- d_final %>% mutate(across(where(is.character), ~ na_if(., "*")))

  # track rows
  d_final$row_id <- seq_len(nrow(d_final))

  # rename current/constant -> nominal/real
  d_final <- d_final %>% rename(nominal = current_price, real = constant_price)

  # pivot longer
  d <- d_final %>% pivot_longer(
    cols = c("nominal", "real"),
    names_to = "price_basis", values_to = "value")

  # append base_year to price_basis if present
  if ("base_year" %in% colnames(d_final)) {
    d <- d %>%
      mutate(price_basis = paste0(price_basis, " base_", base_year)) %>%
      select(-base_year)
  }

  # set revision: if revision column present keep it, else use static_revision_tag if provided
  if (!"revision" %in% colnames(d)) {
    if (!is.null(static_revision_tag)) {
      d <- d %>% mutate(revision = static_revision_tag)
    } else {
      d <- d %>% mutate(revision = "#main")
    }
  }

  # create time using provided function
  d <- d %>% mutate(time = time_fn(.))

  # unique-check prior to main processing
  cat_cols_present <- intersect(cat_cols, colnames(d))
  d_chk <- d[c("time", "revision", "price_basis", cat_cols_present)] %>% distinct()
  if (nrow(d_chk) != nrow(d)) {
    stop("There are duplicate rows found before main processing. Please check the input data and the processing steps.", call. = FALSE)
  }

  # main processing: split by time and revision
  # dl <- d %>% split(list(.$time, .$revision))
  dl <- split(d, interaction(d$time, d$revision, drop = TRUE))

  d_out <- dl %>% map(function(.x) {
    es_convert_gva_proc1(.x, hmap)
  }) %>% bind_rows()

  # rows not processed
  not_done <- d %>% anti_join(d_out, by = "row_id")
  if (nrow(not_done) > 0) {
    warning("There are some rows that could not be processed. Please check the input data and the hmap.", call. = FALSE)
  }

  # final tidy / renames
  d_out <- d_out %>%
    mutate(value = as.numeric(value)) %>%
    rename(time = time,
           meta.release_tag = revision,
           meta.unit = unit,
           meta.price_basis = price_basis,
           value.level = value) %>%
    select(time, meta.release_tag, meta.price_basis, meta.unit, meta.name, meta.disaggregation_group, value.level)

  d_chk <- d_out %>% distinct(time, meta.release_tag, meta.price_basis, meta.name, meta.disaggregation_group)
  if (nrow(d_chk) != nrow(d_out)) {
    stop("There are duplicate rows found in the output data. Please check the processing steps and the input data.", call. = FALSE)
  }

  # attach revision metadata if requested (annual)
  if (use_rev_meta) {
    d_out <- d_out %>%
      mutate(revision_clean = str_clean(meta.release_tag)) %>%
      left_join(rev_meta %>% select(-revision), by = "revision_clean") %>%
      rename(meta.release_order = release_order) %>%
      distinct(time, meta.release_tag, meta.release_order, meta.price_basis, meta.unit, meta.name, meta.disaggregation_group, value.level)
  } else {
    d_out <- d_out %>%
      mutate(meta.release_order = 1) %>%
      distinct(time, meta.release_tag, meta.release_order, meta.price_basis, meta.unit, meta.name, meta.disaggregation_group, value.level)
  }

  list(data = d_out, hmap = hmap)
}

# Quarterly wrapper
es_convert_gva_qtr <- function(dat, hmap, static_revision_tag = "#main") {
  if (missing(hmap) || is.null(hmap)) {
    hmap <- es_load_predefined_data("hmap_gva")
  }

  expected_cols <- c("base_year", "series", "year", "indicator", "frequency", "revision",
                     "industry", "subindustry", "institutional_sector", "quarter",
                     "current_price", "constant_price", "unit")

  required_cols <- c("year", "quarter", "industry", "current_price", "constant_price")
  can_keep_cols <- c("base_year", "unit")
  cat_cols <- "industry"

  time_fn <- function(d) paste0(d$year, " ", d$quarter)

  es_convert_gva_common(dat = dat,
                        hmap = hmap,
                        expected_cols = expected_cols,
                        required_cols = required_cols,
                        can_keep_cols = can_keep_cols,
                        cat_cols = cat_cols,
                        time_fn = time_fn,
                        freq_to_check = "quarterly",
                        remove_empty_cat_cols = FALSE,
                        use_rev_meta = FALSE,
                        static_revision_tag = static_revision_tag)
}

# Annual wrapper
es_convert_gva_annual <- function(dat, hmap) {
  if (missing(hmap) || is.null(hmap)) {
    hmap <- es_load_predefined_data("hmap_gva")
  }

  expected_cols <- c("base_year", "series", "year", "indicator", "frequency", "revision",
                     "industry", "subindustry", "institutional_sector", "quarter",
                     "current_price", "constant_price", "unit")

  required_cols <- c("year", "revision",
                     "industry", "subindustry", "institutional_sector",
                     "current_price", "constant_price")

  can_keep_cols <- c("unit", "base_year")
  cat_cols <- c("industry", "subindustry", "institutional_sector")

  time_fn <- function(d) d$year

  es_convert_gva_common(dat = dat,
                        hmap = hmap,
                        expected_cols = expected_cols,
                        required_cols = required_cols,
                        can_keep_cols = can_keep_cols,
                        cat_cols = cat_cols,
                        time_fn = time_fn,
                        freq_to_check = "annual",
                        remove_empty_cat_cols = TRUE,
                        use_rev_meta = TRUE,
                        static_revision_tag = NULL)
}




#' Convert GVA Data to Long Time Series Format
#'
#' @description
#' A unified entry point for converting Gross Value Added (GVA) data sourced
#' from eSankhyiki into a standardised long-format time series. Supports both
#' quarterly and annual input data. Frequency can be supplied explicitly or
#' detected automatically from a `frequency` column in the data.
#'
#' @param dat A data frame containing raw GVA data. Must include columns
#'   appropriate for the frequency (see Details). If `freq` is not supplied,
#'   a `frequency` column must be present with a single unique value of either
#'   `"quarterly"` or `"annual"`.
#' @param hmap Optional. A named list used as the hierarchy/disaggregation map.
#'   If `NULL` or missing, the predefined `"hmap_gva"` map is loaded via
#'   \code{es_load_predefined_data()}. Only override this for custom mappings.
#' @param freq Optional. A string specifying the data frequency: `"quarterly"`
#'   or `"annual"`. If omitted, frequency is auto-detected from the
#'   `frequency` column in `dat`.
#'
#' @details
#' **Quarterly data** (`freq = "quarterly"`) requires columns:
#' `year`, `quarter`, `industry`, `current_price`, `constant_price`.
#' Optional columns: `base_year`, `unit`, `frequency`.
#'
#' **Annual data** (`freq = "annual"`) requires columns:
#' `base_year`, `year`, `revision`, `industry`, `subindustry`,
#' `institutional_sector`, `current_price`, `constant_price`.
#' Optional column: `unit`.
#'
#' In both cases, valid column names from the full expected set are:
#' `base_year`, `series`, `year`, `indicator`, `frequency`, `revision`,
#' `industry`, `subindustry`, `institutional_sector`, `quarter`,
#' `current_price`, `constant_price`, `unit`.
#' Any column outside this set will cause an error.
#'
#' The output is a standardised long-format object.
#'
#' @return A long-format time series object,
#'   containing columns such as `time`, `meta.release_tag`, `meta.release_order`,
#'   `meta.price_basis`, `meta.unit`, `meta.name`, `meta.disaggregation_group`,
#'   and `value.level`.
#' @export
es_convert_gva <- function(dat, hmap, freq){
  if (missing(hmap) || is.null(hmap)) {
    hmap <- es_load_predefined_data("hmap_gva")
  }

  if(missing(freq)){
    # check and detect from "frequency" col
    if(!"frequency" %in% colnames(dat)){
      stop("Frequency not specified and no 'frequency' column found in the data. Please specify frequency as 'quarterly' or 'annual', or ensure there is a 'frequency' column in the data.", call. = FALSE)
    }

    freqs <- dat$frequency %>% unique
    if(length(freqs) != 1 || !all(tolower(freqs) %in% c("quarterly", "annual"))){
      stop("Multiple frequencies found in the data or frequency is not 'quarterly'/'annual'. Please check the 'frequency' column in the data.", call. = FALSE)
    }

    freq <- tolower(freqs[1])
  }

  if(freq == "quarterly"){
    tdl <- es_convert_gva_qtr(dat, hmap)
  } else if(freq == "annual"){
    tdl <- es_convert_gva_annual(dat, hmap)
  } else {
    stop("Unsupported frequency. Please specify frequency as 'quarterly' or 'annual'.", call. = FALSE)
  }

  tdf_long_make(tdl)

}



