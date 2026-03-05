
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

es_convert_gva_annual <- function(dat, hmap) {

  if(missing(hmap)){
    hmap <- es_load_predefined_data("hmap_gva")
  }

  colnames(dat) <- str_clean(colnames(dat), no_space = TRUE)

  expected_cols <- c("base_year", "series", "year", "indicator", "frequency", "revision",
                     "industry", "subindustry", "institutional_sector", "quarter",
                     "current_price", "constant_price", "unit")

  required_cols <- c("base_year", "year", "revision",
                     "industry", "subindustry","institutional_sector",
                     "current_price", "constant_price")

  can_keep_cols <- c("unit")


  if(!all(required_cols %in% colnames(dat))){
    stop("Input data is missing some required columns.",
         "\nMissing columns are: ",
         paste0(setdiff(required_cols, colnames(dat)), collapse = ", "), call. = FALSE)
  }

  if(!all(colnames(dat) %in% expected_cols)){
    stop("Input data has some unexpected columns.",
         "\nUnexpected columns are: ",
         paste0(setdiff(colnames(dat), expected_cols), collapse = ", "), call. = FALSE)
  }

  # Check if the known revisions in the data are present in the revision metadata
  rev_meta <- es_load_predefined_data("revision_meta")

  rev_meta <- rev_meta %>%
    mutate(revision_clean = str_clean(revision))

  non_present_revs <- dat$revision %>% str_clean %>% unique %>% setdiff(rev_meta$revision_clean)

  if(length(non_present_revs)>0){
    stop("These revisions found in the data are not present in the revision metadata. Please check the input data and the revision metadata. Revisions found: ",
         paste0(non_present_revs, collapse = ", "), call. = FALSE)
  }


  all_cats <- hmap %>%
    as.matrix %>% as.character %>% unique %>% str_clean %>% unique

  cat_cols <- c( "industry", "subindustry", "institutional_sector")

  # Change any category values that contain "gross value added" (Total Gross Value Added) to "Gross Value Added"
  dat <- dat %>%
    mutate(
      across(
        .cols = any_of(cat_cols),
        .fns = ~ ifelse(
          str_detect(tolower(.x), "gross value added"),
          "Gross Value Added", .x)
      ))

  # filter which are not in the hmap categories
  d_filt <- dat %>%
    filter(
      if_any(
        .cols = any_of(cat_cols),
        .fns = ~ str_clean(.x) %in% all_cats
      ))


  # Now filter based on category cols which are left with empty and total only
  cat_state <- d_filt[cat_cols] %>% map_lgl(
    function(cl){
      cls <- cl %>% str_clean %>% unique
      cls <- cls %>% setdiff("gross value added")
      cls <- cls[!is.na(cls)]
      length(cls)>0
    }
  )

  if(any(!cat_state)){
    rem_cats <- names(which(!cat_state))
    d_filt <- d_filt %>%
      filter(if_all(any_of(rem_cats), is.na))
  }else{
    rem_cats <- NULL
  }

  final_cols <- intersect(c(required_cols, can_keep_cols), colnames(dat)) %>% setdiff(rem_cats)

  d_final <- d_filt[final_cols] %>% distinct()

  d_final <- d_final %>% mutate(across(where(is.character), ~na_if(., "*")))

  # For tracking
  d_final$row_id <- seq(NROW(d_final))

  # Process after is tuned to >> # Now the flow is done mainly for "industry", "subindustry"
  # TODO "institutional_sector"

  # Rename c("current_price", "constant_price") to "nominal", "real"

  d_final <- d_final %>%
    rename(
      nominal = current_price,
      real = constant_price
    )

  d <- d_final %>%
    pivot_longer(
      cols = c("nominal", "real"),
      names_to = "price_basis", values_to = "value"
    )

  if("base_year" %in% colnames(d_final)){
    d <- d %>% mutate(
      price_basis = paste0(price_basis, " base_", base_year)
    )

    d <- d %>% select(-base_year)
  }

  # Unique check before main processing
  cat_cols <- intersect(cat_cols, colnames(d))
  d_chk <- d[c("year","revision","price_basis", cat_cols)] %>% distinct()

  if(NROW(d_chk)!=NROW(d)){
    stop("There are duplicate rows found before main processing. Please check the input data and the processing steps.", call. = FALSE)
  }

  ########## Main Processing #########

  dl <- d %>% split(list(.$year, .$revision))

  do_for_a_section <- function(dn) {
    if(NROW(dn) == 0) return(tibble::tibble())

    dout <- tibble::tibble()

    if("industry" %in% colnames(dn)){
      dn_ind <- dn %>% filter(is.na(subindustry))
      dgs <- hmap_which_disaggregation_group(dn_ind$industry, hmap)

      if(!is.na(dgs) && length(dgs)>0){

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

        dn_ind_part <- dn_ind_part %>% select(year, revision, price_basis, meta.name, meta.disaggregation_group, value, row_id, unit)
        dout <- dout %>% bind_rows(dn_ind_part)

      }

    }

    if(("subindustry" %in% colnames(dn)) && ("industry" %in% colnames(dn))){
      dn_sub <- dn %>% filter(!is.na(subindustry))
      dgs <- hmap_which_disaggregation_group(dn_sub$subindustry, hmap)

      if(!is.na(dgs) && length(dgs)>0){
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

        dn_sub_part <- dn_sub_part %>% select(year, revision, price_basis, meta.name, meta.disaggregation_group, value, row_id, unit)
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
          select(year, revision, price_basis, meta.name, meta.disaggregation_group, value, row_id, unit)
        dout <- dout %>% bind_rows(dn_inst_part)
      }

    }

    # Ideally only "Gross Value Added" would be remaining
    d_not_done <- dn %>% anti_join(dout, by = "row_id")

    if(NROW(d_not_done)>0){
      d_not_done <- d_not_done %>%
        rowwise() %>%
        mutate(
          cat_this = c_across(all_of(cat_cols)) %>%
            na.omit() %>%
            unique() %>%
            sort() %>%
            paste(collapse = " ")
        ) %>%
        ungroup()

      dgs <- hmap_which_disaggregation_group(d_not_done$cat_this, hmap)

      # warn if more than 1 disaggregation group is found
      if(length(unique(dgs))>1){
        warning(
          "More than 1 disaggregation group found for the remaining categories in year: ",
          unique(d_not_done$year)[1], " and revision: ",
          unique(d_not_done$revision)[1],
          ". This may lead to incorrect mapping. Please check the input data and the hmap. Disaggregation groups found: ",
          paste0(unique(dgs), collapse = ", "))
      }

      dn_part <- d_not_done %>%
        mutate(meta.name = cat_this,
               meta.disaggregation_group = dgs) %>%
        select(year, revision, price_basis, meta.name, meta.disaggregation_group, value, row_id, unit)

      dout <- dout %>% bind_rows(dn_part)

      # Ideally only it should be empty
      d_not_done <- dn %>% anti_join(dout, by = "row_id")
      if(NROW(d_not_done)>0){
        warning("There are still some rows that could not be mapped to any disaggregation group for year: ",
                unique(d_not_done$year)[1], " and revision: ",
                unique(d_not_done$revision)[1],
                ". Please check the input data and the hmap.", call. = FALSE)
      }



    }

    dout

  }

  d_out <- dl %>% map(do_for_a_section) %>% bind_rows()

  not_done <- d %>% anti_join(d_out, by = "row_id")

  if(NROW(not_done)>0){
    warning("There are some rows that could not be processed. Please check the input data and the hmap.", call. = FALSE)
  }

  d_out <- d_out %>%
    mutate(value = as.numeric(value)) %>%
    rename(time = year,
           meta.release_tag = revision,
           meta.unit = unit,
           meta.price_basis = price_basis,
           value.level = value) %>%
    select(time, meta.release_tag, meta.price_basis, meta.unit, meta.name, meta.disaggregation_group, value.level)

  d_chk <- d_out %>%
    distinct(time, meta.release_tag, meta.price_basis, meta.name, meta.disaggregation_group)

  if(NROW(d_chk)!=NROW(d_out)){
    stop("There are duplicate rows found in the output data. Please check the processing steps and the input data.", call. = FALSE)
  }


  # Attach revision metadata

  d_out <- d_out %>%
    mutate(revision_clean = str_clean(meta.release_tag)) %>%
    left_join(rev_meta %>% select(-revision), by = "revision_clean")

  d_out <- d_out %>%
    rename(meta.release_order = release_order) %>%
    distinct(time, meta.release_tag, meta.release_order, meta.price_basis, meta.unit, meta.name, meta.disaggregation_group, value.level)

  list(data = d_out, hmap = hmap)

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



