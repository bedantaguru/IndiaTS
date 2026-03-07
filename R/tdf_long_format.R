

tdf_long_check_structure  <- function(dat, hmap, add_latest = FALSE, retain_known_disaggregation_groups_only = FALSE){

  if(is.list(dat) && !is.data.frame(dat)){
    if(!all(c("data", "hmap") %in% names(dat))){
      stop("When list supplied it should be a list with 'data' and 'hmap' elements.", call. = FALSE)
    }
    return(
      tdf_long_check_structure(
        dat = dat$data, hmap = dat$hmap,
        add_latest =  add_latest,
        retain_known_disaggregation_groups_only = retain_known_disaggregation_groups_only)
    )
  }

  if(!is.data.frame(dat)){
    stop("dat should be a data.frame or tibble.", call. = FALSE)
  }

  if(!is.data.frame(hmap)){
    stop("hmap should be a data.frame or tibble.", call. = FALSE)
  }

  value_cols <- colnames(dat)[colnames(dat) %>% str_detect("^value\\.")]
  meta_cols <- colnames(dat)[colnames(dat) %>% str_detect("^meta\\.")]

  if(length(value_cols)==0){
    stop("Input data.frame must have at least one column starting with 'value.'!!", call. = FALSE)
  }

  if(!"time" %in% colnames(dat)){
    stop("Input data.frame must have a 'time' column!!", call. = FALSE)
  }

  other_cols <- colnames(dat)[!(colnames(dat) %>% str_detect("^value\\.") | colnames(dat) %>% str_detect("^meta\\.") | colnames(dat) %>% str_detect("^time$"))]
  if(length(other_cols)>0){
    warning(paste0("These columns do not start with 'value.' or 'meta.' and are not 'time'. They will be ignored: ",
                   paste0(other_cols, collapse = ", ")))
  }

  # take only required columns
  dat <- dat[c("time", value_cols, meta_cols)]

  # Possible change of data
  dat2 <- tdf_long_cast_required_cols(dat)

  dat2 <- tdf_long_release_tag_task(dat2, add_latest = add_latest)

  # check if meta.name and meta.disaggregation_group values are present in hierarchy map
  dat2 <- tdf_long_hierarchy_map_task(dat2, hmap, retain_known_disaggregation_groups_only = retain_known_disaggregation_groups_only)

  # check time column
  dat2 <- tdf_long_time_task(dat2)

  return(invisible(dat2))

}

tdf_long_cast_required_cols <- function(dat){

  if(!("meta.name" %in% colnames(dat))){
    dat <- dat %>% mutate(`meta.name` = "indicator")
  }

  if(!"meta.disaggregation_group" %in% colnames(dat)){
    dat <- dat %>% mutate(`meta.disaggregation_group` = "indicator")
  }

  if(!"meta.price_basis" %in% colnames(dat)){
    dat <- dat %>% mutate(`meta.price_basis` = "as-is")
  }

  if(!"meta.release_tag" %in% colnames(dat)){
    dat <- dat %>% mutate(`meta.release_tag` = "as-is")
  }

  dat
}


tdf_long_release_tag_task <- function(dat, add_latest = FALSE){

  dat2 <- dat %>% filter(meta.release_tag != "#main")

  if(NROW(dat2)==0) return(dat)

  chk <- dat2 %>% distinct() %>%
    group_by(time, meta.name, meta.disaggregation_group, meta.price_basis) %>%
    count() %>% filter(n>1)

  if(NROW(chk) > 0){

    chk2 <- dat2 %>%
      group_by(time, meta.name, meta.disaggregation_group, meta.price_basis, meta.release_tag) %>%
      cols_causing_group_variation()

    if(length(chk2) > 0){
      stop(
        paste0("The following columns have multiple values for the same combination of time, meta.name, meta.disaggregation_group, meta.price_basis and meta.release_tag. Please ensure that these columns have only one value for each unique combination of the grouping variables: ",
               paste0(chk2, collapse = ", ")), call. = FALSE)
    }

    if(!("meta.release_date" %in% colnames(dat2)) & !("meta.release_order" %in% colnames(dat2))){
      stop("To determine the latest revision it is required to have either 'meta.release_date' or 'meta.release_order' column in the data. Please ensure that at least one of these columns is present to identify the latest revision.", call. = FALSE)
    }

    if(add_latest){

      # if  meta.release_date exists then take the latest revision only based on release date otherwise take the latest revision based on release order
      if("meta.release_date" %in% colnames(dat2)){
        dpart <- dat2 %>% group_by(time, meta.name, meta.disaggregation_group, meta.price_basis) %>%
          slice_max(order_by = meta.release_date, n = 1, with_ties = FALSE) %>%
          ungroup()
      } else if("meta.release_order" %in% colnames(dat2)){
        # if release date is not there but release tag is there then take the latest revision based on release tag order (assuming release tag has a natural order in the data)
        dpart <- dat2 %>% group_by(time, meta.name, meta.disaggregation_group, meta.price_basis) %>%
          slice_max(order_by = meta.release_order, n = 1, with_ties = FALSE) %>%
          ungroup()
      }

      dpart <- dpart %>% mutate(meta.release_tag = "#main")
      dat2 <- bind_rows(dat2, dpart)

    }

  } else {
    dat2 <- dat2 %>% mutate(meta.release_tag = "#main")
  }

  return(dat2)

}


tdf_long_hierarchy_map_task <- function(
    dat, hmap,
    retain_known_disaggregation_groups_only = FALSE) {
  known_dgs <- colnames(hmap)
  data_dgs <- dat$meta.disaggregation_group %>% unique()

  if(length(intersect(known_dgs, data_dgs)) == 0){
    stop("None of the disaggregation groups in data are present in hierarchy_map. Please check the data and hierarchy_map.", call. = FALSE)
  }

  not_in_hmap_dgs <- setdiff(data_dgs, known_dgs)
  if(length(not_in_hmap_dgs) > 0){

    if(retain_known_disaggregation_groups_only){
      warning(paste0("These disaggregation groups are present in data but not in hierarchy_map. They will be filtered out: ",
                     paste0(not_in_hmap_dgs, collapse = ", ")), call. = FALSE)
      dat <- dat %>% filter(meta.disaggregation_group %in% known_dgs)
    } else{
      warning(paste0("These disaggregation groups are present in data but not in hierarchy_map: ",
                     paste0(not_in_hmap_dgs, collapse = ", ")), call. = FALSE)
    }

  }

  all_possible_names <- hmap %>% as.matrix() %>% as.character() %>%
    unique() %>% str_clean() %>% unique()

  all_dat_names <- dat$meta.name %>% unique() %>% str_clean() %>% unique()

  in_data_but_not_in_hmap <- setdiff(all_dat_names, all_possible_names)

  if(length(in_data_but_not_in_hmap) > 0){
    if(retain_known_disaggregation_groups_only){
      warning(paste0("These meta.name values are present in data but not in hierarchy_map. They will be filtered out: ",
                     paste0(in_data_but_not_in_hmap, collapse = ", ")), call. = FALSE)
      dat <- dat %>% filter(str_clean(meta.name) %in% all_possible_names)
    } else{
      warning(paste0("These meta.name values are present in data but not in hierarchy_map: ",
                     paste0(in_data_but_not_in_hmap, collapse = ", ")), call. = FALSE)
    }
  }

  return(dat)

}

tdf_long_time_task <- function(dat){
  dat$time <- as_fiscal_period(dat$time)

  fqs <- frequency(dat$time)
  if(length(unique(fqs))>1){
    stop("Multiple frequencies found in time column. Please ensure that all entries in time column have the same frequency.", call. = FALSE)
  }

  dat

}

tdf_long_make <- function(dat, hmap, add_latest = TRUE, retain_known_disaggregation_groups_only = TRUE){

  if(is.list(dat) && !is.data.frame(dat)){
    if(!all(c("data", "hmap") %in% names(dat))){
      stop("When list supplied it should be a list with 'data' and 'hmap' elements.", call. = FALSE)
    }
    return(
      tdf_long_make(
        dat = dat$data, hmap = dat$hmap,
        add_latest =  add_latest,
        retain_known_disaggregation_groups_only = retain_known_disaggregation_groups_only)
    )
  }

  dat2 <- tdf_long_check_structure(
    dat,
    hmap = hmap,
    add_latest = add_latest,
    retain_known_disaggregation_groups_only = retain_known_disaggregation_groups_only)

  obj <- list(data = dat2, hmap = hmap)
  class(obj) <- tdf_long_class
  obj

}


tdf_long_check_shallow <- function(obj, return_result = FALSE){
  if(!is.list(obj)){
    if(return_result) return(FALSE)
    stop("Object should be a list with 'data' and 'hmap' elements.", call. = FALSE)
  }

  if(!all(c("data", "hmap") %in% names(obj))){
    if(return_result) return(FALSE)
    stop("Object should be a list with 'data' and 'hmap' elements.", call. = FALSE)
  }

  if(!is.data.frame(obj$data)){
    if(return_result) return(FALSE)
    stop("The 'data' element of the object should be a data.frame or tibble.", call. = FALSE)
  }

  if(!is.data.frame(obj$hmap)){
    if(return_result) return(FALSE)
    stop("The 'hmap' element of the object should be a data.frame or tibble.", call. = FALSE)
  }

  if(!inherits(obj, tdf_long_class)){
    warning(paste0("Object should have class ", paste0(tdf_long_class[1], collapse = ", ")), call. = FALSE)
  }

  return(invisible(TRUE))
}
