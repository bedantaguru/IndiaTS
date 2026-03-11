
aggregate_component <- function(tdl){

  tdl_now <- tdl

  repeat{
    agg_part <- aggregate_component_part(tdl_now)

    tdl_now <- agg_part$main_agg

    chk <- check_component_disaggregation_layer_completeness(tdl_now)

    if(!chk$any_missed) break()
  }

  tdl_now

}

check_component_disaggregation_layer_completeness <- function(tdl){

  dat_this <- tdl$data
  hmap_this <- tdl$hmap

  hm <- colnames(hmap_this) %>%
    map(function(cn) {
      hmap_this[cn] %>%
        distinct %>%
        purrr::set_names("meta.name") %>%
        mutate(meta.disaggregation_group = cn)
    }) %>%
    bind_rows()

  hma <- hm %>%
    group_by(meta.disaggregation_group) %>%
    summarise(n_names = n_distinct(meta.name), .groups = "drop")

  dta <- dat_this %>%
    group_by(time, meta.release_tag, meta.price_basis, meta.disaggregation_group) %>%
    summarise(n_names = n_distinct(meta.name), .groups = "drop")

  dta_chk <- dta %>%
    left_join(hma, by = "meta.disaggregation_group", suffix = c("","_expected"))

  dta_missed <- dta_chk %>% filter(n_names!=n_names_expected)

  list(
    any_missed = (NROW(dta_missed)>0),
    missed_data = dta_missed %>% select(-n_names, -n_names_expected)
  )

}

aggregate_component_part <- function(tdl){

  hmap <- tdl$hmap

  hmap_info <- hmap_get_stats(hmap)

  # For data tracking
  tdl$data$lineage <- ""
  if("meta.data_lineage" %in% colnames(tdl$data)){
    tdl$data$lineage <- tdl$data$meta.data_lineage
  }

  tdl_lst <- split(tdl$data, tdl$data$meta.disaggregation_group)

  tdf_agg_l <- tdl_lst %>%
    map(function(nd) {
      tdf_long_op_for_fixed_dg(nd, hmap = hmap, hmap_info = hmap_info)
    })

  # tdf_agg <- tdf_agg_l %>%
  #   rows_append_distinct(
  #     primary_key = c("time", "meta.release_tag", "meta.price_basis",
  #                     "meta.name", "meta.disaggregation_group"))

  tdf_agg_all <- tdf_agg_l %>% dplyr::bind_rows()

  tdf_agg_all <- tdf_agg_all %>%
    mutate(lineage_len = nchar(lineage))

  tdf_agg_min_lin <- tdf_agg_all %>%
    group_by(time, meta.release_tag, meta.price_basis, meta.name, meta.disaggregation_group) %>%
    dplyr::slice_min(order_by = lineage_len, n = 1, with_ties = FALSE) %>%
    ungroup()

  # lineage data is generated here but not used (kept only for debugging)
  # tdf_agg <- tdf_agg %>% rename(meta.data_lineage = lineage)
  tdf_agg <- tdf_agg_min_lin %>%
    mutate(meta.data_lineage = lineage) %>%
    select(-lineage, -lineage_len)

  # But this is already part of the below tdf_long_check_structure(tdf_agg, hmap)

  tdl_out <- tdf_long_make(
    dat = tdf_agg,
    hmap = hmap
  )

  list(
    main_agg = tdl_out,
    all_agg_dat = tdf_agg_all
  )

}


attach_parent <- function(tdl, parent_disaggregation_layer){

  tdf_long_check_shallow(tdl)

  hmap <- tdl$hmap

  hmap_info <- hmap_get_stats(hmap)

  if(missing(parent_disaggregation_layer)){
    parent_disaggregation_layer <- "indicator" %>% intersect(colnames(hmap))
    if(length(parent_disaggregation_layer)==0){
      stop("indicator column not found in hierarchy map (can not auto set). Please specify the parent_disaggregation_layer argument.", call. = FALSE)
    }
  }

  if(!parent_disaggregation_layer %in% colnames(hmap)){
    stop("Specified parent_disaggregation_layer not found in hierarchy map.", call. = FALSE)
  }

  dat <- tdl$data

  if("meta.parent" %in% colnames(dat)){
    dat <- dat %>% select(-meta.parent)
  }

  if("meta.parent_disaggregation_group" %in% colnames(dat)){
    dat <- dat %>% select(-meta.parent_disaggregation_group)
  }

  child_disaggregation_layers <- hmap_info$from_to_map %>%
    filter(to == parent_disaggregation_layer) %>%
    pull(from)

  this_layers <- c(child_disaggregation_layers, parent_disaggregation_layer)

  if(length(child_disaggregation_layers)==0){
    message("No child disaggregation layers found for the specified parent_disaggregation_layer in hierarchy map.")
  }

  # Note that attach parent dis-aggregation layer filters rows
  dat <- dat %>%
    filter(meta.disaggregation_group %in% this_layers)

  dat2 <- this_layers %>% map(
    function(dg){
      attach_parent_for_a_dg(dg, dat, hmap, parent_disaggregation_layer)
    }
  ) %>% bind_rows()

  tdl$data <- dat2

  tdl

}

attach_parent_for_a_dg <- function(dg, dat, hmap, parent_disaggregation_layer){
  dp <- dat %>% filter(meta.disaggregation_group == dg)
  this_map <- hmap[c(parent_disaggregation_layer,
                     dg)]
  colnames(this_map) <- c("meta.parent", "meta.name")
  this_map <- this_map %>%
    mutate(meta.parent_disaggregation_group = parent_disaggregation_layer)
  this_map <- distinct(this_map)

  dp <- dp %>%
    left_join(this_map, by = "meta.name")
  dp
}


tdf_long_op_for_fixed_dg <- function(nd, hmap, hmap_info){

  if(length(unique(nd$meta.disaggregation_group)) != 1){
    stop("Multiple disaggregation groups found in the data", call. = FALSE)
  }

  # For reverting back to original columns after aggregation
  nd_orig_cols <- colnames(nd)

  # If meta.parent present then remove it
  if("meta.parent" %in% nd_orig_cols){
    nd <- nd %>% select(-meta.parent)
  }


  if(!("meta.accumulation_rule" %in% nd_orig_cols)){
    # Create a dummy accumulation rule column if not present just to ensure that the code runs without error. This will be discarded later in the code.
    nd <- nd %>% mutate(meta.accumulation_rule = "sum")
  }

  if(!("meta.temporal_accumulation_rule" %in% nd_orig_cols)){
    # Create a dummy temporal accumulation rule column if not present just to ensure that the code runs without error. This will be discarded later in the code.
    nd <- nd %>% mutate(meta.temporal_accumulation_rule = "sum")
  }


  agg_fn <- switch(
    nd$meta.accumulation_rule[1],
    "sum" = sum,
    "mean" = mean,
    stop(paste0("Unknown accumulation rule: ", nd$meta.accumulation_rule[1]), call. = FALSE))

  if( "meta.unit" %in% nd_orig_cols){
    # Over multiple meta.names, there can be multiple units. But for a given
    # combination of time, meta.disaggregation_group, meta.price_basis and
    # meta.release_tag, there should be only one unit. This is checked here.

    unit_chk <- nd %>% group_by(time, meta.release_tag, meta.price_basis) %>%
      summarise(n_unit = n_distinct(meta.unit),.groups = "drop") %>%
      filter(n_unit > 1)
    if(NROW(unit_chk) > 0){
      stop("Multiple units found in a single combination of time, meta.disaggregation_group, meta.price_basis and meta.release_tag. Please ensure that each such combination has a unique unit.", call. = FALSE)
    }
  } else {
    # Create a dummy unit column if not present just to ensure that the code runs without error. This will be discarded later in the code.
    nd <- nd %>% mutate(meta.unit = "unit")
  }

  if(!("meta.release_date" %in% nd_orig_cols)){
    # A dummy release date is added here just to ensure that the code runs without error. This will be discarded later in the code.
    nd <- nd %>% mutate(meta.release_date = as.Date("2018-03-15"))
  }

  if(!("meta.release_order" %in% nd_orig_cols)){
    # A dummy release order is added here just to ensure that the code runs without error. This will be discarded later in the code.
    nd <- nd %>% mutate(meta.release_order = 1)
  }

  # This can be inferred again from data or directly nd$meta.disaggregation_group[1]
  hthis <- hmap_which_disaggregation_group(nd$meta.name, hmap = hmap)[1]

  h_to_do <- hmap_info$from_to_map %>%
    filter(from %in% hthis)


  if(NROW(h_to_do)>0){
    # It means some more aggregation is possible

    all_4_pks <- nd %>% distinct(time, meta.release_tag, meta.price_basis, meta.disaggregation_group)

    hs <- h_to_do$to %>%
      map(function(.x){
        tdf_long_op_for_taget_dg(
          .x,
          nd = nd, hmap = hmap,
          hthis = hthis, all_4_pks = all_4_pks, agg_fn = agg_fn,
          nd_orig_cols = nd_orig_cols)
      }) %>%
      # Here each node already have different meta.disaggregation_group and thus will not have any impact of rows_append_distinct. Both rbind and this are same.
      rows_append_distinct(
        primary_key = c("time", "meta.release_tag", "meta.price_basis",
                        "meta.name", "meta.disaggregation_group"))

    # Ideally here also nd and hs would have different dgs and thus rows_append_distinct may be same as rbind.
    final_variant <- rows_append_distinct(
      nd, hs,
      primary_key = c("time", "meta.release_tag", "meta.price_basis",
                      "meta.name", "meta.disaggregation_group")) %>%
      distinct()
  } else {
    final_variant <- nd
  }

  final_variant[intersect(nd_orig_cols, colnames(final_variant))]
}


tdf_long_op_for_taget_dg <- function(dg, nd, hmap, hthis, all_4_pks, agg_fn, nd_orig_cols){
  this_map <- hmap[c(hthis,dg)]
  colnames(this_map) <- c("meta.name", "meta.parent")
  this_map <- distinct(this_map)

  this_map_ext <- this_map %>% dplyr::cross_join(all_4_pks)

  nd_dg <- nd %>%
    full_join(
      this_map_ext,
      by = c("time", "meta.release_tag", "meta.price_basis",
             "meta.name", "meta.disaggregation_group"))

  # Which Parent can not be derived.
  any_missing <- nd_dg %>%
    group_by(time, meta.release_tag, meta.price_basis,
             meta.disaggregation_group, meta.parent) %>%
    summarise(v_check = agg_fn(value.level)) %>% filter(is.na(v_check))

  # Removing those parents
  nd_dg_rem_pk_with_missing <- nd_dg %>%
    anti_join(
      any_missing,
      by = c("time", "meta.release_tag", "meta.price_basis",
             "meta.disaggregation_group","meta.parent"))

  if(NROW(nd_dg_rem_pk_with_missing)==0) {
    return(tibble())
  }

  tdf_long_op_for_a_parent_in_fixed_dg(
    nd_dg_rem_pk_with_missing,
    hmap = hmap,
    nd_orig_cols = nd_orig_cols,
    agg_fn = agg_fn)
}

tdf_long_op_for_a_parent_in_fixed_dg <- function(nd_v, hmap, nd_orig_cols, agg_fn){
  if(nd_v$meta.parent[1] == "#root"){
    # no further aggregation possible or not defined.
    return(nd_v)
  }

  # Check if all categories are present or not
  hthis <- hmap_which_disaggregation_group(nd_v$meta.name, hmap = hmap)[1]
  expected_cats <- unique(hmap[[hthis]])
  actual_cats <- unique(nd_v$meta.name)
  missing_cats <- setdiff(expected_cats, actual_cats)
  extra_cats <- setdiff(actual_cats, expected_cats)
  if(length(missing_cats)>0){
    warning(
      paste0("These categories are expected based on hierarchy_map but not found in data for meta.disaggregation_group:- ", hthis, ": ",
             paste0(missing_cats, collapse = ", ")), call. = FALSE)
  }

  if(length(extra_cats)>0){
    warning(
      paste0("These categories are found in data but not expected based on hierarchy_map for meta.disaggregation_group:- ", hthis, ": ",
             paste0(extra_cats, collapse = ", ")), call. = FALSE)
  }


  nd_ag <- nd_v %>%
    rename(src = meta.name) %>%
    rename(meta.name = meta.parent) %>%
    group_by(time, meta.name, meta.disaggregation_group, meta.price_basis, meta.release_tag) %>%
    summarise(
      value.level = agg_fn(value.level),
      meta.unit = meta.unit[1],
      meta.release_date = max(meta.release_date),
      meta.release_order = max(meta.release_order),
      meta.accumulation_rule = meta.accumulation_rule[1],
      meta.temporal_accumulation_rule = meta.temporal_accumulation_rule[1],
      lineage = paste0(lineage[1], " > ", meta.disaggregation_group[1],":", paste0(unique(src), collapse = " + ")),
      .groups = "drop"
    ) %>%
    mutate(
      # no further aggregation possible or not defined.
      meta.parent = "#root"
    )

  hthis <- hmap_which_disaggregation_group(nd_ag$meta.name, hmap = hmap)[1]
  nd_ag <- nd_ag %>%
    mutate(
      meta.disaggregation_group = hthis
    )

  # remove the dummy release date and release order if they were added
  nd_ag <- nd_ag[intersect(nd_orig_cols, colnames(nd_ag))]
  nd_ag
}

