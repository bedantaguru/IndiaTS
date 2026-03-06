

aggregate_component <- function(tdl){

  hmap <- tdl$hmap

  hmap_info <- hmap_get_stats(hmap)

  # For data tracking
  tdl$data$lineage <- ""

  tdl_lst <- split(tdl$data, tdl$data$meta.disaggregation_group)

  tdf_agg <- tdl_lst %>%
    map(function(nd) {
      tdf_long_op_for_fixed_dg(nd, hmap = hmap, hmap_info = hmap_info)
    }) %>%
    rows_append_distinct(
      primary_key = c("time", "meta.release_tag", "meta.price_basis",
                      "meta.name", "meta.disaggregation_group"))

  # lineage data is generated here but not used (kept only for debugging)
  # tdf_agg <- tdf_agg %>% rename(meta.data_lineage = lineage)
  tdf_agg <- tdf_agg %>% select(-lineage)

  # This is again kept for safety (may be removed later)
  # But this is already part of the below tdf_long_check_structure(tdf_agg, hmap)

  tdf_long_make(
    dat = tdf_agg,
    hmap = hmap
  )

}


attach_parent <- function(tdl, parent_disaggregation_layer){

  # tdf_long_check_shallow(tdl)
  #
  # hmap <- tdl$hmap
  #
  # hmap_info <- hmap_get_stats(hmap)

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
    for_a_disaggregation_group <- function(dg){
      this_map <- hmap[c(hthis,dg)]
      colnames(this_map) <- c("meta.name", "meta.parent")
      this_map <- distinct(this_map)

      nd_dg <- nd %>%
        left_join(this_map, by = "meta.name")

      tdf_long_op_for_a_parent_in_fixed_dg(
        nd_dg,
        hmap = hmap,
        nd_orig_cols = nd_orig_cols,
        agg_fn = agg_fn)
    }

    hs <- h_to_do$to %>%
      map(for_a_disaggregation_group) %>%
      rows_append_distinct(
        primary_key = c("time", "meta.release_tag", "meta.price_basis",
                        "meta.name", "meta.disaggregation_group"))

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

