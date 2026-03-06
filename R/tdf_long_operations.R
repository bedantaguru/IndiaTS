
# TODO DEL it
# td0 <- read_rds("tests/testthat/testdata/qtr_base_rev_gva.rds") %>% es_convert_gva_annual() %>% tdf_long_make()



calculate_standard_measures <- function(tdl){
  # Calculate:

  # Growth Rate, Momentum, Contribution to Growth Rate, Contribution to
  # Momentum, YoY Growth Rate, QoQ Growth Rate, etc. for all possible
  # combinations of meta.disaggregation_group and meta.name based on the
  # hierarchy map.

  # It requires a parent.


  if(!"value.level" %in% colnames(tdl$data)){
    stop("value.level column not found in data. Please ensure that the data has value.level column for calculations.", call. = FALSE)
  }

  if(!"meta.parent" %in% colnames(tdl$data) || !"meta.parent_disaggregation_group" %in% colnames(tdl$data)){
    message(
      "Note: meta.parent column not found in data. ",
      "Auto attaching parent disaggregation layer (highest level in hierarchy map) as meta.parent. ",
      "If you want to suppress this message, please use attach_parent explicitly."
    )

    tdl <- attach_parent(tdl)
  }

  dat_orig <- tdl$data

  dat <- dat_orig %>%
    mutate(time_prev_period = previous_period(time),
           time_prev_year = previous_year(time))

  # Attach last year and last period levels
  dat <- dat %>%
    left_join(
      dat %>%
        select(time, meta.release_tag, meta.price_basis, meta.name, meta.disaggregation_group,
               value.level_prev_prd = value.level),
      by = c("time_prev_period" = "time",
             "meta.release_tag", "meta.price_basis", "meta.name", "meta.disaggregation_group")
    ) %>%
    left_join(
      dat %>%
        select(time, meta.release_tag, meta.price_basis, meta.name, meta.disaggregation_group,
               value.level_prev_year = value.level),
      by = c("time_prev_year" = "time",
             "meta.release_tag", "meta.price_basis", "meta.name", "meta.disaggregation_group")
    )

  # Calculate Growth Rate, Momentum
  dat <- dat %>%
    mutate(
      value.growth_rate = (value.level/value.level_prev_year - 1)*100,
      value.momentum = (value.level/value.level_prev_prd - 1)*100,
      value.ln_growth_rate = log(value.level/value.level_prev_year)*100,
      value.ln_momentum = log(value.level/value.level_prev_prd)*100
    )

  # Base Effect
  dat <- dat %>%
    left_join(
      dat %>%
        mutate(
          value.base_effect = -value.ln_momentum,
          # This should be - ln_base_effect (but using this for convenience)
          value.ln_base_effect = -value.ln_momentum
        ) %>%
        select(time, meta.release_tag, meta.price_basis, meta.name, meta.disaggregation_group,
               value.base_effect,
               value.ln_base_effect),
      by = c("time_prev_year" = "time",
             "meta.release_tag", "meta.price_basis", "meta.name", "meta.disaggregation_group")
    )

  # Previous Period in Growth Rates
  dat <- dat %>%
    left_join(
      dat %>%
        select(time, meta.release_tag, meta.price_basis, meta.name, meta.disaggregation_group,
               value.growth_rate_prev_prd = value.growth_rate,
               value.ln_growth_rate_prev_prd = value.ln_growth_rate),
      by = c("time_prev_period" = "time",
             "meta.release_tag", "meta.price_basis", "meta.name", "meta.disaggregation_group")
    )

  # Delta of Growth Rate
  dat <- dat %>%
    mutate(
      value.delta_growth_rate = value.growth_rate - value.growth_rate_prev_prd,
      value.delta_ln_growth_rate = value.ln_growth_rate - value.ln_growth_rate_prev_prd
    )

  # Share in parent (for this meta.disaggregation_group and meta.name)
  dat <- dat %>%
    group_by(time, meta.release_tag, meta.price_basis,
             meta.disaggregation_group,
             meta.parent, meta.parent_disaggregation_group) %>%
    mutate(
      value.share_pct = value.level / sum(value.level, na.rm = TRUE)*100
    ) %>%
    ungroup()

  # Attach last year and last period shares
  dat <- dat %>%
    left_join(
      dat %>%
        select(time, meta.release_tag, meta.price_basis, meta.name, meta.disaggregation_group,
               value.share_prev_prd = value.share_pct),
      by = c("time_prev_period" = "time",
             "meta.release_tag", "meta.price_basis", "meta.name", "meta.disaggregation_group")
    ) %>%
    left_join(
      dat %>%
        select(time, meta.release_tag, meta.price_basis, meta.name, meta.disaggregation_group,
               value.share_prev_year = value.share_pct),
      by = c("time_prev_year" = "time",
             "meta.release_tag", "meta.price_basis", "meta.name", "meta.disaggregation_group")
    )

  # Now Contribution in growth rate
  dat <- dat %>%
    ungroup() %>%
    mutate(
      value.contribution_growth_rate = value.share_prev_year * value.growth_rate / 100,
      value.contribution_momentum = value.share_prev_prd * value.momentum / 100
    ) %>%
    group_by(time, meta.release_tag, meta.price_basis,
             meta.disaggregation_group,
             meta.parent, meta.parent_disaggregation_group) %>%
    mutate(
      value.contribution_growth_rate_pct = value.contribution_growth_rate * sum(value.contribution_growth_rate, na.rm = TRUE)*100,
      value.contribution_momentum_pct = value.contribution_momentum * sum(value.contribution_momentum, na.rm = TRUE)*100
    )

  # Remove intermediate time_prev_period and time_prev_year columns
  dat <- dat %>% select(-time_prev_period, -time_prev_year)


  # Deflator Calculation
  # (Special Calculation and this also adds rows - at the end)


  allowed_cols <- c("time", "meta.name", "meta.disaggregation_group", "meta.release_tag", "meta.price_basis", "value.level",
                    "meta.release_order", "meta.release_date")

  this_allowed_cols <- colnames(dat_orig) %>% intersect(allowed_cols)


  dat_defl <- dat_orig %>%
    select(all_of(this_allowed_cols)) %>%
    mutate(
      meta.price_basis = meta.price_basis %>% str_clean()
    ) %>%
    mutate(
      meta.is_real = meta.price_basis %>% str_detect("real"),
      meta.is_nominal = meta.price_basis %>% str_detect("nominal")
    ) %>%
    mutate(
      # remove real or nominal from string part and make str_trim
      meta.price_basis = meta.price_basis %>% str_remove("real|nominal") %>% str_trim()
    )

  # Check if deflator can be calculated
  chk <- FALSE
  dat_defl_calc <- NULL

  if(all(!is.na(dat_defl$meta.is_real)) && all(!is.na(dat_defl$meta.is_nominal))){
    if(all(!dat_defl$meta.is_real==dat_defl$meta.is_nominal)) chk <- TRUE
  }


  if(chk){
    dat_defl_real <- dat_defl %>% filter(meta.is_real) %>% select(-meta.is_real, -meta.is_nominal)
    dat_defl_nominal <- dat_defl %>% filter(meta.is_nominal) %>% select(-meta.is_real, -meta.is_nominal)

    dat_defl_calc <- dat_defl_real %>%
      inner_join(
        dat_defl_nominal,
        by = c("time", "meta.name", "meta.disaggregation_group", "meta.release_tag", "meta.price_basis",
               intersect(c("meta.release_order", "meta.release_date"), colnames(dat_defl_real))),
        suffix = c("_real", "_nominal")
      ) %>%
      mutate(
        value.deflator = value.level_nominal / value.level_real*100
      )

    dat_defl_calc <- dat_defl_calc %>%
      select(-value.level_real, -value.level_nominal)

    # Deflator Inflation
    dat_defl_calc <- dat_defl_calc %>%
      mutate(time_prev_year = previous_year(time)) %>%
      left_join(
        dat_defl_calc %>%
          select(time, meta.release_tag, meta.price_basis, meta.name, meta.disaggregation_group,
                 value.deflator_prev_year = value.deflator),
        by = c("time_prev_year" = "time",
               "meta.release_tag", "meta.price_basis", "meta.name", "meta.disaggregation_group")
      ) %>%
      mutate(
        value.deflator_inflation = (value.deflator/value.deflator_prev_year - 1)*100
      )

    dat_defl_calc <- dat_defl_calc %>% select(-time_prev_year)

  } else {
    message("Deflator calculation skipped as real and nominal price basis could not be identified in data.")
  }

  tdl$data <- dat

  if(is.null(dat_defl_calc)){
    list(main = tdl)
  }

  list(main = tdl, deflator = dat_defl_calc)
}

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

