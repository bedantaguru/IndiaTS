
get_gdp_approx_release_date <- function(year_vec, revision_vec) {

  # ----------------------------------------------------------------
  # MOSPI Release Calendar Logic (Procedural Gaps):
  #
  # Base Year YYYY refers to the start of the FY (e.g., 2025 for FY 2025-26).
  #
  # 1. FAE (First Advance): Jan 7th of Base + 1 (Pre-Budget)
  # 2. SAE (Second Advance): End of Feb of Base + 1
  # 3. PE (Provisional): End of May of Base + 1
  #
  # -- Standard Revision Cycle (1-Year Gaps) --
  # 4. FRE (First Revised): End of Feb of Base + 2
  # 5. SRE (Second Revised): End of Feb of Base + 3
  # 6. TRE (Third Revised): End of Feb of Base + 4
  # 7. Final Estimates: End of Feb of Base + 5
  #
  # -- Exceptional/Historical --
  # 8. Additional Revision: End of Feb of Base + 8 (Distinctly far gap)
  # ----------------------------------------------------------------

  revision_meta <- tibble::tibble(
    revision = c(
      "First Advance Estimates",
      "Second Advance Estimates",
      "Provisional Estimates",
      "First Revised Estimates",
      "Second Revised Estimates",
      "Third Revised Estimates",
      "Final Estimates",
      "Additional Revision"
    ),

    # Release Order
    release_order = 1:8,

    # Year Offsets to enforce the Final > TRE > SRE gap
    year_offset = c(1, 1, 1, 2, 3, 4, 5, 8),

    # Months: Jan (1), Feb (2), May (5)
    month       = c(1, 2, 5, 2, 2, 2, 2, 2),

    # Day Logic: 7 for FAE, -1 for End of Month
    fix_day     = c(7, -1, -1, -1, -1, -1, -1, -1)
  )

  if(missing(year_vec)){
    return(revision_meta %>% dplyr::select(-year_offset, -month, -fix_day))
  }

  if(length(year_vec) != length(revision_vec)){
    stop("Length of year_vec and revision_vec must be the same.", call. = FALSE)
  }

  base_year <- as.integer(substr(year_vec, 1, 4))

  result <- tibble::tibble(
    Year     = year_vec,
    Revision = revision_vec
  ) %>%
    dplyr::left_join(revision_meta, by = c("Revision" = "revision")) %>%
    dplyr::mutate(
      target_year = base_year + year_offset,

      release_date = dplyr::case_when(
        fix_day > 0 ~ lubridate::make_date(target_year, month, fix_day),
        fix_day == -1 ~ lubridate::ceiling_date(
          lubridate::make_date(target_year, month, 1),
          unit = "month"
        ) - lubridate::days(1),
        TRUE ~ NA_Date_
      )
    ) %>%
    dplyr::select(Year, Revision, release_date)

  return(result %>% dplyr::pull(release_date))
}



DEV <- function(){


  dat <- readRDS(testthat::test_path("testdata", "gdp_dat.rds"))

  dat_hmap <- readRDS(testthat::test_path("testdata", "hierarchical_map_gva.rds"))


  ####################

  #replace in all char columns where * (exactly only * and space char is there nothing else ) is there into NA
  dat <- dat %>% mutate(across(where(is.character), ~na_if(., "*")))


  # dat %>% group_by(Year,Revision) %>% summarise(
  #   n_IS = n_distinct(Industry), n_SubIS =n_distinct(Subindustry), n_Inst = n_distinct(`Institutional Sector`), .groups = "drop") %>%
  #   group_by(Revision, n_IS, n_SubIS, n_Inst) %>%
  #   summarise(yrs = paste0(Year, collapse = "; ")) %>%
  #   arrange(Revision) %>% (function(x){View(x);clipr::write_clip(x)})()


  dat_subind_rep <- dat %>%
    group_by(Year, Industry) %>%
    summarise(n_subindustry  = n_distinct(Subindustry), .groups = "drop") %>%
    group_by(Industry) %>%
    summarise( n_subindustry = max(n_subindustry), .groups = "drop") %>%
    filter(!is.na(Industry)) %>%
    filter(n_subindustry==1)

  dat_subind_rep <- dat_subind_rep %>%
    mutate(subindustry_rep = Industry) %>%
    select(-n_subindustry)

  dat <- dat %>% left_join(dat_subind_rep, by = "Industry")

  dat <- dat %>% mutate(subindustry_rep = ifelse(is.na(Subindustry), subindustry_rep, Subindustry))

  dat <- dat %>% mutate(release_date = get_gdp_approx_release_date( Year, Revision))

  # sum, mean, weighted average (TODO later)
  dat$accumulation_rule <- "sum"

  # sum, mean, last
  dat$temporal_accumulation_rule  <- "sum"
  dat$row_id <- seq(NROW(dat))


  # 9 category of industry :- Early Industry only (FAE/PE/SAE)
  d1 <- dat %>% filter(Revision %in% c("First Advance Estimates", "Second Advance Estimates", "Provisional Estimates"))
  #d1 <- d1 %>% filter(Industry != "Total Gross Value Added")
  d1 <- d1 %>% pivot_longer(cols = c( `Current Price`, `Constant Price`), names_to = "real_nominal", values_to = "value")
  d1 <-  d1 %>% mutate(price_basis = real_nominal %>% tolower() %>% str_detect("constant") %>% ifelse(yes = "real", no = "nominal"))


  dp1 <- d1 %>%
    filter(Industry != "Total Gross Value Added") %>%
    mutate(
      meta.is_most_granular  = TRUE,
      meta.is_published  = TRUE)

  dg_this <- which_disaggregation_group(dp1$Industry, hmap = dat_hmap)

  dp1 <- dp1 %>%
    mutate(
      meta.disaggregation_group = dg_this) %>%
    select(
      time = Year,
      #meta.domain = Indicator,
      meta.name = Industry,
      value.level = value,
      meta.price_basis = price_basis,
      meta.accumulation_rule = accumulation_rule,
      meta.unit = Unit,
      meta.release_tag = Revision,
      meta.release_date = release_date,
      meta.parent = Indicator,
      meta.disaggregation_group,
      meta.is_most_granular,
      meta.is_published,
      # for tracking only
      row_id)

  dp1_ag <- d1 %>%
    filter(Industry == "Total Gross Value Added") %>%
    mutate(
      meta.is_most_granular  = FALSE,
      Industry = "Gross Value Added",
      meta.is_published  = TRUE)

  dg_this <- which_disaggregation_group(dp1_ag$Industry, hmap = dat_hmap)

  dp1_ag <- dp1_ag %>%
    mutate(meta.disaggregation_group = dg_this) %>%
    select(
      time = Year,
      #meta.domain = Indicator,
      meta.name = Industry,
      value.level = value,
      meta.price_basis = price_basis,
      meta.accumulation_rule = accumulation_rule,
      meta.unit = Unit,
      meta.release_tag = Revision,
      meta.release_date = release_date,
      #meta.parent = "#root",
      meta.disaggregation_group,
      meta.is_most_granular,
      meta.is_published,
      # for tracking only
      row_id)

  # 11 category of industry :- Early Industry only (Final/FRE/SRE) including sub industries

  d2 <- dat %>%
    filter(Revision %in% c("Additional Revision", "Final Estimates",
                           "First Revised Estimates", "Second Revised Estimates",
                           "Third Revised Estimates"))
  d2 <- d2 %>% mutate(Subindustry = subindustry_rep)
  # d2 <- d2 %>% filter(!is.na(Subindustry))
  # d2 <- d2 %>% filter(Subindustry!= "Total Gross Value Added")
  d2 <- d2 %>% pivot_longer(cols = c( `Current Price`, `Constant Price`), names_to = "real_nominal", values_to = "value")
  d2 <-  d2 %>% mutate(price_basis = real_nominal %>% tolower() %>% str_detect("constant") %>% ifelse(yes = "real", no = "nominal"))

  dp2 <- d2 %>%
    filter(!is.na(Subindustry)) %>%
    filter(Subindustry!= "Total Gross Value Added") %>%
    mutate(meta.is_most_granular  = TRUE,
           meta.is_published  = TRUE)

  dg_this <- which_disaggregation_group(dp2$Subindustry, hmap = dat_hmap)

  dp2 <- dp2 %>%
    mutate(meta.disaggregation_group = dg_this) %>%
    select(
      time = Year,
      #meta.domain = Indicator,
      meta.name = Subindustry,
      value.level = value,
      meta.price_basis = price_basis,
      meta.accumulation_rule = accumulation_rule,
      meta.unit = Unit,
      meta.release_tag = Revision,
      meta.release_date = release_date,
      meta.parent = Industry,
      meta.disaggregation_group,
      meta.is_most_granular,
      # for tracking only
      row_id)


  dp2_ag1 <- d2 %>%
    filter(is.na(subindustry_rep), !is.na(Industry)) %>%
    filter(Industry!= "Total Gross Value Added") %>%
    mutate(meta.is_most_granular  = FALSE,
           meta.is_published  = TRUE)

  dg_this <- which_disaggregation_group(dp2_ag1$Industry, hmap = dat_hmap)


  dp2_ag1 <- dp2_ag1 %>%
    mutate(meta.disaggregation_group = dg_this) %>%
    select(
      time = Year,
      #meta.domain = Indicator,
      meta.name = Industry,
      value.level = value,
      meta.price_basis = price_basis,
      meta.accumulation_rule = accumulation_rule,
      meta.unit = Unit,
      meta.release_tag = Revision,
      meta.release_date = release_date,
      meta.parent = Industry,
      meta.disaggregation_group,
      meta.is_most_granular,
      # for tracking only
      row_id)

  dp2_ag2 <- d2 %>%
    filter(Subindustry == "Total Gross Value Added") %>%
    filter(Industry == "Total Gross Value Added") %>%
    mutate(meta.is_most_granular  = FALSE,
           Subindustry = "Gross Value Added",
           meta.is_published  = TRUE)

  dg_this <- which_disaggregation_group(dp2_ag2$Subindustry, hmap = dat_hmap)

  dp2_ag2 <- dp2_ag2 %>%
    mutate(meta.disaggregation_group = dg_this) %>%
    select(
      time = Year,
      #meta.domain = Indicator,
      meta.name = Subindustry,
      value.level = value,
      meta.price_basis = price_basis,
      meta.accumulation_rule = accumulation_rule,
      meta.unit = Unit,
      meta.release_tag = Revision,
      meta.release_date = release_date,
      meta.parent = Industry,
      meta.disaggregation_group,
      meta.is_most_granular,
      # for tracking only
      row_id)

  # Institutional Sector

  # d3 <- dat %>%
  #   filter(Revision %in% c("Additional Revision", "Final Estimates",
  #                          "First Revised Estimates", "Second Revised Estimates",
  #                          "Third Revised Estimates"))
  # d3 <- d3 %>% filter(!is.na(`Institutional Sector`))
  # d3 <- d3 %>% pivot_longer(cols = c( `Current Price`, `Constant Price`), names_to = "real_nominal", values_to = "value")
  # d3 <- d3 %>% mutate(price_basis = real_nominal %>% tolower() %>% str_detect("constant") %>% ifelse(yes = "real", no = "nominal"))
  # d3 <- d3 %>% filter(!is.na(value))
  # d3 <- d3 %>% filter(`Institutional Sector`!= "Total Gross Value Added")

  d3 <- d2 %>% filter(!is.na(`Institutional Sector`))


  dp3 <- d3 %>%
    filter(`Institutional Sector`!= "Total Gross Value Added") %>%
    mutate(meta.is_most_granular  = FALSE,
           meta.is_published  = TRUE)


  dp3 <- dp3 %>%
    mutate(
      meta.disaggregation_group = "Institutional Sector") %>%
    select(
      time = Year,
      #meta.domain = Indicator,
      meta.name = `Institutional Sector`,
      value.level = value,
      meta.price_basis = price_basis,
      meta.accumulation_rule = accumulation_rule,
      meta.unit = Unit,
      meta.release_tag = Revision,
      meta.release_date = release_date,
      meta.parent = Indicator,
      meta.disaggregation_group,
      meta.is_most_granular,
      # meta.parent_1 = Indicator,
      # meta.disaggregation_group_1,
      # for tracking only
      row_id)

  dp_all_gr <- bind_rows(dp1, dp2, dp3)

  #dp_all_agg <- bind_rows(dp1_ag, dp2_ag1, dp2_ag2)

  #dp_all <- bind_rows(dp_all_gr, dp_all_agg)

  dp_all <- dp_all_gr

  exclusions <- dat %>% anti_join(dp_all, by = "row_id")

  dp_all$row_id <- NULL

  as_tdf_long(dp_all, hierarchy_map = dat_hmap)

}

check_possible_tdf_long <- function(d){

}

as_tdf_long <- function(d, hierarchy_map = NULL, retain_known_disaggregation_groups_only = TRUE){


  if(!is.data.frame(d)){
    stop("Input must be a data.frame!!", call. = FALSE)
  }

  if(!"time" %in% colnames(d)){
    stop("Input data.frame must have a 'time' column!!", call. = FALSE)
  }

  value_cols <- colnames(d)[colnames(d) %>% str_detect("^value\\.")]
  meta_cols <- colnames(d)[colnames(d) %>% str_detect("^meta\\.")]

  if(length(value_cols)==0){
    stop("Input data.frame must have at least one column starting with 'value.'!!", call. = FALSE)
  }


  # check if there are any columns which are not time, value.* or meta.*
  other_cols <- colnames(d)[!(colnames(d) %>% str_detect("^value\\.") | colnames(d) %>% str_detect("^meta\\.") | colnames(d) %>% str_detect("^time$"))]
  if(length(other_cols)>0){
    warning(paste0("These columns do not start with 'value.' or 'meta.' and are not 'time'. They will be ignored: ",
                   paste0(other_cols, collapse = ", ")))
  }

  # take only required columns
  d <- d[c("time", value_cols, meta_cols)]

  # Check for multiple release
  chk <- d %>% group_by(time, meta.name, meta.disaggregation_group, meta.price_basis) %>% count() %>% filter(n>1)

  if(NROW(chk) > 0){

    d <- d %>% filter(meta.release_tag != "#main")

    chk2 <- d %>% distinct() %>%
      group_by(time, meta.name, meta.disaggregation_group, meta.price_basis, meta.release_tag) %>%
      count() %>% filter(n>1)

    if(NROW(chk2) > 0){
      stop("Duplicate entries found for the same combination of time, meta.name, meta.price_basis, meta.disaggregation_group and meta.release_tag. Please ensure that each combination is unique.", call. = FALSE)
    }

    # if  meta.release_date exists then take the latest revision only based on release date otherwise take the latest revision based on release order
    if("meta.release_date" %in% colnames(d)){
      dpart <- d %>% group_by(time, meta.name, meta.disaggregation_group, meta.price_basis) %>%
        slice_max(order_by = meta.release_date, n = 1, with_ties = FALSE) %>%
        ungroup()
    } else if("meta.release_order" %in% colnames(d)){
      # if release date is not there but release tag is there then take the latest revision based on release tag order (assuming release tag has a natural order in the data)
      dpart <- d %>% group_by(time, meta.name, meta.disaggregation_group, meta.price_basis) %>%
        slice_max(order_by = meta.release_order, n = 1, with_ties = FALSE) %>%
        ungroup()
    } else {
      stop("Cannot determine latest revision as neither meta.release_date nor meta.release_order is present in the data. Please ensure that at least one of these columns is available to identify the latest revision.", call. = FALSE)
    }

    dpart <- dpart %>% mutate(meta.release_tag = "#main")
    d <- bind_rows(d, dpart)
  } else {
    d <- d %>% mutate(meta.release_tag = "#main")
  }


  chk <- d %>% group_by(time, meta.name, meta.disaggregation_group, meta.price_basis, meta.release_tag) %>% count()

  if(any(chk$n>1)){
    stop("Duplicate entries found for the same combination of time, meta.name, meta.price_basis, and meta.release_tag. Please ensure that each combination is unique.", call. = FALSE)
  }

  if(!is.null(hierarchy_map)){
    if(!is.data.frame(hierarchy_map)){
      stop("hierarchy_map must be a data.frame!!", call. = FALSE)
    }

    if(retain_known_disaggregation_groups_only){
      known_dgs <- colnames(hierarchy_map)
      data_dgs <- d$meta.disaggregation_group %>% unique()

      if(length(intersect(known_dgs, data_dgs)) == 0){
        stop("None of the disaggregation groups in data are present in hierarchy_map. Please check the data and hierarchy_map.", call. = FALSE)
      }

      not_in_hmap_dgs <- setdiff(data_dgs, known_dgs)
      if(length(not_in_hmap_dgs)>0){
        message(paste0("These disaggregation groups are present in data but not in hierarchy_map. They will be filtered out: ",
                       paste0(not_in_hmap_dgs, collapse = ", ")))
        d <- d %>% filter(meta.disaggregation_group %in% known_dgs)
      }
    }
  }

  # is meta. accumulation_rule and temporal_accumulation_rule not present init with default sum
  if(!"meta.accumulation_rule" %in% colnames(d)){
    d <- d %>% mutate(meta.accumulation_rule = "sum")
  }

  if(!"meta.temporal_accumulation_rule" %in% colnames(d)){
    d <- d %>% mutate(meta.temporal_accumulation_rule = "sum")
  }

  class(d) <- tdf_class

  d$time <- as_fiscal_period(d$time)

  fqs <- frequency(d$time)
  if(length(unique(fqs))>1){
    warning("Multiple frequencies found in time column. Please ensure that all entries in time column have the same frequency.", call. = FALSE)
  }

  attr(d, "shape") <- "long"

  attr(d, "hierarchy_map") <- hierarchy_map

  d

}


which_disaggregation_group <- function(meta_names, hmap){
  meta_names <- unique(meta_names)
  hmvec <- hmap %>% map_dbl(function(.x){length(intersect(.x, meta_names))/length(meta_names)})
  if(max(hmvec) == 0){
    return(NA_character_)
  }
  hmvec %>% which.max() %>% names %>% .[1]
}


aggregate_component <- function(tdf_l, hierarchy_map = NULL){

  if(!is.null(hierarchy_map)){
    hmap <- hierarchy_map
  } else {
    hmap <- attr(tdf_l, "hierarchy_map")
  }


  if(!is.null(hmap)){
    hmap_present <- TRUE



    get_hmap_stats <- function(){
      hhd <- colnames(hmap) %>%
        map(
          function(.x){
            hmap %>% group_by(.data[[.x]]) %>%
              summarise_all(n_distinct) %>% ungroup() %>%
              select(-1) %>% summarise_all(max) %>%
              pivot_longer(
                cols = everything(),
                names_to = "disaggregation_group", values_to = "ord") %>%
              mutate(hierarchy = .x)
          }) %>%
        bind_rows()

      hhd_upper <- hhd %>% filter(ord<=1) %>% select(-ord)
      hhd_upper
    }

    hmap_stats <- get_hmap_stats()

  } else {
    hmap_present <- FALSE
  }

  tdf_l$lineage <- ""

  tdf_ll <- split(tdf_l, tdf_l$meta.disaggregation_group)

  for_a_node <- function(nd){

    nd_orig_cols <- colnames(nd)

    agg_fn <- switch(
      nd$meta.accumulation_rule[1],
      "sum" = sum,
      "mean" = mean,
      stop(paste0("Unknown accumulation rule: ", nd$meta.accumulation_rule[1]), call. = FALSE))

    if( "meta.unit" %in% nd_orig_cols){
      unit_chk <- nd %>% group_by(time, meta.price_basis, meta.release_tag) %>%
        summarise(n_unit = n_distinct(meta.unit),.groups = "drop") %>%
        filter(n_unit > 1)
      if(NROW(unit_chk) > 0){
        stop("Multiple units found in a single combination of time, meta.disaggregation_group, meta.price_basis and meta.release_tag. Please ensure that each such combination has a unique unit.", call. = FALSE)
      }
    }

    if(!("meta.release_date" %in% nd_orig_cols)){
      # A dummy release date is added here just to ensure that the code runs without error. This will be discarded later in the code.
      nd <- nd %>% mutate(meta.release_date = as.Date("2018-03-15"))
    }

    if(!("meta.release_order" %in% nd_orig_cols)){
      # A dummy release order is added here just to ensure that the code runs without error. This will be discarded later in the code.
      nd <- nd %>% mutate(meta.release_order = 1)
    }

    do_for_a_variant <- function(nd_v){
      if(nd_v$meta.parent[1] == "#root"){
        # no further aggregation possible or not defined.
        return(nd_v)
      }

      # check if all categories are present or not

      if(hmap_present){
        hthis <- which_disaggregation_group(nd_v$meta.name, hmap = hmap)[1]
        expected_cats <- unique(hmap[[hthis]])
        actual_cats <- unique(nd_v$meta.name)
        missing_cats <- setdiff(expected_cats, actual_cats)
        if(length(missing_cats)>0){
          warning(paste0("These categories are expected based on hierarchy_map but not found in data for meta.disaggregation_group ", hthis, ": ",
                         paste0(missing_cats, collapse = ", ")), call. = FALSE)
        }
      }

      nd_ag <- nd_v %>%
        #select(-meta.name) %>%
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
          meta.parent = "#root",
          meta.is_most_granular = FALSE
        )

      if(hmap_present){
        hthis <- which_disaggregation_group(nd_ag$meta.name, hmap = hmap)[1]
        nd_ag <- nd_ag %>%
          mutate(
            meta.disaggregation_group = hthis
          )
      } else {
        nd_ag <- nd_ag %>%
          mutate(
            meta.disaggregation_group = "domain"
          )
      }

      # remove the dummy release date and release order if they were added
      nd_ag <- nd_ag[intersect(nd_orig_cols, colnames(nd_ag))]
      nd_ag
    }

    own_variant <- do_for_a_variant(nd)

    final_variant <- own_variant

    if(hmap_present){
      hthis <- which_disaggregation_group(nd$meta.name, hmap = hmap)[1]
      own_variant_hthis <- which_disaggregation_group(own_variant$meta.name, hmap = hmap)[1]
      h_to_do <- hmap_stats %>%
        filter(hierarchy %in% hthis) %>%
        filter(!(disaggregation_group %in% own_variant_hthis))

      if(NROW(h_to_do)>0){
        # It means some more aggregation is possible
        for_a_disaggregation_group <- function(dg){
          this_map <- hmap[c(hthis,dg)]
          colnames(this_map) <- c("meta.name", "meta.parent")
          this_map <- distinct(this_map)

          nd_dg <- nd %>% select(-meta.parent) %>%
            left_join(this_map, by = "meta.name")
          do_for_a_variant(nd_dg)
        }

        h_extra <- h_to_do$disaggregation_group %>%
          map(for_a_disaggregation_group) %>%
          bind_rows()

        final_variant <- bind_rows(own_variant, h_extra) %>% distinct()
      }
    }

    final_variant[intersect(nd_orig_cols, colnames(final_variant))]
  }

  tdf_agg <- tdf_ll %>% map(for_a_node) %>% bind_rows()

  if("meta.release_date" %in% colnames(tdf_agg)){
    tdf_agg <- tdf_agg %>% group_by(time, meta.name, meta.disaggregation_group, meta.price_basis, meta.release_tag) %>%
      slice_max(order_by = meta.release_date, n = 1, with_ties = FALSE)
  } else  if("meta.release_order" %in% colnames(tdf_agg)){
    tdf_agg <- tdf_agg %>% group_by(time, meta.name, meta.disaggregation_group, meta.price_basis, meta.release_tag) %>%
      slice_max(order_by = meta.release_order, n = 1, with_ties = FALSE)
  }

  tdf_agg %>% distinct()

}


CHECK <- function(){

  td <- DEV()

  d0 <- aggregate_component(td)
  #d0 <- aggregate_component(td %>% filter(time=="2011-12"))


  # f<- function(){
  #   # check
  #   d0 %>%
  #     group_by(time, meta.name, meta.disaggregation_group, meta.price_basis, meta.release_tag) %>%
  #     mutate(n=n()) %>% ungroup() %>% filter(n>1) %>%
  #     group_by(time, meta.name, meta.release_tag, meta.price_basis) %>%
  #     summarise(mv=min(value.level), MV = max(value.level), .groups = "drop") %>% ungroup() %>%
  #     mutate(chk = abs(MV/mv-1)) %>% filter(chk>0.004)
  # }


  d0 %>% group_by(time, meta.name, meta.price_basis, meta.release_tag) %>% count() %>% filter(n>1)


  d0 %>%
    group_by(time, meta.name, meta.price_basis, meta.release_tag,
             meta.disaggregation_group) %>% mutate(n=n()) %>% filter(n>1)->uu
  uu %>% summarise(rd = value.level %>% range() %>% diff() %>% as.numeric()) %>% group_by(meta.release_tag) %>% summarise(max_rd = max(rd))

}
