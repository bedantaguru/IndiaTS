



# hmap : Hierarchy map

#' Manage and Extract Hierarchy Maps
#'
#' @description
#' `hmap()` is the dispatcher for interacting with the hierarchy map (`hmap`)
#' attached to a long-format time-series data frame (`tdl`). It can extract
#' the map, estimate or retrieve weights, inspect hierarchy structure, identify
#' disaggregation groups, or modify/compact the hierarchy.
#'
#' @param tdl A long-format time-series data frame containing an `hmap` attribute.
#' @param weight Logical. Return the currently attached aggregation weights (`wtd`).
#' @param retrieve_weight Logical. Estimate aggregation weights using OLS
#'   regressions between parent and child series.
#' @param add_weight A data frame of custom weights to attach. Required columns:
#'   `meta.name`, `meta.parent`, `wtd`, `meta.disaggregation_group`, and
#'   `meta.parent_disaggregation_group`.
#' @param get_stat Logical. Return hierarchy statistics, including parent-child
#'   mappings and waterfalls.
#' @param detect_disaggregation_group Character vector of series names. Returns
#'   the group with the highest coverage of the supplied names.
#' @param add A data frame of new mappings, a two-sided formula, a vector of formulas,
#'   or a named list of formulas to merge into `hmap`.
#' @param set A data frame that completely replaces the existing `hmap`.
#' @param compact Logical. Compact `tdl` and `hmap` by removing unmapped data.
#' @param prune_data Logical. Reduce the `tdl` data frame to the bare minimum observations
#'   (lowest waterfall rank) required to aggregate up to main figures without redundancy.
#' @param sync_data Logical. Aggregate and populate all disaggregation layers across
#'   the hierarchy using existing base components.
#' @param resync_data Logical. Perform a complete data reset by first pruning
#'   (`prune_data = TRUE`) and then recalculating all layers (`sync_data = TRUE`).
#' @param ... Operation-specific arguments passed to underlying child functions.
#'   For `retrieve_weight`: `allow_discrepancy` (allow an OLS intercept),
#'   `rescale` (rescale weights to sum to 1), and `force` (continue when individual
#'   branches fail). For `compact`: `data_filter` (logical) controls whether
#'   orphaned data in `tdl` is actively removed alongside the compacted map.
#'
#' @details
#' **Operation Priority:**
#' `hmap()` evaluates exactly one operation per call in this priority order:
#' `weight` > `retrieve_weight` > `add_weight` > `get_stat` >
#' `detect_disaggregation_group` > `add` > `set` > `compact` >
#' `prune_data` > `sync_data` > `resync_data`.
#' If no operation is requested, it returns the raw hierarchy map attached to `tdl`.
#'
#' **Formula-Based Additions (`add`):**
#' You can now easily define custom re-aggregations using formulas. You may pass a
#' single formula, or you can provide a vector or named list of formulas. In these
#' formulas, you can set the left-hand side (LHS) as the new parent series, and
#' you can specify the existing child series on the right-hand side (RHS).
#' For example, you may use:
#' \itemize{
#'   \item **Single Formula:** `add = Parent ~ Child1 + Child2`
#'   \item **Vector of Formulas:** `add = c(Parent1 ~ C1 + C2, Parent2 ~ C3 + C4)`
#'   \item **Named List:** `add = list(custom_layer = Parent ~ C1 + C2)`
#' }
#' If you supply unnamed formulas or formula vectors, the name of the new
#' disaggregation group can simply be inferred from the LHS parent variable.
#' You can also rely on the function to automatically map any unmentioned series
#' in the target group to themselves, so you don't have to worry about data loss.
#'
#' **Key Concepts:**
#' * **Weights:** Estimated via OLS. While the default regression drops the intercept,
#'   you may pass parameters like `allow_discrepancy` via `...` to include it. Low
#'   R-squared values trigger a warning of unreliable derivation.
#' * **Waterfalls:** Ordered sequences of disaggregation groups used to safely
#'   traverse aggregation relationships.
#' * **Compacting:** Updates the active hierarchy to only include relevant mappings.
#'   By default (`data_filter = TRUE`), orphaned data in `tdl` is dropped (and throws
#'   an error if all data is removed). If you set it to `FALSE`, the map is compacted
#'   but the original unmapped data is retained in `tdl`.
#' * **Data Syncing & Pruning:** `prune_data` strips the dataset down to the most
#'   granular layer necessary to rebuild the hierarchy. `sync_data` performs the opposite,
#'   calculating and filling in values for all upper disaggregation layers. `resync_data`
#'   safely combines both to refresh the entire dataset.
#'
#' @return
#' Polymorphic output based on the requested operation:
#' \itemize{
#'   \item **Default:** The `hmap` data frame.
#'   \item **`weight` / `retrieve_weight`:** A data frame of current or estimated weights.
#'   \item **`get_stat`:** A list containing `from_to_map` and `waterfalls`.
#'   \item **`detect_disaggregation_group`:** Character string of the best-matching group.
#'   \item **`add_weight` / `add` / `set` / `compact` / `prune_data` / `sync_data` / `resync_data`:** The modified `tdl` object.
#' }
#'
#' @export
hmap <- function(
    tdl,
    weight = FALSE, retrieve_weight = FALSE, add_weight = NULL,
    get_stat = FALSE, detect_disaggregation_group = NULL,
    add = NULL, set = NULL, compact = FALSE,
    prune_data = FALSE, sync_data = FALSE, resync_data = FALSE,
    ...){

  # Get Hmap
  hm <- attr(tdl, "hmap")

  if(weight){
    return(attr(hm,"wtd"))
  }

  if(retrieve_weight){
    return(hmap_retrieve_weight_for_index_type(tdl, ...))
  }

  if(!is.null(add_weight) && is.data.frame(add_weight) && NROW(add_weight)>0){
    return(
      hmap_add_weight(tdl, wtd = add_weight)
    )
  }

  if(get_stat){
    return(hmap_get_stats(hm))
  }

  if(!is.null(detect_disaggregation_group) && is.character(detect_disaggregation_group) && length(detect_disaggregation_group)>0){
    return(
      hmap_which_disaggregation_group(meta_names = detect_disaggregation_group, hmap = hm)
    )
  }

  if(!is.null(add) && is.data.frame(add) && NROW(add)>0){
    return(
      hmap_add_on_tdf_long(tdl, new_hmap = add)
    )
  }

  # alternative add mode where add is a list of formula type
  if(is.list(add) || inherits(add, "formula")){
    tdl_try <- tryCatch(
      hmap_add_on_tdf_long_from_formula(tdl, fin = add),
      error = function(e) e
    )

    if (inherits(tdl_try, "error")) {
      e <- tdl_try
      while (!is.null(e$parent)) {
        e <- e$parent
      }
      warning(
        "Trying to use formula ... See the error:\n",
        conditionMessage(e)
      )
    } else if(inherits(tdl_try, class(tdl))){
      return(tdl_try)
    }


  }


  if(!is.null(set) && is.data.frame(set) && NROW(set)>0){
    attr(tdl, "hmap") <- set
    return(tdl)
  }

  if(compact){
    return(
      hmap_compact(tdl, ...)
    )
  }

  if(prune_data){
    return(hmap_prune_data(tdl))
  }

  if(sync_data){
    return(hmap_sync_data(tdl))
  }

  if(resync_data){
    return(hmap_resync_data(tdl))
  }


  hm
}

# returns a list summary
hmap_get_stats <- function(hmap){

  hmd <- colnames(hmap) %>%
    map(
      function(.x){
        hmap %>%
          group_by(.data[[.x]]) %>%
          summarise_all(n_distinct) %>%
          ungroup() %>%
          select(-1) %>%
          summarise_all(max) %>%
          pivot_longer(
            cols = everything(),
            names_to = "disaggregation_group", values_to = "ord") %>%
          mutate(hierarchy = .x)
      }) %>%
    bind_rows()

  hmd_upper <- hmd %>% filter(ord<=1) %>% select(-ord) %>%
    select(from = hierarchy, to = disaggregation_group)

  dg_lvl <- hmd_upper %>% group_by(from) %>% summarise(lvl_up = n(), .groups = "drop")

  hmd_upper_lvl <- hmd_upper %>%
    left_join(dg_lvl, by = c("from" = "from")) %>%
    left_join(dg_lvl, by = c("to" = "from"), suffix = c("_from", "_to"))

  # fill NA by 0
  hmd_upper_lvl <- hmd_upper_lvl %>%
    mutate(lvl_up_to = ifelse(is.na(lvl_up_to), 0, lvl_up_to)) %>%
    mutate(lvl_up_from = ifelse(is.na(lvl_up_from), 0, lvl_up_from))


  water_falls <- list()

  trails_front <- function(init){
    lvls <- c(init)
    repeat{
      next_lvl <- hmd_upper_lvl %>%
        filter(from == tail(lvls,1))

      if(NROW(next_lvl)==0) {
        break
      }

      next_lvl <- next_lvl %>%
        filter(lvl_up_to == max(lvl_up_to)) %>%
        pull(to)

      if(length(next_lvl)==0){
        break
      }
      lvls <- c(lvls, next_lvl[1])
    }
    lvls
  }

  trails_back <- function(end_point){
    lvls <- c(end_point)
    repeat{
      next_lvl <- hmd_upper_lvl %>%
        filter(to == head(lvls,1))

      if(NROW(next_lvl)==0) {
        break
      }

      prev_lvl <- next_lvl %>%
        filter(lvl_up_from == min(lvl_up_from)) %>%
        pull(from)

      if(length(prev_lvl)==0){
        break
      }
      lvls <- c(prev_lvl[1], lvls)
    }
    lvls
  }

  trails <- function(mid){
    t1 <- trails_front(mid)
    t2 <- trails_back(mid)
    if(length(t1)>1){
      c(t2, t1[-1])
    } else {
      t2
    }
  }

  water_falls[[1]] <- trails(hmd_upper_lvl$from[1])

  repeat{

    done_so_far <- unlist(water_falls) %>% unique()
    not_in <- hmd_upper_lvl %>% filter(!from %in% done_so_far) %>% pull(from) %>% unique()

    if(length(not_in)==0){
      break
    }

    this_trail <- trails(not_in[1])

    water_falls %>%
      map_lgl(function(nn) all(this_trail %in% nn)) %>%
      any() -> is_in

    if(!is_in){
      water_falls[[length(water_falls)+1]] <- this_trail
    }
  }


  list(from_to_map = hmd_upper_lvl, waterfalls = water_falls)


}

#
# Here disaggregation_group and disaggregation_layers are same thing, just
# different naming. We can use either of them.
# Returns a char-vector or single character
hmap_which_disaggregation_group <- function(meta_names, hmap, return_covs = FALSE){
  if(NROW(hmap)==0){
    return(NA_character_)
  }
  meta_names <- unique(meta_names)
  hmvec <- hmap %>% map_dbl(function(.x){
    length(intersect(.x, meta_names))/length(unique(.x))
  })
  if(max(hmvec) == 0){
    return(NA_character_)
  }
  if(return_covs) return(hmvec[hmvec>0])
  which(hmvec==max(hmvec)) %>% names
}

# tdl = tdf long

# Returns tdf_long_list (internal)
hmap_add <- function(tdl,  new_hmap){

  this_hmap <- tdl$hmap
  common_cols <- intersect(colnames(this_hmap), colnames(new_hmap))

  chk <- common_cols %>%
    map_lgl(function(cn){
      extra <- new_hmap[[cn]] %>% setdiff(this_hmap[[cn]])
      length(extra)==0
    })

  if(!all(chk)){
    stop("Not possible to merge!", call. = FALSE)
  }

  merged_hmap <-  this_hmap %>%
    left_join(new_hmap, by = common_cols, relationship = "many-to-many")

  merged_hmap <- merged_hmap %>%
    dplyr::mutate_all(~tidyr::replace_na(., "#NR"))

  tdl$hmap <- dplyr::distinct(merged_hmap)

  tdl

}

# Returns tdf_long
hmap_add_on_tdf_long <- function(tdl,  new_hmap){
  tdl_in <- tdf_long_make(dat = dplyr::as_tibble(tdl), hmap = attr(tdl,"hmap"), minimal = TRUE)
  int_out <- hmap_add(tdl_in, new_hmap)
  as_tdf_long(int_out)
}

# Returns tdf_long
hmap_add_weight <- function(tdl, wtd){
  hm <- attr(tdl, "hmap")

  expected_cols <- c("meta.name", "meta.parent", "wtd", "meta.disaggregation_group",
                     "meta.parent_disaggregation_group")

  if(length(setdiff(expected_cols, colnames(wtd)))>0){
    stop("Expected columns not found in wtd!", call. = FALSE)
  }

  wtd_cleaned <- wtd |>
    dplyr::filter(!is.na(wtd)) |>
    dplyr::filter(meta.name %in% tdl$meta.name,
                  meta.disaggregation_group %in% tdl$meta.disaggregation_group) |>
    dplyr::filter(meta.parent %in% unlist(hm)) |>
    dplyr::filter(meta.parent_disaggregation_group %in% colnames(hm))

  if(NROW(wtd_cleaned)==0 && NROW(wtd)>0) warning("wtd may be malformed!", call. = FALSE)

  attr(hm, "wtd") <- wtd_cleaned
  attr(tdl, "hmap") <- hm
  tdl
}

# Returns tdf_long
hmap_compact <- function(tdl, data_filter = TRUE){
  hm <- attr(tdl, "hmap")

  if(NROW(hm)>0){
    hm_clean <- hm[intersect(unique(tdl$meta.disaggregation_group), colnames(hm))]
    tdl_clean <- tdl[tdl$meta.disaggregation_group %in% colnames(hm_clean),]
    tdl_lst <- tdl |> dplyr::distinct(meta.name, meta.disaggregation_group)
    tdl_lst <- tdl_lst |> split(tdl_lst$meta.disaggregation_group)

    for(nn in tdl_lst){

      grp_col <- nn$meta.disaggregation_group[1]

      # 1. Filter hm_clean: keep rows where the hm_clean value exists in nn$meta.name
      hm_clean <- hm_clean[hm_clean[[grp_col]] %in% nn$meta.name, ]

      # 2. Filter tdl_clean:
      # Keep the row IF it belongs to a DIFFERENT group
      # OR (if it belongs to this group) its name survived the hm_clean filtering
      keep_rows <- (tdl_clean$meta.disaggregation_group != grp_col) |
        (tdl_clean$meta.name %in% hm_clean[[grp_col]])

      tdl_clean <- tdl_clean[keep_rows, ]
    }



    if(NROW(tdl_clean)<NROW(tdl)){
      if (!data_filter) message("H-map computation resulted in data loss. You can avoid this by setting `data_filter = TRUE`.")
      if(NROW(tdl_clean) == 0) stop("All data filtered!", call. = FALSE)
    }

    if(data_filter){
      tdl <- tdl_clean
    }


    attr(tdl, "hmap") <- dplyr::distinct(hm_clean)

  }

  tdl

}

# Returns hmap
hmap_retrieve_weight_for_index_type <- function(tdl, allow_discrepancy = FALSE, rescale= FALSE, force = TRUE){

  f_rel_tag <-  " ~ -1 + "

  if(allow_discrepancy) f_rel_tag <-  " ~ "

  hm <- hmap(tdl)

  hs <- hmap_get_stats(hm)

  #  Helper functions
  for_a_grp <- function(fg1){
    # This assumes two dgs have distinct names
    if(length(intersect(fg1[[1]], fg1[[2]]))!=0){
      stop("Common names in two disaggregation groups not supported!", call. = FALSE)
    }

    tdl_this <- tdl |> dplyr::filter(
      meta.disaggregation_group %in% names(fg1), meta.name %in% unname(unlist(fg1)))

    if(length(setdiff(tdl_this$meta.disaggregation_group, names(fg1)))!=0){
      stop("Not all disaggregation groups present for wtd estimation!", call. = FALSE)
    }

    name_dep <- intersect(fg1[[2]][1], tdl$meta.name) |> unique()
    name_indep <- intersect(unique(fg1[[1]]), tdl$meta.name) |> unique()

    if(length(name_dep)*length(name_indep)==0){
      stop("Not possible to estimate wtd as requird name missing!", call. = FALSE)
    }

    tdf_this <- tdl_this |> as_tdf()

    formula_this <- stats::as.formula(
      paste0(
        paste0("`",name_dep[1],"`"),
        f_rel_tag,
        paste0("`",name_indep,"`", collapse = " + ")
      )
    )

    xinfo <- stats::lm(formula = formula_this, data = tdf_this)

    coef_df <- data.frame(
      variable = names(stats::coef(xinfo)),
      coefficient = unname(stats::coef(xinfo))
    )

    r_squared <- summary(xinfo)$r.squared

    if(rescale){
      if(abs(sum(coef_df$coefficient)-1)<0.01){
        coef_df$coefficient <- coef_df$coefficient/sum(coef_df$coefficient)
      }
    }
    if(r_squared < 0.8){
      message("Low R-Sq (weight derivation might be wrong)!")
    }

    coef_df$variable <- stringr::str_remove_all(coef_df$variable, "`")

    colnames(coef_df) <- c(colnames(fg1)[1], "wtd")

    fg1_out <- fg1 |> dplyr::left_join(coef_df, by = colnames(fg1)[1])

    if(NROW(fg1_out) != NROW(fg1) || length(setdiff(fg1[[1]], fg1_out[[1]]))!=0){
      stop("Join failed due to name mismatch! (weight derivation failed).", call. = FALSE)
    }

    fg1_out
  }

  # Order it Important here in ft_dgs (has to be from waterfall)
  for_a_ft_dg <- function(ft_dgs){
    hm_this <- hm[ft_dgs] |> dplyr::distinct()

    # This is order dependent thus ft_dgs is order sensitive (order from waterfall)
    f_groups <- hm_this |> split(hm_this[[2]])

    f_all_grp <- f_groups |> purrr::map(for_a_grp) |> dplyr::bind_rows()

    f_all_grp$meta.disaggregation_group <- colnames(f_all_grp)[1]
    f_all_grp$meta.parent_disaggregation_group <- colnames(f_all_grp)[2]
    colnames(f_all_grp)[1:2] <- c("meta.name","meta.parent")

    f_all_grp
  }

  for_a_ft_dg_safe  <- function(ft_dgs){
    tryCatch(
      for_a_ft_dg(ft_dgs),
      error = function(e) NULL
    )
  }

  for_a_waterfall <- function(waterfall1){
    if(length(waterfall1)<2) stop("No layers to determine wtd!", call. = FALSE)

    ft_all_dgs <- dplyr::tibble(from = waterfall1[-length(waterfall1)], to = waterfall1[-1])

    for_a_ft_dg_this <- if(force) for_a_ft_dg_safe else for_a_ft_dg

    for_this_waterfall <- 1:NROW(ft_all_dgs) |> purrr::map(\(i){
      for_a_ft_dg_this(as.character(ft_all_dgs[i,]))
    }) |> dplyr::bind_rows()
  }

  for_a_waterfall_safe <- function(waterfall1){
    tryCatch(
      for_a_waterfall(waterfall1),
      error = function(e) NULL
    )
  }


  all_waterfalls <- hs$waterfalls |> purrr::map(\(wf){
    for_a_waterfall_this <- if(force) for_a_waterfall_safe else for_a_waterfall
    for_a_waterfall_this(wf)
  }) |> rows_append_distinct(
    primary_key = c("meta.name", "meta.parent", "meta.disaggregation_group",
                    "meta.parent_disaggregation_group"))


  all_waterfalls

}


hmap_normalize_form_in <- function(form_in) {

  as_named_list_of_formula_vector <- function(form_lst){

    chk1 <- as_formula_vector(form_lst)

    if(is_valid_formula_vector(chk1)) {
      form_lst <- list(user_defined_disaggregation_group = chk1)
    } else {
      lst2 <- form_lst |> purrr::map(as_formula_vector)
      chk2 <- lst2 |> purrr::map_lgl(is_valid_formula_vector)

      if(all(chk2)) {
        form_lst <- lst2
      } else {
        stop(
          "The input list must be a non-empty named list of formulas / vector of formulas.",
          call. = FALSE
        )
      }

    }

    if (!is.list(form_lst) || length(form_lst) == 0 || is.null(names(form_lst))) {
      stop(
        "The input list must be a non-empty named list",
        call. = FALSE
      )
    }

    if (any(names(form_lst) == "" | is.na(names(form_lst)))) {
      stop(
        "All elements of input list must have a name.",
        call. = FALSE
      )
    }

    form_lst

  }

  as_formula_vector <- function(x){

    if(is_two_sided_formula(x)) {
      return(c(x))
    }

    if(is_valid_formula_vector(x)) {
      return(x)
    }

    "invalid"

  }

  is_valid_formula_vector <- function(fv) {
    # A valid formula vector MUST be an unnamed list AND not empty
    if (is.list(fv) && length(fv) > 0 && is.null(names(fv))) {
      chk <- fv |> purrr::map_lgl(is_two_sided_formula)
      all(chk)
    } else {
      FALSE
    }
  }

  is_two_sided_formula <- function(form) {
    inherits(form, "formula") && length(form) == 3
  }

  as_named_list_of_formula_vector(form_in)

}


hmap_create_hmap_from_formula <- function(fin, present_hmap){

  # Create hmap from formulas

  fin <- hmap_normalize_form_in(fin)

  all_hmap_names <- present_hmap |>
    unlist() |>
    unname() |>
    unique()

  for_a_formula <- function(form, nname) {

    # Extract variable names from LHS and RHS
    parent_col <- all.vars(form[[2]]) |> unique()
    child_cols <- all.vars(form[[3]]) |> unique()

    if (length(parent_col) != 1) {
      stop(
        "There must be exactly one parent on the left side of the formula.",
        call. = FALSE
      )
    }

    parent_present <- all(parent_col %in% all_hmap_names)
    children_present <- all(child_cols %in% all_hmap_names)

    # Neither side is present in the existing hmap
    if (!parent_present && !children_present) {
      stop(
        paste0(
          "Formula `", nname,
          "` cannot be added: neither all child names nor the parent name ",
          "is present in the existing hierarchy map."
        ),
        call. = FALSE
      )
    }

    # Both sides are already present — relationship is ambiguous
    if (parent_present && children_present) {
      stop(
        paste0(
          "Formula `", nname,
          "` is ambiguous: both the parent and all child names are already ",
          "present in the existing hierarchy map."
        ),
        call. = FALSE
      )
    }

    if (parent_present) {

      # Parent exists
      p_dg <- hmap_which_disaggregation_group(
        meta_names = parent_col,
        hmap = present_hmap
      )

      cn_put <- c(p_dg, nname)

      message("Note: This case is not well explored. Contacting the author for clarification may be helpful.")

    } else {

      # Children exist
      c_dg <- hmap_which_disaggregation_group(
        meta_names = child_cols,
        hmap = present_hmap
      )

      cn_put <- c(nname, c_dg)

    }

    out_hm <- dplyr::tibble(
      col_parent = parent_col,
      col_child = child_cols
    )

    colnames(out_hm) <- cn_put

    out_hm

  }

  for_a_node_name <- function(nname){

    all_maps <- fin[[nname]] |> purrr::map(~for_a_formula(.x, nname)) |> dplyr::bind_rows()

    if(NCOL(all_maps)>2) {
      stop(
        paste0(
          "`", nname, "` combines multiple disaggregation layers: ",
          paste(setdiff(colnames(all_maps), nname), collapse = ", "),
          ". Please use a separate name for each re-aggregation layer."
        ),
        call. = FALSE
      )
    }

    cname <- intersect(colnames(all_maps), colnames(present_hmap))
    rnames <- unique(setdiff(present_hmap[[cname]], all_maps[[cname]]))

    rest <- dplyr::tibble(
      parent = rnames,
      child = rnames
    )

    colnames(rest) <- colnames(all_maps)

    all_maps_filled <- all_maps |> dplyr::bind_rows(rest) |> dplyr::distinct()

    all_maps_filled

  }

  hmap_new_lst <- names(fin) |>
    purrr::map(for_a_node_name)

  hmap_new_lst

}


hmap_add_on_tdf_long_from_formula <- function(tdl, fin){
  # Retrieve all hmaps from formula
  hms <- hmap_create_hmap_from_formula(fin, attr(tdl,"hmap"))

  tdl_out <- tdl

  for(hm1 in hms){
    tdl_out <- hmap_add_on_tdf_long(tdl_out, new_hmap = hm1)
  }

  tdl_out

}


hmap_prune_data <- function(tdl){

  # Prune to bare minimum to add up to main figures

  hm <- attr(tdl, "hmap")
  hs <- hmap_get_stats(hm)

  dgs <- tdl$meta.disaggregation_group |> unique()

  dg_rnk <- dgs |> purrr::map_int(\(dg){
    wfs <- hs$waterfalls
    min(wfs |> purrr::map(~which(.x == dg)) |> unlist())
  })

  dgt <- dplyr::tibble(
    meta.disaggregation_group = dgs,
    meta.disaggregation_group_rank = dg_rnk
  )

  tdl2 <- tdl |> dplyr::left_join(dgt, by = "meta.disaggregation_group")

  tdl2 <- tdl2 |> dplyr::group_by(time, meta.release_tag, meta.price_basis) |>
    dplyr::filter(meta.disaggregation_group_rank == min(meta.disaggregation_group_rank))

  tdl2$meta.disaggregation_group_rank <- NULL

  tdl2
}

hmap_sync_data <- function(tdl){
  # Create all disaggregation layer values.
  aggregate(tdl, type = "component")
}

hmap_resync_data <- function(tdl){
  hmap_prune_data(tdl)
  hmap_sync_data(tdl)
}



