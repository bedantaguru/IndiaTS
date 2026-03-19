
# hmap : Hierarchy map
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

# Here disaggregation_group and disaggregation_layers are same thing, just
# different naming. We can use either of them.
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
hmap_add <- function(tdl,  new_hmap){

}
