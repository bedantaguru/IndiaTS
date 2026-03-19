
#  It takes two price basis tdf-long form
splice_series <- function(tdl, target_price_basis){

  tdf_long_check_structure(tdl)

  td <- tdl$data

  # Checks
  pbs <- td %>% group_by(meta.price_basis) %>%
    summarise(n_times = n_distinct(time), max_time = max(time))

  if(length(pbs$meta.price_basis)!=2) {
    stop("The provided tdf_long object does not contain exactly two price bases. Please check the data and ensure it contains exactly two distinct meta.price_basis values.", call. = FALSE)
  }

  if(sum(str_detect(pbs$meta.price_basis, "nominal"))*sum(str_detect(pbs$meta.price_basis, "real"))!=0){
    warning("The provided tdf_long object contains two price bases, but they contain 'real' and 'nominal' in their names. This may lead to ambiguity in splicing. (Choose either).", call. = FALSE)
  }

  if(missing(target_price_basis)){
    px <- pbs %>% dplyr::arrange(dplyr::desc(max_time), n_times) %>%
      pull(meta.price_basis)
    target_price_basis <- px[1]
  }

  series_in_tar <- td %>% filter(meta.price_basis == target_price_basis) %>%
    select(time, meta.release_tag,
           # meta.price_basis, (this is not required as it is same)
           meta.name, meta.disaggregation_group, value.level)
  series_to_conv <- td %>% filter(meta.price_basis != target_price_basis) %>%
    select(time, meta.release_tag,
           # meta.price_basis, (this is not required as it is same)
           meta.name, meta.disaggregation_group, value.level)

  common_td <- series_in_tar %>%
    inner_join(
      series_to_conv,
      by = c("time", "meta.release_tag", "meta.name", "meta.disaggregation_group"),
      suffix = c("_in_tar","_to_conv"))


  # various methods

  fq_this <- frequency(common_td$time, singular = TRUE)
  fi <- freq_info_for_one_freq(fq_this)

  # the time_part will be same as time in case of annual data for rest it would differ

  common_td <- common_td %>%
    mutate(time_part = fi$converter(time, with_year = FALSE))

  all_annual_parts <- fi$converter(
    seq(from = as.Date("2018-03-15"), to = as.Date("2019-05-15"), by  = 28),
    with_year = FALSE) %>% unique()


  if((length(unique(common_td$time))>length(unique(common_td$time_part)))
     && (length(setdiff(all_annual_parts, common_td$time_part)) == 0) ) {
    gvs <- c("time_part","meta.release_tag", "meta.name", "meta.disaggregation_group")
  } else {
    gvs <- c("meta.release_tag", "meta.name", "meta.disaggregation_group")
  }

  ## avg_linking_factor
  cx <- common_td %>%
    mutate(linking_factor = value.level_in_tar/value.level_to_conv) %>%
    group_by(
      across(dplyr::all_of(gvs))
    ) %>%
    summarise(
      avg_linking_factor = mean(linking_factor),
      qd_linking_factor = quartile_deviation(linking_factor), .groups = "drop")

  #  more less this number is that good the series is : mean(cx$qd_linking_factor)

  # many release vintage will not gt any value so extending cx
  cx_ext <- cx %>%
    group_by(
      across(dplyr::all_of(setdiff(gvs, "meta.release_tag")))
    ) %>%
    summarise(avg_linking_factor = mean(avg_linking_factor), .groups = "drop") %>%
    dplyr::cross_join(
      series_to_conv %>% distinct(meta.release_tag) %>%
        filter(!(meta.release_tag %in% cx$meta.release_tag))) %>%
    bind_rows(
      cx %>%
        select(dplyr::all_of(gvs), avg_linking_factor) %>%
        distinct()
    )



  series_converted <- series_to_conv %>%
    mutate(time_part = fi$converter(time, with_year = FALSE)) %>%
    inner_join(
      cx_ext,
      by = gvs
    ) %>%
    mutate(value.level_conv = value.level * avg_linking_factor) %>%
    mutate(meta.price_basis = target_price_basis, meta.is_spliced = TRUE) %>%
    select(time, meta.release_tag, meta.price_basis, meta.name, meta.disaggregation_group,
           value.level = value.level_conv, meta.is_spliced)


  this_jvs <- c("time", "meta.release_tag","meta.price_basis","meta.name","meta.disaggregation_group")

  # For checking how the splicing worked
  series_bench <- series_converted %>%
    inner_join(
      td %>% filter(meta.price_basis == target_price_basis) %>%
        select(all_of(this_jvs), value.level),
      by = this_jvs, suffix = c("_spliced","")) %>%
    mutate(df = abs((value.level/value.level_spliced-1)))

  # Check this mean(series_bench$df)*100

  series_converted <- series_converted %>%
    anti_join(
      td %>% filter(meta.price_basis == target_price_basis),
      by = c("time", "meta.release_tag","meta.price_basis","meta.name","meta.disaggregation_group"))


  series_converted_ext <- series_converted %>%
    bind_rows(
      td %>% filter(meta.price_basis == target_price_basis) %>%
        mutate(meta.is_spliced = FALSE)
    ) %>%
    select(time, meta.release_tag, meta.price_basis, meta.name, meta.disaggregation_group, value.level, meta.is_spliced) %>%
    distinct()

  tdl_out <- tdl

  tdl_out$data <- series_converted_ext

  tdf_long_check_structure(tdl_out)

  tdl_out

}

