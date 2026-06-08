

#' Splice Time Series Across Different Price Bases
#'
#' @description
#' Connects and aligns time series data that have undergone a base year revision
#' (or change in price basis) to create a single, continuous historical series.
#'
#' @details
#' This function takes a dataset containing exactly two distinct price bases
#' (e.g., an old base year and a new base year) and splices the historical series
#' onto the target series.
#'
#' The splicing operation calculates an \code{avg_linking_factor} based on the
#' overlapping periods between the two series. If the data is sub-annual and
#' contains full seasonal cycles in the overlap, it computes season-specific
#' linking factors (e.g., separate factors for Q1, Q2, Q3, and Q4). Otherwise,
#' it computes a single, flat linking factor.
#'
#' The historical series is multiplied by this linking factor, and the newly
#' extended historical periods are appended to the target series. A new boolean
#' column, \code{meta.is_spliced}, is added to the output to flag the back-casted
#' (converted) observations.
#'
#' @param tdl A \code{tdf_long} object containing the time-series data.
#' The data must contain exactly two distinct values in the \code{meta.price_basis} column.
#' @param target_price_basis A character string specifying which price basis
#' should be retained as the benchmark (usually the newer base year). If left
#' as \code{NULL}, the function automatically defaults to the price basis that
#' contains the most recent time period.
#' @param ... Additional arguments passed to methods.
#'
#' @return A \code{tdf_long} object containing the spliced, continuous time series.
#' The original \code{meta.price_basis} dimension is collapsed to the target basis,
#' and a new column \code{meta.is_spliced} is added to track which rows were mathematically converted.
#'
#' @export
splice_series <- function(tdl, target_price_basis = NULL, ...){
  UseMethod("splice_series")
}

#' @export
splice_series.tdf_long <- function(tdl, target_price_basis = NULL, return_diagnostics = FALSE, ...){
  tdl |>
    to_tdf_long_list() |>
    splice_series.tdf_long_list(
      target_price_basis = target_price_basis,
      return_diagnostics = return_diagnostics, ...) |>
    as_tdf_long()
}


#  It takes two price basis tdf-long form

#' @export
splice_series.tdf_long_list <- function(
    tdl,
    target_price_basis = NULL,
    mean_type = c("auto", "GM", "AM"), return_diagnostics = FALSE, ...){

  # Match the mean_type argument, defaulting to the first option ("auto")
  mean_type <- match.arg(mean_type)

  tdf_long_check_structure(tdl)

  td <- tdl$data

  if(sum(stringr::str_detect(colnames(td),"value\\.level"))!=1 || !any(colnames(td)=="value.level")){
    stop("In data, value.level column is must! And there should be only one value.* column!", call. = FALSE)
  }

  # Checks
  pbs <- td %>% group_by(meta.price_basis) %>%
    summarise(n_times = n_distinct(time), max_time = max(time))

  if(length(pbs$meta.price_basis)!=2) {
    stop("The provided tdf_long_list object does not contain exactly two price bases. Please check the data and ensure it contains exactly two distinct meta.price_basis values.", call. = FALSE)
  }

  if(sum(str_detect(pbs$meta.price_basis, "nominal"))*sum(str_detect(pbs$meta.price_basis, "real"))!=0){
    warning("The provided tdf_long_list object contains two price bases, but they contain 'real' and 'nominal' in their names. This may lead to ambiguity in splicing. (Choose either).", call. = FALSE)
  }

  if(missing(target_price_basis) || is.null(target_price_basis)){
    px <- pbs %>% dplyr::arrange(dplyr::desc(max_time), n_times) %>%
      pull(meta.price_basis)
    target_price_basis <- px[1]
  }

  if(!(target_price_basis %in% pbs$meta.price_basis)){
    # Try to auto match (based on string matching)
    if(length(target_price_basis)!=1) stop("Not scalar entry for target_price_basis!", call. = FALSE)
    pbs_to_match <- unique(pbs$meta.price_basis)
    if_there_PTXQC <- any(file.exists(file.path(.libPaths(), "PTXQC")))

    # x vector and y scalar
    match_measure <- function(x, y){
      if(if_there_PTXQC){
        x |> purrr::map_int(~PTXQC::LCS(.x, y) |> nchar())
      } else {
        stringr::str_detect(x, y) |> as.numeric()
      }
    }

    try_match <- dplyr::tibble(
      pb = pbs_to_match,
      m = match_measure(pb, target_price_basis)
    )

    target_price_basis <- try_match$pb[which.max(try_match$m)][1]
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
     && (length(setdiff(common_td$time_part, all_annual_parts)) == 0) ) {
    gvs <- c("time_part","meta.release_tag", "meta.name", "meta.disaggregation_group")
  } else {
    gvs <- c("meta.release_tag", "meta.name", "meta.disaggregation_group")
  }

  # Helper function to dynamically calculate mean based on the vector's values
  aggregate_fn <- function(x) {
    x <- x[!is.na(x)]
    if(length(x)==0) return(NA)

    mt <- mean_type
    if (mt == "auto") {
      mt <- if (all(x > 0)) "GM" else "AM"
    }

    if (mt == "GM") {
      exp(mean(log(x)))
    } else {
      mean(x)
    }
  }

  ## avg_linking_factor
  cx <- common_td %>%
    mutate(linking_factor = value.level_in_tar/value.level_to_conv) %>%
    group_by(
      across(dplyr::all_of(gvs))
    ) %>%
    summarise(
      avg_linking_factor = aggregate_fn(linking_factor),
      qd_linking_factor = quartile_deviation(linking_factor), .groups = "drop")

  # more less this number is that good the series is : mean(cx$qd_linking_factor)

  # many release vintage will not get any value so extending cx
  cx_ext <- cx %>%
    group_by(
      across(dplyr::all_of(setdiff(gvs, "meta.release_tag")))
    ) %>%
    summarise(avg_linking_factor = aggregate_fn(avg_linking_factor), .groups = "drop") %>%
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
  if(return_diagnostics) return(list(bench = series_bench, linking_factor_df = cx, linking_factor_df_ext = cx_ext))

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

  # restore release order / date
  rel_tag_flt_cols <- c("meta.release_order","meta.release_date")
  rel_tag_flt_cols <- colnames(td) |> intersect(rel_tag_flt_cols)
  rmap <- td |>
    dplyr::select(meta.release_tag, all_of(rel_tag_flt_cols)) |>
    dplyr::group_by(meta.release_tag) |>
    dplyr::summarise_all(max) |>
    dplyr::ungroup()

  series_converted_ext <- series_converted_ext |>
    dplyr::left_join(rmap, by = "meta.release_tag")

  tdl_out <- tdl

  tdl_out$data <- series_converted_ext

  tdf_long_check_structure(tdl_out)

  tdl_out

}
