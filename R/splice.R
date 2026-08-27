

#' Splice Time Series Across Different Price Bases
#'
#' @description
#' Connects and aligns time series data that have undergone a base year revision
#' (or change in price basis) to create a single, continuous historical series.
#'
#' @details
#' This function takes a dataset containing two or more distinct price bases
#' (e.g., an old base year and a new base year) and splices the historical series
#' onto the target series. If more than two price bases are present, chain-linking
#' is applied iteratively.
#'
#' Two primary splicing methods are supported:
#' * **`linking_factor` (Default):** Calculates an average linking factor based on
#'   the overlapping periods between the two series. The historical series is
#'   multiplied by this linking factor. If the overlapping series are non-stationary
#'   (e.g., non-I(0)), a warning may be issued.
#' * **`growth_preserving`:** Converts the historical series into growth rates
#'   (or momentum) and applies those rates sequentially backwards from the
#'   earliest available point in the target series.
#'
#' The newly extended historical periods are appended to the target series, and a
#' new boolean column, `meta.is_spliced`, is added to the output to flag the
#' back-casted (converted) observations.
#'
#' @param tdl A time-series data object (e.g., `tdf_long` or `tdf_long_list`).
#' @param target_price_basis A character string specifying which price basis
#'   should be retained as the benchmark (usually the newer base year). If left
#'   as `NULL`, the function automatically defaults to the price basis that
#'   contains the most recent time period.
#' @param splice_method Character. The mathematical approach used to join the series.
#'   Either `"linking_factor"` (default) or `"growth_preserving"`.
#' @param growth_anchor Character. Applicable only if `splice_method = "growth_preserving"`.
#'   Determines what metric is preserved from the old series: `"growth_rate"` (default)
#'   or `"momentum"`. Note that using `"momentum"` requires continuous periods.
#' @param mean_type Character. Applicable only if `splice_method = "linking_factor"`.
#'   Determines how the average linking factor is calculated across the overlapping
#'   periods. Options are `"auto"` (default, switches based on data properties),
#'   `"GM"` (Geometric Mean), or `"AM"` (Arithmetic Mean).
#' @param return_diagnostics Logical. If `TRUE` and using the `"linking_factor"`
#'   method, returns a list containing diagnostic dataframes (e.g., benchmark
#'   comparisons and linking factors) instead of the finalized spliced data.
#'   Defaults to `FALSE`.
#' @param ... Additional arguments passed to specific methods.
#'
#' @return A continuous time series object (of the same class as `tdl`).
#'   The original `meta.price_basis` dimension is collapsed to the target basis,
#'   and a new column `meta.is_spliced` is added to track which rows were
#'   mathematically converted.
#'
#' @export
splice_series <- function(tdl, target_price_basis = NULL, ...){
  UseMethod("splice_series")
}

#' @export
splice_series.tdf_long <- function(
    tdl,
    target_price_basis = NULL,
    splice_method = c("linking_factor","growth_preserving"),
    growth_anchor = c("growth_rate","momentum"),
    mean_type = c("auto", "GM", "AM"), return_diagnostics = FALSE, ...){

  tdl |>
    to_tdf_long_list() |>
    splice_series.tdf_long_list(
      target_price_basis = target_price_basis,
      splice_method = splice_method,
      mean_type = mean_type,
      growth_anchor = growth_anchor,
      return_diagnostics = return_diagnostics, ...) |>
    as_tdf_long()
}


#  It takes two price basis tdf-long form

#' @export
splice_series.tdf_long_list <- function(
    tdl,
    target_price_basis = NULL,
    splice_method = c("linking_factor","growth_preserving"),
    growth_anchor = c("growth_rate","momentum"),
    mean_type = c("auto", "GM", "AM"), return_diagnostics = FALSE, ...){

  n_pbase <- length(unique(tdl$data$meta.price_basis))

  if (n_pbase < 2) {
    stop("Fewer than two price bases found. Nothing to link.",
         call. = FALSE)
  }

  if (n_pbase > 2) {
    message("Multiple price bases found. Applying chain-linking splicing.")
    sp_out <- splice_series_for_multi_pbase(
      tdl = tdl,
      target_price_basis = target_price_basis,
      splice_method = splice_method,
      mean_type = mean_type,
      growth_anchor = growth_anchor, ...)
  }

  if (n_pbase == 2) {
    sp_out <- splice_series_core_for_two_pbase(
      tdl = tdl,
      target_price_basis = target_price_basis,
      splice_method = splice_method,
      mean_type = mean_type,
      growth_anchor = growth_anchor,
      return_diagnostics = return_diagnostics,
      ...
    )
  }

  sp_out

}



splice_helper_match_price_basis <- function(target_price_basis, pbs_to_match) {

  if (length(target_price_basis) != 1L) {
    stop(
      "`target_price_basis` must be scalar.",
      call. = FALSE
    )
  }

  if (!length(pbs_to_match)) {
    return(target_price_basis)
  }

  # Check whether PTXQC is installed, without loading it
  has_PTXQC <- any(
    file.exists(
      file.path(.libPaths(), "PTXQC")
    )
  )

  match_measure <- function(x, y) {
    if (has_PTXQC) {
      purrr::map_int(
        x,
        \(z) PTXQC::LCS(z, y) |> nchar()
      )
    } else {
      stringr::str_detect(
        x,
        stringr::fixed(y)
      ) |> as.numeric()
    }
  }

  try_match <- dplyr::tibble(
    pb = pbs_to_match,
    m = match_measure(pbs_to_match, target_price_basis)
  )

  try_match$pb[which.max(try_match$m)][1L]
}

splice_series_core_for_two_pbase_linking_factor <- function(
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

  target_price_basis <- splice_helper_match_price_basis(
    target_price_basis, unique(pbs$meta.price_basis))

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

  # Helper Functions
  safe_ndiff <- function(x){
    tryCatch(
      forecast::ndiffs(x),
      error = function(e) NA_integer_
    )
  }

  # Helper function to dynamically calculate mean based on the vector's values
  aggregate_fn <- function(x, trim_frac = 0.1) {

    x <- x[!is.na(x)]
    if(length(x)==0) return(NA)

    mt <- mean_type
    if (mt == "auto") {

      nd <- safe_ndiff(x)
      if(is.na(nd)) nd <- 0

      cv <- quartile_deviation(x)/mean(x)*100
      if(is.na(cv)) cv <- 0

      if(nd>0 || cv > 0.05) trim_frac <- 0.2

      if(cv < 0.05 && nd==0) trim_frac <- 0


      mt <- if (all(x > 0)) "GM" else "AM"
    }

    if (mt == "GM") {
      exp(mean(log(x), trim = trim_frac, na.rm = TRUE))
    } else {
      mean(x, trim = trim_frac, na.rm = TRUE)
    }
  }




  ## avg_linking_factor
  cx <- common_td |>
    mutate(linking_factor = value.level_in_tar/value.level_to_conv) |>
    group_by(
      across(dplyr::all_of(gvs))
    ) |>
    dplyr::arrange(as.Date(.data$time)) |>
    summarise(
      avg_linking_factor = aggregate_fn(linking_factor),
      qd_linking_factor = quartile_deviation(linking_factor),
      integration_order = safe_ndiff(linking_factor), .groups = "drop")

  # more less this number is that good the series is : mean(cx$qd_linking_factor)

  non_I0_series <- cx |> dplyr::filter(integration_order>0) |> dplyr::pull(meta.name) |> unique()

  if(length(non_I0_series)>0){
    message(
      paste0("Presence of non-I(0) linking factor found in: ",
             paste0(non_I0_series, collapse = ", "),
             ". Simple splicing may not be adequate.")
    )
  }

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


splice_series_core_for_two_pbase_gr_preserve <- function(
    tdl,
    target_price_basis = NULL,
    growth_anchor = c("growth_rate","momentum"),
    ...){

  growth_anchor <- match.arg(growth_anchor)

  if(growth_anchor == "momentum") {
    td <- tdl$data
    td_chk <- td |> dplyr::group_by(meta.release_tag, meta.price_basis, meta.name, meta.disaggregation_group) |>
      dplyr::summarise(cont = is_continuous(time), .groups = "drop")
    td_chk <- td_chk |> dplyr::filter(!cont)
    if(NROW(td_chk)>0){
      td <- td |>
        dplyr::anti_join(
          td_chk,
          by = c("meta.release_tag","meta.price_basis","meta.name","meta.disaggregation_group"))
      message(sprintf(
        "Momentum calculation requires continuous periods. Dropped %d non-continuous series.",
        NROW(td_chk)
      ))
      tdl$data <- td
    }
  }



  tdf_long_check_structure(tdl)

  td <- tdl$data

  if (dplyr::n_distinct(td$meta.release_tag) > 1L) {

    message(
      "Multiple `meta.release_tag` values were found. ",
      "This splicing method currently supports only the `#main` release; ",
      "other releases will be excluded."
    )

    td <- td |>
      dplyr::filter(meta.release_tag == "#main")

    if (NROW(td) == 0L) {
      stop(
        "No `#main` release was found in `td`. ",
        "The splicing operation cannot proceed.",
        call. = FALSE
      )
    }
  }

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

  target_price_basis <- splice_helper_match_price_basis(
    target_price_basis, unique(pbs$meta.price_basis))

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


  if(growth_anchor == "growth_rate") {
    series_to_conv <- series_to_conv |> compute_metric(growth_rate = TRUE)
    series_in_tar <- series_in_tar |> compute_metric(growth_rate = TRUE)

    series_in_tar_no_gr <- series_in_tar |> dplyr::filter(!is.na(value.growth_rate))
  } else {
    series_to_conv <- series_to_conv |> compute_metric(momentum = TRUE)
    series_in_tar <- series_in_tar |> compute_metric(momentum = TRUE)

    series_in_tar_no_gr <- series_in_tar |> dplyr::filter(!is.na(value.momentum))
  }



  series_to_conv_in_old_only <- series_to_conv |>
    dplyr::anti_join(series_in_tar_no_gr, by = c("time", "meta.release_tag", "meta.name", "meta.disaggregation_group"))

  series_to_conv_in_old_only$value.level <- NA_real_

  hm_diff <- series_to_conv_in_old_only |>
    dplyr::distinct(meta.name, meta.disaggregation_group) |>
    dplyr::anti_join(
      series_in_tar_no_gr |>
        dplyr::distinct(meta.name, meta.disaggregation_group),
      by = c("meta.name","meta.disaggregation_group")
    )

  if(NROW(hm_diff)>0){
    message(
      "Dropping unmatched series from the following disaggregation group(s): ",
      paste(unique(hm_diff$meta.disaggregation_group), collapse = ", ")
    )

    series_to_conv_in_old_only <- series_to_conv_in_old_only |>
      dplyr::anti_join(
        hm_diff,
        by = c("meta.name","meta.disaggregation_group")
      )

  }

  # Growth rate preservation method.
  series_in_tar_ext <- rows_append_distinct(
    series_in_tar_no_gr, series_to_conv_in_old_only,
    primary_key = c("time", "meta.release_tag", "meta.name", "meta.disaggregation_group")
  )

  # Get level from Growth rate
  series_converted <- suppressMessages(
    convert_to_level(series_in_tar_ext)
  )
  series_converted$meta.price_basis <- target_price_basis

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

splice_series_core_for_two_pbase <- function(
    tdl,
    target_price_basis = NULL,
    splice_method = c("linking_factor","growth_preserving"),
    mean_type = c("auto", "GM", "AM"),
    growth_anchor = c("growth_rate","momentum"), return_diagnostics = FALSE, ...) {

  splice_method <- match.arg(splice_method)

  if(splice_method == "linking_factor"){

    splice_series_core_for_two_pbase_linking_factor(
      tdl = tdl, target_price_basis = target_price_basis,
      mean_type = mean_type, return_diagnostics = return_diagnostics, ...)

  } else {

    splice_series_core_for_two_pbase_gr_preserve(
      tdl = tdl, target_price_basis = target_price_basis,
      growth_anchor = growth_anchor, ...)

  }

}


splice_series_for_multi_pbase <- function(
    tdl,
    target_price_basis = NULL,
    splice_method = c("linking_factor","growth_preserving"),
    mean_type = c("auto", "GM", "AM"),
    growth_anchor = c("growth_rate","momentum"), return_diagnostics = FALSE, ...){

  step1 <- splice_series_for_multi_pbase_part(
    tdl = tdl,
    splice_method = splice_method,
    growth_anchor = growth_anchor,
    mean_type = mean_type, ...)

  if(is.null(target_price_basis)){
    return(step1)
  }

  if(target_price_basis == step1$data$meta.price_basis[1]){
    return(step1)
  }

  # Proceed further for target_price_basis no NULL case and  not recent one

  target_price_basis <- splice_helper_match_price_basis(
    target_price_basis, unique(tdl$data$meta.price_basis))

  new_tdl <- tdl

  new_tdl$data <- dplyr::bind_rows(
    new_tdl$data |> dplyr::filter(meta.price_basis == target_price_basis),
    step1$data
  )

  step2 <- splice_series_core_for_two_pbase(
    new_tdl,
    target_price_basis = target_price_basis,
    splice_method = splice_method,
    growth_anchor = growth_anchor,
    mean_type = mean_type, ...)

  step2

}

splice_series_for_multi_pbase_part <- function(
    tdl,
    # target_price_basis has to be max pbase (NULL)
    splice_method = c("linking_factor","growth_preserving"),
    mean_type = c("auto", "GM", "AM"),
    growth_anchor = c("growth_rate","momentum"), ...) {

  pbs <- unique(tdl$data$meta.price_basis) |> sort()
  pbs_d <- dplyr::tibble(from = pbs[-length(pbs)], to = pbs[-1])

  tdld_now <- tdl$data |> dplyr::filter(FALSE)

  for(i in 1:NROW(pbs_d)){
    target <- pbs_d[i,]

    tdld_this <- dplyr::bind_rows(tdl$data, tdld_now) |>
      dplyr::filter(meta.price_basis %in% c(target$to, target$from)) |>
      dplyr::distinct()

    tdld_this_spl <- splice_series_core_for_two_pbase(
      tdf_long_make(tdld_this, tdl$hmap, minimal = TRUE),
      target_price_basis = target$to,
      splice_method = splice_method,
      growth_anchor = growth_anchor,
      mean_type = mean_type,
      ...
    )

    # Check identical(unique(tdld_this_spl$data$time) |> sort(), unique(tdld_this$time) |> sort())

    tdld_now <- tdld_this_spl$data[colnames(tdld_now)]
  }

  tdl_out <- tdl

  tdl_out$data <- tdld_now

  tdf_long_check_structure(tdl_out)

  tdl_out
}

