test_that("NAS long series works", {

  nas_full <- readRDS(test_path("testdata", "gdp_gva_long.rds"))

  nas_full_tdl <- nas_full |> purrr::map_depth(2, es_convert_composite)

  implied_fig <- function(nd){

    nd$annual$data <- nd$annual$data %>% filter(str_detect(meta.price_basis, "real"))
    nd$quarterly$data <- nd$quarterly$data %>% filter(str_detect(meta.price_basis, "real"))

    ltdl <-  tdf_long_temporal_link(nd$annual, nd$quarterly)

    lt_qa <- linked_tdf_long_implied_figures(ltdl)

    ol <- list()

    ol$quarterly <- lt_qa$high_freq %>% aggregate_component()

    ol$annual <- tla <- lt_qa$low_freq %>% aggregate_component()

    ol
  }

  nf <- nas_full_tdl |> purrr::map_depth(1, implied_fig)

  sp_gva <- splice_series(nf$gva$annual)
  sp_gdp <- splice_series(nf$gdp$annual)

  chk11 <- sp_gdp$data |> filter(meta.release_tag == "#main", meta.disaggregation_group == "indicator") |>
    compute_metrics(growth_rate = TRUE) |> select(time, spl_lvl = value.level , spl_gr = value.growth_rate)
  chk12 <- nf$gdp$annual$data |> filter(meta.release_tag == "#main", meta.disaggregation_group == "indicator") |>
    compute_metrics(growth_rate = TRUE) |> group_by(time) |> filter(meta.price_basis == max(meta.price_basis)) |>
    ungroup() |> select(time, lvl = value.level, gr = value.growth_rate)
  chk13 <- chk11 |>  dplyr::left_join(chk12, by = "time")

  expect_equal(round(mean(abs(chk13$spl_gr-chk13$gr), na.rm = TRUE), 2), 0)
  expect_gt(round(mean(abs(chk13$spl_lvl-chk13$lvl), na.rm = TRUE), 2), 10000)


  chk21 <- sp_gva$data |> filter(meta.release_tag == "#main", meta.disaggregation_group == "indicator") |>
    compute_metrics(growth_rate = TRUE) |> select(time, lvl1 = value.level , gr1 = value.growth_rate)
  chk22 <- sp_gva
  chk22$data <- chk22$data |> filter(meta.release_tag == "#main", meta.disaggregation_group != "indicator")
  chk22 <- aggregate_component(chk22)
  chk23 <- chk22$data |> filter(meta.release_tag == "#main", meta.disaggregation_group == "indicator") |>
    compute_metrics(growth_rate = TRUE) |> select(time, lvl2 = value.level , gr2 = value.growth_rate)
  chk24 <- chk21 |> dplyr::left_join(chk23, by = "time")

  expect_gt(round(mean(abs(chk24$gr1-chk24$gr2), na.rm = TRUE), 2), 0)
  expect_lt(round(mean(abs(chk24$gr1-chk24$gr2), na.rm = TRUE), 2), 1)

  # Quarterly series GVA

  chk31 <- nf$gva$quarterly$data |>
    filter(meta.release_tag=="#main", meta.disaggregation_group == "indicator") |>
    group_by(meta.low_freq_time, meta.price_basis) |>
    mutate(n_qtrs = n_distinct(fiscal_quarter(time, with_year = FALSE))) |>
    filter(n_qtrs==4) |>
    group_by(meta.low_freq_time) |>
    filter(meta.price_basis == max(meta.price_basis)) |>
    dplyr::arrange(as.Date(time)) |> ungroup() |> select(-n_qtrs) |>
    compute_metrics(annual_contribution = TRUE)

  chk32 <- chk31 |>
    select(time, meta.price_basis, value.level, value.annual_contribution, year = meta.low_freq_time) |>
    left_join(chk21 |> select(year = time, value.level_yr = lvl1), by = c("year"))

  chk32 <- chk32 |> mutate(value.splice_level = value.level_yr*value.annual_contribution/100)

  chk33 <- chk32 |> mutate(meta.price_basis = sp_gva$data$meta.price_basis[1]) |>
    select(time, value.level = value.splice_level, meta.price_basis) |>
    dplyr::arrange(as.Date(time)) |> compute_metrics(growth_rate = T)

  chk34 <- chk31 |> select(time, value.level, pbase = meta.price_basis) |> compute_metrics(growth_rate = TRUE)

  chk35 <- chk33 |> dplyr::left_join(chk34, by = "time", suffix = c("","_asis")) |>
    mutate(dlt = abs(value.growth_rate - value.growth_rate_asis) |> round(0)) |>
    mutate(pyr = pbase |> fiscal_year(), yr = time |> fiscal_year())

  chk35_bad <- chk35 |> filter(pyr==yr)
  chk35_ok <- chk35 |> filter(pyr!=yr)

  expect_gt(mean(chk35_bad$dlt), 15)

  expect_lt(mean(chk35_ok$dlt, na.rm = TRUE), 1)

  # Quarterly series GDP
  chk41 <- nf$gdp$quarterly$data |>
    filter(meta.release_tag=="#main", meta.disaggregation_group == "indicator") |>
    group_by(meta.low_freq_time, meta.price_basis) |>
    mutate(n_qtrs = n_distinct(fiscal_quarter(time, with_year = FALSE))) |>
    filter(n_qtrs==4) |>
    group_by(meta.low_freq_time) |>
    filter(meta.price_basis == max(meta.price_basis)) |>
    dplyr::arrange(as.Date(time)) |> ungroup() |> select(-n_qtrs) |>
    compute_metrics(annual_contribution = TRUE)

  chk42 <- chk41 |>
    select(time, meta.price_basis, value.level, value.annual_contribution, year = meta.low_freq_time) |>
    left_join(chk11 |> select(year = time, value.level_yr = spl_lvl), by = c("year"))

  chk42 <- chk42 |> mutate(value.splice_level = value.level_yr*value.annual_contribution/100)

  chk43 <- chk42 |> mutate(meta.price_basis = sp_gva$data$meta.price_basis[1]) |>
    select(time, value.level = value.splice_level, meta.price_basis) |>
    dplyr::arrange(as.Date(time)) |> compute_metrics(growth_rate = T)

  chk44 <- chk41 |> select(time, value.level, pbase = meta.price_basis) |> compute_metrics(growth_rate = TRUE)

  chk45 <- chk43 |> dplyr::left_join(chk44, by = "time", suffix = c("","_asis")) |>
    mutate(dlt = abs(value.growth_rate - value.growth_rate_asis) |> round(0)) |>
    mutate(pyr = pbase |> fiscal_year(), yr = time |> fiscal_year())

  chk45_bad <- chk45 |> filter(pyr==yr)
  chk45_ok <- chk45 |> filter(pyr!=yr)


  expect_gt(mean(chk45_bad$dlt), 20)
  expect_lt(mean(chk45_ok$dlt, na.rm = TRUE), 1)

})


test_that("NAS long series works on public functions", {

  nas_full <- readRDS(test_path("testdata", "gdp_gva_long.rds"))

  n0 <- nas_full |> purrr::map_depth(2, es_convert)

  implied_vals <- function(nd){

    nd <- nd |> purrr::map(~.x |> filter(str_detect(meta.price_basis, "real")))

    tlink <-  temporal_link(nd$annual, nd$quarterly)

    qa <- compute_implied(tlink)

    ol <- list()

    ol$quarterly <- qa$high_freq |> aggregate(type = "component")
    ol$annual <- tla <- qa$low_freq |> aggregate(type = "component")

    ol
  }

  nf <- n0 |> purrr::map(implied_vals)

  sp_gva <- splice_series(nf$gva$annual)
  sp_gdp <- splice_series(nf$gdp$annual)

  # Quarterly series GVA

  dchk1 <- nf$gva$quarterly |>
    filter(meta.release_tag=="#main", meta.disaggregation_group == "indicator") |>
    group_by(meta.low_freq_time, meta.price_basis) |>
    mutate(n_qtrs = n_distinct(fiscal_quarter(time, with_year = FALSE))) |>
    filter(n_qtrs==4) |>
    group_by(meta.low_freq_time) |>
    filter(meta.price_basis == max(meta.price_basis)) |>
    dplyr::arrange(as.Date(time)) |> ungroup() |> select(-n_qtrs) |>
    compute_metrics(annual_contribution = TRUE)

  dchk2 <- dchk1 |>
    select(time, meta.price_basis, value.level, value.annual_contribution, year = meta.low_freq_time) |>
    left_join(
      sp_gva |>
        filter(meta.release_tag=="#main", meta.disaggregation_group == "indicator") |>
        select(year = time, value.level_yr = value.level), by = c("year"))

  dchk2 <- dchk2 |> mutate(value.splice_level = value.level_yr*value.annual_contribution/100)

  dchk3 <- dchk2 |> mutate(meta.price_basis = sp_gva$meta.price_basis[1]) |>
    select(time, value.level = value.splice_level, meta.price_basis) |>
    dplyr::arrange(as.Date(time)) |> compute_metrics(growth_rate = T)

  dchk4 <- dchk1 |> select(time, value.level, pbase = meta.price_basis) |> compute_metrics(growth_rate = TRUE)

  dchk5 <- dchk3 |> dplyr::left_join(dchk4, by = "time", suffix = c("","_asis")) |>
    mutate(dlt = abs(value.growth_rate - value.growth_rate_asis) |> round(0)) |>
    mutate(pyr = pbase |> fiscal_year(), yr = time |> fiscal_year())

  dchk5_bad <- dchk5 |> filter(pyr==yr)
  dchk5_ok <- dchk5 |> filter(pyr!=yr)

  expect_gt(mean(dchk5_bad$dlt), 15)

  expect_lt(mean(dchk5_ok$dlt, na.rm = TRUE), 1)


})
