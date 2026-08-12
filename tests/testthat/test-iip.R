test_that("IIP works", {
  iip_raw <- readRDS(test_path("testdata", "iip.rds"))

  iip <- iip_raw |> as_tdf()

  iips <- iip |> auto_seas()

  iipsa1 <- iip |> compute_metrics(SAAR = TRUE)

  iipsa2 <- iips |> as_tdf_long()

  iipsa2 <- iipsa2 |> compute_metrics(momentum = TRUE)

  iipsa2 <- iipsa2 |>
    compute_metrics(
      .custom_function = \(.data) ((.data$value.momentum/100+1)^12-1)*100,
      .options = list(.custom_function="data")) |>
    compute_metrics(
      .custom_function = list(saar_calc = \(.data) ((.data$value.momentum/100+1)^12-1)*100),
      .options = list(.custom_function="data")) |>
    # In this case it will be same only
    dplyr::mutate(value.saar_calc_direct = ((.data$value.momentum/100+1)^12-1)*100)

  iipsa1.1 <- iipsa1 |> as_tdf_long()

  iipsa_chk <- iipsa1.1 |>
    dplyr::full_join(iipsa2, suffix = c("_1","_2"), by = c("time","meta.name"))

  expect_true(NROW(iipsa_chk) <= max(NROW(iipsa1.1), NROW(iipsa2)))

  expect_true(max(abs(iipsa_chk$value.level_1 - iipsa_chk$value.saar_calc), na.rm = TRUE) < 0.01)

  expect_equal(iipsa_chk$value.custom_metric, iipsa_chk$value.saar_calc)

  expect_equal(iipsa_chk$value.custom_metric, iipsa_chk$value.saar_calc_direct)

  expect_equal(iipsa_chk$value.custom_metric, iipsa_chk$value.level_1)

})
