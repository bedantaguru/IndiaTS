test_that("IIP works", {
  iip_raw <- readRDS(test_path("testdata", "iip.rds"))

  iip <- iip_raw |> as_tdf()

  iips <- iip |> auto_seas()

  iipsa1 <- iip |> compute_metrics(SAAR = TRUE)

  iipsa2 <- iips |> as_tdf_long()

  iipsa2 <- iipsa2 |> compute_metrics(momentum = TRUE)

})
