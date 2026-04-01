test_that("gva numbers match", {

  d1 <- readRDS(test_path("testdata", "base_rev_gva.rds"))
  d2 <- readRDS(test_path("testdata", "qtr_base_rev_gva.rds"))

  tla <- d1 %>% es_convert_gva()
  tlq <- d2 %>% es_convert_gva()

  # new hmap for additional mapping
  new_hmap <- tibble::tibble(
    sub_industry = c("Crops", "Livestock", "Forestry and Logging",
                     "Fishing and Aquaculture"),
    agri_regroup_1 = c("Crops", "Allied Activities",
                       "Allied Activities", "Allied Activities")
  )

  tla2 <- hmap_add(tla, new_hmap)

  expect_identical(tla2$data, tla$data)

  tla2 <- tla2 |> aggregate_component()

  tla2_sub <- attach_parent(tla2, "agri_flag_type")

  tla2_stats <- compute_standard_metrics(tla2_sub)

  mpr_2025_check <- tla2_stats$main$data %>%
    filter(
      meta.release_tag=="#main",
      str_detect(meta.price_basis, "real base_2011"),
      meta.disaggregation_group == "agri_regroup_1",
      meta.name!="#NR") %>%
    select(time, meta.name, value.contribution_growth_rate) %>%
    filter(!is.na(value.contribution_growth_rate)) %>%
    mutate(value.contribution_growth_rate =
             round(value.contribution_growth_rate, 1)) %>%
    tidyr::pivot_wider(
      id_cols = time,
      names_from = meta.name,
      values_from = value.contribution_growth_rate) %>%
    dplyr::arrange(as.Date(time))


  expect_equal(
    sum(mpr_2025_check$`Allied Activities`), 31.5
  )

  expect_equal(sum(abs(mpr_2025_check$Crops)), 26.6)


  tla2_sub$data <- tla2_sub$data %>%
    filter(str_detect(meta.price_basis, "real"))

  tla2_sub <- splice_series(tla2_sub)

  # After Splice check
  tla2_sub <- attach_parent(tla2_sub, "agri_flag_type")

  tla2_stats <- compute_standard_metrics(tla2_sub)

  mpr_2026_check <- tla2_stats$main$data %>%
    filter(
      meta.release_tag=="#main",
      meta.disaggregation_group == "agri_regroup_1",
      meta.name!="#NR") %>%
    select(time, meta.name, value.contribution_growth_rate) %>%
    filter(!is.na(value.contribution_growth_rate)) %>%
    mutate(value.contribution_growth_rate =
             round(value.contribution_growth_rate, 1)) %>%
    tidyr::pivot_wider(
      id_cols = time,
      names_from = meta.name,
      values_from = value.contribution_growth_rate) %>%
    dplyr::arrange(as.Date(time))

  expect_equal(
    mpr_2026_check %>% filter(time == "2023-24") %>% pull(Crops),
    1.6
  )

  expect_equal(
    mpr_2026_check %>% filter(time == "2023-24") %>% pull(`Allied Activities`),
    1.1
  )

  expect_equal(
    mpr_2026_check %>% filter(time == "2024-25") %>% pull(Crops),
    2.0
  )

  expect_equal(
    mpr_2026_check %>% filter(time == "2024-25") %>% pull(`Allied Activities`),
    2.2
  )

  ltdl0 <-  tdf_long_temporal_link(tlq, tla)
  ltdl1 <-  tdf_long_temporal_link(tla, tlq)

  expect_equal(ltdl0, ltdl1)

  lt_qa <- linked_tdf_long_implied_figures(ltdl0)

  tdl0 <- lt_qa$high_freq %>% aggregate_component()

  tdl_a_all <- lt_qa$low_freq %>% aggregate_component()

  new_linked <- tdf_long_temporal_link(tdl_a_all, tdl0)


  tally_check <- linked_tdf_long_tally(new_linked)

  expect_message(
    linked_tdf_long_implied_figures(new_linked),
    "No implied calculation possible as there are no low frequency"
  )

  expect_lt(
    mean(tally_check$data$value.divergence),
    0.05
  )


  # This should be "2025-26"
  yr1 <- new_linked$high_freq$data %>% filter(!str_detect(meta.price_basis, "2022-23")) %>% pull(time) %>% max() %>% fiscal_year()

  # This should be "2024-25"
  yr2 <-tally_check$data %>% filter(!str_detect(meta.price_basis, "2022-23")) %>% pull(time) %>% max()

  expect_equal(
    yr1-1,
    yr2
  )

  expect_true( yr2 == "2024-25")

  chk_val <- abs(tally_check$data %>% filter(!str_detect(meta.price_basis, "2022-23")) %>% pull(value.divergence) %>% mean())+
    abs(tally_check$data %>% filter(str_detect(meta.price_basis, "2022-23")) %>% pull(value.divergence) %>% mean())

  expect_lt(
    chk_val,
    0.0001
  )

  expect_lt(
    tally_check$data %>% filter(str_detect(meta.price_basis, "2022-23")) %>% pull(value.divergence) %>% mean(),
    0.0001
  )

  expect_message(
    stdm_a <- compute_standard_metrics(tdl_a_all),
    "meta.parent column not found in data"
  )

  expect_message(
    stdm <- compute_standard_metrics(tdl0),
    "meta.parent column not found in data"
  )

  da <- stdm_a$main$data

  d0 <- stdm$main$data


  d <- d0 %>% filter(
    meta.release_tag == "#main",
    stringr::str_detect(meta.price_basis,"real"),
    stringr::str_detect(meta.price_basis,"2022-23"))


  # base momentum check
  dlt <- d0$value.delta_ln_growth_rate - (d0$value.ln_momentum + d0$value.ln_base_effect)

  expect_equal(
    # Upto 10 precision, as there can be some minor differences in the way ln growth rate and momentum are calculated
    round(abs(dlt), 10) %>% max(na.rm = TRUE),
    0
  )

  dlt <- da$value.delta_ln_growth_rate - (da$value.ln_momentum + da$value.ln_base_effect)

  expect_equal(
    # Upto 10 precision, as there can be some minor differences in the way ln growth rate and momentum are calculated
    round(abs(dlt), 10) %>% max(na.rm = TRUE),
    0
  )

  # Random value check
  expect_gt(
    d %>% filter(meta.name == "Industry") %>% summarise(mean(value.share_pct)) %>% pull,
    19.5
  )

  expect_gt(
    d %>% filter(meta.name == "Services") %>% summarise(mean(value.share_pct)) %>% pull,
    59.0
  )

  expect_equal(
    d %>% filter(meta.name == "Industry", time == "Q3:2024-25") %>% pull(value.share_pct) %>% round(1),
    19.3
  )

  expect_equal(
    d %>% filter(meta.name == "Agriculture and Allied", time == "Q4:2025-26") %>% pull(value.growth_rate) %>% round(1),
    2.1
  )

  expect_equal(
    d %>% filter(meta.name == "Gross Value Added", time == "Q4:2025-26") %>% pull(value.growth_rate) %>% round(1),
    7.4
  )

  expect_equal(
    d %>% filter(meta.name == "Gross Value Added") %>% pull(value.share_pct) %>% mean(na.rm = TRUE),
    100
  )

  expect_equal(
    d %>% filter(meta.name == "Gross Value Added") %>% pull(value.contribution_growth_rate_pct) %>% mean(na.rm = TRUE),
    100
  )

  expect_equal(
    da %>% filter(meta.name == "Gross Value Added") %>% pull(value.contribution_growth_rate_pct) %>% mean(na.rm = TRUE),
    100
  )

  expect_equal(
    d %>% filter(meta.name == "Services", time == "Q2:2025-26") %>% pull(value.contribution_growth_rate) %>% round(1),
    5.9
  )

  expect_equal(
    stdm$deflator %>% filter(time=="Q4:2025-26", meta.release_tag=="#main", meta.name=="Gross Value Added") %>% pull(value.deflator_inflation) %>% round(2),
    1.38
  )

  expect_equal(
    d %>% filter(meta.name == "Gross Value Added") %>% pull(value.growth_rate) %>% round(1),
    d %>% filter(meta.name == "Gross Value Added") %>% pull(value.contribution_growth_rate) %>% round(1)
  )

  expect_equal(
    stdm$deflator %>% filter(
      time=="Q3:2023-24", meta.release_tag=="#main",
      meta.name=="Gross Value Added",
      stringr::str_detect(meta.price_basis,"2022-23")) %>% pull(value.deflator_inflation) %>% round(2),
    3.18
  )

  expect_equal(
    stdm_a$deflator %>%
      filter(
        meta.release_tag=="#main",
        meta.name=="Gross Value Added",
        stringr::str_detect(meta.price_basis,"2022-23")) %>%
      filter(!is.na(value.deflator_inflation)) %>%
      pull(value.deflator_inflation) %>%
      sort() %>%
      round(2),
    c(0.92, 2.2, 3.28)
  )

})
