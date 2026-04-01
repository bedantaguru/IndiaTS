test_that("gva splicing test", {

  d1 <- readRDS(test_path("testdata", "base_rev_gva.rds"))
  d2 <- readRDS(test_path("testdata", "qtr_base_rev_gva.rds"))

  tla <- d1 %>% es_convert_gva()
  tlq <- d2 %>% es_convert_gva()

  ltdl <-  tdf_long_temporal_link(tlq, tla)

  lt_qa <- linked_tdf_long_implied_figures(ltdl)

  tlq <- lt_qa$high_freq %>% aggregate_component()

  tla <- lt_qa$low_freq %>% aggregate_component()

  rm(ltdl, d1, d2, lt_qa)

  tla_real <- tla
  tla_real$data <- tla_real$data %>% filter(str_detect(meta.price_basis, "real"))

  tlq_real <- tlq
  tlq_real$data <- tlq_real$data %>% filter(str_detect(meta.price_basis, "real"))

  sp1 <- splice_series(tlq_real)
  sp2 <- splice_series(
    tlq_real,
    target_price_basis = min(tlq_real$data$meta.price_basis))

  common_qtrs <- tlq_real$data %>% group_by(time) %>%
    summarise(n_pb = n_distinct(meta.price_basis)) %>% filter(n_pb>1) %>% pull(time)

  NC <- tlq_real$data %>% filter(time %in% common_qtrs) %>% NROW()

  expect_equal(
    NROW(tlq_real$data)-NC+NC/2,
    NROW(sp1$data)
  )

  expect_equal(
    NROW(sp2$data),
    NROW(sp1$data)
  )

  expect_equal(
    NROW(sp1$data),
    sum(sp1$data$meta.is_spliced) + sum(sp2$data$meta.is_spliced) + NC/2
  )

  # Intentionally made wrong connect where no splicing is done

  tlq_real_wrong_connect <- tlq_real$data %>%
    group_by(time, meta.release_tag, meta.name, meta.disaggregation_group) %>%
    filter(meta.price_basis == max(meta.price_basis)) %>% ungroup()

  tlq_real_wrong_connect$meta.price_basis <-"mixed"

  tlq_real_wrong_connect <- tdf_long_make(tlq_real_wrong_connect, tla_real$hmap, minimal = TRUE)

  msgs <- capture_messages(
    ms_wrong <- compute_standard_metrics(tlq_real_wrong_connect)
  )

  expect_true(
    any(str_detect(msgs, "Deflator calculation skipped"))
  )

  expect_true(
    any(str_detect(msgs, "meta.parent column not found"))
  )

  # it would be too high as series are coupled without splicing
  expect_gt(
    max(abs(ms_wrong$main$data$value.momentum), na.rm = TRUE),
    50
  )

  capture_messages(
    {
      ms_1 <- compute_standard_metrics(sp1)
      ms_2 <- compute_standard_metrics(sp2)
      ms_all <- compute_standard_metrics(tlq_real)
    }
  )


  # Helper
  get_growth_rate <- function(df) {
    df$main$data %>%
      filter(meta.release_tag == "#main", str_detect(meta.name, "Gross")) %>%
      pull(value.growth_rate)
  }

  # Expects
  gr_all   <- get_growth_rate(ms_all)
  gr_1     <- get_growth_rate(ms_1)
  gr_2     <- get_growth_rate(ms_2)
  gr_wrong <- get_growth_rate(ms_wrong)

  expect_equal(sum(is.na(gr_all)),  8L)
  expect_equal(sum(is.na(gr_1)),    4L)
  expect_equal(sum(is.na(gr_2)),    4L)
  expect_equal(sum(is.na(gr_all)),  sum(is.na(gr_1)) + sum(is.na(gr_2)))

  expect_equal(max(gr_1,   na.rm = TRUE), max(gr_all, na.rm = TRUE), tolerance = 1e-3)
  expect_equal(max(gr_2,   na.rm = TRUE), max(gr_all, na.rm = TRUE), tolerance = 1e-3)

  expect_gt(max(gr_wrong,  na.rm = TRUE), max(gr_all, na.rm = TRUE))  # outlier leaks through
  expect_gt(mean(gr_wrong, na.rm = TRUE), mean(gr_all, na.rm = TRUE)) # mean inflated

  # Check this
  # ms_all$main$data %>% filter(meta.release_tag=="#main", str_detect(meta.name, "Gross")) %>% pull(value.growth_rate) %>% summary()
  # ms_1$main$data %>% filter(meta.release_tag=="#main", str_detect(meta.name, "Gross")) %>% pull(value.growth_rate) %>% summary()
  # ms_2$main$data %>% filter(meta.release_tag=="#main", str_detect(meta.name, "Gross")) %>% pull(value.growth_rate) %>% summary()
  # ms_wrong$main$data %>% filter(meta.release_tag=="#main", str_detect(meta.name, "Gross")) %>% pull(value.growth_rate) %>% summary()


})
