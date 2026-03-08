test_that("ts -> tdf -> ts round-trip preserves structure and values", {

  make_ts <- function(freq, start, data, colnames) {
    x <- ts(
      data,
      start = start,
      frequency = freq
    )
    colnames(x) <- colnames
    x
  }

  ts_list <- list(

    monthly = make_ts(
      freq = 12,
      start = c(2015, 1),
      data = cbind(
        var1  = seq_len(36),
        var_2 = seq_len(36) * 2,
        var_3 = seq_len(36) * 0.5
      ),
      colnames = c("var1", "var_2", "var_3")
    ),

    quarterly = make_ts(
      freq = 4,
      start = c(2016, 1),
      data = cbind(
        sales  = seq_len(16),
        profit = seq_len(16) + 100
      ),
      colnames = c("sales", "profit")
    ),

    halfyearly = make_ts(
      freq = 2,
      start = c(2014, 1),
      data = cbind(
        index_a = seq_len(10) * 10,
        index_b = seq_len(10) * 20
      ),
      colnames = c("index_a", "index_b")
    ),

    yearly = make_ts(
      freq = 1,
      start = 2010,
      data = cbind(
        gdp        = seq_len(8) * 100,
        inflation = seq_len(8)
      ),
      colnames = c("gdp", "inflation")
    )
  )

  for (nm in names(ts_list)) {

    original_ts <- ts_list[[nm]]

    capture_output(tdf_obj   <- as_tdf(original_ts))
    rebuilt_ts <- stats::as.ts(tdf_obj)

    ## structural integrity
    expect_equal(frequency(rebuilt_ts), frequency(original_ts), info = nm)
    expect_equal(start(rebuilt_ts), start(original_ts), info = nm)
    expect_equal(end(rebuilt_ts), end(original_ts), info = nm)
    expect_equal(colnames(rebuilt_ts), colnames(original_ts), info = nm)

    ## value integrity (non-strict numeric comparison)
    expect_equal(
      as.matrix(rebuilt_ts),
      as.matrix(original_ts),
      tolerance = 1e-8,
      info = nm
    )
  }

  # Specific case

  nm <- "halfyearly"

  original_ts <- ts_list[[nm]]

  capture_output(tdf_obj   <- as_tdf(original_ts, fiscal_type = FALSE))
  rebuilt_ts <- to_ts(tdf_obj)

  ## structural integrity
  expect_equal(frequency(rebuilt_ts), frequency(original_ts), info = nm)
  expect_equal(start(rebuilt_ts), start(original_ts), info = nm)
  expect_equal(end(rebuilt_ts), end(original_ts), info = nm)
  expect_equal(colnames(rebuilt_ts), colnames(original_ts), info = nm)

  ## value integrity (non-strict numeric comparison)
  expect_equal(
    as.matrix(rebuilt_ts),
    as.matrix(original_ts),
    tolerance = 1e-8,
    info = nm
  )


})



test_that("yearly ts prints fiscal conversion text via cat()", {

  x <- ts(
    cbind(
      y1 = seq_len(6),
      y2 = seq_len(6) * 5
    ),
    start = 2018,
    frequency = 1
  )

  expect_output(
    {
      tdf <- as_tdf(x)
      to_ts(tdf)
    },
    regexp = "yearly dates to fiscal yearly periods"
  )
})

test_that("half-yearly ts prints fiscal conversion text via cat()", {

  x <- ts(
    cbind(
      h1 = seq_len(6),
      h2 = seq_len(6) * 10
    ),
    start = c(2020, 1),
    frequency = 2
  )

  expect_output(
    {
      tdf <- as_tdf(x)
      to_ts(tdf)
    },
    regexp = "half-yearly dates to fiscal half-yearly periods"
  )
})

