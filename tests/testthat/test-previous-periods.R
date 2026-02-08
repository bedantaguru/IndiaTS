
test_that("previous_period works for monthly financial periods", {

  x <- as_financial_period(c("jan 14", "feb 14", "march 14"))

  result <- previous_period(x)

  expect_equal(
    unclass(result),
    c("Dec:2013", "Jan:2014", "Feb:2014")
  )
})



test_that("previous_period and previous_year work for mixed financial frequencies", {

  raw <- c(
    "Q1 FY26",      # financial quarter
    "Q2 2022-23",   # financial quarter
    "Jan 2014",     # financial month
    "Jul 1988"      # financial month
  )

  x <- as_financial_period(raw)

  expect_s3_class(x, financial_period_class)
  expect_length(x, length(raw))

  ## previous period (adjacent)
  prev_p <- previous_period(x)

  expect_equal(
    unclass(prev_p),
    c(
      "Q4:2024-25",  # Q1 FY26 -> previous FY Q4
      "Q1:2022-23",  # Q2 -> Q1
      "Dec:2013",    # Jan -> Dec
      "Jun:1988"     # Jul -> Jun
    )
  )

  ## previous year (YoY, same frequency)
  prev_y <- previous_year(x)

  expect_equal(
    unclass(prev_y),
    c(
      "Q1:2024-25",  # same quarter, previous FY
      "Q2:2021-22",  # same quarter, previous FY
      "Jan:2013",    # same month, previous year
      "Jul:1987"     # same month, previous year
    )
  )

  ## class preservation
  expect_identical(class(prev_p), class(x))
  expect_identical(class(prev_y), class(x))
})



test_that("all supported financial year string formats convert correctly", {

  raw <- c(
    "jan 14",
    "Jan 2014",
    "JAN-2014",
    "January 2014",
    "jan:2014",
    "Jan-14"
  )

  x <- as_financial_period(raw)

  ## sanity
  expect_s3_class(x, financial_period_class)
  expect_length(x, length(raw))

  ## previous period must work uniformly
  result <- previous_period(x)

  expect_equal(
    unclass(result),
    c(
      "Dec:2013",
      "Dec:2013",
      "Dec:2013",
      "Dec:2013",
      "Dec:2013",
      "Dec:2013"
    )
  )
})



test_that("all supported financial quarter formats convert correctly", {

  raw <- c(
    "Q1 FY26",
    "Q1:2025-26",
    "Q1 2025-26",
    "Q1-2025-26",
    "Q1:FY26"
  )

  x <- as_financial_period(raw)

  expect_s3_class(x, financial_period_class)

  result <- previous_period(x)

  expect_equal(
    unclass(result),
    c(
      "Q4:2024-25",
      "Q4:2024-25",
      "Q4:2024-25",
      "Q4:2024-25",
      "Q4:2024-25"
    )
  )
})

