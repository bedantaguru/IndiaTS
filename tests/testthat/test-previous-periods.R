
test_that("previous_period works for monthly fiscal periods", {

  x <- as_fiscal_period(c("jan 14", "feb 14", "march 14"))

  result <- previous_period(x)

  expect_equal(
    unclass(result),
    c("Dec:2013", "Jan:2014", "Feb:2014")
  )
})



test_that("previous_period and previous_year work for mixed fiscal frequencies", {

  raw <- c(
    "Q1 FY26",      # fiscal quarter
    "Q2 2022-23",   # fiscal quarter
    "Jan 2014",     # fiscal month
    "Jul 1988"      # fiscal month
  )

  x <- as_fiscal_period(raw)

  expect_s3_class(x, fiscal_period_class)
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



test_that("all supported fiscal year string formats convert correctly", {

  raw <- c(
    "jan 14",
    "Jan 2014",
    "JAN-2014",
    "January 2014",
    "jan:2014",
    "Jan-14"
  )

  x <- as_fiscal_period(raw)

  ## sanity
  expect_s3_class(x, fiscal_period_class)
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



test_that("all supported fiscal quarter formats convert correctly", {

  raw <- c(
    "Q1 FY26",
    "Q1:2025-26",
    "Q1 2025-26",
    "Q1-2025-26",
    "Q1:FY26"
  )

  x <- as_fiscal_period(raw)

  expect_s3_class(x, fiscal_period_class)

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


test_that("fiscal year parsing works for FYxx, FYxxxx and YYYY-YY formats", {

  raw <- c(
    "FY14",
    "FY 14",
    "FY86",
    "FY 86",
    "FY2026",
    "FY 2088",
    "2013-14",
    "1986-87"
  )

  x <- as_fiscal_period(raw)

  ## sanity
  expect_s3_class(x, fiscal_period_class)
  expect_length(x, length(raw))

  ## previous fiscal year
  result <- previous_period(x)

  expect_equal(
    unclass(result),
    c(
      "2012-13",  # FY14
      "2012-13",  # FY 14
      "1984-85",  # FY86
      "1984-85",  # FY 86
      "2024-25",  # FY2026
      "2086-87",  # FY 2088
      "2012-13",  # 2013-14
      "1985-86"   # 1986-87
    )
  )
})



test_that("previous_period and previous_year work for calendar quarters", {

  raw <- c(
    "Q1 2014",
    "Q3 2014",
    "Q4 2000"
  )

  x <- calendar_quarter(raw)

  ## sanity
  expect_s3_class(x, calendar_period_class)
  expect_length(x, length(raw))

  ## previous adjacent quarter
  prev_p <- previous_period(x)

  expect_equal(
    unclass(prev_p),
    c(
      "Q4:2013",  # Q1 -> previous year Q4
      "Q2:2014",  # Q3 -> Q2 same year
      "Q3:2000"   # Q4 -> Q3 same year
    )
  )

  ## same quarter, previous year
  prev_y <- previous_year(x)

  expect_equal(
    unclass(prev_y),
    c(
      "Q1:2013",  # same quarter, year - 1
      "Q3:2013",
      "Q4:1999"
    )
  )
})

