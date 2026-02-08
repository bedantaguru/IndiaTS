
## ------------------------------------------------------------------
## 1. Deterministic calendar → financial → calendar round-trip
## ------------------------------------------------------------------

test_that("calendar quarter round-trips through financial quarter", {

  years <- 2010:2030
  qtrs  <- paste0("Q", 1:4)

  x <- as.vector(outer(qtrs, years, paste, sep = ":"))
  x <- structure(x, class = calendar_period_class)

  cal  <- calendar_quarter(x)
  fin  <- financial_quarter(cal)
  back <- calendar_quarter(fin)

  expect_equal(back, cal)
})


## ------------------------------------------------------------------
## 2. Loose text input tolerance (user-facing behaviour)
## ------------------------------------------------------------------

test_that("loose calendar quarter text round-trips correctly", {

  x <- c("q1 2015", "q2 2015", "q3 2015", "q4 2015")

  cal  <- calendar_quarter(x)
  fin  <- financial_quarter(cal)
  back <- calendar_quarter(fin)

  expect_equal(back, cal)
})


## ------------------------------------------------------------------
## 3. Financial-quarter stress test across wide year range
##    (1800–2150, randomised)
## ------------------------------------------------------------------

test_that("financial quarters round-trip via calendar across wide year range", {

  set.seed(123)

  n <- 300
  years <- sample(1800:2150, n, replace = TRUE)
  qtrs  <- paste0("Q", sample(1:4, n, replace = TRUE))

  fy_start <- years
  fy_end   <- sprintf("%02d", (years + 1) %% 100)

  fin <- paste0(qtrs, ":", fy_start, "-", fy_end)
  fin <- structure(fin, class = financial_period_class)

  cal  <- calendar_quarter(fin)
  back <- financial_quarter(cal)

  expect_equal(back, fin)
})


## ------------------------------------------------------------------
## 4. Anchor invariance
##    (quarter identity must not depend on anchor)
## ------------------------------------------------------------------

test_that("anchor choice does not change quarter identity", {

  x <- structure(
    paste0("Q", 1:4, ":2019-20"),
    class = financial_period_class
  )

  d_first <- financial_quarter_to_date(x, anchor = "first")
  d_mid   <- financial_quarter_to_date(x, anchor = "mid")
  d_last  <- financial_quarter_to_date(x, anchor = "last")

  expect_equal(
    calendar_quarter(d_first),
    calendar_quarter(d_mid)
  )

  expect_equal(
    calendar_quarter(d_mid),
    calendar_quarter(d_last)
  )
})


## ------------------------------------------------------------------
## 5. Regression guard: FY Q3 and Q4 year-boundary correctness
## ------------------------------------------------------------------

test_that("FY Q3 and Q4 do not leak into adjacent years", {

  x <- structure(
    c("Q3:2014-15", "Q4:2014-15"),
    class = financial_period_class
  )

  d_first <- financial_quarter_to_date(x, anchor = "first")
  d_last  <- financial_quarter_to_date(x, anchor = "last")

  expect_equal(d_first[1], as.Date("2014-10-01"))
  expect_equal(d_last[1],  as.Date("2014-12-31"))

  expect_equal(d_first[2], as.Date("2015-01-01"))
  expect_equal(d_last[2],  as.Date("2015-03-31"))
})


## ------------------------------------------------------------------
## 6. Calendar ↔ financial ↔ calendar idempotence on Date inputs
## ------------------------------------------------------------------

test_that("date-based quarter coercions are idempotent", {

  dates <- seq(
    as.Date("1900-01-01"),
    as.Date("2100-12-31"),
    by = "37 days"
  )

  cal  <- calendar_quarter(dates)
  fin  <- financial_quarter(cal)
  back <- calendar_quarter(fin)

  expect_equal(back, cal)
})
