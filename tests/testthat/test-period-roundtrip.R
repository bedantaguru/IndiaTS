
## ------------------------------------------------------------------
## 1. Deterministic calendar → fiscal → calendar round-trip
## ------------------------------------------------------------------

test_that("calendar quarter round-trips through fiscal quarter", {

  years <- 2010:2030
  qtrs  <- paste0("Q", 1:4)

  x <- as.vector(outer(qtrs, years, paste, sep = ":"))
  x <- structure(x, class = calendar_period_class)

  cal  <- calendar_quarter(x)
  fin  <- fiscal_quarter(cal)
  back <- calendar_quarter(fin)

  expect_equal(back, cal)
})


## ------------------------------------------------------------------
## 2. Loose text input tolerance (user-facing behaviour)
## ------------------------------------------------------------------

test_that("loose calendar quarter text round-trips correctly", {

  x <- c("q1 2015", "q2 2015", "q3 2015", "q4 2015")

  cal  <- calendar_quarter(x)
  fin  <- fiscal_quarter(cal)
  back <- calendar_quarter(fin)

  expect_equal(back, cal)
})


## ------------------------------------------------------------------
## 3. fiscal-quarter stress test across wide year range
##    (1800–2150, randomised)
## ------------------------------------------------------------------

test_that("fiscal quarters round-trip via calendar across wide year range", {

  set.seed(123)

  n <- 300
  years <- sample(1800:2150, n, replace = TRUE)
  qtrs  <- paste0("Q", sample(1:4, n, replace = TRUE))

  fy_start <- years
  fy_end   <- sprintf("%02d", (years + 1) %% 100)

  fin <- paste0(qtrs, ":", fy_start, "-", fy_end)
  fin <- structure(fin, class = fiscal_period_class)

  cal  <- calendar_quarter(fin)
  back <- fiscal_quarter(cal)

  expect_equal(back, fin)
})


## ------------------------------------------------------------------
## 4. Anchor invariance
##    (quarter identity must not depend on anchor)
## ------------------------------------------------------------------

test_that("anchor choice does not change quarter identity", {

  x <- structure(
    paste0("Q", 1:4, ":2019-20"),
    class = fiscal_period_class
  )

  d_first <- fiscal_quarter_to_date(x, anchor = "first")
  d_mid   <- fiscal_quarter_to_date(x, anchor = "mid")
  d_last  <- fiscal_quarter_to_date(x, anchor = "last")

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
    class = fiscal_period_class
  )

  d_first <- fiscal_quarter_to_date(x, anchor = "first")
  d_last  <- fiscal_quarter_to_date(x, anchor = "last")

  expect_equal(d_first[1], as.Date("2014-10-01"))
  expect_equal(d_last[1],  as.Date("2014-12-31"))

  expect_equal(d_first[2], as.Date("2015-01-01"))
  expect_equal(d_last[2],  as.Date("2015-03-31"))
})


## ------------------------------------------------------------------
## 6. Calendar ↔ fiscal ↔ calendar idempotence on Date inputs
## ------------------------------------------------------------------

test_that("date-based quarter coercions are idempotent", {

  dates <- seq(
    as.Date("1900-01-01"),
    as.Date("2100-12-31"),
    by = "37 days"
  )

  cal  <- calendar_quarter(dates)
  fin  <- fiscal_quarter(cal)
  back <- calendar_quarter(fin)

  expect_equal(back, cal)
})



test_that("calendar -> fiscal -> calendar roundtrip is stable for text input", {

  x <- c(
    "Jan 2023",
    "Feb 2024",
    "Q1 2023",
    "Q4 2022"
  )

  cal1 <- as_calendar_period(x, with_year = TRUE)
  fis  <- as_fiscal_period(cal1, with_year = TRUE)
  cal2 <- as_calendar_period(fis, with_year = TRUE)

  expect_s3_class(cal1, calendar_period_class)
  expect_s3_class(fis, fiscal_period_class)
  expect_s3_class(cal2, calendar_period_class)

  expect_identical(as.character(cal2), as.character(cal1))
})


test_that("fiscal mapping preserves correct calendar meaning", {

  x <- c("Jan 2023", "Apr 2023", "Q1 2023", "Q4 2023")

  cal <- as_calendar_period(x, with_year = TRUE)
  fis <- as_fiscal_period(cal, with_year = TRUE)

  ## month checks
  expect_equal(
    as.character(fis[1]),
    "Jan:2023"
  )

  expect_equal(
    as.character(fis[2]),
    "Apr:2023"
  )

  ## quarter checks
  expect_equal(
    as.character(fis[3]),
    "Q4:2022-23"  # Q1 calendar falls in fiscal Q4
  )

  expect_equal(
    as.character(fis[4]),
    "Q3:2023-24"
  )
})

test_that("as_calendar_period errors on mixed-frequency Date input", {

  x <- as.Date(c(
    "2023-01-15",  # month
    "2023-06-30",  # month
    "2023-10-01"   # quarter boundary
  ))

  expect_error(
    as_calendar_period(x, with_year = TRUE),
    "mixed frequencies",
    ignore.case = TRUE
  )
})


test_that("frequency is preserved across roundtrip", {

  x <- as.Date(c(
    "2023-04-01",
    "2023-05-01"
  ))

  cal <- as_calendar_period(x)
  fis <- as_fiscal_period(cal)

  expect_identical(
    frequency.calendar_period(cal),
    frequency.fiscal_period(fis)
  )
})



test_that("calendar -> fiscal -> calendar roundtrip works for monthly Date input", {

  x <- as.Date(c(
    "2023-01-15",
    "2023-02-10",
    "2023-03-20"
  ))

  cal1 <- as_calendar_period(x, with_year = TRUE)
  fis  <- as_fiscal_period(cal1, with_year = TRUE)
  cal2 <- as_calendar_period(fis, with_year = TRUE)

  expect_identical(as.character(cal2), as.character(cal1))
})


test_that("calendar -> fiscal -> calendar roundtrip works for quarterly Date input", {

  x <- as.Date(c(
    "2023-01-10",
    "2023-04-10",
    "2023-07-10"
  ))

  cal1 <- as_calendar_period(x, with_year = TRUE)
  fis  <- as_fiscal_period(cal1, with_year = TRUE)
  cal2 <- as_calendar_period(fis, with_year = TRUE)

  expect_identical(as.character(cal2), as.character(cal1))

  expect_equal(
    "q1 20" %>% as_calendar_period() %>% as.Date(),
    as.Date("2020-03-31")
  )

  expect_equal(
    "jan 85" %>% as_calendar_period() %>% as.Date(),
    as.Date("1985-01-31")
  )

})

