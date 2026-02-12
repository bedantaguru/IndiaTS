
test_that("FY year inference switches at March boundary", {

  x <- c(
    "Jan:2023-24",  # should map to 2024
    "Mar:2023-24",  # should map to 2024
    "Apr:2023-24",  # should map to 2023
    "Dec:2023-24"   # should map to 2023
  )

  res <- fiscal_month(x, with_year = TRUE)

  expect_equal(
    as.character(res),
    c("Jan:2024", "Mar:2024", "Apr:2023", "Dec:2023")
  )
})


test_that("calendar quarter auto-converts when fiscal quarter is NA", {

  x <- c(
    "Q1:2023",     # calendar quarter only
    "Q2:2023-24"   # valid fiscal quarter
  )

  res <- fiscal_quarter(
    x,
    with_year = TRUE,
    auto_convert_calendar_quarter = TRUE
  )

  expect_false(any(is.na(res)))
})



test_that("frequency detection returns mixed when needed", {

  x <- c("Jan:2023", "Q1:2023-24")

  freq <- x %>% as_fiscal_period() %>% stats::frequency(singular = TRUE)

  expect_equal(freq, "mixed")
})


test_that("fiscal year anchor returns correct dates", {

  x <- fiscal_year("FY:2023")

  expect_equal(
    as.Date(x, anchor = "first"),
    as.Date("2022-04-01")
  )

  expect_equal(
    as.Date(x, anchor = "last"),
    as.Date("2023-03-31")
  )

  expect_equal(
    as.Date(x, anchor = "mid"),
    as.Date("2022-09-30")
  )
})


test_that("fiscal quarter and month works on dates", {

  dates <- as.Date(c("2023-01-15", "2023-04-15", "2023-07-15", "2023-10-15"))

  qtr <- fiscal_quarter(dates, with_year = TRUE)
  mth <- fiscal_month(dates, with_year = TRUE)

  expect_equal(
    as.character(qtr),
    c("Q4:2022-23", "Q1:2023-24", "Q2:2023-24", "Q3:2023-24")
  )

  expect_equal(
    as.character(mth),
    c("Jan:2023", "Apr:2023", "Jul:2023", "Oct:2023")
  )

})


test_that("fiscal_quarter infers FY and quarter from lowercase short text", {

  expect_equal(
    as.character(fiscal_quarter("q1 15")),
    "Q4:2014-15"
  )
})



library(testthat)

# ============================================================================
# Test 1: MONTHLY frequency
# ============================================================================
test_that("Monthly dates convert to correct fiscal month format", {
  # Test 1.1: Regular monthly sequence starting in Q4
  monthly_dates_1 <- seq.Date(as.Date("2020-12-01"), by = "month", length.out = 3)
  result_1 <- as_fiscal_period(monthly_dates_1, with_year = TRUE)
  expect_equal(
    unclass(result_1),
    c("Dec:2020", "Jan:2021", "Feb:2021")
  )

  # Test 1.2: Monthly sequence starting in Q1
  monthly_dates_2 <- seq.Date(as.Date("2021-04-01"), by = "month", length.out = 4)
  result_2 <- as_fiscal_period(monthly_dates_2, with_year = TRUE)
  expect_equal(
    unclass(result_2),
    c("Apr:2021", "May:2021", "Jun:2021", "Jul:2021")
  )

  # Test 1.3: Monthly spanning fiscal year boundary
  monthly_dates_3 <- seq.Date(as.Date("2020-02-15"), by = "month", length.out = 4)
  result_3 <- as_fiscal_period(monthly_dates_3, with_year = TRUE)
  expect_equal(
    unclass(result_3),
    c("Feb:2020", "Mar:2020", "Apr:2020", "May:2020")
  )

  # Test 1.4: Monthly without year
  monthly_dates_4 <- seq.Date(as.Date("2019-06-01"), by = "month", length.out = 3)
  result_4 <- as_fiscal_period(monthly_dates_4, with_year = FALSE)
  expect_equal(
    unclass(result_4),
    c("Jun", "Jul", "Aug")
  )
})

# ============================================================================
# Test 2: QUARTERLY frequency
# ============================================================================
test_that("Quarterly dates convert to correct fiscal quarter format", {
  # Test 2.1: Regular quarterly sequence starting Q1
  quarterly_dates_1 <- seq.Date(as.Date("2020-04-01"), by = "quarter", length.out = 4)
  result_1 <- as_fiscal_period(quarterly_dates_1, with_year = TRUE)
  expect_equal(
    unclass(result_1),
    c("Q1:2020-21", "Q2:2020-21", "Q3:2020-21", "Q4:2020-21")
  )

  # Test 2.2: Quarterly starting Q3
  quarterly_dates_2 <- seq.Date(as.Date("2021-10-15"), by = "quarter", length.out = 3)
  result_2 <- as_fiscal_period(quarterly_dates_2, with_year = TRUE)
  expect_equal(
    unclass(result_2),
    c("Q3:2021-22", "Q4:2021-22", "Q1:2022-23")
  )

  # Test 2.3: Quarterly spanning multiple fiscal years
  quarterly_dates_3 <- seq.Date(as.Date("2019-01-01"), by = "quarter", length.out = 5)
  result_3 <- as_fiscal_period(quarterly_dates_3, with_year = TRUE)
  expect_equal(
    unclass(result_3),
    c("Q4:2018-19", "Q1:2019-20", "Q2:2019-20", "Q3:2019-20", "Q4:2019-20")
  )

  # Test 2.4: Quarterly without year
  quarterly_dates_4 <- seq.Date(as.Date("2020-07-01"), by = "quarter", length.out = 4)
  result_4 <- as_fiscal_period(quarterly_dates_4, with_year = FALSE)
  expect_equal(
    unclass(result_4),
    c("Q2", "Q3", "Q4", "Q1")
  )
})

# ============================================================================
# Test 3: HALF-YEARLY frequency
# ============================================================================
test_that("Half-yearly dates convert to correct fiscal half-year format", {
  # Test 3.1: Regular half-yearly starting H1
  halfyearly_dates_1 <- seq.Date(as.Date("2020-04-01"), by = "6 months", length.out = 4)
  result_1 <- as_fiscal_period(halfyearly_dates_1, with_year = TRUE)
  expect_equal(
    unclass(result_1),
    c("H1:2020-21", "H2:2020-21", "H1:2021-22", "H2:2021-22")
  )

  # Test 3.2: Half-yearly starting H2
  halfyearly_dates_2 <- seq.Date(as.Date("2021-10-01"), by = "6 months", length.out = 3)
  result_2 <- as_fiscal_period(halfyearly_dates_2, with_year = TRUE)
  expect_equal(
    unclass(result_2),
    c("H2:2021-22", "H1:2022-23", "H2:2022-23")
  )

  # Test 3.3: Half-yearly with mid-period dates
  halfyearly_dates_3 <- seq.Date(as.Date("2020-07-15"), by = "6 months", length.out = 4)
  result_3 <- as_fiscal_period(halfyearly_dates_3, with_year = TRUE)
  expect_equal(
    unclass(result_3),
    c("H1:2020-21", "H2:2020-21", "H1:2021-22", "H2:2021-22")
  )

  # Test 3.4: Half-yearly without year
  halfyearly_dates_4 <- seq.Date(as.Date("2019-04-01"), by = "6 months", length.out = 3)
  result_4 <- as_fiscal_period(halfyearly_dates_4, with_year = FALSE)
  expect_equal(
    unclass(result_4),
    c("H1", "H2", "H1")
  )
})

# ============================================================================
# Test 4: YEARLY frequency
# ============================================================================
test_that("Yearly dates convert to correct fiscal year format", {
  # Test 4.1: Regular yearly sequence
  yearly_dates_1 <- seq.Date(as.Date("2015-04-01"), by = "year", length.out = 4)
  result_1 <- as_fiscal_period(yearly_dates_1, with_year = TRUE)
  expect_equal(
    unclass(result_1),
    c("2015-16", "2016-17", "2017-18", "2018-19")
  )

  # Test 4.2: Yearly starting mid-year (Q2)
  yearly_dates_2 <- seq.Date(as.Date("2018-08-15"), by = "year", length.out = 3)
  result_2 <- as_fiscal_period(yearly_dates_2, with_year = TRUE)
  expect_equal(
    unclass(result_2),
    c("2018-19", "2019-20", "2020-21")
  )

  # Test 4.3: Yearly starting in Q4
  yearly_dates_3 <- seq.Date(as.Date("2016-02-29"), by = "year", length.out = 3)
  result_3 <- as_fiscal_period(yearly_dates_3, with_year = TRUE)
  expect_equal(
    unclass(result_3),
    c("2015-16", "2016-17", "2017-18")
  )

  # Test 4.4: Yearly starting in Q1 (beginning of FY)
  yearly_dates_4 <- seq.Date(as.Date("2020-04-01"), by = "year", length.out = 5)
  result_4 <- as_fiscal_period(yearly_dates_4, with_year = TRUE)
  expect_equal(
    unclass(result_4),
    c("2020-21", "2021-22", "2022-23", "2023-24", "2024-25")
  )
})

# ============================================================================
# Test 5: MIXED frequency - should generate error
# ============================================================================
test_that("Mixed frequency dates generate appropriate error", {
  # Test 5.1: Mix of monthly and quarterly gaps
  mixed_dates_1 <- as.Date(c(
    "2020-01-01",
    "2020-02-01",  # 1 month gap
    "2020-05-01",  # 3 month gap
    "2021-01-01"   # 8 month gap
  ))
  expect_error(
    as_fiscal_period(mixed_dates_1, with_year = TRUE),
    regexp = "mixed|irregular|frequency|inconsistent|cannot determine",
    ignore.case = TRUE
  )

  # Test 5.2: Random irregular dates
  mixed_dates_2 <- as.Date(c(
    "2020-04-01",
    "2020-04-15",  # 14 days
    "2020-07-01",  # ~2.5 months
    "2021-01-01"   # 6 months
  ))
  expect_error(
    as_fiscal_period(mixed_dates_2, with_year = TRUE),
    regexp = "mixed|irregular|frequency|inconsistent|cannot determine",
    ignore.case = TRUE
  )

  # Test 5.3: Mix of half-yearly and yearly
  mixed_dates_3 <- as.Date(c(
    "2019-04-01",
    "2019-10-01",  # 6 months (half-yearly)
    "2021-04-01"   # 18 months (not consistent)
  ))
  expect_error(
    as_fiscal_period(mixed_dates_3, with_year = TRUE),
    regexp = "mixed|irregular|frequency|inconsistent|cannot determine",
    ignore.case = TRUE
  )

  # Test 5.4: Highly irregular dates
  mixed_dates_4 <- as.Date(c(
    "2020-01-05",
    "2020-02-10",
    "2020-03-20",
    "2020-06-15",
    "2020-12-30"
  ))
  expect_error(
    as_fiscal_period(mixed_dates_4, with_year = TRUE),
    regexp = "mixed|irregular|frequency|inconsistent|cannot determine",
    ignore.case = TRUE
  )
})

# ============================================================================
# Test 6: Edge cases
# ============================================================================
test_that("Edge cases are handled correctly", {
  # Test 6.1: Single date - cannot determine frequency
  single_date <- as.Date("2020-04-01")
  expect_error(
    as_fiscal_period(single_date, with_year = TRUE),
    regexp = "insufficient|single|cannot determine|frequency|need.*more",
    ignore.case = TRUE
  )

  # Test 6.2: Two dates - minimum for frequency detection (monthly)
  two_dates <- as.Date(c("2020-04-01", "2020-05-01"))
  result_two <- as_fiscal_period(two_dates, with_year = TRUE)
  expect_equal(
    unclass(result_two),
    c("Apr:2020", "May:2020")
  )

  # Test 6.3: Empty vector
  empty_dates <- as.Date(character(0))
  expect_error(
    as_fiscal_period(empty_dates, with_year = TRUE)
  )

  # Test 6.4: All NA dates
  all_na_dates <- as.Date(c(NA, NA, NA))
  expect_error(
    as_fiscal_period(all_na_dates, with_year = TRUE),
    regexp = "NA|missing|no.*valid|insufficient",
    ignore.case = TRUE
  )
})

# ============================================================================
# Test 7: Fiscal year boundary scenarios
# ============================================================================
test_that("Fiscal year boundaries are handled correctly", {
  # Test 7.1: Monthly across FY boundary (Mar-Apr)
  # Months use calendar year, so no special FY handling
  fy_boundary_monthly <- as.Date(c("2020-03-01", "2020-04-01", "2020-05-01"))
  result_monthly <- as_fiscal_period(fy_boundary_monthly, with_year = TRUE)
  expect_equal(
    unclass(result_monthly),
    c("Mar:2020", "Apr:2020", "May:2020")
  )

  # Test 7.2: Quarterly across FY boundary (Q4-Q1)
  fy_boundary_quarterly <- seq.Date(as.Date("2020-01-01"), by = "quarter", length.out = 3)
  result_quarterly <- as_fiscal_period(fy_boundary_quarterly, with_year = TRUE)
  expect_equal(
    unclass(result_quarterly),
    c("Q4:2019-20", "Q1:2020-21", "Q2:2020-21")
  )

  # Test 7.3: Half-yearly across FY boundary
  fy_boundary_half <- as.Date(c("2020-02-01", "2020-08-01", "2021-02-01"))
  result_half <- as_fiscal_period(fy_boundary_half, with_year = TRUE)
  expect_equal(
    unclass(result_half),
    c("H2:2019-20", "H1:2020-21", "H2:2020-21")
  )

  # Test 7.4: Yearly across decades
  fy_boundary_yearly <- as.Date(c("2019-12-01", "2020-12-01", "2021-12-01"))
  result_yearly <- as_fiscal_period(fy_boundary_yearly, with_year = TRUE)
  expect_equal(
    unclass(result_yearly),
    c("2019-20", "2020-21", "2021-22")
  )
})



test_that("frequency.calendar_period works with singular = TRUE", {

  # Case 1: all months -> single frequency
  x_month <- c("Jan:2024", "Feb:2023", "Mar:2022")
  expect_equal(
    frequency.calendar_period(x_month, singular = TRUE),
    "month"
  )

  # Case 2: all quarters -> single frequency
  x_quarter <- c("Q1:2020", "Q4:2021")
  expect_equal(
    frequency.calendar_period(x_quarter, singular = TRUE),
    "quarter"
  )

  # Case 3: all years -> single frequency
  x_year <- c("2019", "2020", "2021")
  expect_equal(
    frequency.calendar_period(x_year, singular = TRUE),
    "year"
  )

  # Case 4: mixed frequencies -> "mixed"
  x_mixed <- c("Jan:2024", "Q1:2023", "2022")
  expect_equal(
    frequency.calendar_period(x_mixed, singular = TRUE),
    "mixed"
  )

  # Case 5: all NA after parsing -> mixed (edge case)
  x_na <- c("foo", "bar")
  expect_equal(
    frequency.calendar_period(x_na, singular = TRUE),
    "mixed"
  )
})



test_that("as.Date.calendar_period handles year anchors correctly", {

  x <- "2020"

  # first
  expect_equal(
    as.Date.calendar_period(x, anchor = "first"),
    as.Date("2020-01-01")
  )

  # last
  expect_equal(
    as.Date.calendar_period(x, anchor = "last"),
    as.Date("2020-12-31")
  )

  # mid
  expect_equal(
    as.Date.calendar_period(x, anchor = "mid"),
    as.Date("2020-01-01") +
      as.integer(
        (as.Date("2020-12-31") - as.Date("2020-01-01")) / 2
      )
  )
})



test_that("unordered continuous monthly periods are detected as continuous", {

  x <- as_calendar_period(
    c("Jun 2019", "Apr 2019", "May 2019", "Mar 2019"),
    with_year = TRUE
  )

  expect_true(is_continuous(x))
})


test_that("missing month breaks continuity", {

  x <- as_calendar_period(
    c("Jan 2020", "Feb 2020", "Apr 2020"),
    with_year = TRUE
  )

  expect_false(is_continuous(x))
})

test_that("mixed-frequency periods are not continuous", {

  x <- as_calendar_period(
    c("Jan 2020", "Q1 2020"),
    with_year = TRUE
  )

  expect_false(is_continuous(x))
})

