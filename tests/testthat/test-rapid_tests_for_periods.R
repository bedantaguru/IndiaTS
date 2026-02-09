
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

  freq <- x %>% as_fiscal_period() %>% frequency(singular = TRUE)

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


