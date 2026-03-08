
# ---- Helper constructors used in tests ----
f <- function(x) structure(x, class = fiscal_period_class)
cper <- function(x) structure(x, class = calendar_period_class)

# -------------------------
# fiscal_period: sort/unique
# -------------------------
test_that("sort.fiscal_period sorts ascending and descending", {
  x <- f(c("Mar:2023", "Jan:2023", "Feb:2023"))
  expect_identical(as.character(sort.fiscal_period(x)), c("Jan:2023", "Feb:2023", "Mar:2023"))
  expect_identical(as.character(sort.fiscal_period(x, decreasing = TRUE)), c("Mar:2023", "Feb:2023", "Jan:2023"))
})

test_that("unique.fiscal_period preserves first-occurrence and removes duplicates", {
  x <- f(c("Jan:2023", "Feb:2023", "Jan:2023", "Mar:2023"))
  expect_identical(as.character(unique.fiscal_period(x)), c("Jan:2023", "Feb:2023", "Mar:2023"))
})

test_that("sorting/unique error on mixed-frequency fiscal_period", {
  x_mixed <- f(c("Jan:2023", "Q1:2022-23"))
  expect_error(sort.fiscal_period(x_mixed), "mixed frequencies", ignore.case = TRUE)
  expect_error(unique.fiscal_period(x_mixed), "mixed frequencies", ignore.case = TRUE)
})

# -------------------------
# fiscal_period: Ops
# -------------------------

test_that("period + scalar shifts by that many periods (scalar fast-path)", {
  x <- f(c("Jan:2023", "Feb:2023"))
  expect_identical(as.character(x + 1), as.character(next_period(x)))
  expect_identical(as.character(x - 1), as.character(previous_period(x)))
})

test_that("period + vector performs element-wise shifts", {
  x <- f(c("Jan:2023", "Feb:2023"))
  res <- x + c(1L, 2L)
  expect_identical(as.character(res),
                   c(as.character(next_period(x[1])), as.character(next_period(next_period(x[2])))))
})

test_that("numeric + period works but numeric - period is not supported", {
  x <- f("Jan:2023")
  expect_identical(as.character(1 + x), as.character(x + 1))
  expect_error(1 - x, "not supported", ignore.case = TRUE)
})

test_that("invalid arithmetic combinations raise informative errors", {
  x <- f("Jan:2023")
  y <- f("Feb:2023")
  expect_error(x + y, "not supported", ignore.case = TRUE)            # two periods
  expect_error(x + NA_integer_, "NA shift amounts not supported", ignore.case = TRUE)
  expect_error(x + 1.5, "Fractional shifts are not allowed", ignore.case = TRUE)
  expect_error(x + c(1L, 2L, 3L), "must be length 1 \\(scalar\\) or same length", ignore.case = TRUE)
})

# -------------------------
# calendar_period: sort/unique
# -------------------------
test_that("sort.calendar_period sorts ascending and descending", {
  x <- cper(c("Mar:2023", "Jan:2023", "Feb:2023"))
  expect_identical(as.character(sort.calendar_period(x)), c("Jan:2023", "Feb:2023", "Mar:2023"))
  expect_identical(as.character(sort.calendar_period(x, decreasing = TRUE)), c("Mar:2023", "Feb:2023", "Jan:2023"))
})

test_that("unique.calendar_period preserves first-occurrence and removes duplicates", {
  x <- cper(c("Jan:2023", "Feb:2023", "Jan:2023", "Mar:2023"))
  expect_identical(as.character(unique.calendar_period(x)), c("Jan:2023", "Feb:2023", "Mar:2023"))
})

test_that("sorting/unique error on mixed-frequency calendar_period", {
  x_mixed <- cper(c("Jan:2023", "Q1:2022"))
  expect_error(sort.calendar_period(x_mixed), "mixed frequencies", ignore.case = TRUE)
  expect_error(unique.calendar_period(x_mixed), "mixed frequencies", ignore.case = TRUE)
})

# -------------------------
# calendar_period: Ops
# -------------------------

test_that("calendar_period arithmetic: scalar and element-wise shifts", {
  x <- cper(c("Jan:2023", "Feb:2023"))
  expect_identical(as.character(x + 1), as.character(next_period(x)))
  expect_identical(as.character(x + c(1L, 2L)),
                   c(as.character(next_period(x[1])), as.character(next_period(next_period(x[2])))))
  expect_identical(as.character(1 + x), as.character(x + 1))
  expect_error(1 - x, "not supported", ignore.case = TRUE)
})

test_that("calendar_period arithmetic errors mirror fiscal behavior", {
  x <- cper("Jan:2023")
  expect_error(x + NA_integer_, "NA shift amounts not supported", ignore.case = TRUE)
  expect_error(x + 1.5, "Fractional shifts are not allowed", ignore.case = TRUE)
  expect_error(x + c(1L,2L,3L), "must be length 1 \\(scalar\\) or same length", ignore.case = TRUE)
  expect_error(x + x, "not supported", ignore.case = TRUE)
})


# -------------------------
# class preservation checks
# -------------------------

test_that("fiscal_period class is preserved after operations and subsetting", {

  x <- f(c("Jan:2023", "Feb:2023", "Mar:2023"))

  # sort / unique
  expect_s3_class(sort.fiscal_period(x), fiscal_period_class)
  expect_s3_class(unique.fiscal_period(x), fiscal_period_class)

  # arithmetic
  expect_s3_class(x + 1, fiscal_period_class)
  expect_s3_class(x - 1, fiscal_period_class)
  expect_s3_class(1 + x, fiscal_period_class)

  # subsetting
  expect_s3_class(x[1], fiscal_period_class)
  expect_s3_class(x[1:2], fiscal_period_class)

})


test_that("calendar_period class is preserved after operations and subsetting", {

  x <- cper(c("Jan:2023", "Feb:2023", "Mar:2023"))

  # sort / unique
  expect_s3_class(sort.calendar_period(x), calendar_period_class)
  expect_s3_class(unique.calendar_period(x), calendar_period_class)

  # arithmetic
  expect_s3_class(x + 1, calendar_period_class)
  expect_s3_class(x - 1, calendar_period_class)
  expect_s3_class(1 + x, calendar_period_class)

  # subsetting
  expect_s3_class(x[1], calendar_period_class)
  expect_s3_class(x[1:2], calendar_period_class)

})



# -------------------------
# min / max for fiscal_period
# -------------------------

test_that("min and max work for fiscal_period and preserve class", {

  x <- f(c("Mar:2023", "Jan:2023", "Feb:2023"))

  mn <- min(x)
  mx <- max(x)

  expect_identical(as.character(mn), "Jan:2023")
  expect_identical(as.character(mx), "Mar:2023")

  expect_s3_class(mn, fiscal_period_class)
  expect_s3_class(mx, fiscal_period_class)

})


test_that("min and max work for multiple fiscal_period arguments", {

  x1 <- f("Jan:2023")
  x2 <- f("Mar:2023")
  x3 <- f("Feb:2023")

  expect_identical(as.character(min(x1, x2, x3)), "Jan:2023")
  expect_identical(as.character(max(x1, x2, x3)), "Mar:2023")

})


# -------------------------
# min / max for calendar_period
# -------------------------

test_that("min and max work for calendar_period and preserve class", {

  x <- cper(c("Mar:2023", "Jan:2023", "Feb:2023"))

  mn <- min(x)
  mx <- max(x)

  expect_identical(as.character(mn), "Jan:2023")
  expect_identical(as.character(mx), "Mar:2023")

  expect_s3_class(mn, calendar_period_class)
  expect_s3_class(mx, calendar_period_class)

})


test_that("min and max work for multiple calendar_period arguments", {

  x1 <- cper("Jan:2023")
  x2 <- cper("Mar:2023")
  x3 <- cper("Feb:2023")

  expect_identical(as.character(min(x1, x2, x3)), "Jan:2023")
  expect_identical(as.character(max(x1, x2, x3)), "Mar:2023")

})

# -------------------------
# c() methods for period classes
# -------------------------

test_that("c.fiscal_period concatenates vectors and preserves class", {

  x1 <- f(c("Jan:2023", "Feb:2023"))
  x2 <- f("Mar:2023")

  res <- c(x1, x2)

  expect_identical(
    as.character(res),
    c("Jan:2023", "Feb:2023", "Mar:2023")
  )

  expect_s3_class(res, fiscal_period_class)

})


test_that("c.fiscal_period errors when mixing with non fiscal_period", {

  x <- f("Jan:2023")

  expect_error(
    c(x, "Feb:2023"),
    "fiscal_period",
    ignore.case = TRUE
  )

})


test_that("c.calendar_period concatenates vectors and preserves class", {

  x1 <- cper(c("Jan:2023", "Feb:2023"))
  x2 <- cper("Mar:2023")

  res <- c(x1, x2)

  expect_identical(
    as.character(res),
    c("Jan:2023", "Feb:2023", "Mar:2023")
  )

  expect_s3_class(res, calendar_period_class)

})


test_that("c.calendar_period errors when mixing with non calendar_period", {

  x <- cper("Jan:2023")

  expect_error(
    c(x, "Feb:2023"),
    "calendar_period",
    ignore.case = TRUE
  )

})

