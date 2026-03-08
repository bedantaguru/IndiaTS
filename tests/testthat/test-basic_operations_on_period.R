
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
