# Tests for the s2ts class: constructor, accessors, conversions and methods.

test_that("s2ts() builds a valid object with the expected columns", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by = 5)
  x <- s2ts(value = runif(length(dates)), date = dates, id = "a")
  expect_s3_class(x, "s2ts")
  expect_s3_class(x, "data.table")
  expect_true(all(c("id", "date", "value") %in% names(data.table::as.data.table(x))))
})

test_that("s2ts() aborts on duplicated id/date pairs", {
  expect_error(
    s2ts(value = c(1, 2), date = rep(as.Date("2020-01-01"), 2), id = "a"),
    "[Dd]uplicated"
  )
})

test_that("accessors return the documented shapes", {
  data("ts_raw")
  expect_type(ts_raw$date, "double")          # a Date vector
  expect_s3_class(ts_raw$date, "Date")
  expect_true(is.character(ts_raw$id))
  expect_s3_class(ts_raw$value, "data.table") # wide table, one column per id
  expect_true("date" %in% names(ts_raw$value))
})

test_that("gen_by attribute records the generating function", {
  data("ts_raw"); data("ts_smoothed"); data("ts_filled")
  expect_identical(attr(ts_raw, "gen_by"), "extract_s2ts")
  expect_identical(attr(ts_smoothed, "gen_by"), "smooth_s2ts")
  expect_identical(attr(ts_filled, "gen_by"), "fill_s2ts")
})

test_that("as.s2ts() converts a named numeric vector", {
  v <- setNames(runif(4), as.character(seq(as.Date("2020-01-01"), by = 10, length.out = 4)))
  x <- as.s2ts(v)
  expect_s3_class(x, "s2ts")
  expect_length(x$date, 4)
})

test_that("print.s2ts() returns its input invisibly", {
  data("ts_raw")
  expect_output(res <- withVisible(print(ts_raw)))
  expect_false(res$visible)
  expect_s3_class(res$value, "s2ts")
})
