# Tests for smooth_s2ts() and fill_s2ts() on the bundled sample series.

test_that("smooth_s2ts() returns a smoothed s2ts carrying raw values", {
  data("ts_raw")
  sm <- smooth_s2ts(ts_raw)
  expect_s3_class(sm, "s2ts")
  expect_identical(attr(sm, "gen_by"), "smooth_s2ts")
  expect_true("rawval" %in% names(data.table::as.data.table(sm)))
  expect_setequal(sm$id, ts_raw$id)
})

test_that("fill_s2ts() returns a gap-filled s2ts flagging interpolated values", {
  data("ts_smoothed")
  fl <- fill_s2ts(ts_smoothed)
  expect_s3_class(fl, "s2ts")
  expect_identical(attr(fl, "gen_by"), "fill_s2ts")
  dt <- data.table::as.data.table(fl)
  expect_true("interpolated" %in% names(dt))
  expect_true(any(dt$interpolated))            # some dates are filled
  # filled series is denser than the smoothed input
  expect_gt(nrow(dt), nrow(data.table::as.data.table(ts_smoothed)))
})
