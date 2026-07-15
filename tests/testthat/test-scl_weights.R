# Tests for scl_weights().

test_that("scl_weights() returns a named vector in the 0-1 range", {
  w <- scl_weights()
  expect_true(is.numeric(w))
  expect_true(all(w >= 0 & w <= 1))
  expect_true(all(c("vegetation", "not_vegetated", "snow") %in% names(w)))
})

test_that("scl_weights() lets a single class be overridden", {
  expect_equal(unname(scl_weights(snow = 0)["snow"]), 0)
  expect_equal(unname(scl_weights()["snow"]), 1)
})
