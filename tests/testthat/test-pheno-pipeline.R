# Tests for the phenological metrics pipeline: fit_curve(), extract_pheno() and
# aggregate_pheno(). Metric extraction runs on the bundled fitted object `cf`
# (a real Gu fit) to keep the tests fast and deterministic.

test_that("fit_curve(fit = 'no') returns a per-id list without fitting", {
  data("ts_filled"); data("dt_cycles")
  cf0 <- fit_curve(ts_filled, dt_cycles, fit = "no")
  expect_type(cf0, "list")
  expect_gt(length(cf0), 0)
})

test_that("extract_pheno() returns cycle metrics from a fitted object", {
  data("cf")
  ph <- extract_pheno(cf, method = "trs", trs = 0.25)
  expect_s3_class(ph, "data.table")
  expect_true(all(c("id", "begin", "end", "sos", "eos") %in% names(ph)))
  expect_gt(nrow(ph), 0)
})

test_that("aggregate_pheno() aggregates a series between two metrics", {
  data("ts_filled"); data("dt_pheno")
  aggr <- aggregate_pheno(
    ts_filled, dt_pheno,
    metrics = c("sos", "eos"), fun = "quantile", probs = 0.95, na.rm = TRUE
  )
  expect_s3_class(aggr, "data.table")
  expect_equal(nrow(aggr), nrow(dt_pheno))
})
