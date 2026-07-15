# Tests for cut_cycles() and assign_season().

test_that("cut_cycles() partitions a filled series into dated cycles", {
  data("ts_filled")
  cyc <- cut_cycles(ts_filled)
  expect_s3_class(cyc, "data.table")
  expect_true(all(c("id", "begin", "end", "maxval", "weight") %in% names(cyc)))
  expect_gt(nrow(cyc), 0)
  expect_true(all(cyc$end >= cyc$begin))
})

test_that("assign_season() keeps at most the requested number of cycles per id", {
  data("dt_cycles")
  sel <- assign_season(
    dt_cycles, max_n_cycles = 1,
    pop_win = c("04-01", "08-31"), pop_name = "maxval"
  )
  expect_s3_class(sel, "data.table")
  counts <- sel[, .N, by = id]$N
  expect_true(all(counts <= 1))
  expect_lte(nrow(sel), nrow(dt_cycles))
})
