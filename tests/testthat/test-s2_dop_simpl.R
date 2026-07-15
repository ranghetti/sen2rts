# Regression guard for the internal s2_dop_simpl(), which replaced sen2r::s2_dop().
# The `filter_launch` argument must reproduce sen2r::s2_dop()'s removal of
# theoretical passages predating each satellite launch. The bundled sample data
# (2019-2020, both satellites operational) would hide any divergence, so this is
# tested directly on a multi-mission window starting before the S2B launch.

test_that("the bundled doybase.json is found and well-formed", {
  jp <- system.file("extdata/settings/doybase.json", package = "sen2rts")
  expect_true(file.exists(jp))
  db <- jsonlite::fromJSON(jp)$dop
  expect_gt(nrow(db), 0)
  expect_true(all(c("orbit", "doybase") %in% names(db)))
})

test_that("filter_launch = TRUE drops pre-launch passages, FALSE keeps them", {
  tw <- as.Date(c("2016-03-01", "2018-06-30"))
  kept <- sen2rts:::s2_dop_simpl("022", tw, c("2A", "2B"), filter_launch = FALSE)
  filt <- sen2rts:::s2_dop_simpl("022", tw, c("2A", "2B"), filter_launch = TRUE)

  # without the filter, phantom 2B passages appear before the 2017-06-29 launch
  expect_lt(min(kept[mission == "2B", date]), as.Date("2017-06-29"))
  # with the filter, no 2B passage predates the launch...
  expect_gte(min(filt[mission == "2B", date]), as.Date("2017-06-29"))
  # ...and no 2A passage predates its own launch
  expect_gte(min(filt[mission == "2A", date]), as.Date("2015-06-27"))
  # the filter only ever removes rows
  expect_lt(nrow(filt), nrow(kept))
})

test_that("s2_dop_simpl() returns an empty, well-typed table when nothing matches", {
  out <- sen2rts:::s2_dop_simpl("022", as.Date(c("2016-01-01", "2016-01-02")),
                               "2A", filter_launch = TRUE)
  expect_s3_class(out, "data.table")
  expect_identical(names(out), c("date", "mission", "orbit"))
})
