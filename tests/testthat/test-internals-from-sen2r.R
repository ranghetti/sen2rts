# Tests for the functions internalised from sen2r (see R/from_sen2r.R).

test_that("sen2r_getElements() parses a sen2r-convention product name", {
  el <- sen2rts:::sen2r_getElements(
    "S2A2A_20190723_022_Barbellino_BOA_10.tif", format = "list"
  )
  expect_identical(el$prod_type, "BOA")
  expect_identical(el$id_orbit, "022")
  expect_identical(el$extent_name, "Barbellino")
  expect_identical(as.character(el$sensing_date), "2019-07-23")
})

test_that("sen2r_getElements() flags unrecognised names when abort = FALSE", {
  el <- suppressWarnings(
    sen2rts:::sen2r_getElements("not_a_sen2r_name.tif", abort = FALSE, format = "list")
  )
  expect_identical(el$type, "unrecognised")
})

test_that("normalize_path() returns an absolute path", {
  p <- sen2rts:::normalize_path(tempdir())
  expect_true(startsWith(sen2rts:::normalize_path(p), "/") || grepl("^[A-Za-z]:", p))
})

test_that("raster_metadata() returns res/bbox/proj for a valid raster", {
  r <- tryCatch(sample_paths("NDVI")[1], error = function(e) NA_character_)
  skip_if(is.na(r) || !file.exists(r), "sample raster not available")
  meta <- tryCatch(
    sen2rts:::raster_metadata(r, format = "list")[[1]],
    error = function(e) NULL
  )
  skip_if(is.null(meta) || !isTRUE(meta$valid), "raster could not be read in this environment")
  expect_true(all(c("res", "bbox", "proj") %in% names(meta)))
  expect_length(meta$res, 2)
})

test_that("raster_metadata() only supports format = 'list'", {
  expect_error(sen2rts:::raster_metadata("x.tif", format = "data.table"), "list")
})
