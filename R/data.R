#' @title Boundaries of the sample crops used in the vignette 
#'  "The sen2rts workflow"
#' @description Dataset containing boundaries of some field crops of
#'  the farm [_Bonifiche Ferraresi_](https://bonificheferraresi.it) 
#'  located in the estate of Jolanda di Savoia (FE, Italy),
#'  which are used in the vignette
#'  ["The {sen2rts} workflow"](../articles/workflow.html).
#' @docType data
#' @usage data(samplecrops)
#' @rdname samplecrops
#' @format A polygon `sf` dataset in UTM 32N CRS
#'  with 12 rows (sample crops) and the variables `fid` with crop IDs
#'  ("01" to "12").
#' @examples 
#' data(samplecrops)
#' samplecrops
#' plot(samplecrops)
"samplecrops"

#' @title Sample areas used in documentation examples
#' @description Dataset containing two small ROI (Regions Of Interests) within
#'  field crops "03" and "04" of the dataset [`samplecrops`].
#'  The are used in the documentation examples of [`extract_s2ts()`].
#' @docType data
#' @usage data(sampleroi)
#' @rdname sampleroi
#' @format A polygon `sf` dataset in UTM 32N CRS
#'  with 2 rows (ROI) and the variables `id` with ROI IDs ("01" and "02").
#' @examples 
#' data(sampleroi)
#' sampleroi
#' plot(sampleroi)
"sampleroi"

#' @title Paths of the rasters used in documentation examples
#' @description The sample dataset contains Sentinel-2 derived images 
#'  (NDVI index and SCL) used in the documentation examples of [`extract_s2ts()`].
#'  This archive is not provided as a raster stack using standard `data()` way
#'  because [`extract_s2ts()`] accepts paths as input.
#' @param prod `"NDVI"` or `"SCL"`.
#' @format Each sample archive include 60 images acquired between 2020-01-01 and
#'  2020-09-30 covering the [`sampleroi`] extent, provided in the 
#'  [{sen2r} output format](https://sen2r.ranghetti.info/articles/outstructure.html).
#'  NDVI images are saved in integer format (values are scaled using a 10^4
#'  factor).
#'  
#'  Data size is very small (13x4 pixels per each image) in order not to be heavy
#'  in terms of data size and to ensure examples to be rapidly runnable.
#'  The vignette ["The {sen2rts} workflow"](../articles/workflow.html) makes use
#'  of a larger dataset in order to show a more plausible use case.
#' @return `sample_paths()` returns the paths of the sample archive
#'  (NDVI or SCL, depending on argument `prod`).
#' @export
#' @examples 
#' sen2r_ndvi_paths <- sample_paths("NDVI")
#' head(sen2r_ndvi_paths)
#' sen2r_scl_paths <- sample_paths("SCL")
#' head(sen2r_scl_paths)
sample_paths <- function(prod = "NDVI") {
  archive_dir <- system.file("extdata/sen2r/sampleroi", package = "sen2rts")
  if (!prod %in% c("NDVI", "SCL")) {
    print_message(
      type = "error",
      "Argument 'prod' can only be equal to \"NDVI\" or \"SCL\"."
    )
  }
  load_s2paths(archive_dir, prod_type = prod, file_ext = "tif")
}

#' @title Sample raw time series
#' @description Sample time series extracted from the example image archive 
#'  provided with the package (see [`sample_paths()`]) 
#'  over the extension of [`sampleroi`] polygons.
#'  It was created following the example provided in the [`extract_s2ts()`]
#'  documentation.
#'  It is used in the documentation of function [`smooth_s2ts()`].
#' @docType data
#' @usage data(ts_raw)
#' @rdname ts_raw
#' @format A [`s2ts`] object with 60 dates (in the period 2020-01-01 to
#'  2020-09-30) and 2 IDs (corresponding to [`sampleroi`] polygons).
#' @examples 
#' data(ts_raw)
#' print(ts_raw, topn = 5) # standard print
#' head(as.data.frame(ts_raw)) # see content
#' plot(ts_raw)
"ts_raw"

#' @title Sample smoothed time series
#' @description Sample time series obtained from the dataset [`ts_raw`],
#'  created following the example provided in the [`smooth_s2ts()`]
#'  documentation.
#'  It is used in the documentation of function [`fill_s2ts()`].
#' @docType data
#' @usage data(ts_smoothed)
#' @rdname ts_smoothed
#' @format A [`s2ts`] object with 60 dates (in the period 2020-01-01 to
#'  2020-09-30) and 2 IDs (corresponding to [`sampleroi`] polygons).
#' @examples 
#' data(ts_smoothed)
#' print(ts_smoothed, topn = 5) # standard print
#' head(as.data.frame(ts_smoothed)) # see content
#' plot(ts_smoothed)
"ts_smoothed"

#' @title Sample daily time series
#' @description Sample time series obtained from the dataset [`ts_smoothed`],
#'  created following the example provided in the [`fill_s2ts()`]
#'  documentation.
#'  It is used in the documentation of functions with takes a processed
#'  time series as input, like [`cut_cycles()`], [`fit_curve()`] and
#'  [`aggregate_pheno()`].
#' @docType data
#' @usage data(ts_filled)
#' @rdname ts_filled
#' @format A [`s2ts`] object with 301 dates (in the period 2020-01-01 to
#'  2020-09-30) and 2 IDs (corresponding to [`sampleroi`] polygons).
#' @examples 
#' data(ts_filled)
#' print(ts_filled, topn = 5) # standard print
#' head(as.data.frame(ts_filled)) # see content
#' plot(ts_filled)
"ts_filled"

#' @title Sample seasonal cycles
#' @description Data frame with the cycles extracted from the dataset
#'  [`ts_filled`], created following the example provided in the 
#'  [`cut_cycles()`] documentation.
#'  It is used in the documentation of function [`fit_curve()`].
#' @docType data
#' @usage data(dt_cycles)
#' @rdname dt_cycles
#' @format A data table with 3 records (identified cycles) and the following
#'  fields:
#'  - `id`: the time series ID;
#'  - `year`: the year assigned to each cycle;
#'  - `cycle`: the cycle ID (progressive integer within each year);
#'  - `begin`: the date of the begin of the cycle;
#'  - `end`: the date of the end of the cycle;
#'  - `maxval`: the date of the maximum value of the cycle;
#'  - `weight`: the value of the metric used for ranking seasons.
#' @examples 
#' data(dt_cycles)
#' dt_cycles
"dt_cycles"

#' @title Sample curve fitting interpolation
#' @description Object (list) containing the interpolation (curve fitting) of 
#'  some seasonal cycles ([`dt_cycles`]) extracted from the dataset
#'  [`ts_filled`], created following the example provided in the 
#'  [`fit_curve()`] documentation.
#'  It is used in the documentation of function [`extract_pheno()`].
#' @docType data
#' @usage data(cf)
#' @rdname cf
#' @format A named list of 2 elements (one per input ID) in the output format 
#'  of function [`fit_curve()`].
"cf"

#' @title Sample phenological metrics dataset
#' @description Data frame with the phenological metrics extracted from the 
#'  object [`cf`], created following the example provided in the 
#'  [`extract_pheno()`] documentation.
#'  It is used in the documentation of functions [`assign_season()`] and
#'  [`aggregate_pheno()`].
#' @docType data
#' @usage data(dt_pheno)
#' @rdname dt_pheno
#' @format A data table with 3 records (identified cycles) and the following
#'  fields:
#'  - `id`, `year`, `cycle`, `begin`, `end`, `maxval`, `weight`: 
#'      inherited from [`dt_cycles`]
#'  - `sos`, `eos`, `los`, `pop`, `mgs`, `rsp`, `rau`, `peak`, `msp`, `mau`:
#'      phenological metrics created by method `"trs"` (see 
#'      `phenopix::PhenoTrs()`).
#' @examples 
#' data(dt_pheno)
#' dt_pheno
"dt_pheno"
