#' @title Extract time series from sen2r archives
#' @description Extract time series from a Sentinel-2 data archive
#'  (created with the package `sen2r`) over spatial features (points or
#'  polygons). Quality flags can be added exploiting an additional 
#'  extracted archive (see arguments `scl_paths` and `cld_paths`).
#' @param in_paths Paths of the `sen2r` files (eventually obtained using 
#'  `read_in_cube(..., out_format = "path")`).
#' @param in_sf Object with polygonal or point geometries
#' @param fun (optional) aggregation function (or function name) 
#'  to be used in case of polygonal `in_sf` features. Default is `mean`.
#'  If `scl_paths` and/or `cld_paths` are defined:
#'  - the default `mean` is intended as `weighted.mean()` (where the computed 
#'      quality flags are used as weights); 
#'  - only the alternative value `"best"` is accepted (in this case, the pixel 
#'      with the higher quality flag - or the average of pixels with the same
#'      higher quality flags - is considered).
#' @param in_sf_id (optional) character vector corresponding to the name/names
#'  of the `in_sf` column with the features IDs.
#'  If missing, the row number is used.
#' @param scl_paths (optional) Paths of the SCL files (they must correspond
#'  to `in_paths`); if provided, it is used to weight pixels
#'  during the aggregation and to provide an output quality flag.
#'  See details for the conversion between SCL and weights.
#' @param cld_paths (optional) Paths of the CLD files (they must correspond
#'  to `in_paths`); see `scl_paths`.
#'  See details for the conversion between SCL and weights.
#' @param scl_w (optional) weights to be used for each SCL class,
#'  which can be created using function `scl_weights()`.
#'  If missing, the default outputs of `scl_weights()` are used.
#'  See details for the conversion between SCL and weights.
#' @param fun_w (optional) function to be used to aggregate quality flags
#'  in case of polygonal `in_sf` features. Default is `mean`.
#' @param naming_convention (optional) the naming convention used to extract 
#'  information from `in_paths`, `scl_paths` and `cld_paths` 
#'  (see `sen2r::sen2r_getElements()` for details).
#'  It takes effect only if sen2r version is > 1.5.0.
#' @return The output time series in `s2ts` format.
#' @details To generate pixel weights, SCL and/or CLD layers can be used.
#' 
#'  SCL are categorical layers (12 levels), so each level must be converted
#'  in a 0-1 numeric value. This is done by function `scl_weights()`.
#'  If the user provides only `scl_paths`, the layer of weights will be a
#'  0-1 numeric layer in which each pixel value corresponds to the 0-1 value
#'  associated with the corresponding SCL class.
#'  
#'  CLD are integer layers with the percentage (0-100) of cloud probability.
#'  Assumed that a CLD of 0% is associated to a weight of 1 and a CLD of 100% 
#'  to a weight of 0, intermediate values are computed taking into account 
#'  the output of `scl_weights()` for classes `"cloud_high_probability"`,
#'  `"cloud_medium_probability"` and `"unclassified"`
#'  (this because CLD values are in the range 80-100 when associated to the SCL
#'  class `"cloud_high_probability"`, in the range 20-80 when associated to 
#'  `"cloud_medium_probability"` and in the range 20-80 when associated to 
#'  `"unclassified"` or `"thin_cirrus"`).
#'  The two values `"cloud_medium_probability" - "cloud_high_probability"`
#'  and `"unclassified" - "cloud_medium_probability"` are taken as breaks
#'  to reclassify CLD.
#'  I.e., consider the default case:
#'  `scl_weights()[c("cloud_high_probability", "cloud_medium_probability", "unclassified")]`
#'  returns `0.0 0.1 0.5`; so, breaks `0.05` and `0.35` are used, meaning that
#'  CLD values in the range 80-100% are rescaled to 0-0.05,
#'  CLD values in the range 20-80% are rescaled to 0.05-0.35 and 
#'  CLD values in the range 80-100% are rescaled to 0.35-1.
#'  If the user provides only `cld_paths`, the layer of weights will be a
#'  0-1 numeric layer with the above described values.
#'  
#'  Finally, if the user provides both `scl_paths` and `cld_paths`, the two
#'  layers of weights are combined and the lowest quality flag is considered.
#'  
#' @author Luigi Ranghetti, PhD (2020) \email{luigi@@ranghetti.info}
#' @import data.table
#' @importFrom methods as
#' @importFrom sen2r raster_metadata sen2r_getElements
#' @importFrom sf gdal_utils st_as_sfc st_bbox st_buffer st_crs st_intersection
#'  st_sf st_transform 
#' @importFrom stars read_stars st_get_dimension_values st_set_dimensions st_warp
#' @importFrom stats weighted.mean
#' @importFrom dplyr group_by summarise
#' @importFrom methods as
#' @export
#' @examples
#' # Load input data
#' data("sampleroi")
#' sen2r_ndvi_paths <- sample_paths("NDVI")
#' sen2r_scl_paths <- sample_paths("SCL")
#' 
#' \donttest{
#' # Simple TS extraction from polygons (without quality flags)
#' ts_raw_0 <- extract_s2ts(sen2r_ndvi_paths, sampleroi)
#' print(ts_raw_0, topn = 5)
#' }
#' 
#' # TS extraction from polygons using a SCL archive for quality flags
#' # (example used to produce the sample dataset "ts_raw")
#' ts_raw <- extract_s2ts(
#'   sen2r_ndvi_paths, 
#'   sampleroi,
#'   scl_paths = sen2r_scl_paths
#' )
#' ts_raw$value <- ts_raw$value / 1E4 # reshape to standard NDVI range -1 to 1
#' print(ts_raw, topn = 5) # standard print
#' head(as.data.frame(ts_raw)) # see content
#' plot(ts_raw)
#' 
#' \donttest{
#' # TS extraction from polygons using a different aggregation function
#' ts_raw_2 <- extract_s2ts(sen2r_ndvi_paths, sampleroi, fun = "max")
#' 
#' # TS extraction from points
#' samplepts <- suppressWarnings(sf::st_centroid(sampleroi))
#' ts_raw_3 <- extract_s2ts(sen2r_ndvi_paths, samplepts)
#' }


extract_s2ts <- function(
  in_paths,
  in_sf,
  fun = "mean",
  in_sf_id,
  scl_paths,
  cld_paths,
  scl_w,
  fun_w = "mean",
  naming_convention = "sen2r"
) {
  
  # Avoid check notes for data.table related variables
  sensing_date <- value <- NULL

  ## Check arguments ----
  #TODO
  if (all(
    any(!missing(scl_paths), !missing(cld_paths)),
    !inherits(fun, "character") || !fun %in% c("best", "mean")
  )) {
    print_message(
      type = "warning",
      "Argument 'fun' can only take values 'mean' and 'best' when ",
      "scl_paths and/or cld_paths are defined."
    )
  }
  
  # check bbox format
  if (inherits(in_sf, "sfc")) {
    in_sf <- st_sf(in_sf)
  } else if (!inherits(in_sf, "sf")) {
    print_message(
      type = "error",
      "'in_sf' must be an object of class 'sf' or 'sfc'."
    )
  }
  
  # check in_sf_id
  if (missing(in_sf_id)) {
    in_sf$row_id <- as.character(seq_len(nrow(in_sf)))
    in_sf_id <- "row_id"
  } else if (!in_sf_id %in% names(in_sf)) {
    print_message(
      type = "error",
      "'in_sf_id' must correspond to a column name of 'in_sf'."
    )
  } else {
    in_sf <- eval(parse(text = paste0(
      "summarise(group_by(in_sf, ",in_sf_id,"))"
    )))
  }

  # if ID is a numeric, convert to text
  if (inherits(in_sf[[in_sf_id]], c("integer","numeric"))) {
    in_sf[[in_sf_id]] <- as.character(in_sf[[in_sf_id]])
  }
  
  ## Read in_paths ----
  
  ## Obtain rasterIO from in_sf
  # read grid metadata
  in_meta <- if (packageVersion("sen2r") > package_version("1.5.0")) {
    sen2r_getElements(in_paths, naming_convention = naming_convention)
  } else {
    sen2r_getElements(in_paths)
  }
  inraster_meta <- raster_metadata(in_paths[1], format = "list")[[1]]
  # reproject
  if (st_crs(in_sf) != inraster_meta$proj) {
    in_sf <- st_transform(in_sf, inraster_meta$proj)
  }
  # check bbox format
  in_bbox <- st_bbox(suppressWarnings(st_intersection(
    st_buffer(in_sf,inraster_meta$res),
    st_as_sfc(inraster_meta$bbox)
  )))
  in_RasterIO <- list(
    nXOff = ceiling((in_bbox$xmin - inraster_meta$bbox$xmin) / inraster_meta$res["x"]) + 1,
    nYOff = ceiling((inraster_meta$bbox$ymax - in_bbox$ymax) / inraster_meta$res["y"]) + 1
  )
  in_RasterIO$nXSize = ceiling((in_bbox$xmax - inraster_meta$bbox$xmin) / inraster_meta$res["x"]) - in_RasterIO$nXOff + 1
  in_RasterIO$nYSize = ceiling((inraster_meta$bbox$ymax - in_bbox$ymin) / inraster_meta$res["y"]) - in_RasterIO$nYOff + 1
  
  
  ## Pass through a VRT
  # (to avoid error "")
  sf::gdal_utils(
    "buildvrt",
    source = in_paths,
    destination = vrt_path <- tempfile(fileext = ".vrt"),
    options = c(
      "-separate",
      "-resolution", "highest"
    ),
    quiet = TRUE
  )
  
  ## Read in_cube
  in_cube <- read_stars(vrt_path, RasterIO = in_RasterIO, proxy = FALSE)
  in_cube <- st_set_dimensions(in_cube, "band", in_meta$sensing_date)
  in_cube <- st_set_dimensions(in_cube, names = c("x", "y", "time"))
  
  
  ## Read scl_paths ----
  if (!missing(scl_paths)) {
    
    ## Obtain rasterIO from in_sf
    
    # read grid metadata
    scl_meta <- sen2r_getElements(scl_paths, naming_convention = naming_convention)
    
    # Check consistency between in_cube and scl_cube
    if (anyNA(match(in_meta$sensing_date, scl_meta$sensing_date))) {
      print_message(
        type = "error",
        "All files provided in `in_paths` must have a corresponding image in `scl_paths`."
      )
    }
    scl_paths <- scl_paths[match(in_meta$sensing_date, scl_meta$sensing_date)]
    scl_meta <- scl_meta[match(in_meta$sensing_date, sensing_date)]
    
    sclraster_meta <- sen2r::raster_metadata(scl_paths[1], format = "list")[[1]]
    
    # check bbox format
    scl_bbox <- st_bbox(suppressWarnings(st_intersection(
      st_buffer(in_sf,sclraster_meta$res),
      st_as_sfc(inraster_meta$bbox)
    )))
    scl_RasterIO <- list(
      nXOff = ceiling((scl_bbox$xmin - sclraster_meta$bbox$xmin) / sclraster_meta$res["x"]) + 1,
      nYOff = ceiling((sclraster_meta$bbox$ymax - scl_bbox$ymax) / sclraster_meta$res["y"]) + 1
    )
    scl_RasterIO$nXSize = ceiling((scl_bbox$xmax - sclraster_meta$bbox$xmin) / sclraster_meta$res["x"]) - scl_RasterIO$nXOff + 1
    scl_RasterIO$nYSize = ceiling((sclraster_meta$bbox$ymax - scl_bbox$ymin) / sclraster_meta$res["y"]) - scl_RasterIO$nYOff + 1
    
    ## Pass through a VRT
    # (to avoid error "")
    sf::gdal_utils(
      "buildvrt",
      source = scl_paths,
      destination = scl_vrt_path <- tempfile(fileext = ".vrt"),
      options = c(
        "-separate",
        "-resolution", "highest"
      ),
      quiet = TRUE
    )
    
    # Read scl_sube
    scl_cube <- read_stars(scl_vrt_path, RasterIO = scl_RasterIO, proxy = FALSE)
    
    # Check consistency between in_cube and scl_cube
    # TODO
    
    # Check scl_w
    if (missing(scl_w)) {
      scl_w <- scl_weights()
    } else if (!setequal(names(scl_w), names(scl_weights()))) {
      print_message(
        type = "error",
        "\"scl_w\" must be a named numeric vector ",
        "created with function scl_weights()."
      )
    }
    
    # Convert SCL to weights
    w_cube_scl_raw <- scl_cube
    for (i in seq_along(scl_w)) {
      sel_scl <- i - 1 # 0:11 in a 12-length vector
      w_cube_scl_raw[scl_cube == sel_scl] <- scl_w[i]
    }
    
    # Reshape it
    w_cube_scl <- st_warp_fixing(w_cube_scl_raw, in_cube, method = "near", use_gdal = TRUE)

  }
  
  
  ## Read cld_paths ----
  if (!missing(cld_paths)) {
    
    ## Obtain rasterIO from in_sf
    
    # read grid metadata
    cld_meta <- sen2r_getElements(cld_paths, naming_convention = naming_convention)
    
    # Check consistency between in_cube and cld_cube
    if (anyNA(match(in_meta$sensing_date, cld_meta$sensing_date))) {
      print_message(
        type = "error",
        "All files provided in `in_paths` must have a corresponding image in `cld_paths`."
      )
    }
    cld_paths <- cld_paths[match(in_meta$sensing_date, cld_meta$sensing_date)]
    cld_meta <- cld_meta[match(in_meta$sensing_date, sensing_date)]
    
    cldraster_meta <- sen2r::raster_metadata(cld_paths[1], format = "list")[[1]]
    
    # check bbox format
    cld_bbox <- st_bbox(suppressWarnings(st_intersection(
      st_buffer(in_sf,cldraster_meta$res),
      st_as_sfc(inraster_meta$bbox)
    )))
    cld_RasterIO <- list(
      nXOff = ceiling((cld_bbox$xmin - cldraster_meta$bbox$xmin) / cldraster_meta$res["x"]) + 1,
      nYOff = ceiling((cldraster_meta$bbox$ymax - cld_bbox$ymax) / cldraster_meta$res["y"]) + 1
    )
    cld_RasterIO$nXSize = ceiling((cld_bbox$xmax - cldraster_meta$bbox$xmin) / cldraster_meta$res["x"]) - cld_RasterIO$nXOff + 1
    cld_RasterIO$nYSize = ceiling((cldraster_meta$bbox$ymax - cld_bbox$ymin) / cldraster_meta$res["y"]) - cld_RasterIO$nYOff + 1
    
    ## Pass through a VRT
    # (to avoid error "")
    sf::gdal_utils(
      "buildvrt",
      source = cld_paths,
      destination = cld_vrt_path <- tempfile(fileext = ".vrt"),
      options = c(
        "-separate",
        "-resolution", "highest"
      ),
      quiet = TRUE
    )
    
    # Read cld_cube
    cld_cube <- read_stars(cld_vrt_path, RasterIO = cld_RasterIO, proxy = FALSE)
    
    # Check consistency between in_cube and cld_cube
    # TODO
    
    # Convert CLD to weights
    w_cube_cld_raw_0 <- 1 - cld_cube/100
    scl_cld_val <- scl_weights()[c("cloud_high_probability", "cloud_medium_probability", "unclassified")]
    if (any(diff(scl_cld_val) < 0)) {
      print_message(
        type = "error",
        "SCL weight for class \"cloud_high_probability\" must be higher than for ",
        "\"cloud_medium_probability\", as well as \"cloud_medium_probability\" ",
        "must be higher than \"unclassified\"."
      )
    }
    scl_cld_lim <- c(0, (scl_cld_val[1]+scl_cld_val[2])/2, (scl_cld_val[2]+scl_cld_val[3])/2, 1)
    scl_cld_limref <- c(0, 0.2, 0.8, 1)
    w_cube_cld_raw <- w_cube_cld_raw_0
    w_cube_cld_raw[[1]] <- ifelse(
      w_cube_cld_raw_0[[1]] < scl_cld_limref[2],
      scl_cld_lim[1] + (w_cube_cld_raw_0[[1]]-scl_cld_limref[1]) / diff(scl_cld_limref[1:2]) * diff(scl_cld_lim[1:2]),
      ifelse(
        w_cube_cld_raw_0[[1]] < scl_cld_limref[3],
        scl_cld_lim[2] + (w_cube_cld_raw_0[[1]]-scl_cld_limref[2]) / diff(scl_cld_limref[2:3]) * diff(scl_cld_lim[2:3]),
        scl_cld_lim[3] + (w_cube_cld_raw_0[[1]]-scl_cld_limref[3]) / diff(scl_cld_limref[3:4]) * diff(scl_cld_lim[3:4])
      )
    )
    
    # Reshape it
    w_cube_cld <- st_warp_fixing(w_cube_cld_raw, in_cube, method = "near", use_gdal = TRUE)
    
  }
  
  
  ## Extract TS ----
  
  if (inherits(in_cube, "stars")) {
    
    ts_list <- list()
    for (id in in_sf[[in_sf_id]]) {
      in_cube_array <- in_cube[in_sf[in_sf[[in_sf_id]] == id,]][[1]]
      if (all(missing(scl_paths), missing(cld_paths))) {
        ts_list[[id]] <- data.table(
          "date" = st_get_dimension_values(in_cube,"time"),
          "id" = id,
          "orbit" = in_meta$id_orbit,
          "sensor" = paste0(2, in_meta$mission),
          "value" = apply(in_cube_array, 3, fun, na.rm=TRUE)
        )
      } else {
        # generate w_cube_array from scl and/or cld
        w_cube_array <- if (all(!missing(scl_paths), !missing(cld_paths))) {
          w_cube_scl_array <- w_cube_scl[in_sf[in_sf[[in_sf_id]] == id,]][[1]]
          w_cube_cld_array <- w_cube_cld[in_sf[in_sf[[in_sf_id]] == id,]][[1]]
          apply(
            array(c(w_cube_scl_array, w_cube_cld_array), dim = c(dim(w_cube_scl_array), 2)),
            1:3, min
          )
        } else if (all(!missing(scl_paths), missing(cld_paths))) {
          w_cube_scl[in_sf[in_sf[[in_sf_id]] == id,]][[1]]
        } else if (all(missing(scl_paths), missing(!cld_paths))) {
          w_cube_cld[in_sf[in_sf[[in_sf_id]] == id,]][[1]]
        }
        ts_list[[id]] <- data.table(
          "date" = st_get_dimension_values(in_cube,"time"),
          "id" = id,
          "orbit" = in_meta$id_orbit,
          "sensor" = paste0(2, in_meta$mission),
          "value" = mapply(
            if (fun == "mean") {
              weighted.mean
            } else if (fun == "best") {
              function(x,y,...) {mean(x[y == max(c(1E-19,y),na.rm=TRUE)], ...)}
            },
            lapply(seq_len(dim(in_cube_array)[3]), function(k) in_cube_array[,,k]),
            lapply(seq_len(dim(w_cube_array)[3]), function(k) w_cube_array[,,k] + 1e-9),
            MoreArgs = list(na.rm = TRUE)
          ),
          "qa" = apply(w_cube_array, 3, fun_w, na.rm=TRUE)
        )
      }
    }
    ts_dt <- rbindlist(ts_list)
    ts_dt <- ts_dt[!is.na(value),]
    ts_out <- as(ts_dt, "s2ts")
    attr(ts_out, "gen_by") <- "extract_s2ts"
    ts_out
    
  } # end of IF in_cube is stars / stars_proxy
  
}
