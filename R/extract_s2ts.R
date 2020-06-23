#' @title Extract time series from sen2r archives
#' @description TODO
#' @param in_paths Paths of the sen2r files (eventually obtained using 
#'  `read_in_cube(..., out_format = "path")`).
#' @param in_sf Object with polygonal or point geometries
#' @param fun (optional) aggregation function to be used in case of polygonal
#'  `in_sf` features (see `exactextractr::exact_extract()`).
#'  If `scl_paths` is defined it is not used for aggregation of output values
#'  (`weighted.mean()` is always used for that), but for aggregation of weights
#'  made to compute the `"quality"` attribute.
#' @param in_sf_id (optional) charachter vector corresponding to the name/names
#'  of the `in_sf` column with the features IDs.
#'  If NA (default) the row number is used.
#' @param max_cells_in_memory (optional) argument passed to
#'  `exactextractr::exact_extract()` (it is ignored in case of point features).
#' @param scl_paths (optional) Paths of the SCL files (they must correspond
#'  to `in_paths`); if provided, it is used to weight pixels
#'  during the aggregation and to provide an output quality flag.
#' @param scl_weights (optional) weights to be used for each SCL class,
#'  which can be created using function `scl_weights()`.
#' @param min_cov (optional) quality threshold (0-1) for output values to be
#'  returned (default is 0, meaning that all values are returned).
#' @return The output time series in `s2ts` format. 
#'  If `scl_paths` was provided, an attribute `"quality"`
#'  is also returned, containing relative quality values (0-1).
#' @author Luigi Ranghetti, phD (2020) \email{luigi@@ranghetti.info}
#' @importFrom raster getZ
#' @importFrom sf st_warp
#' @export

#' inpath <- "~/nas-s4a/projects/SATFARMING/BF_datasets/.out_sen2r/Bonifiche_Ferraresi/Jolanda/raster/EO/SIs/S2A/MSAVI2"
#' in_stack <- read_in_cube(inpath, out_format = "RasterStack")
#' extent <- sf::st_read("~/nas-s4a/projects/SATFARMING/BF_datasets/.out_sen2r/Bonifiche_Ferraresi/Jolanda/vector/appezzamenti/2018/bf_jolanda_appezzamenti_2018_1r.shp")
#' extract_s2ts(in_stack, extent)



extract_s2ts <- function(
  in_paths,
  in_sf,
  fun = mean,
  in_sf_id = NA,
  max_cells_in_memory = 3e+07,
  scl_paths = NULL,
  scl_weights = NA
) {
  
  ## Check arguments ----
  #TODO
  
  # check bbox format
  if (!inherits(in_sf, c("sf", "sfc"))) {
    print_message(
      type = "error",
      "'in_sf' must be an object of class 'sf' or 'sfc'."
    )
  }
  
  # if (any(
  #   !inherits(in_cube, "RasterStack"),
  #   !missing(scl_cube) && !inherits(scl_cube, "RasterStack")
  # )) {
  #   print_message(
  #     type = "error",
  #     "Currently, extract_s2ts() supports only input RasterStacks."
  #   )
  # }
  
  
  ## Read in_paths ----
  
  ## Obtain rasterIO from in_sf
  # read grid metadata
  in_meta <- sen2r::sen2r_getElements(in_paths)
  inraster_meta <- sen2r::raster_metadata(in_paths[1], format = "list")[[1]]
  # reproject
  if (st_crs(in_sf) != inraster_meta$proj) {
    in_sf <- st_transform(in_sf, inraster_meta$proj)
  }
  # check bbox format
  in_bbox <- st_bbox(st_buffer(in_sf,inraster_meta$res))
  in_RasterIO <- list(
    nXOff = ceiling((in_bbox$xmin - inraster_meta$bbox$xmin) / inraster_meta$res["x"]),
    nYOff = ceiling((inraster_meta$bbox$ymax - in_bbox$ymax) / inraster_meta$res["y"])
  )
  in_RasterIO$nXSize = ceiling((in_bbox$xmax - inraster_meta$bbox$xmin) / inraster_meta$res["x"]) - in_RasterIO$nXOff
  in_RasterIO$nYSize = ceiling((inraster_meta$bbox$ymax - in_bbox$ymin) / inraster_meta$res["y"]) - in_RasterIO$nYOff
  
  
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
  in_cube <- read_stars(vrt_path, RasterIO = in_RasterIO)
  in_cube <- st_set_dimensions(in_cube, "band", in_meta$sensing_date)
  in_cube <- st_set_dimensions(in_cube, names = c("x", "y", "time"))
  
  
  ## Read scl_paths ----
  
  if (!missing(scl_paths)) {
    
    ## Obtain rasterIO from in_sf
    
    # read grid metadata
    scl_meta <- sen2r::sen2r_getElements(scl_paths)
    
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
    
    # check projection
    if (sclraster_meta$proj != inraster_meta$proj) {
      print_message(
        type = "error",
        "'in_paths' and 'scl_paths' must have the same projection."
      )
    }
    
    # check bbox format
    scl_bbox <- st_bbox(st_buffer(in_sf,sclraster_meta$res))
    scl_RasterIO <- list(
      nXOff = ceiling((scl_bbox$xmin - sclraster_meta$bbox$xmin) / sclraster_meta$res["x"]),
      nYOff = ceiling((sclraster_meta$bbox$ymax - scl_bbox$ymax) / sclraster_meta$res["y"])
    )
    scl_RasterIO$nXSize = ceiling((scl_bbox$xmax - sclraster_meta$bbox$xmin) / sclraster_meta$res["x"]) - scl_RasterIO$nXOff
    scl_RasterIO$nYSize = ceiling((sclraster_meta$bbox$ymax - scl_bbox$ymin) / sclraster_meta$res["y"]) - scl_RasterIO$nYOff
    
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
    scl_cube <- read_stars(scl_vrt_path, RasterIO = scl_RasterIO)
    
    # Check consistency between in_cube and scl_cube
    # TODO
    
    # Check scl_weights
    if (missing(scl_weights)) {
      scl_weights <- scl_weights()
    } else if (!setequal(names(scl_weights), names(scl_weights()))) {
      print_message(
        type = "error",
        "\"scl_weights\" must be a named numeric vector ",
        "created with function scl_weights()."
      )
    }
    
    # Convert SCL to weights
    w_cube_0 <- scl_cube
    for (i in seq_along(scl_weights)) {
      sel_scl <- i - 1 # 0:11 in a 12-length vector
      w_cube_0[scl_cube == sel_scl] <- scl_weights[i]
    }
    
    # Reshape it
    w_cube <- st_warp(w_cube_0, in_cube, method = "near", use_gdal = TRUE)
    
  }
  
  
  if (inherits(in_cube, "stars")) {
    
    s2_ts <- if (missing(scl_paths)) {
      apply(in_cube[in_sf][[1]], 3, fun, na.rm=TRUE)
    } else {
      in_cube_array <- in_cube[in_sf][[1]]
      w_cube_array <- w_cube[in_sf][[1]]
      w_ts <- apply(w_cube_array, 3, fun, na.rm=TRUE)
      mapply(
        weighted.mean,
        lapply(seq_len(dim(in_cube_array)[3]), function(k) in_cube_array[,,k]),
        lapply(seq_len(dim(w_cube_array)[3]), function(k) w_cube_array[,,k]),
        MoreArgs = list(na.rm = TRUE)
      )
    }
    
    names(s2_ts) <- st_get_dimension_values(in_cube,"time")
    
    if (!missing(scl_paths)) {
      attr(s2_ts, "quality") <- w_ts
      names(attr(s2_ts, "quality")) <- names(s2_ts)
    }
    s2_ts
    
    
    
  } # end of IF in_cube is stars / stars_proxy
  
  
  
  # with exactextract TODO
  
  # # Extract values 
  # extract_raw0 <- exactextractr::exact_extract(
  #   in_stack, extent, 
  #   fun = fun,
  #   max_cells_in_memory = max_cells_in_memory,
  #   parallel = TRUE
  # )
  # colnames(extract_raw0) <- getZ(in_stack_sel)
  # 
  # melt(as.data.table(extract_raw0), variable.name = "temp", value.name = "prod") # FIXME prod to actual prod_type
  # 
  # 
  # 
  # extract_raw0
  # #TODO ritorna anche un attributo con i pesi delle date
  
}
