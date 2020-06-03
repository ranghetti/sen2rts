#' @title Extract time series from sen2r archives
#' @description TODO
#' @param s2cube Raster time series imported using `read_s2cube()`
#' @param in_sf Object with polygonal or point geometries
#' @param fun (optional) aggregation function to be used in case of polygonal
#'  `in_sf` features (see `exactextractr::exact_extract()`);
#'  it is ignored in case of point features.
#' @param in_sf_id (optional) charachter vector corresponding to  the name/names
#'  of the `in_sf` column with the features IDs.
#'  If NA (default) the row number is used.
#' @param time_window (optional) time window to import
#'  (used for filtering among `inpath` content).
#' @param max_cells_in_memory (optional) argument passed to
#'  `exactextractr::exact_extract()` (it is ignored in case of point features).
#' @param scl_s2cube (optional) Object of class `s2cube` containing the SCL maps
#'  for each date in `s2cube`; if provided, it is used to weight pixels
#'  during the aggregation and to provide an output quality flag.
#' @param scl_weights (optional) weights to be used for each SCL class,
#'  which can be created using function `scl_weights()`.
#' @return The output time series in `s2ts` format.
#' @author Luigi Ranghetti, phD (2020) \email{luigi@@ranghetti.info}
#' @importFrom raster getZ
#' @export

#' inpath <- "~/nas-s4a/projects/SATFARMING/BF_datasets/.out_sen2r/Bonifiche_Ferraresi/Jolanda/raster/EO/SIs/S2A/MSAVI2"
#' in_stack <- read_s2cube(inpath, out_format = "RasterStack")
#' extent <- sf::st_read("~/nas-s4a/projects/SATFARMING/BF_datasets/.out_sen2r/Bonifiche_Ferraresi/Jolanda/vector/appezzamenti/2018/bf_jolanda_appezzamenti_2018_1r.shp")
#' extract_s2ts(in_stack, extent)



extract_s2ts <- function(
  s2cube,
  in_sf,
  fun = "mean",
  in_sf_id = NA,
  max_cells_in_memory = 3e+07,
  scl_s2cube = NULL,
  scl_weights = NA
) {
  
  # Check arguments
  #TODO
  
  if (any(
    !inherits(s2cube, "RasterStack"),
    !missing(scl_s2cube) && !inherits(scl_s2cube, "RasterStack")
  )) {
    print_message(
      type = "error",
      "Currently, extract_s2ts() supports only input RasterStacks."
    )
  }
  
  # If scl_s2cube is provided, use it to weight the average
  if (!missing(scl_s2cube)) {
    
    # Check consistency between s2cube and scl_s2cube
    
    
    # Check scl_weights
    if (!missing(scl_weights)) {
      scl_weights <- scl_weights()
    } else if (!setequal(names(scl_weights), names(scl_weights()))) {
      print_message(
        type = "error",
        "\"scl_weights\" must be a named numeric vector ",
        "created with function scl_weights()."
      )
    }
    
  }
  
  
  # Extract values 
  extract_raw0 <- exactextractr::exact_extract(
    in_stack, extent, 
    fun = fun,
    max_cells_in_memory = max_cells_in_memory,
    parallel = TRUE
  )
  colnames(extract_raw0) <- getZ(in_stack_sel)
  
  melt(as.data.table(extract_raw0), variable.name = "temp", value.name = "prod") # FIXME prod to actual prod_type
  
  
  
  extract_raw0
  #TODO ritorna anche un attributo con i pesi delle date
  
}
