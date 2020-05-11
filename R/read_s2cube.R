#' @title Read raster cubes from sen2r archives
#' @description TODO
#' @param inpath Path of the directory in which sen2r files are.
#' @param out_format (optional) Output format (one among "RasterStack", "stars" 
#'  and "stars_proxy").
#' @param prod_type (optional) sen2r product type to import
#'  (used for filtering among `inpath` content).
#' @param time_window (optional) time window to import
#'  (used for filtering among `inpath` content).
#' @param s2_orbits (optional) Sentinel-2 orbits to import
#'  (used for filtering among `inpath` content).
#' @param s2_sensors (optional) Sentinel-2 sensir to import  (`"2A"`, `"2B"` or both)
#'  (used for filtering among `inpath` content).
#' @param file_ext (optional) input file extension 
#' (used for filtering among `inpath` content).
#' @return The output raster cube in the selected format.
#' @author Luigi Ranghetti, phD (2020) \email{luigi@@ranghetti.info}
#' @export
#' @import data.table
#' @importFrom sen2r normalize_path sen2r_getElements
#' @importFrom stars read_stars st_redimension
#' @importFrom raster setZ stack
#'
#' @examples
#' json_path <- sen2r::build_example_param_file()
#' out_paths_2 <- sen2r::sen2r(json_path, timewindow = c("2019-07-13", "2019-07-23"))
#' inpath <- file.path(unique(dirname(dirname(out_paths_2))), "MSAVI2")
#' extent <- sf::st_read(system.file("extdata/vector/barbellino.geojson", package = "sen2r"))
#' in_stack <- read_s2cube(inpath, out_format = "RasterStack")
#' in_stars <- read_s2cube(inpath, out_format = "stars")

#' inpath <- "~/nas-s4a/projects/SATFARMING/BF_datasets/.out_sen2r/Bonifiche_Ferraresi/Jolanda/raster/EO/SIs/S2A/MSAVI2"
#' in_stack <- read_s2cube(inpath, out_format = "RasterStack")
#' in_stars <- read_s2cube(inpath, out_format = "stars")
#' in_starsp <- read_s2cube(inpath, out_format = "stars_proxy")


read_s2cube <- function(
  inpath,
  out_format = "RasterStack",
  # extent = NA,
  # res = NA,
  prod_type = NA,
  time_window = NA,
  s2_orbits = NA,
  s2_sensors = c("2A", "2B"),
  file_ext = NA
) {

  # Check inpath and read metadata
  inpath <- normalize_path(inpath)
  in_paths <- list.files(inpath, full.names = TRUE, recursive = TRUE)
  in_paths <- in_paths[!grepl("((thumbnails)|(\\.aux\\.xml$)|(\\.hdr$))", in_paths)]
  in_meta <- data.table(suppressWarnings(sen2r_getElements(in_paths, abort = FALSE)))
  in_meta[,path:=in_paths]
  in_meta <- in_meta[order(sensing_date),]
  
  # Check arguments
  if (!out_format %in% c("RasterStack", "stars_proxy", "stars")) {
    print_message(
      type = "error",
      "\"out_format\" must be one among 'RasterStack', 'stars_proxy' and 'stars'."
    )
  }
  #TODO
  
  # Filter data by arguments
  in_meta <- in_meta[type != "unrecognised",]
  if (!missing(prod_type)) {in_meta <- in_meta[prod_type == prod_type,]}
  if (!missing(s2_orbits)) {in_meta <- in_meta[id_orbit %in% s2_orbits,]}
  if (!missing(s2_sensors)) {in_meta <- in_meta[paste0("2",mission) %in% s2_sensors,]}
  if (!missing(file_ext)) {in_meta <- in_meta[file_ext == file_ext,]}
  if (!missing(time_window)) {
    in_meta <- in_meta[sensing_date >= time_window[1] & sensing_date <= time_window[2],]
  }
  
  # Check univocity of time series in inpath
  if (length(in_meta[,unique(extent_name)]) > 1) {
    print_message(
      type = "error",
      "The extent name or the files found in \"",inpath,"\" is not unique; ",
      "please filter files."
    )
  }
  if (length(in_meta[,unique(prod_type)]) > 1) {
    print_message(
      type = "error",
      "The product type or the files found in \"",inpath,"\" is not unique; ",
      "provide a value fo argument \"prod_type\" or filter files."
    )
  }
  if (any(in_meta[,table(sensing_date)] > 1)) {
    print_message(
      type = "error",
      "Some files refer to the same date; please filter files in \"",inpath,"\"."
    )
  }
  
  # switch format
  if (out_format == "RasterStack") {
    in_cube <- stack(in_meta$path)
    in_cube <- setZ(in_cube, in_meta$sensing_date)
  } else if (out_format %in% c("stars", "stars_proxy")) {
    in_cube <- read_stars(in_meta$path, proxy = out_format=="stars_proxy")
    in_cube <- st_redimension(in_cube, along = list(time = in_meta$sensing_date))
    names(in_cube) <- in_meta[1, paste0("S2_",extent_name,"_",prod_type)]
  }
  
  in_cube
  
}
