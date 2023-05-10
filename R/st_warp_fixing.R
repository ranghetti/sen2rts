#' @title Extract phenological metrics
#' @description Internal function to bypass an encountered error:
#' sometimes (apparently with large stacks) returns stars_proxy 
#' instead than stars: in this case, manually build a stars object.
#' @param src object of class `stars` with source raster
#' @param dest object of class `stars` with target raster geometry
#' @param ... Additional arguments passed to `stars::st_warp()`.
#' @author Luigi Ranghetti, PhD (2020) \email{luigi@@ranghetti.info}
#' @importFrom stars st_warp st_redimension

st_warp_fixing <- function(src, dest, ...) {
  rawpaths <- capture.output({
    stars_cube <- st_warp(src, dest, debug = TRUE, ...)
  })
  tmpaths <- gsub(paste0("^.*(",tempdir(),".+\\.tif).*$"), "\\1", rawpaths)
  if (inherits(stars_cube, "stars_proxy")) {
    stars_length <- dim(stars_cube)[3]
    stars_list <- list()
    for (i in seq_len(stars_length)) {
      stars_list[[i]] <- st_warp(src[,,,i], dest[,,,i], ...)
    }
    stars_cube <- st_redimension(
      do.call(c, stars_list), 
      along = list(band = seq_len(stars_length))
    )
    file.remove(tmpaths[file.exists(tmpaths) & !grepl(names(stars_cube), tmpaths)])
  } else if (inherits(stars_cube, "stars")) {
    file.remove(tmpaths[file.exists(tmpaths)])
  }
  stars_cube
}