#' @title Extract phenological metrics
#' @description Internal function to bypass an encountered error:
#' sometimes (apparently with large stacks) returns stars_proxy 
#' instead than stars: in this case, manually build a stars object.
#' Temporary file paths created on source (this occurs reading VRT) are 
#' stored in the attribute "tempsourcepath".
#' @param src object of class `stars` with source raster
#' @param dest object of class `stars` with target raster geometry
#' @param ... Additional arguments passed to `stars::st_warp()`.
#' @author Luigi Ranghetti, PhD (2020) \email{luigi@@ranghetti.info}
#' @importFrom stars st_warp st_redimension

st_warp_fixing <- function(src, dest, ...) {
  ex_tmptif0 <- list.files(tempdir(), "\\.tif$", full.names = TRUE)
  stars_cube <- st_warp(src, dest, ...)
  ex_tmptif1 <- list.files(tempdir(), "\\.tif$", full.names = TRUE)
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
  }
  attr(stars_cube, "tempsourcepath") <- ex_tmptif1[!ex_tmptif1 %in% ex_tmptif0]
  stars_cube
}