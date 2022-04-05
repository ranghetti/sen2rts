#' @title Extract phenological metrics
#' @description This function computes and returns the approximate pole of
#'  inaccessibility (visual centre) for a polygon using the quadtree-based 
#'  algorithm implemented in `polylabelr::poi()`,
#'  accepting an input in the same format of `sf_centroid()`
#'  (`sf`, `sfc` or `sfg`)
#'  and returning an output in the same format of `x`.
#' @param x object of class `sfg`, `sfc` or `sf`
#' @param ... Ignored
#' @param precision The precision to use when computing the centre
#'  (passed to `polylabelr::poi()`), in the `x` unit.
#'  If missing, by default it is used the 1/1000 of the mean square root of the
#'  area of inputs.
#' @author Luigi Ranghetti, PhD (2021) \email{luigi@@ranghetti.info}
#' @importFrom polylabelr poi
#' @importFrom sf st_set_geometry st_sfc st_area st_point st_crs

st_poi <- function(x, ..., precision) {
  # Check format (to improve)
  if (!inherits(x, c("sfg", "sfc", "sf"))) {
    stop("'x' must be a valid 'sf', 'sfc' or 'sfg' dataset.")
  }
  # Define precision (1/1000 of the side of the "equivalent square")
  if (missing(precision)) {
    precision <- mean(sqrt(st_area(x))/1E3, na.rm=TRUE)
  }
  # Compute POIs
  poi_l <- poi(x, precision = precision)
  # Return output
  if (inherits(x, "sf")) {
    st_set_geometry(
      x,
      st_sfc(
        lapply(poi_l, function(p){st_point(c(p$x, p$y))}),
        crs = st_crs(x)
      )
    )
  } else if (inherits(x, "sfc")) {
    st_sfc(
      lapply(poi_l, function(p){st_point(c(p$x, p$y))}),
      crs = st_crs(x)
    )
  } else if (inherits(x, "sfg")) {
    st_point(c(poi_l$x, poi_l$y))
  }
}
