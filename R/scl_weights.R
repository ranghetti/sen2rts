#' @title Build SCL weights
#' @description Build a vector with relative weights for each SCL class.
#'  Class weights can be defined using function arguments  (all of them 
#'  are optional; default weights are used for each non-provided class).
#' @param nodata (optional) Relative weight (0-1) for SCL class
#'  `"nodata"`.
#' @param saturated_or_defective (optional) Relative weight (0-1) for SCL class
#'  `"saturated_or_defective"`.
#' @param dark_area_pixels (optional) Relative weight (0-1) for SCL class
#'  `"dark_area_pixels"`.
#' @param cloud_shadows (optional) Relative weight (0-1) for SCL class
#'  `"cloud_shadows"`.
#' @param vegetation (optional) Relative weight (0-1) for SCL class
#'  `"vegetation"`.
#' @param not_vegetated (optional) Relative weight (0-1) for SCL class
#'  `"not_vegetated"`.
#' @param water (optional) Relative weight (0-1) for SCL class
#'  `"water"`.
#' @param unclassified (optional) Relative weight (0-1) for SCL class
#'  `"unclassified"`.
#' @param cloud_medium_probability (optional) Relative weight (0-1) for SCL class
#'  `"cloud_medium_probability"`.
#' @param cloud_high_probability (optional) Relative weight (0-1) for SCL class
#'  `"cloud_high_probability"`.
#' @param thin_cirrus (optional) Relative weight (0-1) for SCL class
#'  `"thin_cirrus"`.
#' @param snow (optional) Relative weight (0-1) for SCL class
#'  `"snow"`.
#' @return A named vector of length 12, containing 0-1 relative weights
#'  for each SCL class.
#' @author Luigi Ranghetti, PhD (2020) \email{luigi@@ranghetti.info}
#' @export
#' @examples 
#' # Default weights
#' scl_weights()
#' # Custom weights changing two classes
#' scl_weights(unclassified = 0.25, snow = 0)

scl_weights <- function(
  no_data                  = 0.00, #  0
  saturated_or_defective   = 0.00, #  1
  dark_area_pixels         = 0.00, #  2
  cloud_shadows            = 0.25, #  3
  vegetation               = 1.00, #  4
  not_vegetated            = 1.00, #  5
  water                    = 1.00, #  6
  unclassified             = 0.50, #  7
  cloud_medium_probability = 0.10, #  8
  cloud_high_probability   = 0.00, #  9
  thin_cirrus              = 0.50, # 10
  snow                     = 1.00  # 11
) {
  
  if (any(
    no_data < 0 | no_data > 1,
    saturated_or_defective < 0 | saturated_or_defective > 1,
    dark_area_pixels < 0 | dark_area_pixels > 1,
    cloud_shadows < 0 | cloud_shadows > 1,
    vegetation < 0 | vegetation > 1,
    not_vegetated < 0 | not_vegetated > 1,
    water < 0 | water > 1,
    unclassified < 0 | unclassified > 1,
    cloud_medium_probability < 0 | cloud_medium_probability > 1,
    cloud_high_probability < 0 | cloud_high_probability > 1,
    thin_cirrus < 0 | thin_cirrus > 1,
    snow < 0 | snow > 1
  )) {
    stop("Arguments must be numeric values >= 0 and <= 1.")
  }
  
  c(
    "no_data" = no_data,
    "saturated_or_defective" = saturated_or_defective,
    "dark_area_pixels" = dark_area_pixels,
    "cloud_shadows" = cloud_shadows,
    "vegetation" = vegetation,
    "not_vegetated" = not_vegetated,
    "water" = water,
    "unclassified" = unclassified,
    "cloud_medium_probability" = cloud_medium_probability,
    "cloud_high_probability" = cloud_high_probability,
    "thin_cirrus" = thin_cirrus,
    "snow" = snow
  )
  
}