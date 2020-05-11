

scl_weights <- function(
    no_data = 0,
    saturated_or_defective = 0,
    dark_area_pixels = 0.1,
    cloud_shadows = 0.25,
    vegetation = 1,
    not_vegetated = 1,
    water = 1,
    unclassified = 0.5,
    cloud_medium_probability = 0.25,
    cloud_high_probability = 0.1,
    thin_cirrus = 0.5,
    snow = 1
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