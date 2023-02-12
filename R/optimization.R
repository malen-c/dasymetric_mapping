library(tidyverse)
source('dasymetric.R')


# create random (?) subsets of blocks, then create random covers, 
# test how you can predict the subsets using the covers

# Takes in national land cover polygon and some poly contained within it,
# returns a vector of the areas by land type
get_land_areas <- function(poly, nlcd_poly){
  cropped_nlcd_poly <- crop(nlcd_poly, poly)
  areas <- c(0, 0, 0, 0)
  if(nrow(cropped_nlcd_poly) != 0){
    areas[cropped_nlcd_poly$value - 20] <- expanse(cropped_nlcd_poly)
  }
  return(areas)
}

# Function which takes in same inputs as dasymetric_prediction but instead of
# weights, has two equal-length vectors of the min and max value to check
# for each land-cover type, as well as the increment along which to perform
# a grid search for optimized weights
dasymetric_grid_search <-
  function(nlcd_path, source_polys, target_poly, stat, mins, maxs, increment){
    nlcd_raster <- project(rast(nlcd_path), 'EPSG:4326')
    activeCat(nlcd_raster) <- 0

    nlcd_poly <- nlcd_raster %>%
      clamp(lower=21, upper=24, value=FALSE) %>%
      as.polygons()
    
    target_areas <- get_land_areas(target_poly, nlcd_poly)
    
    source_polys <- source_polys[relate(source_polys, target_poly, 'intersects')]
    intersection_polys <- crop(source_polys, target_poly)
    intersection_areas <- intersection_polys %>%
      lapply(\(x) get_land_areas(x, nlcd_poly)) %>%
      unlist()/expanse(target_poly)
    
    
  }