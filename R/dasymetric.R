library(terra)
library(dplyr)

# This file is a sort of "main" file that contains the code to apply dasymetric
# reweighting to a region It will likely be separated into smaller parts at a
# later date, but for now the overall structure of the project is not yet clear.
# This is also a first pass, and will require a lot of optimization.

weighted_area <- function(poly, weights, nlcd_poly){
  # Calculate the weighted area of a poly via dasymetric mapping
  cropped_nlcd_poly <- crop(nlcd_poly, poly)
  weighted_areas <- expanse(cropped_nlcd_poly) * weights[unlist(values(cropped_nlcd_poly))]
  
  return(sum(weighted_areas))
}

dasymetric_prediction <- 
  function(nlcd_path, source_polys, target_poly, weights, stat){
  # Load in tiff as raster, project to lon/lat, then transform into polys for
  # each land type. activeCat ensures that the value active when transforming
  # is in fact land code.
  
  # NOTE: .xml and aux.xml files need to be in directory as well, not
  # just the tiff!
  nlcd_raster <- project(rast(nlcd_path), 'EPSG:4326')
  activeCat(nlcd_raster) <- 0
  nlcd_poly <- as.polygons(nlcd_raster)
  
  # Filter source_polys to only those containing overlaps with target_poly
  # create intersections between each source poly and the target poly, then
  # calculate weighted area
  source_polys <- source_polys[relate(source_polys, target_poly, 'intersects')]
  intersection_polys <- crop(source_polys, target_poly)
  intersection_areas <- unlist(
    lapply(1:length(intersection_polys), 
           \(x) weighted_area(intersection_polys[x], weights, nlcd_poly))
  )
  
  # Take weighted average of stat of interest
  weighted_stats <- values(source_polys)[stat] * 
    (intersection_areas/weighted_area(target_poly, weights, nlcd_poly))
  
  # Filter out NaN's introduced by weighted source_areas of 0 (i.e. they have
  # no developed land), then get average
  estimate <- sum(weighted_stats, na.rm=TRUE)
  return(estimate)
}