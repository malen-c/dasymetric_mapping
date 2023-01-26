library(terra)
library(dplyr)

# This file is a sort of "main" file that contains the code to apply dasymetric
# reweighting to a region It will likely be separated into smaller parts at a
# later date, but for now the overall structure of the project is not yet clear.
# This is also a first pass, and will require a lot of optimization.

nlcd_path <- 'bay_example/data/nlcd_data/NLCD_2019_Land_Cover_L48_20210604_bdSV3pzqsjEt13zGmeV9.tiff'
weights <- rep(0, 95)
weights[23] <- .7
weights[22] <- 1

weighted_area <- function(poly, weights, nlcd_poly){
  # Calculate the weighted area of a poly via dasymetric mapping
  cropped_nlcd_poly <- crop(nlcd_poly, poly)
  weighted_areas <- expanse(cropped_nlcd_poly) * weights[unlist(values(cropped_nlcd_poly))]
  
  return(sum(weighted_areas))
}

dasymetric_prediction <- function(nlcd_path, source_polys, target_poly, weights,
                                  stat){
  # Load in tiff as raster, project to lon/lat, then transform into polys for
  # each land type. activeCat ensures that the value active when transforming
  # is in fact land code
  nlcd_raster <- project(rast(nlcd_path), 'EPSG:4326')
  activeCat(nlcd_raster) <- 0
  nlcd_poly <- as.polygons(nlcd_raster)
  
  # Filter source_polys to only those containing overlaps with target_poly,
  # calculate weighted areas
  source_polys <- source_polys[relate(source_polys, target_poly, 'overlaps')]
  source_areas <- unlist(
    lapply(1:length(source_polys), 
           \(x) weighted_area(source_polys[x], weights, nlcd_poly))
  )
  
  # Create intersections between every source_poly and the target_poly,
  # calculate areas
  
  intersection_polys <- crop(source_polys, target_poly)
  intersection_areas <- unlist(
    lapply(1:length(intersection_polys), 
           \(x) weighted_area(intersection_polys[x], weights, nlcd_poly))
  )
  
  # Dasymetrically weighted average of stat of interest
  estimate <- sum(
    (intersection_areas/source_areas) * values(source_polys)[stat]
  )
  return(estimate)
}