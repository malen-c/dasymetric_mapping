library(terra)

# This file is a sort of "main" file that contains the code to apply dasymetric
# reweighting to a region.


weighted_area <- function(poly, weights, nlcd_raster){
  # Calculate the weighted area of a poly via dasymetric mapping
  weighted_areas <- c(0, 0, 0, 0)
  area_mat <- expanse(crop(nlcd_raster, poly), byValue=TRUE)
  for(code in area_mat[,'value']){
    weighted_areas[code-20] <- weights[code-20] * area_mat[code-20, 'area']
  }
  return(sum(weighted_areas))
}

population_prediction <- 
  function(nlcd_path, source_polys, target_poly, weights){
    # Load in tiff as raster, project to lon/lat, then transform into polys for
    # each land type. activeCat ensures that the value active when transforming
    # is in fact land code.
    
    # NOTE: tiff.aux.xml file needs to be in directory as well, not
    # just the tiff!
    nlcd_raster <- project(rast(nlcd_path), 'EPSG:4326')
    activeCat(nlcd_raster) <- 0
    
    # Filter source_polys to only those containing overlaps with target_poly
    # create intersections between each source poly and the target poly, then
    # calculate weighted area
    source_polys <- source_polys[relate(source_polys, target_poly, 'intersects')]
    nlcd_raster <- clamp(nlcd_raster, lower=21, upper=24, value=FALSE) %>%
      crop(ext(source_polys))
    
    intersection_polys <- crop(source_polys, target_poly)
    population_estimates <- rep(0, length(source_polys))
    
    for(i in 1:length(source_polys)){
      population_estimates[i] <- 
        values(source_polys)[i, 'value'] *
        weighted_area(intersection_polys[i], weights, nlcd_raster)/
        weighted_area(source_polys[i], weights, nlcd_raster)
    }
    
    # Filter out NaN's introduced by weighted source_areas of 0 (i.e. they have
    # no developed land), then get average
    estimate <- sum(weighted_stats, na.rm=TRUE)
    return(estimate)
  }
