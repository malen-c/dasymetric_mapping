library(terra)

# This file is a sort of "main" file that contains the code to apply dasymetric
# reweighting to a region.
code_inds <- 1:7
names(code_inds) <- as.character(c(21:24, 41:43))

weighted_area <- function(poly, weights, nlcd_raster, summed=T){
  # Calculate the weighted area of a poly via dasymetric mapping
  X <- matrix(nrow=nrow(weights), ncol=7)
  area_mat <- expanse(crop(nlcd_raster, poly), byValue=TRUE)
  for(code in area_mat[,'value']){
    j <- code_inds[as.character(code)]
    X[,j] <- weights[,j] * area_mat[which(area_mat$value == code), 'area']
  }
  if(summed){
    return(rowSums(X, na.rm=T))
  }
  else {
    return(X)
  }
}

areal <- function(source_polys, target_poly){
  # Filter source_polys to only those containing overlaps with target_poly
  # create intersections between each source poly and the target poly, then
  # calculate areas
  source_polys <- source_polys[relate(source_polys, target_poly, 'intersects')]
  intersection_polys <- crop(source_polys, target_poly) %>%
    aggregate(by='GEOID', count=F)
  
  source_polys <- source_polys[which(source_polys$NAME %in% intersection_polys$NAME)]
  
  names(intersection_polys)[2] <- 'value'
  
  population_estimates <- rep(NA, length(source_polys))
  
  # Estimate
  for(id in source_polys$GEOID){
    source_ind <- which(source_polys$GEOID == id)
    intersection_ind <- which(intersection_polys$GEOID == id)
    
    population_estimates[source_ind] <- 
      source_polys$value[source_ind] * 
      expanse(intersection_polys[intersection_ind])/
      expanse(source_polys[source_ind])
  }

  estimate <- sum(population_estimates, na.rm=TRUE)
  return(estimate)
}

dasymetric <- function(nlcd_path, nlcd_raster=NULL, source_polys, target_poly, weights){
    # Load in tiff as raster, project to lon/lat, then transform into polys for
    # each land type. activeCat ensures that the value active when transforming
    # is in fact land code.
    
    # NOTE: tiff.aux.xml file needs to be in directory as well, not
    # just the tiff!
    if(is.null(nlcd_raster)){
      nlcd_raster <- project(rast(nlcd_path), 'EPSG:4326')
      activeCat(nlcd_raster) <- 0
    }
    # Filter source_polys to only those containing overlaps with target_poly
    # create intersections between each source poly and the target poly, then
    # calculate weighted area
    source_polys <- source_polys[relate(source_polys, target_poly, 'intersects')]
    
    # Set non-developed, non-woodland areas to NA by retaining all values between
    # 20 and 40 then dropping barren land which is in the middle
    nlcd_raster <- clamp(nlcd_raster, lower=21, upper=43, value=FALSE) %>%
      classify(matrix(c(31, NA), ncol=2)) %>%
      crop(ext(source_polys))
    
    # Create intersections, aggregate disjoint intersections based on their
    # parent source unit
    intersection_polys <- crop(source_polys, target_poly) %>%
      aggregate(by='GEOID', count=F)
    
    source_polys <- source_polys[which(source_polys$NAME %in% intersection_polys$NAME)]
    
    population_estimates <- matrix(nrow=3, ncol=length(source_polys))
    
    # Rename mean_value to more appropriate value, since it's mean of constant
    names(intersection_polys)[2] <- 'value'
    
    for(id in source_polys$GEOID){
      source_ind <- which(source_polys$GEOID == id)
      intersection_ind <- which(intersection_polys$GEOID == id)
      
      population_estimates[1:3,source_ind] <- 
        source_polys$value[source_ind] * 
        weighted_area(intersection_polys[intersection_ind], weights, nlcd_raster)/
        weighted_area(source_polys[source_ind], weights, nlcd_raster)
    }
    
    # Filter out NaN's introduced by weighted source_areas of 0 (i.e. they have
    # no developed land), then get average
    estimates <- rowSums(population_estimates, na.rm=TRUE)
    return(estimates)
}
