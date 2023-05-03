library(tidyverse)
library(tidycensus)
library(terra)

source('C:/users/malen/desktop/thesis/dasymetric_mapping/R/dasymetric.R')

amos_weights <- c(.05, 1, .85, 0, 0, 0, 0)
get_weights <- function(source_units, nlcd_path){
  # Setting all weights equal to 1 so weighted_areas just returns areas of diff land types,
  # setting names based on land codes
  null_weights <- t(matrix(rep(1,7)))
  source_polys <- vect(source_units)
  
  # Get right projection for nlcd
  nlcd_raster <- project(rast(nlcd_path), 'EPSG:4326')
  activeCat(nlcd_raster) <- 0
  
  # Remove all but developed and forested land from nlcd raster
  nlcd_raster <- clamp(nlcd_raster, lower=21, upper=43, value=FALSE) %>%
    classify(matrix(c(31, NA), ncol=2)) %>%
    crop(ext(source_polys))
  
  # Get matrix of amount of area per land category in each block group
  X <- matrix(nrow=length(source_polys), ncol=7)
  for(i in 1:length(source_polys)){
    X[i,] <- weighted_area(source_polys[i], null_weights, nlcd_raster,sum=F)/expanse(source_polys[i])
  }
  
  # Create vector of population in each block group
  Y <- source_polys$value/expanse(source_polys)
  
  weights <- rep(NA, 7)
  names(weights) <- c((21:24), (41:43))
  
  for(i in 1:7){
    inds <- which(X[,i] > .7)
    # Default to Amos weights if not enough data
    weights[i] <- ifelse(length(inds) <= 2, amos_weights[i], mean(Y[inds]))
  }
  return(weights)
}


#counties <- get_decennial(geography='county', state='OR', variables = "P1_001N", 
#                          year = 2020, cache=T)$GEOID %>%
#  substr(3, 5)


#mat <- matrix(nrow=0, ncol=8)
#for(county in counties){
#  source_units <- get_decennial(geography='block group', state='OR', county=county,
#                                variables = "P1_001N", year = 2020, geometry=T, cache=T) %>%
#    filter(str_detect(NAME, 'Block Group 0', negate=T))
#  mat <- rbind(mat, get_weights(source_units, paste0('C:/users/malen/desktop/oregon nlcd/', county, '/nlcd.tiff')))
#}

#weights <- rep(NA, 7)
#names(weights) <- c((21:24), (41:43))
# 
# for(i in 1:7){
#   inds <- which(mat[,i] > .8)
#   weights[i] <- mean(mat[inds, 8])
# }
# weights[5] <- mean(weights[6:7])
