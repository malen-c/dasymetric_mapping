library(tidyverse)
library(tidycensus)
library(terra)

source('c:/users/malen/desktop/thesis/dasymetric_mapping/R/dasymetric.R')
source('c:/users/malen/desktop/thesis/dasymetric_mapping/R/weight_selection.R')

fake_block_path <- 'c:/users/malen/desktop/fakeblockgroups/'
raster_path <- 'c:/users/malen/desktop/oregon nlcd/'

counties <- get_decennial(geography='county', state='OR', variables = "P1_001N", 
                          year = 2020, cache=T)$GEOID %>%
  substr(3, 5)

# Check which are already done
counties <- counties[!(counties %in% substr(list.files('c:/users/malen/desktop/results'), 1, 3))]

amos_weights <- c(.05, 1, .85, 0, 0, 0, 0)
pooled_weights <- c(1.107205e-03, 1.841542e-03, 2.889976e-03, 6.097145e-03, 0, 6.118587e-05, 1.452174e-04)

weights <- rbind(amos_weights, pooled_weights, NA)

for(county in counties){
  source_units <- get_decennial(geography='block group', state=41, county=county, variables='P1_001N',
                                year=2020, geometry=T, cache=T) %>%
    filter(str_detect(NAME, 'Block Group 0', negate=T))
  target_units <- vect(paste0(fake_block_path, county, '.shp'))
  nlcd_path <- paste0(raster_path, county, '/nlcd.tiff')
  
  nlcd_raster <- project(rast(nlcd_path), 'EPSG:4326')
  activeCat(nlcd_raster) <- 0
  
  weights[3,] <- get_weights(source_units, nlcd_path)
  
  estimates <- matrix(nrow=4, ncol=length(target_units))
  
  for(i in 1:length(target_units)){
    print(paste0(i, '/', length(target_units)))
    estimates[1:3,i] <- 
      plyr::try_default(dasymetric(nlcd_raster=nlcd_raster, 
                                   source_polys = vect(source_units), 
                                   target_poly = target_units[i], 
                                   weights = weights), 
                        NA)
    estimates[4,i] <- plyr::try_default(areal(vect(source_units), target_units[i]), NA)
  }
  out <- data.frame(county=county,
                    population=target_units$value,
                    n_blocks=target_units$n,
                    id=1:length(target_units),
                    t(estimates))
  colnames(out)[5:8] <- c('amos', 'pooled', 'individual', 'areal')
  write.csv(out, paste0('c:/users/malen/desktop/results/', county, '.csv'), row.names = F)
}
