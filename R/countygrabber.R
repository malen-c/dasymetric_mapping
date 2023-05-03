library(tidycensus)
library(magrittr)
library(terra)

geoids <- get_decennial(geography='county', state='OR', variables = "P1_001N", 
                        year = 2020)$GEOID %>%
  substr(3, 5)

for(i in geoids[1]){
  blocks <- get_decennial(geography='block', 
                          state='OR', 
                          county=i, 
                          variables = "P1_001N", 
                          year = 2020, 
                          geometry=T)
  blocks$NAME <- stringr::str_extract(blocks$NAME, 'Block Group (\\d), Census Tract (\\d)+')
  writeVector(vect(blocks), paste0('C:/Users/Malen/Desktop/testy test/', i, '.shp'),
              overwrite=T)
}
