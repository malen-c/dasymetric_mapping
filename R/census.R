library(tidyverse)
library(tidycensus)
library(terra)

source('r/dasymetric.R')

county <- get_decennial(geography = "county", state = 'WI', variables = "P013001", 
                        year = 2010, geometry=TRUE)
subs <- get_decennial(geography = 'county subdivision', state = 'WI', county='063',
                        variables = 'P013001', year=2010, geometry=TRUE)

lacrosse <- county[str_detect(county$NAME, 'La Crosse'),]
weights <- rep(0, 95)
weights[23] <- 1
weights[22] <- .6

dasymetric_prediction('data/WI_nlcd.tiff', vect(subs), vect(lacrosse),
                      weights=weights, stat='value')
