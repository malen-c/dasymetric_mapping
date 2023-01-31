library(tidyverse)
library(tidycensus)
library(terra)

source('dasymetric.R')

county <- get_decennial(geography = "county", state = 'WI', variables = "P013001", 
                        year = 2010, geometry=TRUE)
subdivisions <- get_decennial(geography = 'county subdivision', state = 'WI', county='063',
                        variables = 'P013001', year=2010, geometry=TRUE)

lacrosse <- county[str_detect(county$NAME, 'La Crosse'),]
weights <- rep(0, 95)
weights[23] <- 1
weights[22] <- .6

nlcd_path <- '../data/lacrosse.tiff'
source_polys <- vect(subdivisions)
target_poly <- minRect(source_polys[4:5])
stat <- 'value'

dasymetric_prediction(nlcd_path, source_polys, target_poly, weights, stat)