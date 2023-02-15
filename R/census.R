library(tidyverse)
library(tidycensus)
library(terra)

source('dasymetric.R')

county <- get_decennial(geography = "county", state = 'WI', variables = "P001001", 
                        year = 2010, geometry=TRUE, cache=TRUE)
subdivisions <- get_decennial(geography = 'county subdivision', state = 'WI', county='063',
                        variables = 'P001001', year=2010, geometry=TRUE, cache=TRUE)

lacrosse <- county[str_detect(county$NAME, 'La Crosse'),]
weights <- c(0, .6, 1, 0)

nlcd_path <- '../data/lacrosse.tiff'
source_polys <- vect(subdivisions)
target_poly <- vect(lacrosse)

dasymetric(nlcd_path, source_polys, target_poly, weights)
areal(source_polys, target_poly)
