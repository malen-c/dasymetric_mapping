library(tidyverse)
library(tidycensus)
library(terra)

census_area_hist <- function(geography, color='blue', bins=30, dpi=300){
  census_data <- get_decennial(geography=geography, state='Delaware', variables = "P001001", 
                               year=2010, geometry=TRUE) %>%
    filter(value > 0)
  plot <- data.frame(x = expanse(vect(census_data), unit='km')) %>%
    ggplot(aes(x=x)) +
    geom_histogram(fill=color, color='white', bins=bins) +
    labs(x=expression('Area in'~km^2), y='')
  ggsave(paste0(filename='viz/', geography, '_areas_histogram.png'), 
         plot=plot, device='png', dpi=dpi)
}

census_area_hist('block')
