library(tidyverse)
library(tidycensus)
library(terra)

blocks <- get_decennial(geography='block', state='Delaware', variables='P001001',
                        year=2010, geometry=TRUE) %>%
  mutate(group=str_match(NAME, 'Block Group (\\d+)')[,2],
         tract=str_match(NAME, 'Census Tract (\\d+)')[,2],
         tractgroup=paste0(tract,'.',group))

blocks$area <- expanse(vect(blocks), unit='km')

blocks <- blocks %>%
  sf::st_drop_geometry()

# Note: block group 0 in each census tract appears (at least for Delaware) to 
# be a collection of high area, 0 population blocks, perhaps the ocean or
# forests

blocks %>%
  group_by(group) %>%
  summarize(area=mean(area), pop=mean(value), pop_sd=sd(value),
            n=n())
alpha <- 0.427918
beta <- 16.0041
data.frame(x = rgamma(24000, shape=alpha, rate=beta)) %>%
  ggplot(aes(x=x)) +
  geom_histogram()

census_area_hist <- function(geography, color='blue', bins=30, dpi=300){
  census_data <- get_decennial(geography=geography, state='Delaware', variables = "P001001", 
                               year=2010, geometry=TRUE) %>%
    filter(value > 0, !str_detect('Census Tract 0', NAME))
  plot <- data.frame(x = expanse(vect(census_data), unit='km')) %>%
    sf::st_drop_geometry() %>%
    ggplot(aes(x=x)) +
    geom_histogram(fill=color, color='white', bins=bins) +
    labs(x=expression('Area in'~km^2), y='')
  ggsave(paste0(filename='viz/', geography, '_areas_histogram.png'), 
         plot=plot, device='png', dpi=dpi)
}



census_area_hist('block')
