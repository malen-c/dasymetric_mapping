library(tidyverse)
library(terra)

#setwd('c:/users/malen/desktop/results/')
#filenames <- list.files(pattern = '\\.csv', full.names = TRUE)
#results <- purrr::map_df(filenames, read_csv)
#write.csv(results, 'c:/users/malen/desktop/results.csv', row.names = F)

results <- read_csv('c:/users/malen/desktop/results.csv') %>%
  group_by(county) %>%
  mutate(amos=amos * (sum(population)/sum(amos)), 
         pooled=pooled * (sum(population)/sum(pooled)), 
         individual=individual * (sum(population)/sum(individual)),
         areal=areal * (sum(population)/sum(areal)))

# counties <- substr(list.files('c:/users/malen/desktop/results/'), 1, 3)
# nlcd_path <- 'c:/users/malen/desktop/oregon nlcd/'
# geometries <- vect()
# 
# code_inds <- 1:7
# names(code_inds) <- as.character(c(21:24, 41:43))
# 
# for(county in counties){
#   print('')
#   print(paste('Starting', county, '...'))
#   geometry <- vect(paste0('c:/users/malen/desktop/fakeblockgroups/',
#                           county,
#                           '.shp'))
# 
#   nlcd_raster <- project(rast(paste0(nlcd_path, county, '/nlcd.tiff')), 'EPSG:4326')
#   activeCat(nlcd_raster) <- 0
#   nlcd_raster <- clamp(nlcd_raster, lower=21, upper=43, value=FALSE) %>%
#     classify(matrix(c(31, NA), ncol=2)) %>%
#     crop(ext(geometry))
# 
#   X <- matrix(0.,
#               nrow=nrow(geometry),
#               ncol=7,
#               dimnames = list(1:nrow(geometry), paste0('X', as.character(c(21:24, 41:43)))))
#   for(i in 1:nrow(geometry)){
#     cat(paste0(as.character(i), '/', nrow(geometry), '...'))
#     area <- expanse(crop(nlcd_raster, geometry[i]))[,2]
#     area_mat <- expanse(crop(nlcd_raster, geometry[i]), byValue=TRUE)
#     for(code in area_mat[,'value']){
#       j <- code_inds[as.character(code)]
#       X[i,j] <- area_mat[which(area_mat$value == code), 'area']/area
#     }
#   }
#   X <- X
# 
#   geometry$id <- 1:nrow(geometry)
#   geometry$county <- county
#   values(geometry) <- cbind(values(geometry), X)
# 
#   geometries <- rbind(geometries, geometry)
# }

# writeVector(geometries, 'c:/users/malen/desktop/vec/vec.shp')

df <- sf::st_read('c:/users/malen/desktop/vec/vec.shp')

pdf <- cbind(df, results[,5:8]-results$population)
npdf <- sf::st_drop_geometry(pdf)

mult_raster <- rast('c:/users/malen/desktop/oregon nlcd/051/nlcd.tiff') %>%
  project('EPSG:4326') %>%
  crop(geometries[which(geometries$county=='051'),])

# Developed land plot
df %>%
  filter(county=='051') %>%
  ggplot(aes(fill=(X21))) +
  geom_sf() +
  ggthemes::scale_fill_continuous_tableau(palette = 'Classic Red') +
  labs(fill='') +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank())

# ggsave(filename='c:/users/malen/desktop/mult_dev.png', p, device = 'png', dpi = 360,
#        width = 10, height=5)

# one of these for each weighting scheme
p <- pdf %>%
  filter(county=='051') %>%
  ggplot(aes(fill=abs(amos))) +
  geom_sf() +
  scale_fill_continuous(type='viridis', limits=c(0, 2000)) +
  labs(fill='') +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        plot.margin = unit(c(-2, 0, -2, -.1), "cm"))

ggsave(filename='c:/users/malen/desktop/mult_amos.png', p, device = 'png', dpi = 360,
       width = 10)

# Plots of error rates by land cover types
sum_results <- npdf %>% 
  filter(!(county %in% c('021', '055', '069'))) %>%
  group_by(county) %>% 
  summarize(amos=mean(abs(amos)),
            areal=mean(abs(areal)),
            individual=mean(abs(individual)),
            pooled=mean(abs(pooled)),
            population=sum(value),
            n_size = mean(n))

summary(lm(amos ~ n_size, data=sum_results))

# Making figure of distribution of n_blocks and population per block group for
# both true and fake blocks
true_bgs <- data.frame(n = NULL, population = NULL)
for(county in counties){
  d <- get_decennial(geography='block', state='OR', county=county, variables = "P1_001N", 
                     year = 2020) %>%
    filter(str_detect(NAME, 'Block Group 0', negate=T))
  d$group <- substr(d$GEOID, 1, 12)
  true_bgs <- d %>%
    group_by(group) %>%
    summarize(n = n(), population=sum(value)) %>%
    select(-group) %>%
    rbind(true_bgs)
}

blegh <- data.frame(rorf=c(rep('Real', nrow(true_bgs)), rep('Fake', nrow(npdf))),
                    n=c(true_bgs$n, results$n_blocks),
                    population=c(true_bgs$population, results$population))

blegh %>%
  filter(n<500) %>%
  ggplot(aes(x=n, color=rorf, fill=rorf)) +
  geom_density(linewidth=1.5, alpha=0.5) +
  labs(x='Number of blocks in group', y='Density', color='', fill='')

################################################################################
# 
# library(tidycensus)
# 
# fake_block_path <- 'c:/users/malen/desktop/fakeblockgroups/'
# 
# counties <- get_decennial(geography='county', state='OR', variables = "P1_001N", 
#                           year = 2020, cache=T)$GEOID %>%
#   substr(3, 5)
# 
# missing <- c('051', '067')
# counties <- counties[-which(counties %in% missing)]
# 
# for(county in counties){
#   target_units <- vect(paste0(fake_block_path, county, '.shp'))
#   df <- read.csv(paste0('c:/users/malen/desktop/amos_results/', county, '.csv')) %>%
#     mutate(county=county, population=target_units$value, n_blocks=target_units$n,
#            dasymetric_prediction=target_units$value-dasymetric_error)
#   
#   write.csv(x = df,
#             paste0('c:/users/malen/desktop/amos_results/', county, '.csv'),
#             row.names = F)
# }
  