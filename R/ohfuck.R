library(tidyverse)
library(tidycensus)
library(terra)

counties <- get_decennial(geography='county', state='OR', variables='P1_001N',
                          year = 2020)$GEOID %>%
  substr(3, 5)

outdir <- 'C:/users/malen/Desktop/fakeblockgroups/'

for(county in c('045')){
  #if (county %in% substr(list.files(outdir), 1, 3)){
  #  next 
  #}
  
  print(paste0('Starting ', county, '...'))
  df <- get_decennial(geography='block', state='OR', county=county, variables = "P1_001N", 
                      year = 2020, geometry=T) %>%
    filter(str_detect(NAME, 'Block Group 0', negate=T))
    
  df$group <- substr(df$GEOID, 1, 12)
  
  if (abs(ughh[county] - length(unique(df$group))) < 10){
    next
  }
  
  summary_table <- df %>%
    sf::st_drop_geometry() %>%
    group_by(group) %>%
    summarize(n=n(), total_pop=sum(value))
  group_populations <- summary_table$total_pop
  group_n <- summary_table$n
  
  df <- select(df, value)
  
  stopper <- function(pop, n){
    pop_condition <- (runif(1) < mean(group_populations <= pop))
    n_condition <- (runif(1) < mean(group_n <= n))
    return(pop_condition & n_condition)
  }
  
  vdf <- vect(df)
  vdf$n <- 1
  pop_limit <- 1.5 * max(vdf$value)
  
  n <- 0
  # While any unaggregated blocks remain
  while(any(vdf$n <= min(group_n))){
    print(n)
    i <- sample(which(vdf$n <= min(group_n)), 1)
    current <- vdf[i]
    vdf <- vdf[-i]
    
    current_pop <- current$value
    current_n <- current$n
    
    while(!stopper(current_pop, current_n)){
      #### Find neighbors
      neighbor_inds <- which(relate(current, vdf, relation='touches'))
      # plot(current+vdf[neighbor_inds], col=c('green', rep('red', length(neighbor_inds))))
      # readline()
      
      #### Select random neighbor to subsume
      # There must be some neighbor, under the limit, if not stop
      if (any(current_pop + vdf[neighbor_inds]$value < pop_limit)){
        # Can't exceed population limit (1.5 * max observed population)
        neighbor_inds <- neighbor_inds[which(current_pop + vdf[neighbor_inds]$value < pop_limit)]
        # Grab nearest
        j <- neighbor_inds[nearby(current, vdf[neighbor_inds])[,2]]
        
        # Update current
        current <- aggregate(current + vdf[j])
        current_pop <- current_pop + vdf[j]$value
        current_n <- current_n + vdf[j]$n
        
        # Alter vdf to reflect the combination
        vdf <- vdf[-j]
      }
      # Drop requirements for high repetitions
      else if (n > 300){
        j <- neighbor_inds[nearby(current, vdf[neighbor_inds])[,2]]
        
        # Update current
        current <- aggregate(current + vdf[j])
        current_pop <- current_pop + vdf[j]$value
        current_n <- current_n + vdf[j]$n
        
        # Alter vdf to reflect the combination
        vdf <- vdf[-j]
      }
    }
    
    current$value <- current_pop
    current$n <- current_n
    vdf <- rbind(vdf, current)
    n <- n+1
  }
  writeVector(vdf, filename=paste0(outdir, county, '.shp'), overwrite=T)
}
ughh <- data.frame(county=counties, fake=NA, real=NA)

for(i in 1:length(counties)){
  county_n <- get_decennial(geography='block group', state='OR', county=counties[i], variables = "P1_001N", 
                year = 2020) %>% nrow()
  target_units <- vect(paste0(outdir, counties[i], '.shp'))
  ughh[i, 2] <- nrow(target_units)
  ughh[i, 3] <- county_n
}
