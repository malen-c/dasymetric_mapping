library(tidyverse)
library(tidycensus)

############ DATA SECTION #################

# i
set.seed(1234)

# List of US counties (see https://walker-data.com/tidycensus/reference/fips_codes.html, accessed 3/4/2023)
data(fips_codes)
inds <- sample(1:nrow(fips_codes), 100)
block_counts <- rep(NA, 100)

for(i in 1:100){
  print(i)
  block_counts[i] <- get_decennial(geography='block', 
                                   state=fips_codes$state_code[inds[i]], 
                                   county=fips_codes$county_code[inds[i]],
                                   variables = 'P1_001N', year=2020) %>%
    nrow()
}

# 2131.465
mean(block_counts)
