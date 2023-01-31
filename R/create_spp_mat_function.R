# library(tidyverse)

### Create species matrix (for MRFcov)
# This function turns an abundance/biomass dataset (long format) into a matrix.
# It also scales the numeric covariates for abundance matrix, and all values in biomass matrix.
create_spp_mat <- function(dataset, guild, metric, covariate){
  if (metric == "sp.n" | metric == "abundance") {
    dataset %>% 
      # keep observations only of relevant species
      filter(species %in% guild) %>% 
      # transform to a wide format (rows = transects, col = species + covariates)
      pivot_wider(names_from = species, values_from = sp.n, values_fill = 0) %>% 
      # keep only the relevant covariates
      select(all_of(guild), all_of(covariate)) %>% 
      # standard scale environmental covariates
      mutate(across(.cols = c(all_of(env_cov)), .fns = function(x){as.vector(scale(log10(x)))}))
  } else if(metric == "biomass") {
    dataset %>%
      # mutate(biomass = a*10^b) %>% # modify this in case the dataset doesn't contain biomass
      filter(species %in% guild) %>% 
      pivot_wider(names_from = species, values_from = biomass, values_fn = sum, values_fill = 0) %>% 
      select(all_of(guild), all_of(covariate)) %>% 
      mutate(across(.cols = c(all_of(guild), all_of(env_cov)), .fns = function(x){log10(x)})) %>% 
      mutate(across(all_of(guild), .fns = function(x){if_else(is.finite(x), x, 0)})) %>% 
      mutate(across(.cols = c(all_of(guild), all_of(env_cov)), .fns = function(x){as.vector(scale(x))}))
  } else {
    stop("Metric should be either 'sp.n', 'abundance' or 'biomass'")
  }
}

# Note: biomass in grams
