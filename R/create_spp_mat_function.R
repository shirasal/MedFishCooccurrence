# library(tidyverse)

### Create species matrix (for MRFcov)
# This function turns an abundance/biomass dataset (long format) into a matrix
# It also scales the numeric covariates for abundance matrix, and all values in biomass matrix
create_spp_mat <- function(dataset, guild, metric, covariate){
  if (metric == "sp.n" | metric == "abundance") {
    dataset %>% 
      filter(species %in% guild,
             !is.na(depth)) %>% 
      pivot_wider(names_from = species, values_from = sp.n, values_fn = function(x){sum(x, na.rm = TRUE)}, values_fill = 0) %>% 
      select(all_of(guild), all_of(covariate)) %>% 
      mutate(across(.cols = c(all_of(guild),all_of(env_cov)), .fns = function(x){as.vector(scale(x))}))
  } else if(metric == "biomass") {
    dataset %>%
      filter(species %in% guild,
             !is.na(depth)) %>% 
      pivot_wider(names_from = species, values_from = biomass, values_fn = function(x){sum(x, na.rm = TRUE)}, values_fill = 0) %>% 
      select(all_of(guild), all_of(covariate)) %>% 
      mutate(across(.cols = c(all_of(guild),all_of(env_cov)), .fns = function(x){as.vector(scale(x))}))
  } else {
    stop("Metric should be either 'sp.n', 'abundance' or 'biomass'")
  }
}
