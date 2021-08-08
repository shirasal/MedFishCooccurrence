# library(tidyverse)

### Create species matrix (for MRFcov)
# This function turns an abundance/biomass dataset (long format) into a matrix
# It also scales the numeric covariates for abundance matrix, and all values in biomass matrix
create_spp_mat <- function(dataset, guild, metric, covariate){
  cols <- c(c("lat", "lon", "site", "trans", "species"), env_cov, mpa_cov)
  if (metric == "sp.n" | metric == "abundance") {
    dataset %>% 
      group_by_at(.vars = cols) %>%
      summarise(n = sum(sp.n), .groups = "drop") %>% 
      spread(species, n, fill = 0) %>% 
      # Add unique rownames that describe the site and transect:
      mutate(loc = paste(site, trans)) %>% 
      group_by(loc) %>%
      column_to_rownames("loc") %>% 
      filter(!is.na(depth)) %>% 
      select(all_of(guild), all_of(covariate)) %>% 
      mutate(across(.cols = all_of(env_cov), .fns = function(x){as.vector(scale(x))})) %>% 
      ungroup()
  } else if(metric == "biomass") {
    dataset %>%
      group_by_at(.vars = cols) %>%
      summarise(n = sum(biomass), .groups = "drop") %>% 
      spread(species, n, fill = 0) %>% 
      # Add unique rownames that describe the site and transect:
      mutate(loc = paste(site, trans)) %>% 
      group_by(loc) %>%
      column_to_rownames("loc") %>% 
      filter(!is.na(depth)) %>% 
      select(all_of(guild), all_of(covariate)) %>% 
      mutate(across(.cols = everything(), .fns = function(x){as.vector(scale(x))})) %>% 
      ungroup()
  } else {
    stop("Metric should be either 'sp.n', 'abundance' or 'biomass'")
  }
}
