source("R/packages.R")


# Data wrangling for MEData -----------------------------------------------

medata <- read_rds("data/medata.Rds")
str(medata)

# Create a dataset with relevant information, scaled covariates, and boolean MPA column:
med_clean <- medata %>%
  filter(data.origin != "azz_asi") %>% # presence-absence
  mutate(mpa = if_else(enforcement <= 1, FALSE, TRUE),
         temp = as.vector(scale(tmean)),
         depth = as.vector(scale(depth)),
         sal = as.vector(scale(sal_mean)),
         prod = as.vector(scale(pp_mean)),
         biomass = a*sp.length^b) %>%
  select(site, lon, lat, trans, species, sp.n, biomass, mpa, temp, depth, prod)

# Herbivores require another filtering:
medata %>% distinct(country)
med_clean_east <- medata %>%
  filter(country == "Israel" | country == "Greece" | country == "Turkey") %>% 
  filter(data.origin != "azz_asi") %>% # presence-absence
  mutate(mpa = if_else(enforcement <= 1, FALSE, TRUE),
         temp = as.vector(scale(tmean)),
         depth = as.vector(scale(depth)),
         sal = as.vector(scale(sal_mean)),
         prod = as.vector(scale(pp_mean)),
         biomass = a*sp.length^b) %>%
    select(site, lon, lat, trans, species, sp.n, biomass, mpa, temp, depth, prod)

write_rds(med_clean, "data/processed/med_clean.rds")
write_rds(med_clean_east, "data/processed/med_clean_east.rds")

# Create assisting vectors ------------------------------------------------

# Guilds
groupers <- c("Epinephelus.costae", "Epinephelus.marginatus", "Serranus.cabrilla", "Serranus.scriba")
diplodus <- c("Diplodus.annularis", "Diplodus.puntazzo", "Diplodus.sargus", "Diplodus.vulgaris")
herbivores <- c("Siganus.rivulatus", "Siganus.luridus", "Sarpa.salpa", "Sparisoma.cretense")

# Covariates (based on med_clean column names)
env_cov <- c("temp", "depth", "prod")
mpa_cov <- "mpa"


# Create species matrix (for MRFcov) --------------------------------------

# Define function
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
      select(all_of(guild), all_of(covariate)) %>% 
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
      select(all_of(guild), all_of(covariate)) %>% 
      ungroup()  
  } else {
    stop("Metric should be either 'sp.n', 'abundance' or 'biomass'")
  }
}

# Combine covariate vectors:
all_covs <- c(env_cov, mpa_cov)
write_rds(all_covs, "data/all_covs.rds")


## Abundance matrices ------------------------------------------------------

grps_mat <- create_spp_mat(dataset = med_clean, guild = groupers, metric = "sp.n", covariate = all_covs)
dip_mat <- create_spp_mat(dataset = med_clean, guild = diplodus, metric = "sp.n", covariate = all_covs)
herb_mat <- create_spp_mat(dataset = med_clean_east, guild = herbivores, metric = "sp.n", covariate = all_covs)

### Remove NAs -------------------------------------------------------------
# Remove NAs, but keep track of what was removed:
grps_NAs <- grps_mat %>% as_tibble(rownames = NA) %>% rownames_to_column("ID") %>% 
  filter(is.na(temp) | is.na(depth) | is.na(prod) | is.na(mpa)) %>% 
  select(ID, all_of(all_covs))
nrow(grps_NAs) # 529 observations removed from analysis due to missing information
grps_mat %<>% filter(!is.na(temp), !is.na(depth), !is.na(prod), !is.na(mpa))

dip_NAs <- dip_mat %>% as_tibble(rownames = NA) %>% rownames_to_column("ID") %>% 
  filter(is.na(temp) | is.na(depth) | is.na(prod) | is.na(mpa)) %>% 
  select(ID, all_of(all_covs))
nrow(dip_NAs) # 529 observations removed from analysis due to missing information
dip_mat %<>% filter(!is.na(temp), !is.na(depth), !is.na(prod), !is.na(mpa))

herb_NAs <- herb_mat %>% as_tibble(rownames = NA) %>% rownames_to_column("ID") %>% 
  filter(is.na(temp) | is.na(depth) | is.na(prod) | is.na(mpa)) %>% 
  select(ID, all_of(all_covs))
nrow(dip_NAs) # 529 observations removed from analysis due to missing information
herb_mat %<>% filter(!is.na(temp), !is.na(depth), !is.na(prod), !is.na(mpa))

setdiff(grps_NAs, dip_NAs, herb_NAs) # They're all the same, so I can just keep one

locations_removed <- grps_NAs %>% distinct()
rm(grps_NAs, dip_NAs, herb_NAs)

# Export this list of removed locations:
write_csv(locations_removed, "issues/locations_removed.csv")

# Export species matrices
write_rds(grps_mat, "data/processed/grps_mat.rds")
write_rds(dip_mat, "data/processed/dip_mat.rds")
write_rds(herb_mat, "data/processed/herb_mat.rds")

## Biomass matrices ------------------------------------------------------

grps_mass_mat <- create_spp_mat(dataset = med_clean, guild = groupers, metric = "biomass", covariate = all_covs)
dip_mass_mat <- create_spp_mat(dataset = med_clean, guild = diplodus, metric = "biomass", covariate = all_covs)
herb_mass_mat <- create_spp_mat(dataset = med_clean_east, guild = herbivores, metric = "biomass", covariate = all_covs)

### Remove NAs -------------------------------------------------------------
# Remove NAs, but keep track of what was removed:
grps_NAs <- grps_mass_mat %>% as_tibble(rownames = NA) %>% rownames_to_column("ID") %>% 
  filter(is.na(temp) | is.na(depth) | is.na(prod) | is.na(mpa)) %>% 
  select(ID, all_of(all_covs))
nrow(grps_NAs) # 529 observations removed from analysis due to missing information
grps_mass_mat %<>% filter(!is.na(temp), !is.na(depth), !is.na(prod), !is.na(mpa))

dip_NAs <- dip_mass_mat %>% as_tibble(rownames = NA) %>% rownames_to_column("ID") %>% 
  filter(is.na(temp) | is.na(depth) | is.na(prod) | is.na(mpa)) %>% 
  select(ID, all_of(all_covs))
nrow(dip_NAs) # 529 observations removed from analysis due to missing information
dip_mass_mat %<>% filter(!is.na(temp), !is.na(depth), !is.na(prod), !is.na(mpa))

herb_NAs <- herb_mass_mat %>% as_tibble(rownames = NA) %>% rownames_to_column("ID") %>% 
  filter(is.na(temp) | is.na(depth) | is.na(prod) | is.na(mpa)) %>% 
  select(ID, all_of(all_covs))
nrow(dip_NAs) # 529 observations removed from analysis due to missing information
herb_mass_mat %<>% filter(!is.na(temp), !is.na(depth), !is.na(prod), !is.na(mpa))

setdiff(setdiff(grps_NAs, dip_NAs, herb_NAs), locations_removed) 
# They're all the same, so I can just keep the one from before

rm(grps_NAs, dip_NAs, herb_NAs)

# Export species matrices
write_rds(grps_mass_mat, "data/processed/grps_mass_mat.rds")
write_rds(dip_mass_mat, "data/processed/dip_mass_mat.rds")
write_rds(herb_mass_mat, "data/processed/herb_mass_mat.rds")

# Graphics ----------------------------------------------------------------

guild_colours <- list(grps = "#c54607", dip = "#145d82", herb = "#43aa8b")
write_rds(guild_colours, "data/processed/guild_colours.rds")
