source("R/packages.R")
source("R/assist_vectors.R")
source("R/create_spp_mat_function.R")

# Data wrangling for MEData -----------------------------------------------

medata <- read_rds("data/medata.Rds")
str(medata)

# Create a dataset with relevant information, scaled covariates, and boolean MPA column:
med_clean <- medata %>%
  mutate(mpa = if_else(enforcement <= 1, FALSE, TRUE),
         temp = tmean,
         depth = depth,
         prod = pp_mean,
         biomass = (a*sp.length^b)*sp.n) %>%
  select(site, lon, lat, trans, species, sp.n, biomass, mpa, temp, depth, prod)

# Herbivores require another filtering:
medata %>% distinct(country)
med_clean_east <- medata %>%
  filter(country == "Israel" | country == "Greece" | country == "Turkey") %>% 
  mutate(mpa = if_else(enforcement <= 1, FALSE, TRUE),
         temp = tmean,
         depth = depth,
         prod = pp_mean,
         biomass = (a*sp.length^b)*sp.n) %>%
    select(site, lon, lat, trans, species, sp.n, biomass, mpa, temp, depth, prod)

# write_rds(med_clean, "data/processed/med_clean.rds")
# write_rds(med_clean_east, "data/processed/med_clean_east.rds")


## Abundance matrices ------------------------------------------------------

grps_mat <- create_spp_mat(dataset = med_clean, guild = groupers, metric = "sp.n", covariate = all_covs)
dip_mat <- create_spp_mat(dataset = med_clean, guild = diplodus, metric = "sp.n", covariate = all_covs)
herb_mat <- create_spp_mat(dataset = med_clean_east, guild = herbivores, metric = "sp.n", covariate = all_covs)

# Export species matrices
write_rds(grps_mat, "data/processed/grps_mat.rds")
write_rds(dip_mat, "data/processed/dip_mat.rds")
write_rds(herb_mat, "data/processed/herb_mat.rds")

## Biomass matrices ------------------------------------------------------

grps_mass_mat <- create_spp_mat(dataset = med_clean, guild = groupers, metric = "biomass", covariate = all_covs)
dip_mass_mat <- create_spp_mat(dataset = med_clean, guild = diplodus, metric = "biomass", covariate = all_covs)
herb_mass_mat <- create_spp_mat(dataset = med_clean_east, guild = herbivores, metric = "biomass", covariate = all_covs)


# Export species matrices
write_rds(grps_mass_mat, "data/processed/grps_mass_mat.rds")
write_rds(dip_mass_mat, "data/processed/dip_mass_mat.rds")
write_rds(herb_mass_mat, "data/processed/herb_mass_mat.rds")

# Create dataset relevant for my analysis ---------------------------------
# Long format, for checks etc.

my_data <- med_clean %>% 
  filter(species %in% c(groupers, diplodus, herbivores)) %>% 
  mutate(sp_group = case_when(species %in% groupers ~ "Groupers",
                              species %in% diplodus ~ "Seabreams",
                              species %in% herbivores ~ "Herbivores")) %>% 
  mutate(sp_group = factor(sp_group, levels = c("Groupers", "Seabreams", "Herbivores"))) %>% 
  mutate(col = case_when(sp_group == "Groupers" ~ "#c54607",
                         sp_group == "Seabreams" ~ "#145d82",
                         sp_group == "Herbivores" ~ "#43aa8b")) %>% 
  mutate(col = as_factor(col))

write_rds(my_data, "data/processed/my_data.rds")
