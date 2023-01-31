source("R/packages.R")
source("R/assist_vectors.R")
source("R/create_spp_mat_function.R")

# Data wrangling for MEData -----------------------------------------------

medata <- read_rds("data/medata.Rds")
str(medata)

# Create my base data set:
# Includes only relevant columns, scaled covariates, and boolean MPA column
# Without asinara_add site, as it is presence-absence data
# Without missing data in relevant columns: protection (+enfocement), tmean (+sal, +pp), depth, sp.length, a (+b):
med_clean <- medata %>%
  filter(site != "asinara_add") %>% 
  filter(!is.na(protection), !is.na(tmean), !is.na(depth), !is.na(sp.length), !is.na(a)) %>% 
  mutate(mpa = if_else(as.integer(enforcement) <= 1, FALSE, TRUE),
         temp = tmean,
         depth = depth,
         prod = pp_mean,
         biomass = (a*sp.length^b)*sp.n) %>%
  group_by(across(c(-sp.n, -biomass))) %>% 
  summarise(abund = sum(sp.n),
            biomass = sum(biomass), .groups = "drop") %>%
  select(site, lon, lat, unique_trans_id, species, sp.n = abund, biomass, mpa, temp, depth, prod)

# Herbivores require another filtering - keeping only the eastern Mediterranean, where there is their distribution overlaps:
medata %>% distinct(country)
med_clean_east <- medata %>%
  filter(site != "asinara_add") %>% 
  filter(!is.na(protection), !is.na(tmean), !is.na(depth), !is.na(sp.length), !is.na(a)) %>% 
  filter(country %in% c("Turkey", "Greece", "Israel", "Cyprus")) %>% 
  mutate(mpa = if_else(as.integer(enforcement) <= 1, FALSE, TRUE),
         temp = tmean,
         depth = depth,
         prod = pp_mean,
         biomass = (a*sp.length^b)*sp.n) %>%
  group_by(across(c(-sp.n, -biomass))) %>% 
  summarise(abund = sum(sp.n),
            biomass = sum(biomass), .groups = "drop") %>%
  select(site, lon, lat, unique_trans_id, species, sp.n = abund, biomass, mpa, temp, depth, prod)

sapply(med_clean, function(x) sum(is.na(x)))
sapply(med_clean_east, function(x) sum(is.na(x)))

# Export medata
write_rds(med_clean, "data/processed/med_clean.rds")
write_rds(med_clean_east, "data/processed/med_clean_east.rds")

# Create dataset relevant for my analysis ---------------------------------
# Long format, for checks etc.

my_data <- medata %>%
  filter(site != "asinara_add") %>% 
  filter(!is.na(protection), !is.na(tmean), !is.na(depth), !is.na(sp.length), !is.na(a)) %>% 
  mutate(mpa = if_else(as.integer(enforcement) <= 1, FALSE, TRUE),
         temp = tmean,
         depth = depth,
         prod = pp_mean,
         biomass = (a*sp.length^b)*sp.n) %>%
  group_by(across(c(-sp.n, -biomass))) %>% 
  summarise(abund = sum(sp.n),
            biomass = sum(biomass), .groups = "drop") %>%
  select(site, lon, lat, unique_trans_id, species, sp.n = abund, biomass, mpa, temp, depth, prod) %>% 
  mutate(sp_group = case_when(species %in% guilds$groupers ~ "Groupers",
                              species %in% guilds$diplodus ~ "Seabreams",
                              species %in% guilds$herbivores ~ "Herbivores")) %>% 
  filter(!is.na(sp_group)) %>% 
  mutate(sp_group = factor(sp_group, levels = c("Groupers", "Seabreams", "Herbivores"))) %>% 
  mutate(col = case_when(sp_group == "Groupers" ~ "#c54607",
                         sp_group == "Seabreams" ~ "#145d82",
                         sp_group == "Herbivores" ~ "#43aa8b")) %>% 
  mutate(col = as_factor(col))

write_rds(my_data, "data/processed/my_data.rds")


# Create species matrices for analysis ------------------------------------

grps_mat <- create_spp_mat(dataset = med_clean, guild = groupers, metric = "biomass", covariate = all_covs)
dip_mat <- create_spp_mat(dataset = med_clean, guild = diplodus, metric = "biomass", covariate = all_covs)
herb_mat <- create_spp_mat(dataset = med_clean_east, guild = herbivores, metric = "biomass", covariate = all_covs)

write_rds(grps_mat, "data/processed/grps_mat.rds")
write_rds(dip_mat, "data/processed/dip_mat.rds")
write_rds(herb_mat, "data/processed/herb_mat.rds")
