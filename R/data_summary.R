source("R/packages.R")
library(gt)
source("R/functions.R")
source("R/assist_vectors.R")

medata <- read_rds("data/medata.Rds")
my_data <- read_rds("data/processed/med_clean.rds") %>% 
  filter(species %in% c(guilds$groupers, guilds$diplodus, guilds$herbivores))

spatial_models <- read_rds("data/results/spatial_models.rds")

data_sum <- my_data %>% 
  select(site, lon, lat, unique_trans_id, mpa, temp, depth, prod) %>% 
  distinct()


# -------------------------------------------------------------------------


# Sites metadata and number of transects
data_sum %>% 
  group_by(site, mpa) %>% 
  summarise(temp_range = str_glue("{round(min(temp), 2)} - {round(max(temp), 2)}"),
            depth_range = str_glue("{round(min(depth), 1)} - {round(max(depth), 1)}"),
            prod_range = str_glue("{round(min(prod), 4)} - {round(max(prod), 4)}"),
            n_transects = n_distinct(unique_trans_id)) %>% 
  write.table("data/processed/sites_table.txt", sep = ",", quote = FALSE, row.names = FALSE)



# Summary of RI results

grps_key_coef_tbl <- bind_rows(spatial_models$grps_spat$key_coefs, .id = "species")
dip_key_coef_tbl <- bind_rows(spatial_models$dip_spat$key_coefs, .id = "species")
herb_key_coef_tbl <- bind_rows(spatial_models$herb_spat$key_coefs, .id = "species")

key_coef_tbl <- bind_rows(groupers = grps_key_coef_tbl, seabreams = dip_key_coef_tbl, herbivores = herb_key_coef_tbl, .id = "group")

key_coef_tbl %>% distinct(Variable)

## How many species were affected by environment?
key_coef_tbl %>% 
  filter(Variable %in% env_cov)

## How many species were affected by the interaction between MPA/env and other species?

key_coef_tbl %>% 
  filter(str_detect(Variable, "_"))

### Just the species...
key_coef_tbl %>% 
  filter(str_detect(Variable, "_")) %>% 
  distinct(species)

### With a 0.1 threshold?
key_coef_tbl %>% 
  filter(Rel_importance >= 0.1, str_detect(Variable, "_"))

key_coef_tbl %>% 
  filter(str_detect(Variable, "_")) %>% 
  distinct(species)

### Only in the seabreams group?
dip_key_coef_tbl %>% 
  filter(Rel_importance >= 0.1, str_detect(Variable, "."), !str_detect(Variable, "_")) %>% 
  distinct(species)

## Groupers
grps_key_coef_tbl

## Seabreams
dip_key_coef_tbl

## Herbivores
herb_key_coef_tbl