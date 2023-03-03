source("R/packages.R")
source("R/functions.R")
source("R/assist_vectors.R")

medata <- read_rds("data/medata.Rds")
my_data <- read_rds("data/processed/med_clean.rds") %>% 
  filter(species %in% c(guilds$groupers, guilds$diplodus, guilds$herbivores))

data_sum <- my_data %>% 
  select(site, lon, lat, unique_trans_id, mpa, temp, depth, prod) %>% 
  distinct()

data_sum %>% write_csv("data/processed/sites_table.csv")

