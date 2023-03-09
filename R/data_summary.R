source("R/packages.R")
library(gt)
source("R/functions.R")
source("R/assist_vectors.R")

medata <- read_rds("data/medata.Rds")
my_data <- read_rds("data/processed/med_clean.rds") %>% 
  filter(species %in% c(guilds$groupers, guilds$diplodus, guilds$herbivores))

data_sum <- my_data %>% 
  select(site, lon, lat, unique_trans_id, mpa, temp, depth, prod) %>% 
  distinct()

# Sites metadata and number of transects
data_sum %>% 
  group_by(site, mpa) %>% 
  summarise(temp_range = str_glue("{round(min(temp), 2)} - {round(max(temp), 2)}"),
            depth_range = str_glue("{round(min(depth), 1)} - {round(max(depth), 1)}"),
            prod_range = str_glue("{round(min(prod), 4)} - {round(max(prod), 4)}"),
            n_transects = n_distinct(unique_trans_id)) %>% 
  write.table("data/processed/sites_table.txt", sep = ",", quote = FALSE, row.names = FALSE)
