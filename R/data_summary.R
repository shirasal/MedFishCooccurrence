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

data_sum %>% 
  count(site, mpa, temp, depth) %>% 
  gt() %>% 
  tab_header(
    title = "Sites summary",
    subtitle = "Only transects with at least one the focal species of either group (groupers, seabreams or herbivores) are included."
  ) %>% 
  fmt_number(
    columns = c(temp, depth), 
    ) %>% 
  fmt_integer(
    columns = n
  ) %>% 
  cols_align(align = "left") %>% 
  cols_label(site = md("**Site**"), 
             mpa = md("**MPA**"), 
             temp = md("**Temperature (SST)**"), 
             depth = md("**Depth**"), 
             n = md("**n Transects**")) %>% 
  opt_align_table_header(align = "center") %>% gtsave(filename = "sites_table.rtf", path = "data/processed/")

# data_sum %>% 
#   count(site, mpa, temp, depth) %>% 
#   write_csv("data/processed/sites_table.csv")

