library(tidyverse)

med_clean <- read_rds("data/processed/med_clean.rds")
med_clean_east <- read_rds("data/processed/med_clean_east.rds")

egg::ggarrange(med_clean %>% filter(species %in% groupers) %>% ggplot() + aes(x = species, y = biomass) + geom_boxplot(),
          med_clean %>% filter(species %in% groupers) %>% ggplot() + aes(x = species, y = sp.n) + geom_boxplot())


egg::ggarrange(med_clean %>% filter(species %in% diplodus) %>% ggplot() + aes(x = species, y = biomass) + geom_boxplot(),
               med_clean %>% filter(species %in% diplodus) %>% ggplot() + aes(x = species, y = sp.n) + geom_boxplot())


egg::ggarrange(med_clean %>% filter(species %in% herbivores) %>% ggplot() + aes(x = species, y = biomass) + geom_boxplot(),
               med_clean %>% filter(species %in% herbivores) %>% ggplot() + aes(x = species, y = sp.n) + geom_boxplot())


my_data <- read_rds("data/processed/my_data.rds")

my_data %>% 
  filter(species == diplodus[4]) %>% 
  ggplot() + aes(x = biomass, y = abundance) + geom_point() + ggtitle(diplodus[4])

cor.test(my_data$abundance, my_data$biomass)

# How many transects in total?
my_data %>% count(site, trans) # 1769

# How many NAs in 'depth', which will be excluded?
my_data %>% group_by(site, trans) %>% filter(is.na(depth)) %>% count(site, trans) # 518

# -------------------------------------------------------------------------

medata <- read_rds("data/medata.Rds")

medata %>% nrow()

med_check <- medata %>%
  filter(data.origin != "azz_asi") %>% # presence-absence
  mutate(mpa = if_else(enforcement <= 1, FALSE, TRUE),
         temp = tmean,
         depth = depth,
         prod = pp_mean,
         biomass = (a*sp.length^b)*sp.n)

# How many observation?
med_check %>% nrow()

# How many species?
med_check %>% distinct(species) %>% count()
med_check %>% distinct(species) %>% print(n = Inf)

# Which seasons
med_check %>% distinct(season)

# Which countries
med_check %>% distinct(country)

# How many locations?
med_check %>% colnames()

med_check %>% distinct(site, trans) %>% count()
med_check %>% distinct(site) %>% count()

# How many transects?
med_check %>% distinct(site, trans) %>% group_by(site) %>% summarise(n_trans = n())
med_check %>% distinct(site, trans) %>% group_by(site) %>% summarise(n_trans = n()) %>% print(n = Inf)
# med_check %>% distinct(site, trans) %>% group_by(site) %>% summarise(n_trans = n()) %>% view()

med_check %>% 
  filter(str_detect(site, "assecret")) %>% 
  count(site, trans) %>% 
  count(site)

## Crete sites are actually transects (see issue #8 in MEData repo)
## Change sites name to site = first 14 chars

med_check %>% 
  filter(str_detect(site, "assecret")) %>% 
  distinct(site) %>% 
  print(n = Inf)

### Site

med_check %>% 
  filter(str_detect(site, "assecret")) %>% 
  mutate(site_new = str_sub(string = site, start = 1, end = 14)) %>% 
  distinct(site_new) %>% 
  print(n = Inf)

### Transect
med_check %>% 
  filter(str_detect(site, "assecret")) %>% 
  mutate(trans_new = str_sub(string = site, start = 15, end = 15)) %>% 
  distinct(trans_new) %>% 
  print(n = Inf)

med_new <- med_check %>% 
  mutate(obs_site = site,
         trans = as.character(trans))

# Create new site names for Crete (by date)
med_new[str_detect(med_new$site, "assecret"),]$site <- substr(med_new[str_detect(med_new$site, "assecret"),]$site, start = 1, stop = 14)

med_new %>% 
  filter(str_detect(site, "assecret")) %>%
  count(site, obs_site) %>% 
  # count(site)
  print(n = Inf)

# Create new transect names for Crete (dive, team, transect)
med_new[str_detect(med_new$site, "assecret"),]$trans <- substr(med_new[str_detect(med_new$site, "assecret"),]$obs_site, start = 15, stop = 21)

med_new %>% 
  filter(str_detect(site, "assecret")) %>%
  count(site, trans) %>% 
  # count(site)
  print(n = Inf)

# Check again N transects:
med_new %>% 
  distinct(site, trans) %>% 
  group_by(site) %>% 
  summarise(n_trans = n()) %>% 
  print(n = Inf)

# Mean N transects per site:
med_new %>% 
  distinct(site, trans) %>% 
  group_by(site) %>% 
  summarise(n_trans = n()) %>% 
  summarise(mean = mean(n_trans),
            median = median(n_trans))
  print(n = Inf)


med_new <- med_new %>% select(-obs_site)



