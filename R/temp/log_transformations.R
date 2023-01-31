# Log transformations

library(tidyverse)
source("R/assist_vectors.R")

med_clean <- medata %>%
  filter(site != "asinara_add") %>% 
  mutate(enforce = as.integer(enforcement)) %>% 
  mutate(mpa = if_else(enforce <= 1, FALSE, TRUE),
         temp = tmean,
         depth = depth,
         prod = pp_mean,
         biomass = (a*sp.length^b)*sp.n) %>%
  select(site, lon, lat, unique_trans_id, species, sp.n, biomass, mpa, temp, depth, prod)

med_clean %>% filter(species %in% c(groupers, diplodus, herbivores)) %>% 
  distinct(site, species, sp.n) %>% 
  ggplot() + 
  aes(species, sp.n, col = site) + 
  geom_point(alpha = .2) + 
  theme(legend.position = "none", axis.text.x = element_text(angle = 90))

guild_table <- tibble(grp = c(rep(names(guilds[1]), 4), rep(names(guilds[2]), 4), rep(names(guilds[3]), 4)),
                      species = c(guilds[[1]], guilds[[2]], guilds[[3]]),
                      clr = c(rep(guilds[[4]][[1]], 4), rep(guilds[[4]][[2]], 4), rep(guilds[[4]][[3]], 4)))

# BIOMASS -----------------------------------------------------------------

## No transformation
med_clean %>% filter(species %in% c(groupers, diplodus, herbivores)) %>% 
  left_join(guild_table) %>% 
  group_by(grp, clr, unique_trans_id, species) %>% 
  summarise(total_biomass = sum(biomass)) %>% 
  ggplot() + 
  aes(species, total_biomass, col = clr) + 
  geom_point(alpha = .2) + 
  theme(legend.position = "none", axis.text.x = element_text(angle = 90)) + 
  facet_wrap(~grp, scales = "free")

med_clean %>% filter(species %in% c(groupers, diplodus, herbivores)) %>% 
  left_join(guild_table) %>% 
  group_by(grp, clr, unique_trans_id, species) %>% 
  summarise(total_biomass = sum(biomass)) %>% 
  ggplot() + 
  aes(species, total_biomass, col = clr) + 
  geom_point(alpha = .2) + 
  theme(legend.position = "none", axis.text.x = element_text(angle = 90))

## Log10
med_clean %>% filter(species %in% c(groupers, diplodus, herbivores)) %>% 
  left_join(guild_table) %>% 
  mutate(total_biomass_log10 = log10(biomass)) %>% 
  ggplot() + 
  aes(species, total_biomass_log10, col = clr) + 
  geom_point(alpha = .2) + 
  theme(legend.position = "none", axis.text.x = element_text(angle = 90)) +
  ggtitle("Log10 (BIOMASS)")

## Log2
med_clean %>% filter(species %in% c(groupers, diplodus, herbivores)) %>% 
  left_join(guild_table) %>% 
  mutate(total_biomass_log2 = log2(biomass)) %>% 
  ggplot() + 
  aes(species, total_biomass_log2, col = clr) + 
  geom_point(alpha = .2) + 
  theme(legend.position = "none", axis.text.x = element_text(angle = 90)) +
  ggtitle("Log2 (BIOMASS)")

