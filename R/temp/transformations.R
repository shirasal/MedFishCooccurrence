# Transformations

library(tidyverse)

med_clean <- read_rds("data/processed/med_clean.rds")

relevant_species <- list(groupers = read_rds("data/processed/grps_mat.rds") %>% select(1:4) %>% colnames(),
                         seabreams = read_rds("data/processed/dip_mat.rds") %>% select(1:4) %>% colnames(),
                         herbivores = read_rds("data/processed/herb_mat.rds") %>% select(1:4) %>% colnames())

med_clean %>% filter(species %in% array(unlist(relevant_species))) %>% 
  distinct(site, species, sp.n) %>% 
  ggplot() + 
  aes(species, sp.n, col = site) + 
  geom_point(alpha = .2) + 
  ggeasy::easy_remove_legend() + 
  ggeasy::easy_rotate_x_labels()

# Groupers

## No transformation
med_clean %>% filter(species %in% relevant_species$groupers) %>% 
  distinct(site, species, sp.n) %>% 
  ggplot() + 
  aes(species, sp.n, col = site) + 
  geom_point(alpha = .2) + 
  ggeasy::easy_remove_legend() + 
  ggeasy::easy_rotate_x_labels()

## Log10
med_clean %>% filter(species %in% relevant_species$groupers) %>% 
  distinct(site, species, sp.n) %>% 
  ggplot() + 
  aes(species, sp.n, col = site) + 
  geom_point(alpha = .2) + 
  scale_y_log10() + 
  ggeasy::easy_remove_legend() + 
  ggeasy::easy_rotate_x_labels() + 
  ggtitle("Log10")

## SQRT
med_clean %>% filter(species %in% relevant_species$groupers) %>% 
  distinct(site, species, sp.n) %>% 
  ggplot() + 
  aes(species, sp.n, col = site) + 
  geom_point(alpha = .2) + 
  scale_y_sqrt() + 
  ggeasy::easy_remove_legend() + 
  ggeasy::easy_rotate_x_labels() + 
  ggtitle("SQRT")


# Seabreams

## No transformation
med_clean %>% filter(species %in% relevant_species$seabreams) %>% 
  distinct(site, species, sp.n) %>% 
  ggplot() + 
  aes(species, sp.n, col = site) + 
  geom_point(alpha = .2) + 
  ggeasy::easy_remove_legend() + 
  ggeasy::easy_rotate_x_labels()

## Log10
med_clean %>% filter(species %in% relevant_species$seabreams) %>% 
  distinct(site, species, sp.n) %>% 
  ggplot() + 
  aes(species, sp.n, col = site) + 
  geom_point(alpha = .2) + 
  scale_y_log10() + 
  ggeasy::easy_remove_legend() + 
  ggeasy::easy_rotate_x_labels() + 
  ggtitle("Log10")

## SQRT
med_clean %>% filter(species %in% relevant_species$seabreams) %>% 
  distinct(site, species, sp.n) %>% 
  ggplot() + 
  aes(species, sp.n, col = site) + 
  geom_point(alpha = .2) + 
  scale_y_sqrt() + 
  ggeasy::easy_remove_legend() + 
  ggeasy::easy_rotate_x_labels() + 
  ggtitle("SQRT")


# Herbivores

## No transformation
med_clean %>% filter(species %in% relevant_species$herbivores) %>% 
  distinct(site, species, sp.n) %>% 
  ggplot() + 
  aes(species, sp.n, col = site) + 
  geom_point(alpha = .2) + 
  ggeasy::easy_remove_legend() + 
  ggeasy::easy_rotate_x_labels()

## Log10
med_clean %>% filter(species %in% relevant_species$herbivores) %>% 
  distinct(site, species, sp.n) %>% 
  ggplot() + 
  aes(species, sp.n, col = site) + 
  geom_point(alpha = .2) + 
  scale_y_log10() + 
  ggeasy::easy_remove_legend() + 
  ggeasy::easy_rotate_x_labels() + 
  ggtitle("Log10")

## SQRT
med_clean %>% filter(species %in% relevant_species$herbivores) %>% 
  distinct(site, species, sp.n) %>% 
  ggplot() + 
  aes(species, sp.n, col = site) + 
  geom_point(alpha = .2) + 
  scale_y_sqrt() + 
  ggeasy::easy_remove_legend() + 
  ggeasy::easy_rotate_x_labels() + 
  ggtitle("SQRT")


