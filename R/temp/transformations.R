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


# Nonparanormal -----------------------------------------------------------

med_clean %>% filter(species %in% unlist(relevant_species)) %>% 
  mutate(npn = qnorm(rank(log2(sp.n + 0.01)) / (length(sp.n) + 1))) %>% 
  mutate(guild = case_when(species %in% unlist(relevant_species[1]) ~ names(relevant_species[1]),
                           species %in% unlist(relevant_species[2]) ~ names(relevant_species[2]),
                           species %in% unlist(relevant_species[3]) ~ names(relevant_species[3]))) %>% 
  distinct(site, species, guild, npn) %>% 
  ggplot() + 
  aes(species, npn, col = site) + 
  geom_point(alpha = .2) + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Nonparanormal") +
  facet_wrap(~guild, scales = "free")


med_clean %>% filter(species %in% unlist(relevant_species)) %>% 
  mutate(npn = qnorm(rank(log2(sp.n + 0.01)) / (length(sp.n) + 1))) %>% 
  mutate(guild = case_when(species %in% unlist(relevant_species[1]) ~ names(relevant_species[1]),
                           species %in% unlist(relevant_species[2]) ~ names(relevant_species[2]),
                           species %in% unlist(relevant_species[3]) ~ names(relevant_species[3]))) %>% 
  mutate(guild = factor(guild, levels = names(relevant_species))) %>%
  distinct(site, species, guild, npn) %>% 
  ggplot() + 
  aes(species, npn) + 
  geom_boxplot(aes(col = guild)) + 
  geom_hline(aes(yintercept = 0), col = "black") + 
  scale_color_manual(values = as.vector(unlist(guild_colours))) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Nonparanormal") +
  facet_wrap(~guild, scales = "free")

med_clean %>% filter(species %in% unlist(relevant_species)) %>% 
  mutate(npn = log2(sp.n + 0.01)) %>% 
  mutate(guild = case_when(species %in% unlist(relevant_species[1]) ~ names(relevant_species[1]),
                           species %in% unlist(relevant_species[2]) ~ names(relevant_species[2]),
                           species %in% unlist(relevant_species[3]) ~ names(relevant_species[3]))) %>% 
  mutate(guild = factor(guild, levels = names(relevant_species))) %>%
  distinct(site, species, guild, npn) %>% 
  ggplot() + 
  aes(species, npn) + 
  geom_boxplot(aes(col = guild)) + 
  geom_hline(aes(yintercept = 0), col = "black") + 
  scale_color_manual(values = as.vector(unlist(guild_colours))) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Nonparanormal (old transform)") +
  facet_wrap(~guild, scales = "free")

