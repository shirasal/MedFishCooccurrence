## Run if not done yet!!!
# source("R/2 biomass_spatial_models.R", echo = TRUE)

# Groupers ----------------------------------------------------------------

grps_dat <- species_mats$grps

# To fit without interactions, the simplest way given the package constraints is to set these
# columns to zero so they will be regularised out of the model
# Cross-multiply the data with covariates manually
grps_prepped_dat <- prep_MRF_covariates(grps_dat, n_nodes = 4)

# Find the covariate * species interaction columns in the prepped data and set to zero
# cov_names_remove <- paste(colnames(dat[,-c(1:n_nodes)]), '_', colnames(dat[,c(1:n_nodes)]), sep = '')
# prepped_dat[,cov_names_remove] <- 0
grps_prepped_dat <- grps_prepped_dat %>% 
  mutate(across(.cols = 9:ncol(grps_prepped_dat), .fns = function(x) 0))

# Re-fit the model, setting prep_covariates to FALSE
grps_no_int <- MRFcov_spatial(grps_prepped_dat, coords = coord_dfs$grps, 
                              n_nodes = 4, prep_covariates = F, family = "gaussian")
grps_no_int$graph
grps_no_int$direct_coefs

grps_no_int$key_coefs

grps_noint_relimp <- rel_imp_sum(grps_no_int)

# Seabreams ---------------------------------------------------------------

dip_dat <- species_mats$dip

# To fit without interactions, the simplest way given the package constraints is to set these
# columns to zero so they will be regularised out of the model
# Cross-multiply the data with covariates manually
dip_prepped_dat <- prep_MRF_covariates(dip_dat, n_nodes = 4)

# Find the covariate * species interaction columns in the prepped data and set to zero
# cov_names_remove <- paste(colnames(dat[,-c(1:n_nodes)]), '_', colnames(dat[,c(1:n_nodes)]), sep = '')
# prepped_dat[,cov_names_remove] <- 0
dip_prepped_dat <- dip_prepped_dat %>% 
  mutate(across(.cols = 9:ncol(dip_prepped_dat), .fns = function(x) 0))

# Re-fit the model, setting prep_covariates to FALSE
dip_no_int <- MRFcov_spatial(dip_prepped_dat, coords = coord_dfs$dip, 
                             n_nodes = 4, prep_covariates = F, family = "gaussian")
dip_no_int$graph
dip_no_int$direct_coefs

dip_no_int$key_coefs

dip_noint_relimp <- rel_imp_sum(dip_no_int)

# Herbivores --------------------------------------------------------------

herb_dat <- species_mats$herb

# To fit without interactions, the simplest way given the package constraints is to set these
# columns to zero so they will be regularised out of the model
# Cross-multiply the data with covariates manually
herb_prepped_dat <- prep_MRF_covariates(herb_dat, n_nodes = 4)

# Find the covariate * species interaction columns in the prepped data and set to zero
# cov_names_remove <- paste(colnames(dat[,-c(1:n_nodes)]), '_', colnames(dat[,c(1:n_nodes)]), sep = '')
# prepped_dat[,cov_names_remove] <- 0
herb_prepped_dat <- herb_prepped_dat %>% 
  mutate(across(.cols = 9:ncol(herb_prepped_dat), .fns = function(x) 0))

# Re-fit the model, setting prep_covariates to FALSE
herb_no_int <- MRFcov_spatial(herb_prepped_dat, coords = coord_dfs$herb, 
                              n_nodes = 4, prep_covariates = F, family = "gaussian")
herb_no_int$graph
herb_no_int$direct_coefs

herb_no_int$key_coefs

herb_noint_relimp <- rel_imp_sum(herb_no_int)


# Plot --------------------------------------------------------------------

# Tibble for the facet names and their order:
cov_titles_noint <- tibble(covariate = c("env", "mpa", "bio"), 
                           facet.title = factor(c("Environment", "MPA", "Biotic Associations"),
                                                levels = c("Environment", "MPA", "Biotic Associations")))

p_relimp_grps_noint <- grps_noint_relimp %>%
  pivot_longer(2:length(.)) %>%
  rename(covariate = name, rel_imp = value) %>%
  mutate(species = str_replace_all(species, "\\.", "\\ ")) %>%
  group_by(species) %>% nest() %>% 
  mutate(new_data = map(data, function(x) right_join(x, cov_titles_noint, by = "covariate"))) %>% 
  select(-data) %>% unnest(cols = c(new_data)) %>% replace_na(list(rel_imp = 0)) %>%
  # Plot:
  ggplot() +
  aes(x = species, y = rel_imp) +
  stat_summary(geom = "bar", fun = mean, position = "dodge",  fill = guild_colours$grps) +
  facet_wrap(~facet.title, nrow = 1) +
  labs(subtitle = "(A) Groupers", y = "Relative Importance (prop.)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"), strip.placement = "outside",
        axis.title.x = element_blank(), 
        strip.text.x = element_text(size = 12, face = "bold"),
        plot.margin = margin(.2,1,.2,1, "cm"),
        plot.title = element_text(size = 16))


p_relimp_dip_noint <- dip_noint_relimp %>%
  pivot_longer(2:length(.)) %>%
  rename(covariate = name, rel_imp = value) %>%
  mutate(species = str_replace_all(species, "\\.", "\\ ")) %>%
  group_by(species) %>% nest() %>% 
  mutate(new_data = map(data, function(x) right_join(x, cov_titles_noint, by = "covariate"))) %>% 
  select(-data) %>% unnest(cols = c(new_data)) %>% replace_na(list(rel_imp = 0)) %>%
  # Plot:
  ggplot() +
  aes(x = species, y = rel_imp) +
  stat_summary(geom = "bar", fun = mean, position = "dodge",  fill = guild_colours$dip) +
  facet_wrap(~facet.title, nrow = 1) +
  labs(subtitle = "(B) Seabreams", y = "Relative Importance (prop.)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"), strip.placement = "outside",
        axis.title.x = element_blank(), 
        strip.text.x = element_text(size = 12, face = "bold"),
        plot.margin = margin(.2,1,.2,1, "cm"),
        plot.title = element_text(size = 16))

p_relimp_herb_noint <- herb_noint_relimp %>%
  pivot_longer(2:length(.)) %>%
  rename(covariate = name, rel_imp = value) %>%
  mutate(species = str_replace_all(species, "\\.", "\\ ")) %>%
  group_by(species) %>% nest() %>% 
  mutate(new_data = map(data, function(x) right_join(x, cov_titles_noint, by = "covariate"))) %>% 
  select(-data) %>% unnest(cols = c(new_data)) %>% replace_na(list(rel_imp = 0)) %>%
  # Plot:
  ggplot() +
  aes(x = species, y = rel_imp) +
  stat_summary(geom = "bar", fun = mean, position = "dodge",  fill = guild_colours$herb) +
  facet_wrap(~facet.title, nrow = 1) +
  labs(subtitle = "(C) Herbivores", y = "Relative Importance (prop.)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"), strip.placement = "outside",
        axis.title.x = element_blank(), 
        strip.text.x = element_text(size = 12, face = "bold"),
        plot.margin = margin(.2,1,.2,1, "cm"),
        plot.title = element_text(size = 16))

patch_plot_noint <- p_relimp_grps_noint / p_relimp_dip_noint / p_relimp_herb_noint
# Remove y axis titles from first and third subplots and enlarge the middle one's
patch_plot_noint[[1]] <- patch_plot_noint[[1]] + theme(axis.title.y = element_blank())
patch_plot_noint[[2]] <- patch_plot_noint[[2]] + theme(axis.title.y = element_text(size = 14, vjust = 5))
patch_plot_noint[[3]] <- patch_plot_noint[[3]] + theme(axis.title.y = element_blank())

patch_plot_noint
# ggsave(patch_plot_noint, filename = "rel_imp_noint.png", device = "png", path = "figures/appendix",
#        dpi = 150, height = 10, width = 10, units = "in")
# ggsave(patch_plot_noint, filename = "rel_imp_noint.pdf", device = "pdf", path = "figures/appendix",
#        dpi = 150, height = 10, width = 10, units = "in")

