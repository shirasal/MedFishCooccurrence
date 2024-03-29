
source("R/packages.R")
source("R/functions.R")
source("R/create_spp_mat_function.R")
source("R/assist_vectors.R")

med_clean <- read_rds("data/processed/med_clean.rds")
med_clean_east <- read_rds("data/processed/med_clean_east.rds")

############################# CREATE SPECIES MATRICES and COORDINATES DATA FRAMES #############################

grps_mat <- create_spp_mat(dataset = med_clean, guild = groupers, metric = "biomass", covariate = all_covs)
dip_mat <- create_spp_mat(dataset = med_clean, guild = diplodus, metric = "biomass", covariate = all_covs)
herb_mat <- create_spp_mat(dataset = med_clean_east, guild = herbivores, metric = "biomass", covariate = all_covs)

grps_coords <- med_clean %>% filter(species %in% groupers, !is.na(depth)) %>% 
  pivot_wider(names_from = species, values_from = biomass, values_fill = 0, values_fn = sum) %>% 
  select(lon, lat)

dip_coords <- med_clean %>% filter(species %in% diplodus, !is.na(depth)) %>% 
  pivot_wider(names_from = species, values_from = biomass, values_fill = 0, values_fn = sum) %>% 
  select(lon, lat)

herb_coords <- med_clean_east %>% filter(species %in% herbivores, !is.na(depth)) %>% 
  pivot_wider(names_from = species, values_from = biomass, values_fill = 0, values_fn = sum) %>% 
  select(lon, lat)

nrow(grps_mat) == nrow(grps_coords)
nrow(dip_mat) == nrow(dip_coords)
nrow(herb_mat) == nrow(herb_coords)

species_mats <- list(grps_mat, dip_mat, herb_mat)
names(species_mats) <- c("grps", "dip", "herb")

coord_dfs <- list(grps_coords, dip_coords, herb_coords)
names(coord_dfs) <- c("grps", "dip", "herb")

############################################### SPATIAL MODELS ###############################################

set.seed(100)
spatial_models <- map2(.x = species_mats, .y = coord_dfs, 
                         function(x, y){MRFcov_spatial(x, n_nodes = 4, family = "gaussian", coords = y)})
names(spatial_models) <- c("grps_spat", "dip_spat", "herb_spat")

spatial_models$dip_spat$key_coefs

######################################### SUMMARISE MODELS AND PLOT #########################################

spat_relimp <- lapply(spatial_models, rel_imp_sum)
names(spat_relimp) <- c("grps", "dip", "herb")

spat_relimp$grps
spat_relimp$dip
spat_relimp$herb

# R-squared values from 'R/model_eval.R'
p_relimp_grps_spat <- spat_relimp$grps %>% 
  plot_relimp(guild_col = "grps", guild_name = expression(paste("(A) Groupers (R"^2," = 0.378)")))

p_relimp_dip_spat <- spat_relimp$dip %>% select(-`NA`) %>% 
  plot_relimp(guild_col = "dip", guild_name = expression(paste("(B) Seabreams (R"^2," = 0.36)")))

p_relimp_herb_spat <- spat_relimp$herb %>% select(-`NA`) %>% 
  plot_relimp(guild_col = "herb", guild_name = expression(paste("(C) Herbivores (R"^2," = 0.562)")))

# ggsave(plot = p_relimp_grps_spat, filename = "figures/relimp_grps_spat.png", device = "png",
#        dpi = 300, width = 11.74, height = 4, units = "in")
# ggsave(plot = p_relimp_dip_spat, filename = "figures/relimp_dip_spat.png", device = "png",
#        dpi = 300, width = 11.74, height = 4, units = "in")
# ggsave(plot = p_relimp_herb_spat, filename = "figures/relimp_herb_spat.png", device = "png",
#        dpi = 300, width = 11.74, height = 4, units = "in")

(patch_plot_spat <- p_relimp_grps_spat / p_relimp_dip_spat / p_relimp_herb_spat)
# Remove y axis titles from first and third subplots and enlarge the middle one's
patch_plot_spat[[1]] <- patch_plot_spat[[1]] + theme(axis.title.y = element_blank())
patch_plot_spat[[2]] <- patch_plot_spat[[2]] + theme(axis.title.y = element_text(size = 14, vjust = 5))
patch_plot_spat[[3]] <- patch_plot_spat[[3]] + theme(axis.title.y = element_blank())

patch_plot_spat
# ggsave("figures/rel_imp_spat.png", device = "png", dpi = 150, height = 10, width = 10, units = "in")
# ggsave("figures/rel_imp_spat.pdf", device = "pdf", dpi = 150, height = 10, width = 10, units = "in")

## Nonstationarity ---------------------------------------------------------

all_relimp_spat <- list(grps = spat_relimp$grps,
                        dip = spat_relimp$dip,
                        herb = spat_relimp$herb)

# Compare stationary and nonstationary effects:
lapply(all_relimp_spat, function(x){
  cov_titles <- tibble(covariate = c("env", "mpa", "bio", "temp_bio", "mpa_bio"),
                       facet.title = factor(c("Environment", "MPA", "Biotic Associations",
                                              "Temp * Biotic", "MPA * Biotic"),
                                            levels = c("Environment", "MPA", "Biotic Associations",
                                                       "Temp * Biotic", "MPA * Biotic")))
  x %>% 
    pivot_longer(2:length(.)) %>%
    rename(covariate = name, rel_imp = value) %>%
    mutate(species = str_replace_all(species, "\\.", "\\ ")) %>%
    group_by(species) %>% nest() %>% 
    mutate(new_data = map(data, function(x) right_join(x, cov_titles, by = "covariate"))) %>% 
    select(-data) %>% unnest(cols = c(new_data)) %>% replace_na(list(rel_imp = 0)) %>% 
    mutate(nonstationary = str_detect(facet.title, "\\*")) %>% 
    group_by(species, nonstationary) %>% 
    na.omit() %>% 
    summarise(sum = sum(rel_imp)) %>% 
    group_by(nonstationary) %>% 
    summarise(mean_RI = mean(sum))
})
