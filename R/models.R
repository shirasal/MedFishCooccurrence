############################### MODELS ###############################
source("R/packages.R")
source("R/functions.R")
source("R/assist_vectors.R")

matrices_mass <- list("data/processed/grps_mat.rds",
                      "data/processed/dip_mat.rds",
                      "data/processed/herb_mat.rds")


# Biomass models ----------------------------------------------------------

species_mats_mass <- lapply(matrices_mass, read_rds)
names(species_mats_mass) <- c("grps_mass_mat", "dip_mass_mat", "herb_mass_mat")

set.seed(100)
biomass_models <- lapply(species_mats_mass, function(x){MRFcov(x, n_nodes = 4, family = "gaussian")})
names(biomass_models) <- c("grps_mass", "dip_mass", "herb_mass")

biomass_models$dip_mass$key_coefs

## Relative importance ----------------------------------------------------

mass_relimp <- lapply(biomass_models, rel_imp_sum)
names(mass_relimp) <- c("grps_mass_relimp", "dip_mass_relimp", "herb_mass_relimp")

mass_relimp$grps_mass_relimp
mass_relimp$dip_mass_relimp
mass_relimp$herb_mass_relimp

p_relimp_grps_mass <- mass_relimp$grps_mass_relimp %>% 
  plot_relimp(guild_col = "grps", guild_name = "Groupers")

p_relimp_dip_mass <- mass_relimp$dip_mass_relimp %>% select(-`NA`) %>% 
  plot_relimp(guild_col = "dip", guild_name = "Seabreams")

p_relimp_herb_mass <- mass_relimp$herb_mass_relimp %>% select(-`NA`) %>% 
  plot_relimp(guild_col = "herb", guild_name = "Herbivores")

# ggsave(plot = p_relimp_grps_mass, filename = "figures/relimp_grps_mass.png", device = "png",
#        dpi = 300, width = 11.74, height = 4, units = "in")
# ggsave(plot = p_relimp_dip_mass, filename = "figures/relimp_dip_mass.png", device = "png",
#        dpi = 300, width = 11.74, height = 4, units = "in")
# ggsave(plot = p_relimp_herb_mass, filename = "figures/relimp_herb_mass.png", device = "png",
#        dpi = 300, width = 11.74, height = 4, units = "in")

(patch_plot_mass <- p_relimp_grps_mass / p_relimp_dip_mass / p_relimp_herb_mass)
# Remove y axis titles from first and third subplots and enlarge the middle one's
patch_plot_mass[[1]] <- patch_plot_mass[[1]] + theme(axis.title.y = element_blank())
patch_plot_mass[[2]] <- patch_plot_mass[[2]] + theme(axis.title.y = element_text(size = 14, vjust = 5))
patch_plot_mass[[3]] <- patch_plot_mass[[3]] + theme(axis.title.y = element_blank())

patch_plot_mass
# ggsave("figures/rel_imp_mass.png", device = "png", dpi = 150, height = 10, width = 10, units = "in")
# ggsave("figures/rel_imp_mass.pdf", device = "pdf", dpi = 150, height = 10, width = 10, units = "in")


## Nonstationarity ---------------------------------------------------------

all_relimp_mass <- list(grps = mass_relimp$grps_mass_relimp,
                        dip = mass_relimp$dip_mass_relimp,
                        herb = mass_relimp$herb_mass_relimp)

# Compare stationary and nonstationary effects:
lapply(all_relimp_mass, function(x){
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


