############################### MODELS ###############################
source("R/packages.R")
source("R/functions.R")

env_cov <- read_rds("data/all_covs.rds")[1:3]
mpa_cov <- read_rds("data/all_covs.rds")[4]
guild_colours <- read_rds("data/processed/guild_colours.rds")

matrices <- list("data/processed/grps_mat.rds",
                 "data/processed/dip_mat.rds",
                 "data/processed/herb_mat.rds")

species_mats <- lapply(matrices, read_rds)
names(species_mats) <- c("grps_mat", "dip_mat", "herb_mat")

set.seed(100)

# Nonspatial Poisson CRF --------------------------------------------------

poisson_models <- lapply(species_mats, function(x){MRFcov(x, n_nodes = 4, family = "poisson")})
names(poisson_models) <- c("grps_pois", "dip_pois", "herb_pois")


## Relative importance ----------------------------------------------------

pois_relimp <- lapply(poisson_models, rel_imp_sum)
names(pois_relimp) <- c("grps_pois_relimp", "dip_pois_relimp", "herb_pois_relimp")

p_relimp_grps_pois <- pois_relimp$grps_pois_relimp %>% select(-`NA`) %>% plot_relimp("grps", "Groupers")
p_relimp_dip_pois <- pois_relimp$dip_pois_relimp %>% select(-`NA`) %>% plot_relimp("dip", "Seabreams")
p_relimp_herb_pois <- pois_relimp$herb_pois_relimp %>% select(-`NA`) %>% plot_relimp("herb", "Herbivores")

# ggsave(plot = p_relimp_grps_pois, filename = "figures/relimp_grps_pois_nonspat.png", device = "png",
#        dpi = 300, width = 11.74, height = 4, units = "in")
# ggsave(plot = p_relimp_dip_pois, filename = "figures/relimp_dip_pois_nonspat.png", device = "png",
#        dpi = 300, width = 11.74, height = 4, units = "in")
# ggsave(plot = p_relimp_herb_pois, filename = "figures/relimp_herb_pois_nonspat.png", device = "png",
#        dpi = 300, width = 11.74, height = 4, units = "in")

patch_plot <- p_relimp_grps_pois / p_relimp_dip_pois / p_relimp_herb_pois
# Remove y axis titles from first and third subplots and enlarge the middle one's
patch_plot[[1]] <- patch_plot[[1]] + theme(axis.title.y = element_blank())
patch_plot[[2]] <- patch_plot[[2]] + theme(axis.title.y = element_text(size = 14, vjust = 5))
patch_plot[[3]] <- patch_plot[[3]] + theme(axis.title.y = element_blank())

patch_plot
# ggsave("figures/rel_imp_pois_nonspat.png", device = "png", 
#        dpi = 150, height = 10, width = 10, units = "in")

# ggsave("figures/rel_imp_pois_nonspat.pdf", device = "pdf", dpi = 150, height = 10, width = 10, units = "in")


## Nonstationarity -------------------------------------------------------

all_relimp <- list(grps = pois_relimp$grps_pois_relimp,
                   dip = pois_relimp$dip_pois_relimp,
                   herb = pois_relimp$herb_pois_relimp)

# Compare stationary and nonstationary effects:
lapply(all_relimp, function(x){
  x %>% 
    pivot_longer(2:ncol(.), names_to = "type", values_to = "rel_imp") %>% 
    mutate(nonstationary = str_detect(type, "_bio")) %>% 
    group_by(species, nonstationary) %>% 
    na.omit() %>% 
    summarise(sum = sum(rel_imp)) %>% 
    group_by(nonstationary) %>% 
    summarise(mean_RI = mean(sum))
})


# Biomass models ----------------------------------------------------------

matrices_mass <- list("data/processed/grps_mass_mat.rds",
                      "data/processed/dip_mass_mat.rds",
                      "data/processed/herb_mass_mat.rds")

species_mats_mass <- lapply(matrices_mass, read_rds)
names(species_mats_mass) <- c("grps_mass_mat", "dip_mass_mat", "herb_mass_mat")

biomass_models <- lapply(species_mats_mass, function(x){MRFcov(x, n_nodes = 4, family = "gaussian")})
names(biomass_models) <- c("grps_mass", "dip_mass", "herb_mass")

biomass_models$dip_mass$key_coefs

## Relative importance ----------------------------------------------------

mass_relimp <- lapply(biomass_models, rel_imp_sum)
names(mass_relimp) <- c("grps_mass_relimp", "dip_mass_relimp", "herb_mass_relimp")

mass_relimp$grps_mass_relimp
mass_relimp$dip_mass_relimp
mass_relimp$herb_mass_relimp

cov_titles <- tibble(covariate = c("env", "mpa", "bio", "temp_bio", "mpa_bio"),
                     facet.title = factor(c("Environment", "MPA", "Biotic Associations",
                                            "Temp * Biotic", "MPA * Biotic"),
                                          levels = c("Environment", "MPA", "Biotic Associations",
                                                     "Temp * Biotic", "MPA * Biotic")))

p_relimp_grps_mass <- mass_relimp$grps_mass_relimp %>% 
  plot_relimp(guild_col = "grps", guild_name = "Groupers")
  
p_relimp_dip_mass <- mass_relimp$dip_mass_relimp %>% select(-`NA`) %>% 
  plot_relimp(guild_col = "dip", guild_name = "Seabreams")

p_relimp_herb_mass <- mass_relimp$herb_mass_relimp %>% select(-`NA`) %>% 
  pivot_longer(2:length(.)) %>%
  rename(covariate = name, rel_imp = value) %>%
  add_row(species = colnames(species_mats_mass$herb_mass_mat[1:4]), covariate = "mpa_bio", rel_imp = 0) %>% 
  right_join(cov_titles, by = "covariate") %>% 
  mutate(species = str_replace_all(species, "\\.", "\\ ")) %>% 
  filter(!is.na(species)) %>% 
  # Plot:
  ggplot() +
  aes(x = species, y = rel_imp) +
  stat_summary(geom = "bar", fun = mean, position = "dodge",  fill = guild_colours$herb) +
  facet_wrap(~facet.title, nrow = 1) +
  labs(subtitle = "Herbivores", y = "Relative Importance (prop.)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"), strip.placement = "outside",
        axis.title.x = element_blank(), 
        strip.text.x = element_text(size = 12, face = "bold"),
        plot.margin = margin(.2,1,.2,1, "cm"))


# ggsave(plot = p_relimp_grps_mass, filename = "figures/relimp_grps_mass.png", device = "png",
#        dpi = 300, width = 11.74, height = 4, units = "in")
# ggsave(plot = p_relimp_dip_mass, filename = "figures/relimp_dip_mass.png", device = "png",
#        dpi = 300, width = 11.74, height = 4, units = "in")
# ggsave(plot = p_relimp_herb_mass, filename = "figures/relimp_herb_mass.png", device = "png",
#        dpi = 300, width = 11.74, height = 4, units = "in")

patch_plot_mass <- p_relimp_grps_mass / p_relimp_dip_mass / p_relimp_herb_mass
# Remove y axis titles from first and third subplots and enlarge the middle one's
patch_plot_mass[[1]] <- patch_plot_mass[[1]] + theme(axis.title.y = element_blank())
patch_plot_mass[[2]] <- patch_plot_mass[[2]] + theme(axis.title.y = element_text(size = 14, vjust = 5))
patch_plot_mass[[3]] <- patch_plot_mass[[3]] + theme(axis.title.y = element_blank())

patch_plot_mass
# ggsave("figures/rel_imp_mass.png", device = "png", dpi = 150, height = 10, width = 10, units = "in")
# ggsave("figures/rel_imp_mass.pdf", device = "pdf", dpi = 150, height = 10, width = 10, units = "in")

