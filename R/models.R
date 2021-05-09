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

# Nonspatial Poisson CRF --------------------------------------------------

poisson_models <- lapply(species_mats, function(x){MRFcov(x, n_nodes = 4, family = "poisson")})
names(poisson_models) <- c("grps_pois", "dip_pois", "herb_pois")


## Relative importance ----------------------------------------------------

pois_relimp <- lapply(poisson_models, rel_imp_sum)
names(pois_relimp) <- c("grps_pois_relimp", "dip_pois_relimp", "herb_pois_relimp")

p_relimp_grps_pois <- plot_relimp(pois_relimp$grps_pois_relimp, "grps", "Groupers")
# ggsave(plot = p_relimp_grps_pois, filename = "figures/relimp_grps_pois_nonspat.png", device = "png", 
#        dpi = 300, width = 11.74, height = 4, units = "in")
p_relimp_dip_pois <- plot_relimp(pois_relimp$dip_pois_relimp, "dip", "Seabreams")
# ggsave(plot = p_relimp_dip_pois, filename = "figures/relimp_dip_pois_nonspat.png", device = "png", 
#        dpi = 300, width = 11.74, height = 4, units = "in")
p_relimp_herb_pois <- plot_relimp(pois_relimp$herb_pois_relimp, "herb", "Herbivores")
# ggsave(plot = p_relimp_herb_pois, filename = "figures/relimp_herb_pois_nonspat.png", device = "png", 
#        dpi = 300, width = 11.74, height = 4, units = "in")


patch_plot <- p_relimp_grps_pois / p_relimp_dip_pois / p_relimp_herb_pois
# Remove y axis titles from first and third subplots and enlarge the middle one's
patch_plot[[1]] <- patch_plot[[1]] + theme(axis.title.y = element_blank())
patch_plot[[2]] <- patch_plot[[2]] + theme(axis.title.y = element_text(size = 14, vjust = 5))
patch_plot[[3]] <- patch_plot[[3]] + theme(axis.title.y = element_blank())

patch_plot
# ggsave(filename = "figures/rel_imp_pois_nonspat.png", device = "png", 
#        dpi = 150, height = 10, width = 10, units = "in")


### Nonstationarity -------------------------------------------------------

all_relimp <- list(grps = grps_pois_relimp,
                   dip = dip_pois_relimp,
                   herb = herb_pois_relimp)

# Compare stationary and nonstationary effects:
lapply(all_relimp, function(x){
x %>% 
  pivot_longer(2:6, names_to = "type", values_to = "rel_imp") %>% 
  mutate(nonstationary = str_detect(type, "_bio_")) %>% 
  group_by(species, nonstationary) %>% 
  summarise(sum = sum(rel_imp)) %>% 
  group_by(nonstationary) %>% 
  summarise(mean(sum))
})
