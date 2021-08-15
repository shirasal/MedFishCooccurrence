############################### MODELS ###############################
source("R/packages.R")
source("R/functions.R")

env_cov <- read_rds("data/all_covs.rds")[1:3]
mpa_cov <- read_rds("data/all_covs.rds")[4]
guild_colours <- read_rds("data/processed/guild_colours.rds")

matrices <- list("data/processed/grps_mat.rds",
                 "data/processed/dip_mat.rds",
                 "data/processed/herb_mat.rds")

# Nonspatial Poisson CRF --------------------------------------------------

species_mats <- lapply(matrices, read_rds)
names(species_mats) <- c("grps_mat", "dip_mat", "herb_mat")

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

