############################### MODELS ###############################
source("R/packages.R")
source("R/functions.R")

matrices <- list("data/processed/grps_mat.rds",
              "data/processed/dip_mat.rds",
              "data/processed/herb_mat.rds")

species_mats <- lapply(matrices, read_rds)
names(species_mats) <- c("grps_mat", "dip_mat", "herb_mat")

# Nonspatial Poisson CRF --------------------------------------------------

grps_pois <- MRFcov(grps_mat, n_nodes = 4, family = "poisson")
dip_pois <- MRFcov(dip_mat, n_nodes = 4, family = "poisson")
herb_pois <- MRFcov(herb_mat, n_nodes = 4, family = "poisson")


## Relative importance ----------------------------------------------------

grps_pois_relimp <- rel_imp_sum(grps_pois)
dip_pois_relimp <- rel_imp_sum(dip_pois)
herb_pois_relimp <- rel_imp_sum(herb_pois)

p_relimp_grps_pois <- plot_relimp(grps_pois_relimp, "grps", "Groupers")
ggsave(plot = p_relimp_grps_pois, filename = "figures/relimp_grps_pois_nonspat.png", device = "png", 
       dpi = 300, width = 11.74, height = 4, units = "in")
p_relimp_dip_pois <- plot_relimp(dip_pois_relimp, "dip", "Seabreams")
ggsave(plot = p_relimp_dip_pois, filename = "figures/relimp_dip_pois_nonspat.png", device = "png", 
       dpi = 300, width = 11.74, height = 4, units = "in")
p_relimp_herb_pois <- plot_relimp(herb_pois_relimp, "herb", "Herbivores")
ggsave(plot = p_relimp_herb_pois, filename = "figures/relimp_herb_pois_nonspat.png", device = "png", 
       dpi = 300, width = 11.74, height = 4, units = "in")


patch_plot <- p_relimp_grps_pois / p_relimp_dip_pois / p_relimp_herb_pois
# Remove y axis titles from first and third subplots and enlarge the middle one's
patch_plot[[1]] <- patch_plot[[1]] + theme(axis.title.y = element_blank())
patch_plot[[2]] <- patch_plot[[2]] + theme(axis.title.y = element_text(size = 14, vjust = 5))
patch_plot[[3]] <- patch_plot[[3]] + theme(axis.title.y = element_blank())

patch_plot
ggsave(filename = "figures/rel_imp_pois_nonspat.png", device = "png", 
       dpi = 150, height = 10, width = 10, units = "in")

