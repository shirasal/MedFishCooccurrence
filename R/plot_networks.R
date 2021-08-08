source("R/packages.R")
source("R/functions.R")
source('R/models.R', echo = TRUE)

# Overall networks --------------------------------------------------------

plot_graph(poisson_models$grps_pois, "Groupers")
# ggsave("figures/groupers_network_overall.png", device = "png", dpi = 300, width = 4, unit = "in")

plot_graph(poisson_models$dip_pois, "Seabreams")
# ggsave("figures/seabreams_network_overall.png", device = "png", dpi = 300, width = 4, unit = "in")

plot_graph(poisson_models$herb_pois, "Herbivores")
# ggsave("figures/herb_network_overall.png", device = "png", dpi = 300, width = 4, unit = "in")


# Network gradients -------------------------------------------------------

guilds <- list(groupers = colnames(species_mats$grps_mat)[1:4],
               diplodus = colnames(species_mats$dip_mat)[1:4],
               herbivores = colnames(species_mats$herb_mat)[1:4])


## Temperature networks for Poisson models ---------------------------------

# png(filename = "figures/groupers_net_temp.png", res = 150, width = 12, height = 3.7, units = "in")
plotMRF_net_cont(species_mats$grps_mat, poisson_models$grps_pois, node_names = guilds$groupers, covariate = "temp")
# dev.off()

# png(filename = "figures/seabreams_net_temp.png", res = 150, width = 12, height = 3.7, units = "in")
plotMRF_net_cont(species_mats$dip_mat, poisson_models$dip_pois, node_names = guilds$diplodus, covariate = "temp")
# dev.off()

# png(filename = "figures/herbivores_net_temp.png", res = 150, width = 12, height = 3.7, units = "in")
plotMRF_net_cont(species_mats$herb_mat, poisson_models$herb_pois, node_names = guilds$herbivores, covariate = "temp")
# dev.off()


## MPA networks for Poisson models -----------------------------------------

# png(filename = "figures/groupers_net_mpa.png", res = 150, width = 13, height = 7.38, units = "in")
plotMRF_net_factor(species_mats$grps_mat, poisson_models$grps_pois, guilds$grouper, covariate = "mpa")
# dev.off()

# png(filename = "figures/seabreams_net_mpa.png", res = 150, width = 13, height = 7.38, units = "in")
plotMRF_net_factor(species_mats$dip_mat, poisson_models$dip_pois, guilds$diplodus, covariate = "mpa")
# dev.off()

# png(filename = "figures/herbivores_net_mpa.png", res = 150, width = 13, height = 7.38, units = "in")
plotMRF_net_factor(species_mats$herb_mat, poisson_models$herb_pois, guilds$herbivores, covariate = "mpa")
# dev.off()


# Biomass network gradients ------------------------------------------------

plot_graph(biomass_models$grps_mass, "Groupers")
# ggsave("figures/mass/groupers_mass_network_overall.png", device = "png", dpi = 300, width = 4, unit = "in")

plot_graph(biomass_models$dip_mass, "Seabreams")
# ggsave("figures/mass/seabreams_mass_network_overall.png", device = "png", dpi = 300, width = 4, unit = "in")

plot_graph(biomass_models$herb_mass, "Herbivores")
# ggsave("figures/mass/herb_mass_network_overall.png", device = "png", dpi = 300, width = 4, unit = "in")


## Temperature networks for Biomass models ---------------------------------

plotMRF_net_cont(species_mats_mass$grps_mass_mat, biomass_models$grps_mass, node_names = guilds$groupers, covariate = "temp")
plotMRF_net_cont(species_mats_mass$dip_mass_mat, biomass_models$dip_mass, node_names = guilds$diplodus, covariate = "temp")
plotMRF_net_cont(species_mats_mass$herb_mass_mat, biomass_models$herb_mass, node_names = guilds$herbivores, covariate = "temp")

# png(filename = "figures/mass/groupers_net_temp_mass.png", res = 150, width = 12, height = 3.7, units = "in")
# plotMRF_net_cont(species_mats_mass$grps_mass_mat, biomass_models$grps_mass, node_names = guilds$groupers, covariate = "temp")
# dev.off()
# 
# png(filename = "figures/mass/seabreams_net_temp_mass.png", res = 150, width = 12, height = 3.7, units = "in")
# plotMRF_net_cont(species_mats_mass$dip_mass_mat, biomass_models$dip_mass, node_names = guilds$diplodus, covariate = "temp")
# dev.off()
# 
# png(filename = "figures/mass/herbivores_net_temp_mass.png", res = 150, width = 12, height = 3.7, units = "in")
# plotMRF_net_cont(species_mats_mass$herb_mass_mat, biomass_models$herb_mass, node_names = guilds$herbivores, covariate = "temp")
# dev.off()


## MPA networks for Biomass models -----------------------------------------

plotMRF_net_factor(species_mats_mass$grps_mass_mat, biomass_models$grps_mass, guilds$groupers, covariate = "mpa")
plotMRF_net_factor(species_mats_mass$dip_mass_mat, biomass_models$dip_mass, guilds$diplodus, covariate = "mpa")
plotMRF_net_factor(species_mats_mass$herb_mass_mat, biomass_models$herb_mass, guilds$herbivores, covariate = "mpa")

# png(filename = "figures/mass/groupers_net_mpa_mass.png", res = 150, width = 13, height = 7.38, units = "in")
# plotMRF_net_factor(species_mats_mass$grps_mass_mat, biomass_models$grps_mass, guilds$groupers, covariate = "mpa")
# dev.off()
# 
# png(filename = "figures/mass/seabreams_net_mpa_mass.png", res = 150, width = 13, height = 7.38, units = "in")
# plotMRF_net_factor(species_mats_mass$dip_mass_mat, biomass_models$dip_mass, guilds$diplodus, covariate = "mpa")
# dev.off()
# 
# png(filename = "figures/mass/herbivores_net_mpa_mass.png", res = 150, width = 13, height = 7.38, units = "in")
# plotMRF_net_factor(species_mats_mass$herb_mass_mat, biomass_models$herb_mass, guilds$herbivores, covariate = "mpa")
# dev.off()


