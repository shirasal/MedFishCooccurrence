source("R/packages.R")
source("R/functions.R")
source('R/models.R', echo = TRUE)

# Biomass network gradients ------------------------------------------------

plot_graph(biomass_models$grps_mass, "Groupers")
# ggsave("figures/networks/groupers_mass_network_overall.png", device = "png", dpi = 300, width = 4, unit = "in")

plot_graph(biomass_models$dip_mass, "Seabreams")
# ggsave("figures/networks/seabreams_mass_network_overall.png", device = "png", dpi = 300, width = 4, unit = "in")

plot_graph(biomass_models$herb_mass, "Herbivores")
# ggsave("figures/networks/herb_mass_network_overall.png", device = "png", dpi = 300, width = 4, unit = "in")


## Temperature networks for Biomass models ---------------------------------

plotMRF_net_cont(species_mats_mass$grps_mass_mat, biomass_models$grps_mass, node_names = guilds$groupers, covariate = "temp")

plotMRF_net_cont(species_mats_mass$dip_mass_mat, biomass_models$dip_mass, node_names = guilds$diplodus, covariate = "temp")

plotMRF_net_cont(species_mats_mass$herb_mass_mat, biomass_models$herb_mass, node_names = guilds$herbivores, covariate = "temp")

# png(filename = "figures/networks/groupers_net_temp_mass.png", res = 150, width = 12, height = 3.7, units = "in")
# plotMRF_net_cont(species_mats_mass$grps_mass_mat, biomass_models$grps_mass, node_names = guilds$groupers, covariate = "temp")
# dev.off()
# 
# png(filename = "figures/networks/seabreams_net_temp_mass.png", res = 150, width = 12, height = 3.7, units = "in")
# plotMRF_net_cont(species_mats_mass$dip_mass_mat, biomass_models$dip_mass, node_names = guilds$diplodus, covariate = "temp")
# dev.off()
# 
# png(filename = "figures/networks/herbivores_net_temp_mass.png", res = 150, width = 12, height = 3.7, units = "in")
# plotMRF_net_cont(species_mats_mass$herb_mass_mat, biomass_models$herb_mass, node_names = guilds$herbivores, covariate = "temp")
# dev.off()


## MPA networks for Biomass models -----------------------------------------

plotMRF_net_factor(species_mats_mass$grps_mass_mat, biomass_models$grps_mass, guilds$groupers, covariate = "mpa")

plotMRF_net_factor(species_mats_mass$dip_mass_mat, biomass_models$dip_mass, guilds$diplodus, covariate = "mpa")

plotMRF_net_factor(species_mats_mass$herb_mass_mat, biomass_models$herb_mass, guilds$herbivores, covariate = "mpa")

# png(filename = "figures/networks/groupers_net_mpa_mass.png", res = 150, width = 13, height = 7.38, units = "in")
# plotMRF_net_factor(species_mats_mass$grps_mass_mat, biomass_models$grps_mass, guilds$groupers, covariate = "mpa")
# dev.off()
# 
# png(filename = "figures/networks/seabreams_net_mpa_mass.png", res = 150, width = 13, height = 7.38, units = "in")
# plotMRF_net_factor(species_mats_mass$dip_mass_mat, biomass_models$dip_mass, guilds$diplodus, covariate = "mpa")
# dev.off()
# 
# png(filename = "figures/networks/herbivores_net_mpa_mass.png", res = 150, width = 13, height = 7.38, units = "in")
# plotMRF_net_factor(species_mats_mass$herb_mass_mat, biomass_models$herb_mass, guilds$herbivores, covariate = "mpa")
# dev.off()


