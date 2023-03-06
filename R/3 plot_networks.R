source("R/packages.R")
source("R/functions.R")
source("R/2 biomass_spatial_models.R", echo = TRUE)

# Biomass network gradients ------------------------------------------------

plot_graph(spatial_models$grps, "Groupers")
# ggsave("figures/networks/groupers_mass_network_overall.png", device = "png", dpi = 300, width = 4, unit = "in")

plot_graph(spatial_models$dip, "Seabreams")
# ggsave("figures/networks/seabreams_mass_network_overall.png", device = "png", dpi = 300, width = 4, unit = "in")

plot_graph(spatial_models$herb, "Herbivores")
# ggsave("figures/networks/herb_mass_network_overall.png", device = "png", dpi = 300, width = 4, unit = "in")


## Temperature networks for Biomass models ---------------------------------

# Groupers
png(filename = "figures/networks/groupers_net_temp_mass.png", res = 150, width = 12, height = 3.7, units = "in")
plotMRF_net_cont(data = species_mats$grps, MRF_mod = spatial_models$grps_spat, node_names = guilds$groupers,
                 covariate = all_covs[1], main = "", cutoff = 0.03, plot = TRUE)
dev.off()

# Seabreams
png(filename = "figures/networks/seabreams_net_temp_mass.png", res = 150, width = 12, height = 3.7, units = "in")
plotMRF_net_cont(data = species_mats$dip, MRF_mod = spatial_models$dip_spat, node_names = guilds$diplodus,
                 covariate = all_covs[1], main = "", cutoff = 0.03, plot = TRUE)
dev.off()

# Herbivores
png(filename = "figures/networks/herbivores_net_temp_mass.png", res = 150, width = 12, height = 3.7, units = "in")
plotMRF_net_cont(data = species_mats$herb, MRF_mod = spatial_models$herb_spat, node_names = guilds$herbivores, 
                 covariate = all_covs[1], main = "", cutoff = 0.03, plot = TRUE)
dev.off()


## MPA networks for Biomass models -----------------------------------------

plotMRF_net_factor(species_mats$grps, spatial_models$grps, guilds$groupers, covariate = "mpa")

plotMRF_net_factor(species_mats$dip, spatial_models$dip, guilds$diplodus, covariate = "mpa")

plotMRF_net_factor(species_mats$herb, spatial_models$herb, guilds$herbivores, covariate = "mpa")

# png(filename = "figures/networks/groupers_net_mpa_mass.png", res = 150, width = 8, height = 5, units = "in")
# plotMRF_net_factor(species_mats$grps, spatial_models$grps, guilds$groupers, covariate = "mpa")
# dev.off()
# 
# png(filename = "figures/networks/seabreams_net_mpa_mass.png", res = 150, width = 8, height = 5, units = "in")
# plotMRF_net_factor(species_mats$dip, spatial_models$dip, guilds$diplodus, covariate = "mpa")
# dev.off()
# 
# png(filename = "figures/networks/herbivores_net_mpa_mass.png", res = 150, width = 8, height = 5, units = "in")
# plotMRF_net_factor(species_mats$herb, spatial_models$herb, guilds$herbivores, covariate = "mpa")
# dev.off()

