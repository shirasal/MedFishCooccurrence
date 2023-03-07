source("R/packages.R")
source("R/functions.R")
source("R/2 biomass_spatial_models.R", echo = TRUE)

# Biomass network gradients ------------------------------------------------

png(filename = "figures/networks/groupers_mass_network_overall.png", res = 150, height = 6, width = 6, unit = "in")
plotMRF_net(data = species_mats$grps, MRF_mod = spatial_models$grps_spat, node_names = guilds$groupers, 
            covariate = all_covs[2], cutoff = 0.03, type = "all")
dev.off()

png(filename = "figures/networks/seabreams_mass_network_overall.png", res = 150, height = 6, width = 6, unit = "in")
plotMRF_net(data = species_mats$dip, MRF_mod = spatial_models$dip_spat, node_names = guilds$diplodus, 
            covariate = all_covs[2], cutoff = 0.03, type = "all")
dev.off()

png(filename = "figures/networks/herb_mass_network_overall.png", res = 150, height = 6, width = 6, unit = "in")
plotMRF_net(data = species_mats$herb, MRF_mod = spatial_models$herb_spat, node_names = guilds$herbivores, 
            covariate = all_covs[2], cutoff = 0.03, type = "all")
dev.off()


## Temperature networks for Biomass models ---------------------------------

# Groupers
png(filename = "figures/networks/groupers_net_temp_mass.png", res = 150, width = 12, height = 3.7, units = "in")
plotMRF_net(data = species_mats$grps, MRF_mod = spatial_models$grps_spat, node_names = guilds$groupers,
                 covariate = all_covs[1], cutoff = 0.03, type = "cont")
dev.off()

# Seabreams
png(filename = "figures/networks/seabreams_net_temp_mass.png", res = 150, width = 12, height = 3.7, units = "in")
plotMRF_net(data = species_mats$dip, MRF_mod = spatial_models$dip_spat, node_names = guilds$diplodus,
                 covariate = all_covs[1], cutoff = 0.03, type = "cont")
dev.off()

# Herbivores
png(filename = "figures/networks/herbivores_net_temp_mass.png", res = 150, width = 12, height = 3.7, units = "in")
plotMRF_net(data = species_mats$herb, MRF_mod = spatial_models$herb_spat, node_names = guilds$herbivores, 
                 covariate = all_covs[1], cutoff = 0.03, type = "cont")
dev.off()


## MPA networks for Biomass models -----------------------------------------

# Groupers
png(filename = "figures/networks/groupers_net_mpa_mass.png", res = 150, width = 8, height = 5, units = "in")
plotMRF_net(data = species_mats$grps, MRF_mod = spatial_models$grps_spat, 
                   covariate = all_covs[4], cutoff = 0.03, type = "factor")
dev.off()

# Seabreams
png(filename = "figures/networks/seabreams_net_mpa_mass.png", res = 150, width = 8, height = 5, units = "in")
plotMRF_net(data = species_mats$dip, MRF_mod = spatial_models$dip_spat, 
                   covariate = all_covs[4], cutoff = 0.03, type = "factor")
dev.off()

# Herbivores
png(filename = "figures/networks/herbivores_net_mpa_mass.png", res = 150, width = 8, height = 5, units = "in")
plotMRF_net(data = species_mats$herb, MRF_mod = spatial_models$herb_spat, node_names = guilds$herbivores, 
                 covariate = all_covs[4], cutoff = 0.03, type = "factor")
dev.off()

