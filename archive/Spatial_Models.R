
source("R/packages.R")
source("R/functions.R")
source("R/create_spp_mat_function.R")
source("R/assist_vectors.R")

medata <- read_rds("data/medata.Rds") %>% 
  mutate(site = case_when(str_starts(site, "assecret") ~ 
                            str_extract(site, "assecret\\d\\d\\d\\d\\d\\d\\d"),
                          TRUE ~ as.character(site)))

med_clean <- medata %>%
  mutate(mpa = if_else(enforcement <= 1, FALSE, TRUE),
         temp = tmean,
         prod = pp_mean,
         biomass = (a*sp.length^b)*sp.n) %>%
  select(site, lon, lat, trans, species, sp.n, biomass, mpa, temp, depth, prod)

med_clean_east <- medata %>%
  filter(country == "Israel" | country == "Greece" | country == "Turkey") %>% # extra filtering - east med
  filter(data.origin != "azz_asi") %>% # presence-absence
  mutate(mpa = if_else(enforcement <= 1, FALSE, TRUE),
         temp = tmean,
         prod = pp_mean,
         biomass = (a*sp.length^b)*sp.n) %>%
  select(site, lon, lat, trans, species, sp.n, biomass, mpa, temp, depth, prod)

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

coord_dfs <- list(grps_coords, dip_coords, herb_coords)

############################################### SPATIAL MODELS ###############################################

set.seed(100)
biomass_models <- map2(.x = species_mats, .y = coord_dfs, 
                         function(x, y){MRFcov_spatial(x, n_nodes = 4, family = "gaussian", coords = y)})
names(biomass_models) <- c("grps_mass", "dip_mass", "herb_mass")

biomass_models$dip_mass$key_coefs

# names(species_mats) <- c("grps_mat", "dip_mat", "herb_mat")
# names(coord_dfs) <- c("grps_coords", "dip_coords", "herb_coords")

######################################### SUMMARISE MODELS AND PLOT #########################################

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
# ggsave("figures/mass/rel_imp_mass.png", device = "png", dpi = 150, height = 10, width = 10, units = "in")
# ggsave("figures/mass/rel_imp_mass.pdf", device = "pdf", dpi = 150, height = 10, width = 10, units = "in")
