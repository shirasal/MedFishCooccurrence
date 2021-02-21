############################### BINARY MODEL ###############################
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

grps_pa <- species_mats$grps_mat
grps_pa[,1:4] <- ifelse(grps_pa[,1:4] > 0, 1, 0)

dip_pa <- species_mats$dip_mat
dip_pa[,1:4] <- ifelse(dip_pa[,1:4] > 0, 1, 0)

herb_pa <- species_mats$herb_mat
herb_pa[,1:4] <- ifelse(herb_pa[,1:4] > 0, 1, 0)

# Nonspatial Binomial CRF -------------------------------------------------

grps_bi <- MRFcov(grps_pa, n_nodes = 4, family = "binomial")
dip_bi <- MRFcov(dip_pa, n_nodes = 4, family = "binomial")
herb_bi <- MRFcov(herb_pa, n_nodes = 4, family = "binomial")


## Relative importance ----------------------------------------------------

grps_bi_relimp <- rel_imp_sum(grps_bi)
dip_bi_relimp <- rel_imp_sum(dip_bi)
herb_bi_relimp <- rel_imp_sum(herb_bi)

p_relimp_grps_bi <- plot_relimp(grps_bi_relimp, "grps", "Groupers")
# ggsave(plot = p_relimp_grps_bi, filename = "figures/relimp_grps_bi_nonspat.png", device = "png", 
#        dpi = 300, width = 11.74, height = 4, units = "in")
p_relimp_dip_bi <- plot_relimp(dip_bi_relimp, "dip", "Seabreams")
# ggsave(plot = p_relimp_dip_bi, filename = "figures/relimp_dip_bi_nonspat.png", device = "png", 
#        dpi = 300, width = 11.74, height = 4, units = "in")
p_relimp_herb_bi <- plot_relimp(herb_bi_relimp, "herb", "Herbivores")
# ggsave(plot = p_relimp_herb_bi, filename = "figures/relimp_herb_bi_nonspat.png", device = "png", 
#        dpi = 300, width = 11.74, height = 4, units = "in")

patch_bi_plot <- p_relimp_grps_bi / p_relimp_dip_bi / p_relimp_herb_bi
# Remove y axis titles from first and third subplots and enlarge the middle one's
patch_bi_plot[[1]] <- patch_bi_plot[[1]] + theme(axis.title.y = element_blank())
patch_bi_plot[[2]] <- patch_bi_plot[[2]] + theme(axis.title.y = element_text(size = 14, vjust = 5))
patch_bi_plot[[3]] <- patch_bi_plot[[3]] + theme(axis.title.y = element_blank())

patch_bi_plot
ggsave(filename = "figures/rel_imp_bi_nonspat.png", device = "png", 
       dpi = 150, height = 10, width = 10, units = "in")


### Nonstationarity -------------------------------------------------------

all_relimp <- list(grps = grps_bi_relimp,
                   dip = dip_bi_relimp,
                   herb = herb_bi_relimp)

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
