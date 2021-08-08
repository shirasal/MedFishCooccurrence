
### Guilds
groupers <- c("Epinephelus.costae", "Epinephelus.marginatus", "Serranus.cabrilla", "Serranus.scriba")
diplodus <- c("Diplodus.annularis", "Diplodus.puntazzo", "Diplodus.sargus", "Diplodus.vulgaris")
herbivores <- c("Siganus.rivulatus", "Siganus.luridus", "Sarpa.salpa", "Sparisoma.cretense")

guild_colours <- list(grps = "#c54607", dip = "#145d82", herb = "#43aa8b")

# write_rds(guild_colours, "data/processed/guild_colours.rds")

guilds <- list(groupers = c("Epinephelus.costae", "Epinephelus.marginatus", "Serranus.cabrilla", "Serranus.scriba"),
     diplodus = c("Diplodus.annularis", "Diplodus.puntazzo", "Diplodus.sargus", "Diplodus.vulgaris"),
     herbivores = c("Siganus.rivulatus", "Siganus.luridus", "Sarpa.salpa", "Sparisoma.cretense"),
     colours = list(groupers = "#c54607", diplodus = "#145d82", herbivores = "#43aa8b"))

# write_rds(guilds, "data/processed/guilds_list.rds")

#### Covariates (based on med_clean column names)
env_cov <- c("temp", "depth", "prod")
mpa_cov <- "mpa"
all_covs <- c(env_cov, mpa_cov)
# write_rds(all_covs, "data/all_covs.rds")