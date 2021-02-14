# source("R/packages.R")

### Summarise the relative importance of each type of covariate for each species
rel_imp_sum <- function(guild_mod){
  env_relimp <- sapply(guild_mod$key_coefs, FUN = function(x) x %>%  # Take the taxa model and apply the following:
                         filter(Variable %in% env_cov) %>%  # Filter by relevant covariates
                         summarise(n = sum(Rel_importance))) %>% # Summarise Rel_importance column
    unlist() %>% # Take out of the list
    enframe(name = "species", value = "env_rel_imp") %>%  # Rearrange
    mutate(species = str_sub(string = species, end = -3)) # Fix species names
  
  mpa_relimp <- sapply(guild_mod$key_coefs, FUN = function(x) x %>% 
                            filter(Variable %in% mpa_cov) %>%
                            summarise(n = sum(Rel_importance))) %>% 
    unlist() %>%
    enframe(name = "species", value = "mpa_rel_imp") %>% 
    mutate(species = str_sub(string = species, end = -3))
  
  biotic_relimp <- sapply(guild_mod$key_coefs, FUN = function(x) x %>% 
                            filter(!(Variable %in% env_cov | Variable %in% mpa_cov | str_detect(string = Variable, pattern = "_"))) %>%
                            summarise(n = sum(Rel_importance))) %>% 
    unlist() %>%
    enframe(name = "species", value = "biotic_rel_imp") %>% 
    mutate(species = str_sub(string = species, end = -3))
  
  env_bio_relimp <- sapply(guild_mod$key_coefs, FUN = function(x) x %>% 
                             filter(str_detect(string = Variable, pattern = "temp_")) %>% 
                             summarise(n = sum(Rel_importance))) %>% 
    unlist() %>%
    enframe(name = "species", value = "env_bio_rel_imp") %>% 
    mutate(species = str_sub(string = species, end = -3))
  
  mpa_bio_relimp <- sapply(guild_mod$key_coefs, FUN = function(x) x %>% 
                                filter(str_detect(string = Variable, pattern = "mpa_")) %>% 
                                summarise(n = sum(Rel_importance))) %>% 
    unlist() %>%
    enframe(name = "species", value = "mpa_bio_rel_imp") %>% 
    mutate(species = str_sub(string = species, end = -3))
  
  env_relimp %>%
    left_join(mpa_relimp, by = "species") %>%
    left_join(biotic_relimp, by = "species") %>%
    left_join(env_bio_relimp, by = "species") %>%
    left_join(mpa_bio_relimp, by = "species")
}

### Plot relative importance of covariates by covariate for each species, within guild:
plot_relimp <- function(rel_imp_df, guild_col, guild_name){
  rel_imp_df %>%
    pivot_longer(2:length(.)) %>%
    rename(covariate = name, rel_imp = value) %>%
    mutate(species = str_replace_all(species, "\\.", "\\ ")) %>%
    mutate(facet.title = case_when(covariate == "env_rel_imp" ~ "Environment",
                                   covariate == "mpa_rel_imp" ~ "MPA",
                                   covariate == "biotic_rel_imp" ~ "Biotic Associations",
                                   covariate == "env_bio_rel_imp" ~ "Temp * Biotic",
                                   covariate == "mpa_bio_rel_imp" ~ "MPA * Biotic")) %>%
    mutate(facet.title = fct_relevel(facet.title,
                                     "Environment", "MPA", "Biotic Associations",
                                     "Temp * Biotic", "MPA * Biotic")) %>%
    ggplot() +
    aes(x = species, y = rel_imp) +
    stat_summary(geom = "bar", fun = mean, position = "dodge",  fill = guild_colours[[guild_col]]) +
    facet_wrap(~facet.title, nrow = 1) +
    labs(subtitle = guild_name, y = "Relative Importance (prop.)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"), strip.placement = "outside",
          axis.title.x = element_blank(), 
          strip.text.x = element_text(size = 12, face = "bold"),
          plot.margin = margin(.2,1,.2,1, "cm"))
}
# 
# 
# 
# 
# ### MPA predictions visualisation
# vis_mpa_pred_pair <- function(species_i, species_j, spp_mat, spp_mod, guild){
#   # Create a vector of all other species except species_j
#   all_other_species <- guild[-which(guild == species_j)]
#   
#   # Scenario 1: species_j is absent, other species are at their mean abundance
#   set.seed(10)
#   j_abs <- spp_mat %>% 
#     mutate(temp = median(temp),
#            depth = median(depth),
#            prod = median(prod),
#            across(all_of(species_j), .fns = function(x) 0),
#            across(all_of(all_other_species), .fns = mean)) %>% 
#     group_by(mpa) %>% 
#     sample_n(1) %>% 
#     ungroup()
#   # Scenario 2: species_j is at its 90th percentile abundance, other species are at their mean abundance
#   set.seed(10)
#   j_max <- spp_mat %>% 
#     mutate(temp = median(temp),
#            depth = median(depth),
#            prod = median(prod),
#            across(.cols = all_of(species_j), .fns = function(x) quantile(x, 0.9)),
#            across(all_of(all_other_species), .fns = mean)) %>% 
#     group_by(mpa) %>% 
#     sample_n(1) %>% 
#     ungroup()
#   
#   # Create predictions
#   ## For when species j is absent
#   predict_abs <- predict_MRF(j_abs, spp_mod) %>% 
#     `colnames<-`(guild) %>% 
#     as_data_frame() %>% 
#     mutate(mpa = j_abs$mpa)
#   ## For when species j is absent
#   predict_max <- predict_MRF(j_max, spp_mod) %>% 
#     `colnames<-`(guild) %>% 
#     as_data_frame() %>% 
#     mutate(mpa = j_max$mpa)
#   
#   # Put the two scenarios together
#   mpa_predict <- bind_rows(predict_abs, predict_max, .id = "scenario") %>% 
#     mutate(scenario = case_when(scenario == 1 ~ "absent",
#                                 scenario == 2 ~ "present"))
#   
#   # Visualise the predictions
#   ## Create a dataframe with all the predictions, sorted by scenario
#   predictions_mpa <- mpa_predict %>% 
#     pivot_longer(cols = all_of(2:5),
#                  names_to = "species",
#                  values_to = "prediction", 
#                  names_repair = "minimal")
#   
#   ## Plot the predictions:
#   predictions_mpa %>%
#     filter(species == species_i) %>% 
#     ggplot() +
#     aes(x = mpa, y = prediction, fill = scenario) +
#     stat_summary(geom = "bar", fun = "mean", position = "dodge") +
#     stat_summary(geom = "errorbar", fun.data = "mean_se", position = position_dodge(width = 0.8), width = 0.2) +
#     xlab("MPA") + ylab("Observation predictions (nonparanormal)") +
#     labs(subtitle = stringr::str_replace(species_i, "\\.", "\\ "),
#          fill = stringr::str_replace(species_j, "\\.", "\\ ")) +
#     scale_fill_manual(labels = c('Absent','Present'), values = c("#031D44", "#FF99C9")) +
#     theme(legend.title = element_text(face = "bold.italic"), plot.subtitle = element_text(face = "bold.italic"))
# }
# 
# ### Temperature predictions visualisation
# vis_temp_pred_pair <- function(species_i, species_j, spp_mat, spp_mod, guild){
#   # Create a vector of all other species except species_j
#   all_other_species <- guild[-which(guild == species_j)]
#   
#   # Scenario 1: species_j is absent, other species are at their mean abundance
#   set.seed(10)
#   j_abs <- spp_mat %>% 
#     mutate(depth = median(depth),
#            prod = median(prod),
#            mpa = TRUE,
#            temperature = spp_mat$temp * attr(spp_mat$temp, 'scaled:scale') + attr(spp_mat$temp, 'scaled:center'),
#            across(.cols = all_of(species_j), .funs = function(x) 0),
#            across(.cols = all_of(all_other_species), .fns = mean)) %>% 
#     group_by(temperature = round(temperature, digits = 1)) %>% 
#     sample_n(1) %>%  
#     ungroup() %>% 
#     mutate(temp = scale(temperature))
#   # Scenario 2: species_j is at its 90th percentile abundance, other species are at their mean abundance
#   set.seed(10)
#   j_max <- spp_mat %>% 
#     mutate(depth = median(depth),
#            prod = median(prod),
#            mpa = TRUE,
#            temperature = spp_mat$temp * attr(spp_mat$temp, 'scaled:scale') + attr(spp_mat$temp, 'scaled:center'),
#            across(.cols = all_of(species_j), .funs = quantile(species_j, 0.9)),
#            across(.cols = all_of(all_other_species), .fns = mean)) %>% 
#     group_by(temperature = round(temperature, digits = 1)) %>% 
#     sample_n(1) %>%  
#     ungroup() %>% 
#     mutate(temp = scale(temperature))
#   
#   # Create predictions
#   ## For when species j is absent
#   predict_abs <- predict_MRF(j_abs, spp_mod) %>% 
#     `colnames<-`(guild) %>% 
#     as_data_frame() %>% 
#     mutate(temp = j_abs$temperature)
#   ## For when species j is absent
#   predict_max <- predict_MRF(j_max, spp_mod) %>% 
#     `colnames<-`(guild) %>% 
#     as_data_frame() %>% 
#     mutate(temp = j_max$temperature)
#   
#   # Put the two scenarios together
#   temp_predict <- bind_rows(predict_abs, predict_max, .id = "scenario") %>% 
#     mutate(scenario = case_when(scenario == 1 ~ "absent",
#                                 scenario == 2 ~ "present"))
#   
#   # Visualise the predictions
#   ## Create a dataframe with all the predictions, sorted by scenario
#   predictions_temp <- temp_predict %>% 
#     pivot_longer(cols = all_of(2:5),
#                  names_to = "species",
#                  values_to = "prediction", 
#                  names_repair = "minimal")
#   
#   ## Plot the predictions:
#   predictions_temp %>%
#     filter(species == species_i) %>% 
#     ggplot() +
#     aes(x = temp, y = prediction, col = scenario) +
#     geom_smooth(method = "lm", formula = y ~ x, cex = 3, alpha = 0.1) +
#     xlab(paste("Temperature", "(°C)")) + ylab("Observations prediction") +
#     labs(subtitle = stringr::str_replace(species_i, "\\.", "\\ "),
#          col = stringr::str_replace(species_j, "\\.", "\\ ")) +
#     scale_color_manual(labels = c('Absent','Present'), values = c("#031D44", "#FF99C9")) +
#     theme(legend.title = element_text(face = "bold.italic"), plot.subtitle = element_text(face = "bold.italic"))
# }
# 
# ### Temperature raw data visualisation
# vis_raw_temp <- function(spp_mat, species_i, species_j) {
#   spp_mat %>% 
#     mutate(scenario = if_else(spp_mat[[species_j]] == 0, "absent", "present"),
#            Temperature = spp_mat$temp * attr(spp_mat$temp, 'scaled:scale') + attr(spp_mat$temp, 'scaled:center')) %>% 
#     ggplot() +
#     aes(x = Temperature, y = log2(spp_mat[[species_i]] + 0.1), group = scenario) +
#     geom_point(aes(colour = scenario), alpha = 0.1) +
#     stat_smooth(aes(colour = scenario), method = "gam", alpha = 0.3) +
#     xlab("Temperature (°C)") + ylab("Abundance (nonparanormal)") +
#     ggtitle(str_replace(species_i, "\\.", "\\ ")) +
#     scale_color_manual(name = str_replace(species_j, "\\.", "\\ "),
#                        labels = c('Absent','Present'), values = c("#031D44", "#FF99C9")) +
#     theme(plot.title = element_text(face = "bold.italic"),
#           legend.title = element_text(face = "bold.italic"))
# }
# 
# ### MPA raw data visualisation
# vis_raw_mpa <- function(spp_mat, species_i, species_j){
#   spp_mat %>% 
#     mutate(scenario = if_else(spp_mat[[species_j]] == 0, "absent", "present")) %>% 
#     ggplot() +
#     aes(x = mpa, y = log2(spp_mat[[species_i]] + 0.1), group = scenario) +
#     geom_boxplot(aes(colour = scenario), fill = "ghostwhite") +
#     xlab("MPA") + ylab("Observations prediction") +
#     ggtitle(str_replace(species_i, "\\.", "\\ ")) +
#     scale_color_manual(name = str_replace(species_j, "\\.", "\\ "), 
#                        labels = c('Absent','Present'), values = c("#031D44", "#FF99C9")) +
#     theme(plot.title = element_text(face = "bold.italic"),
#           legend.title = element_text(face = "bold.italic"))
# }
# 
# 
# 
# 
# ### Summarise coefficients for each species, separating positive from negative
# coefs_sum <- function(guild_mod, guild){
#   env_effect <- lapply(guild_mod$key_coefs, FUN = function(x) x %>%
#                          filter(Variable %in% env_cov) %>%
#                          group_by(Standardised_coef > 0) %>% 
#                          summarise(coefs = sum(Standardised_coef), .groups = "drop") %>% 
#                          transmute(direction = `Standardised_coef > 0`, env_coef = coefs) %>%
#                          mutate(direction = case_when(.$direction == TRUE ~ "pos",
#                                                       .$direction == FALSE ~ "neg",
#                                                       TRUE ~ as.character(.$direction)))) %>% 
#     bind_rows(.id = "id")
#   
#   temp_effect <- lapply(guild_mod$key_coefs, FUN = function(x) x %>%
#                           filter(Variable == "temp") %>%
#                           group_by(Standardised_coef > 0) %>% 
#                           summarise(coefs = sum(Standardised_coef), .groups = "drop") %>% 
#                           transmute(direction = `Standardised_coef > 0`, temp_coef = coefs) %>%
#                           mutate(direction = case_when(.$direction == TRUE ~ "pos",
#                                                        .$direction == FALSE ~ "neg",
#                                                        TRUE ~ as.character(.$direction)))) %>% 
#     bind_rows(.id = "id")
#   
#   anthro_effect <- lapply(guild_mod$key_coefs, FUN = function(x) x %>%
#                             filter(Variable %in% mpa_cov) %>%
#                             group_by(Standardised_coef > 0) %>% 
#                             summarise(coefs = sum(Standardised_coef), .groups = "drop") %>% 
#                             transmute(direction = `Standardised_coef > 0`, mpa_coef = coefs) %>%
#                             mutate(direction = case_when(.$direction == TRUE ~ "pos",
#                                                          .$direction == FALSE ~ "neg",
#                                                          TRUE ~ as.character(.$direction)))) %>% 
#     bind_rows(.id = "id")
#   
#   
#   biotic_effect <- lapply(guild_mod$key_coefs, FUN = function(x) x %>%
#                             filter(Variable %in% colnames(guild_mod$graph)) %>%
#                             group_by(Standardised_coef > 0) %>% 
#                             summarise(coefs = sum(Standardised_coef), .groups = "drop") %>% 
#                             transmute(direction = `Standardised_coef > 0`, bio_coef = coefs) %>%
#                             mutate(direction = case_when(.$direction == TRUE ~ "pos",
#                                                          .$direction == FALSE ~ "neg",
#                                                          TRUE ~ as.character(.$direction)))) %>% 
#     bind_rows(.id = "id")
#   
#   env_bio_effect <- lapply(guild_mod$key_coefs, FUN = function(x) x %>%
#                              filter(str_detect(string = Variable, pattern = "temp_")) %>%
#                              group_by(Standardised_coef > 0) %>% 
#                              summarise(coefs = sum(Standardised_coef), .groups = "drop") %>%
#                              transmute(direction = `Standardised_coef > 0`, bio_env_coef = coefs) %>%
#                              mutate(direction = case_when(.$direction == TRUE ~ "pos",
#                                                           .$direction == FALSE ~ "neg",
#                                                           TRUE ~ as.character(.$direction)))) %>%
#     bind_rows(.id = "id")
#   
#   anthro_bio_effect <- lapply(guild_mod$key_coefs, FUN = function(x) x %>%
#                                 filter(str_detect(string = Variable, pattern = "mpa_")) %>%
#                                 group_by(Standardised_coef > 0) %>% 
#                                 summarise(coefs = sum(Standardised_coef), .groups = "drop") %>%
#                                 transmute(direction = `Standardised_coef > 0`, bio_mpa_coef = coefs) %>%
#                                 mutate(direction = case_when(.$direction == TRUE ~ "pos",
#                                                              .$direction == FALSE ~ "neg",
#                                                              TRUE ~ as.character(.$direction)))) %>%
#     bind_rows(.id = "id")
#   
#   
#   bind_rows(env_effect, temp_effect, anthro_effect, biotic_effect, 
#             env_bio_effect, anthro_bio_effect) %>% 
#     arrange(direction, id) %>% 
#     rename(species = id)
# }
# 
