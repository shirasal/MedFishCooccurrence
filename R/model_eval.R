source("R/packages.R")
source("R/functions.R")
source("R/2 biomass_spatial_models.R", echo = TRUE)

set.seed(100)

cv.biomass.list <- map2(.x = species_mats, .y = coord_dfs, .f = function(x, y){
  cv_MRF_diag_rep_spatial(data = x, coords = y, n_nodes = 4, family = "gaussian",
                  compare_null = TRUE, plot = FALSE, n_fold_runs = 100)})
names(cv.biomass.list) <- names(guilds)[1:3] 

cv_MRF_diag_rep_spatial(data = species_mats$grps, coords = coord_dfs$grps, 
                        n_nodes = 4, family = "gaussian",
                        compare_null = TRUE, plot = FALSE, n_fold_runs = 100)

# Plot each
lapply(1:3, function(i){
  guild <- cv.biomass.list[[i]]
  deviance <- guild %>% 
    ggplot() + aes(y = Rsquared, x = model) + 
    geom_boxplot() + 
    theme(axis.text.x = element_blank()) +
    labs(x = '') +
    ggtitle(names(cv.biomass.list)[i])
  mse <- guild %>% 
    ggplot() + aes(y = MSE, x = model) + 
    geom_boxplot()
  
  return(gridExtra::grid.arrange(deviance, mse, ncol = 1))
})

# Generate a table of R-squared values for each group
tibble(Group = c("Groupers", "Seabreams", "Herbivores"),
       Mean_R2 = c(cv.biomass.list$grps %>% filter(model == "CRF") %>% 
                     summarise(round(mean(Rsquared), 3)) %>% as.double(),
                   cv.biomass.list$dip %>% filter(model == "CRF") %>% 
                     summarise(round(mean(Rsquared), 3)) %>% as.double(),
                   cv.biomass.list$herb %>% filter(model == "CRF") %>% 
                     summarise(round(mean(Rsquared), 3)) %>% as.double()
       )) %>% #write_csv("data/results/mean_r_squared_values.csv")
  formattable::formattable()

