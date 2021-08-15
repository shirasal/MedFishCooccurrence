source("R/models.R")

set.seed(100)
cv.biomass.list <- lapply(species_mats_mass, FUN = function(x){
  cv_MRF_diag_rep(data = x, n_nodes = 4, family = 'gaussian',
                  compare_null = TRUE, plot = FALSE, n_fold_runs = 100)})
names(cv.biomass.list) <- names(guilds)[1:3] 

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

tibble(Group = c("Groupers", "Seabreams", "Herbivores"),
       Mean_R2 = c(cv.biomass.list$groupers %>% filter(model == "CRF") %>% 
                     summarise(round(mean(Rsquared), 3)) %>% as.double(),
                   cv.biomass.list$diplodus %>% filter(model == "CRF") %>% 
                     summarise(round(mean(Rsquared), 3)) %>% as.double(),
                   cv.biomass.list$herbivores %>% filter(model == "CRF") %>% 
                     summarise(round(mean(Rsquared), 3)) %>% as.double()
       )) %>% 
  formattable::formattable()

