
predictions <- predict_MRF(data = species_mats$dip_mat, MRF_mod = poisson_models$dip_pois)

head(predictions)
colnames(predictions) <- colnames(species_mats$dip_mat[,1:4])

predictions <- predictions %>% 
  as.data.frame() %>% 
  rownames_to_column("site")
  
dip_obs <- species_mats$dip_mat %>% 
  select(1:4) %>% 
  rownames_to_column("site")

ggplot() + 
  geom_point(data = dip_obs, aes(x = site, y = Diplodus.sargus), shape = 19, cex = 2, col = "blue", alpha = .5) +
  geom_point(data = predictions, aes(x = site, y = Diplodus.sargus), shape = 1, cex = 2, col = "red") + 
  scale_y_log10()

ggplot() + 
  geom_point(data = dip_obs, aes(x = site, y = Diplodus.vulgaris), shape = 19, cex = 2, col = "blue", alpha = .5) +
  geom_point(data = predictions, aes(x = site, y = Diplodus.vulgaris), shape = 1, cex = 2, col = "red") + 
  scale_y_log10()

dip_obs %>% 
  select(site, obs = Diplodus.sargus) %>% 
  mutate(pred = predictions$Diplodus.sargus) %>% 
  ggplot() + aes(x = obs, y = pred) %>% 
  geom_point() + 
  ggtitle("Diplodus sargus")

dip_obs %>% 
  select(site, obs = Diplodus.vulgaris) %>% 
  mutate(pred = predictions$Diplodus.vulgaris) %>% 
  ggplot() + aes(x = obs, y = pred) %>% 
  geom_point() + 
  ggtitle("Diplodus vulgaris")

dip_obs %>% 
  select(site, obs = Diplodus.puntazzo) %>% 
  mutate(pred = predictions$Diplodus.puntazzo) %>% 
  ggplot() + aes(x = obs, y = pred) %>% 
  geom_point() + 
  ggtitle("Diplodus puntazzo")

dip_obs %>% 
  select(site, obs = Diplodus.annularis) %>% 
  mutate(pred = predictions$Diplodus.annularis) %>% 
  ggplot() + aes(x = obs, y = pred) %>% 
  geom_point() + 
  ggtitle("Diplodus annularis")
