# Run models.R first

lapply(names(biomass_models$grps_mass$key_coefs), function(x)
  biomass_models$grps_mass$key_coefs[[x]] %>% 
    mutate(focal_species = x)) %>% 
  bind_rows %>% 
  mutate(focal_species = str_replace_all(focal_species, "\\.", "\\ "),
         Variable = str_replace_all(Variable, "\\.", "\\ "),
         Variable = str_replace_all(Variable, "_", "\\ x "),
         Variable = str_replace_all(Variable, "temp", "Temperature"),
         Variable = str_replace_all(Variable, "depth", "Depth"),
         Variable = str_replace_all(Variable, "prod", "Productivity"),
         Variable = str_replace_all(Variable, "mpa", "MPA")) %>%
  select("Focal species" = focal_species, Variable, RI = Rel_importance, 
         "Key coefficient" = Standardised_coef) %>% 
  formattable::formattable(align = c("l", "l", "l", "l"))

lapply(names(biomass_models$dip_mass$key_coefs), function(x)
  biomass_models$dip_mass$key_coefs[[x]] %>% 
    mutate(focal_species = x)) %>% 
  bind_rows %>% 
  mutate(focal_species = str_replace_all(focal_species, "\\.", "\\ "),
         Variable = str_replace_all(Variable, "\\.", "\\ "),
         Variable = str_replace_all(Variable, "_", "\\ x "),
         Variable = str_replace_all(Variable, "temp", "Temperature"),
         Variable = str_replace_all(Variable, "depth", "Depth"),
         Variable = str_replace_all(Variable, "prod", "Productivity"),
         Variable = str_replace_all(Variable, "mpa", "MPA")) %>%
  select("Focal species" = focal_species, Variable, RI = Rel_importance, 
         "Key coefficient" = Standardised_coef) %>% 
  formattable::formattable(align = c("l", "l", "l", "l"))

lapply(names(biomass_models$herb_mass$key_coefs), function(x)
  biomass_models$herb_mass$key_coefs[[x]] %>% 
    mutate(focal_species = x)) %>% 
  bind_rows %>% 
  mutate(focal_species = str_replace_all(focal_species, "\\.", "\\ "),
         Variable = str_replace_all(Variable, "\\.", "\\ "),
         Variable = str_replace_all(Variable, "_", "\\ x "),
         Variable = str_replace_all(Variable, "temp", "Temperature"),
         Variable = str_replace_all(Variable, "depth", "Depth"),
         Variable = str_replace_all(Variable, "prod", "Productivity"),
         Variable = str_replace_all(Variable, "mpa", "MPA")) %>%
  select("Focal species" = focal_species, Variable, RI = Rel_importance, 
         "Key coefficient" = Standardised_coef) %>% 
  formattable::formattable(align = c("l", "l", "l", "l"))


# Save --------------------------------------------------------------------

lapply(names(biomass_models$grps_mass$key_coefs), function(x)
  biomass_models$grps_mass$key_coefs[[x]] %>% 
    mutate(focal_species = x)) %>% 
  bind_rows %>% 
  mutate(focal_species = str_replace_all(focal_species, "\\.", "\\ "),
         Variable = str_replace_all(Variable, "\\.", "\\ "),
         Variable = str_replace_all(Variable, "_", "\\ x "),
         Variable = str_replace_all(Variable, "temp", "Temperature"),
         Variable = str_replace_all(Variable, "depth", "Depth"),
         Variable = str_replace_all(Variable, "prod", "Productivity"),
         Variable = str_replace_all(Variable, "mpa", "MPA")) %>%
  select("Focal species" = focal_species, Variable, RI = Rel_importance, 
         "Key coefficient" = Standardised_coef) %>% 
  write_csv("data/results/grps_coefs.csv")

lapply(names(biomass_models$dip_mass$key_coefs), function(x)
  biomass_models$dip_mass$key_coefs[[x]] %>% 
    mutate(focal_species = x)) %>% 
  bind_rows %>% 
  mutate(focal_species = str_replace_all(focal_species, "\\.", "\\ "),
         Variable = str_replace_all(Variable, "\\.", "\\ "),
         Variable = str_replace_all(Variable, "_", "\\ x "),
         Variable = str_replace_all(Variable, "temp", "Temperature"),
         Variable = str_replace_all(Variable, "depth", "Depth"),
         Variable = str_replace_all(Variable, "prod", "Productivity"),
         Variable = str_replace_all(Variable, "mpa", "MPA")) %>%
  select("Focal species" = focal_species, Variable, RI = Rel_importance, 
         "Key coefficient" = Standardised_coef) %>% 
  write_csv("data/results/dip_coefs.csv")

lapply(names(biomass_models$herb_mass$key_coefs), function(x)
  biomass_models$herb_mass$key_coefs[[x]] %>% 
    mutate(focal_species = x)) %>% 
  bind_rows %>% 
  mutate(focal_species = str_replace_all(focal_species, "\\.", "\\ "),
         Variable = str_replace_all(Variable, "\\.", "\\ "),
         Variable = str_replace_all(Variable, "_", "\\ x "),
         Variable = str_replace_all(Variable, "temp", "Temperature"),
         Variable = str_replace_all(Variable, "depth", "Depth"),
         Variable = str_replace_all(Variable, "prod", "Productivity"),
         Variable = str_replace_all(Variable, "mpa", "MPA")) %>%
  select("Focal species" = focal_species, Variable, RI = Rel_importance, 
         "Key coefficient" = Standardised_coef) %>% 
  write_csv("data/results/herb_coefs.csv")
