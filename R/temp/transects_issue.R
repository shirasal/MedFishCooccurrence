######################## TRANSECTs ########################

source(packages)

# What is the unit of observation in my data?
# Right now, I have all transects, ever. But maybe some points are represented more than others?

medata <- read_rds("data/medata.Rds")

medata %>% 
  ggplot() + 
  aes(x = lon, y = lat, col = country) +
  geom_count(alpha = .5)
ggsave("figures/EDA/transects_count_by_location.png", scale = c(2,1))
