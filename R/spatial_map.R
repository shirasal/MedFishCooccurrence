library(raster)
library(sf)
library(tidyverse)
library(sdmpredictors) 

med_shp <- sf::st_read("C:/Users/shira/Documents/MSc/GIS/Mediterranean Sea/Mediterranean_Sea_Area.shp")
med_ext <- raster::extent(med_shp)

# list_layers(marine = TRUE) %>% View

# Temperature -------------------------------------------------------------
# tmean <- load_layers("BO_sstmean", datadir = "data/Bio-Oracle") # BO21_tempmean_ss
tmean <- raster("data/Bio-Oracle/BO_sstmean_lonlat.tif")
med_temp <- raster::crop(tmean, med_ext)
map_temp <- rasterToPoints(med_temp)
med_temp_df <- data.frame(map_temp) %>% rename(Longitude = x, Latitude = y, Temperature = "BO_sstmean_lonlat")


# Depth -------------------------------------------------------------------
# bathy <- load_layers("BO_bathymean", datadir = "data/Bio-Oracle")
bathy <- raster("data/Bio-Oracle/BO_bathymean_lonlat.tif")
med_bathy <- raster::crop(bathy, med_ext)
map_bathy <- rasterToPoints(med_bathy, maxpixels = 10)
med_bathy_df <- data.frame(map_bathy) %>% rename(Longitude = x, Latitude = y, Bathymetry = "BO_bathymean_lonlat")

# MPAs --------------------------------------------------------------------
med_clean <- read_rds("data/processed/med_clean.rds")


# Plot --------------------------------------------------------------------
med_clean %>% 
  mutate(mpa = ifelse(is.na(mpa), "Unknown", mpa)) %>% 
  ggplot() +
  aes(y = Latitude, x = Longitude) + 
  stat_contour(data = med_bathy_df, aes(z = Bathymetry), col = "lightgray") +
  geom_raster(data = med_temp_df, aes(fill = Temperature), alpha = .5) +
  scale_fill_gradientn(colours = c("#313695", "#ffffbf", "#a50026"), name = "Temperature °C") +
  # stat_sum(data = med_clean, aes(x = lon, y = lat, shape = mpa), alpha = .5) +
  geom_point(aes(x = lon, y = lat, shape = mpa), size = 3,alpha = .5) + 
  scale_shape_manual(values = c(4, 18, 1), 
                     labels = c("Not protected", "Protected", "No protection data"), name = "Sampling sites") +
  theme_bw() +
  coord_quickmap() + 
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14, angle = 90),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right")

# ggsave("figures/med_map.png", device = "png", dpi = 150, scale = c(2,2))

