#Load packages
source("00_load_packages.R")

########################## 1. Read nightlights #################################

### Read Terrain Ruggedness Index for the whole planet
night_2000 <- terra::rast(here("shapefiles", "nightlights", "data",
                               "F152000.v4b_web.stable_lights.avg_vis.tif"))

night_2010 <- terra::rast(here("shapefiles", "nightlights", "data",
                               "F182010.v4d_web.stable_lights.avg_vis.tif"))

# Get Brazilian Borders
brazil <- getData("GADM",country="Brazil",level=0)
brazil <- terra::vect(brazil)

# Reprojects raster
terra::crs(night_2000) <- terra::crs(brazil)
terra::crs(night_2010) <- terra::crs(brazil)


# Restrict to Brazilian borders
night_2000_crop <- terra::crop(night_2000, brazil)
night_2000_mask <- terra::mask(night_2000, brazil)


night_2010_crop <- terra::crop(night_2010, brazil)
night_2010_mask <- terra::mask(night_2010, brazil)


# plot(night_2000)
# plot(night_2010)
# plot(night_2000_crop)
# plot(night_2000_mask)
# plot(night_2010_mask)

# Saving
terra::writeRaster(night_2000_mask, filename = here("output", "nightlights",
                                                    "nightlights_2000_br_raw.tif"))

terra::writeRaster(night_2010_mask, filename = here("output", "nightlights",
                                                    "nightlights_2010_br_raw.tif"))





############ 2. Calculating average nightlights per municipality ###############


night_2000 <- raster::raster(here("output", "nightlights",
                                  "nightlights_2000_br_raw.tif"))

night_2010 <- raster(here("output", "nightlights",
                          "nightlights_2010_br_raw.tif"))

# 2000 municipality borders
mun_2000 <- read_municipality(year=2000)

# 2010 municipality borders
mun_2010 <- read_municipality(year=2010)


# State borders
state_2000 <- read_state(year=2000)
state_2010 <- read_state(year=2010)



# First, to a SpatialPointsDataFrame
night_2000_pts <- rasterToPoints(night_2000, spatial = TRUE)
night_2000_df  <- data.frame(night_2000_pts)
rm(night_2000_pts, night_2000)


night_2010_pts <- rasterToPoints(night_2010, spatial = TRUE)
night_2010_df  <- data.frame(night_2010_pts)
rm(night_2010_pts, night_2010)


ggplot() +
  geom_raster(data = night_2000_df , aes(x = x, y = y, fill = nightlights_2000_br_raw)) +
  geom_sf(data = state_2000, fill = NA) +
  ggtitle("Teste 2000")


teste <- full_join(night_2000_df, night_2010_df, by = c("x", "y"))

teste %<>% mutate(diff_lights = nightlights_2010_br_raw - nightlights_2000_br_raw)

ggplot() +
  geom_raster(data = night_2010_df , aes(x = x, y = y, fill = nightlights_2010_br_raw)) +
  geom_sf(data = state_2000, fill = NA) +
  ggtitle("2010 Night Lights") +
  labs(fill = "Night Light Intensity") +
  theme(axis.title = element_blank(), legend.title=element_text(size=16), 
        plot.title = element_text(size=16)) 
  


ggplot() +
  geom_raster(data = teste , aes(x = x, y = y, fill = diff_lights)) +
  geom_sf(data = state_2000, fill = NA) +
  ggtitle("Change in Night Lights") +
  labs(fill = "Change in Night Light Intensity (2000-2010)") +
  theme(axis.title = element_blank(), legend.title=element_text(size=16), 
        plot.title = element_text(size=16)) 




### Mean value inside each municipality
rm(list = ls())
night_2000 <- terra::rast(here("output", "nightlights", "nightlights_2000_br_raw.tif"))
night_2010 <- terra::rast(here("output", "nightlights", "nightlights_2010_br_raw.tif"))


# 2000 municipality borders
mun_2000 <- read_municipality(year=2000)

# 2010 municipality borders
mun_2010 <- read_municipality(year=2010)

# Transforming into terra format
mun_2000 <- terra::vect(mun_2000)
mun_2010 <- terra::vect(mun_2010)


# Reprojects raster
terra::crs(mun_2000) <- terra::crs(night_2000)
terra::crs(mun_2010) <- terra::crs(night_2010)


night_2000_avg <- terra::extract(night_2000, mun_2000, fun = mean, na.rm = TRUE)


night_2000_avg %<>% mutate(cod = mun_2000$code_muni) %>% 
  mutate(mun_name = mun_2000$name_muni) %>% dplyr::select(-ID)

colnames(night_2000_avg) <- c("light_2000", "cod", "mun_name_2000")

night_2010_avg <- terra::extract(night_2010, mun_2010, fun = mean, na.rm = TRUE)

night_2010_avg %<>% mutate(cod = mun_2010$code_muni) %>% 
  mutate(mun_name = mun_2010$name_muni) %>% dplyr::select(-ID)


colnames(night_2010_avg) <- c("light_2010", "cod", "mun_name_2010")


light_join <- inner_join(night_2000_avg, night_2010_avg, by = "cod")

# Passing logs
light_join %<>% mutate(log_light_2000 = log(1 + light_2000)) %>%
  mutate(log_light_2010 = log(1 + light_2010)) %>%
  mutate(diff_light = light_2010 - light_2000) %>%
  mutate(log_diff_light = log_light_2010 - log_light_2000) %>% as_tibble()


save(light_join, file = here("output", "nightlights", "light_2000_2010.RData"))


