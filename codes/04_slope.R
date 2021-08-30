#Load packages
source("00_load_packages.R")

##################### 1. Read TRI and slope files ##############################

### Read Terrain Ruggedness Index for the whole planet
tri <- terra::rast(here("shapefiles", "slope", "data", "tri.txt"))

# Get Brazilian Borders
brazil <- getData("GADM",country="Brazil",level=0)
brazil <- terra::vect(brazil)

# Reprojects raster
terra::crs(tri) <- terra::crs(brazil)


# Restrict to Brazilian borders
tri_crop <- terra::crop(tri, brazil)

tri_mask <- terra::mask(tri_crop, brazil)

# plot(tri)
# plot(tri_crop)
# plot(tri_mask)


# Saving
terra::writeRaster(tri_mask, filename = here("output", "slope", "tri_br_raw.tif"))




### Read Slope for the whole planet
slope <- terra::rast(here("shapefiles", "slope", "data", "slope.txt"))

# Get Brazilian Borders
brazil <- getData("GADM",country="Brazil",level=0)
brazil <- terra::vect(brazil)

# Reprojects raster
terra::crs(slope) <- terra::crs(brazil)

# Restrict to Brazilian borders
slope_crop <- terra::crop(slope, brazil)

slope_mask <- terra::mask(slope_crop, brazil)

# plot(slope)
# plot(slope_crop)
# plot(slope_mask)


# Saving
terra::writeRaster(slope_mask, filename = here("output", "slope", "slope_br_raw.tif"))




### Read Cell Area for the whole planet
cellarea <- terra::rast(here("shapefiles", "slope", "data", "cellarea.txt"))

# Get Brazilian Borders
brazil <- getData("GADM",country="Brazil",level=0)
brazil <- terra::vect(brazil)

# Reprojects raster
terra::crs(cellarea) <- terra::crs(brazil)

# Restrict to Brazilian borders
cellarea_crop <- terra::crop(cellarea, brazil)

cellarea_mask <- terra::mask(cellarea_crop, brazil)

# plot(cellarea)
# plot(cellarea_crop)
# plot(cellarea_mask)


# Saving
terra::writeRaster(cellarea_mask, filename = here("output", "slope", "cellarea_br_raw.tif"))




####### 2. Merging with cellarea and calculating average TRI and slope #########

# Load TRI in Brazil
tri_br <- raster::raster(here("output", "slope", "tri_br_raw.tif"))

# Load Slope in Brazil
slope_br <- raster(here("output", "slope", "slope_br_raw.tif"))

# Load Cell Area in Brazil
cellarea_br <- raster(here("output", "slope", "cellarea_br_raw.tif"))

# 1872 municipality borders
mun_1872 <- st_read(here("shapefiles","mun_borders", "municip_1872", "malha_municipal_1872.shp"))



# Reprojects shapefile
mun_1872 = st_transform(mun_1872, projection(tri_br))

# Restrict to 1872 Brazilian borders
tri_br_mask <- mask(tri_br, mun_1872)

mun_1872 = st_transform(mun_1872, projection(cellarea_br))
cellarea_br_mask <- mask(cellarea_br, mun_1872)

mun_1872 = st_transform(mun_1872, projection(slope_br))
slope_br_mask <- mask(slope_br, mun_1872)


# Note that Acre is not there anymore
# plot(tri_br_mask)
# plot(cellarea_br_mask)
     
# Extracting Values
tri_br_values      = raster::extract(x = tri_br_mask, y = mun_1872, df = TRUE)
slope_br_values    = raster::extract(x = slope_br_mask, y = mun_1872, df = TRUE)
cellarea_br_values = raster::extract(x = cellarea_br_mask, y = mun_1872, df = TRUE)


# Transform millimeters to meters
tri_br_values %<>% mutate(tri_br_raw = tri_br_raw/1000)

# Transform slope magnitudes
slope_br_values %<>% mutate(slope_br_raw = slope_br_raw/1000)

# Join and calcualte weighted average
tri_slope_br <- tri_br_values

tri_slope_br$cellarea_br_raw <- cellarea_br_values$cellarea_br_raw
tri_slope_br$slope_br_raw <- slope_br_values$slope_br_raw


tri_slope_br %<>% group_by(ID) %>% mutate(tri_mean = mean(tri_br_raw, na.rm = TRUE)) %>%
  mutate(tri_weighted = weighted.mean(tri_br_raw, cellarea_br_raw, na.rm = TRUE)) %>%
  mutate(slope_mean = mean(slope_br_raw, na.rm = TRUE)) %>%
  mutate(slope_weighted = weighted.mean(slope_br_raw, cellarea_br_raw, na.rm = TRUE)) %>% 
  ungroup()

tri_slope_br %<>% distinct(ID, .keep_all = TRUE)

tri_slope_br %<>% dplyr::select(ID, tri_weighted, slope_weighted)

tri_slope_br$codigo <- mun_1872$codigo
tri_slope_br$nome   <- mun_1872$nome

tri_slope_br_1872 <- tri_slope_br %>%
  dplyr::select(codigo, nome, tri_weighted, slope_weighted)

# Assign TRI class dummies

load(here("output", "slope", "tri_slope_br_1872.RData"))

tri_slope_br_1872 %<>% 
  mutate(d_tri_intermediate = ifelse(tri_weighted >= 162 & tri_weighted <= 239, 1, 0 )) %>%
  mutate(d_tri_moderate = ifelse(tri_weighted >= 240 & tri_weighted <= 497, 1, 0 )) %>%
  mutate(d_tri_intermediate_higher = ifelse(tri_weighted >= 162, 1, 0 )) %>% 
  mutate(d_tri_slightly_higher = ifelse(tri_weighted >= 117, 1, 0 ))

# Saving
save(tri_slope_br_1872, file = here("output", "slope", "tri_slope_br_1872.RData"))



####################### 3. Average Altitude ####################################

### Read Altitude file
altitude <- terra::rast(here("data", "slope", "data", "slope_br", "altitude_br.asc"))

# Get Brazilian Borders
brazil <- getData("GADM",country="Brazil",level=0)
brazil <- terra::vect(brazil)

# Reprojects raster
terra::crs(altitude) <- terra::crs(brazil)


# Restrict to Brazilian borders
altitude_crop <- terra::crop(altitude, brazil)

altitude_mask <- terra::mask(altitude_crop, brazil)

#plot(altitude)
#plot(altitude_crop)
#plot(altitude_mask)


# 1960-2010 AMCs
amc_1960 <- st_read(here("data","amc", "amc_1960_2010.shp"))


# Reprojects shapefile
altitude_mask <- raster::raster(altitude_mask)

amc_1960 = st_transform(amc_1960, projection(altitude_mask))

# Restrict to 1960 AMC Brazilian borders
altitude_br_mask <- mask(altitude_mask, amc_1960)


# Extracting Values
altitude_br_values = raster::extract(x = altitude_mask, y = amc_1960, df = TRUE)



# Join and calcualte weighted average
altitude_br <-altitude_br_values %>% group_by(ID) %>%
  mutate(altitude = mean(altitude_br, na.rm = TRUE)) %>%
  ungroup()

altitude_br %<>% distinct(ID, .keep_all = TRUE)

altitude_br %<>% dplyr::select(ID, altitude)


altitude_br$code2010    <- amc_1960$GEOCODIG_M
altitude_br$uf_amc      <- amc_1960$UF
altitude_br$uf_sigla    <- amc_1960$SIGLA
altitude_br$amc_1960    <- amc_1960$amc_1960_2




altitude_1960 <- altitude_br %>%
  dplyr::select(code2010, uf_amc, uf_sigla, amc_1960, altitude)


# Saving
save(altitude_1960, file = here("output", "slope", "altitude_1960amc.RData"))


