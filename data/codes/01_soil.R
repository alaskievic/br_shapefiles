#Load packages
source("00_load_packages.R")


############### 1. Reads Soil Shapefile from EMBRAPA ###########################

# Soil shapefile
soil_shp <- st_read(here("data", "soil", "soil_types", "brasil_solos_5m_20201104.shp"))

# 1872 municipality borders
mun_1872 <- st_read(here("data", "mun_borders", "municip_1872", "malha_municipal_1872.shp"))

st_crs(soil_shp)
st_crs(mun_1872)

# Assign same coordinate system
soil_shp <- st_transform(soil_shp, crs = st_crs(mun_1872))

# Calculate total municipality area
mun_1872$area_m2 <- st_area(mun_1872)

int <- as_tibble(st_intersection(soil_shp, mun_1872))

int %<>% mutate(area_km2 = area_m2/1000000)

# Soil Area for each soiltype
int$area_soil <- st_area(int$geometry)

int %<>% as_tibble() %>% mutate(area_soil = area_soil/1000000)


# Plotting
# plot (soil_shp$geometry, col='green')
# plot(mun_1872$geometry, add=T)
# plot(int$geometry, col='red', add=T)


# Brazil total area
test <- int %>% distinct(codigo, .keep_all = TRUE) %>% mutate(tot_area = sum(area_km2))

int %<>% group_by(LEG_SINOT) %>% mutate(soil_share = area_soil/area_km2) %>% ungroup()

soil_share <- int %>% dplyr::select(Area_km2, ORDEM1, LEG_SINOT, codigo, nome, area_km2, 
                                    area_soil, soil_share)

soil_share %<>% group_by(ORDEM1, codigo) %>% mutate(soil_share_group = sum(soil_share)) %>% ungroup()


#### Creating final datasets
soil_group_final <- soil_share %>%
  dplyr::select(ORDEM1, codigo, nome, area_km2, soil_share_group)

# Remove duplicates
soil_group_final %<>% unique()
soil_group_final %<>% mutate(ORDEM1 = ifelse(is.na(ORDEM1), "AGUA", ORDEM1))

# Transform to wider
soil_group_wider <- soil_group_final %>% 
                    pivot_wider(names_from = ORDEM1, 
                                values_from = soil_share_group)


# Calculate share only for "terra roxa" (what is the actual definition?)
soil_roxa <- soil_share %>%
  dplyr::select(ORDEM1, LEG_SINOT, codigo, nome, area_km2, soil_share)


soil_roxa_1 <- soil_roxa %>%
  filter(LEG_SINOT == "LVd  - Latossolos Vermelhos Distroficos"|
           LEG_SINOT == "LVdf - Latossolos Vermelhos Distroferricos"|
           LEG_SINOT == "LVe  - Latossolos Vermelhos Eutroficos"|
           LEG_SINOT == "LVef - Latossolos Vermelhos Eutroferricoss")

soil_roxa_2 <- soil_roxa %>%
  filter(LEG_SINOT == "LVd  - Latossolos Vermelhos Distroficos"|
           LEG_SINOT == "LVdf - Latossolos Vermelhos Distroferricos"|
           LEG_SINOT == "LVe  - Latossolos Vermelhos Eutroficos"|
           LEG_SINOT == "LVef - Latossolos Vermelhos Eutroferricoss"|
           LEG_SINOT == "NVd - Nitossolos Vermelhos Distroficos"|
           LEG_SINOT == "NVe - Nitossolos Vermelhos Eutroficos")

soil_roxa_complete <- soil_roxa %>%
  filter(LEG_SINOT == "LVd  - Latossolos Vermelhos Distroficos"|
         LEG_SINOT == "LVdf - Latossolos Vermelhos Distroferricos"|
         LEG_SINOT == "LVe  - Latossolos Vermelhos Eutroficos"|
         LEG_SINOT == "LVef - Latossolos Vermelhos Eutroferricoss"|
         LEG_SINOT == "NVd - Nitossolos Vermelhos Distroficos"|
         LEG_SINOT == "NVe - Nitossolos Vermelhos Eutroficos"|
         LEG_SINOT == "PVd - Argissolos Vermelhos Distroficos"|
         LEG_SINOT == "PVe - Argissolos Vermelhos Eutroficos")

soil_roxa_1 %<>% group_by(codigo) %>% mutate(share_roxa_lato = sum(soil_share)) %>%
  ungroup()

soil_roxa_1 %<>% distinct(codigo, .keep_all = TRUE)


soil_roxa_2 %<>% group_by(codigo) %>% mutate(share_roxa_lato_nito = sum(soil_share)) %>%
  ungroup()

soil_roxa_2 %<>% distinct(codigo, .keep_all = TRUE)


soil_roxa_complete %<>% group_by(codigo) %>% mutate(share_roxa_all_red = sum(soil_share)) %>%
  ungroup()

soil_roxa_complete %<>% distinct(codigo, .keep_all = TRUE)


# Joining
soil_final <- full_join(dplyr::select(soil_group_final, c("ORDEM1", "nome", "area_km2",
                                                          "codigo", "soil_share_group")),
                        dplyr::select(soil_roxa_1, c("codigo", "share_roxa_lato")),
                        by = "codigo") %>%
  full_join(., dplyr::select(soil_roxa_2, c("codigo", "share_roxa_lato_nito")), by = "codigo") %>%
  full_join(., dplyr::select(soil_roxa_complete, c("codigo", "share_roxa_all_red")), by = "codigo")
                  

soil_group_wider %<>% rename("AflorRochas" = "AFLORAMENTOS DE ROCHAS")

# Transform to wider
soil_group_wider_final <- soil_final %>% 
  pivot_wider(names_from = ORDEM1, 
              values_from = soil_share_group)

# Saving
save(soil_group_wider_final, file = here("output", "soil", "soil_share.RData"))


############### 2. Reads Soil Erosion TIFF from EMBRAPA ########################

# Soil erosion file
erosion_tif <- terra::rast(here("data", "raw", "soil", "soil_erosion",
                         "Brasil_suscetibilidade",
                         "Suscetibilidade.tif"))

# Get Brazilian Borders
brazil <- getData("GADM",country="Brazil",level=0)
brazil <- terra::vect(brazil)

# Reprojects raster
terra::crs(erosion_tif) <- terra::crs(brazil)

# Restrict to Brazilian borders
erosion_crop <- terra::crop(erosion_tif, brazil)
erosion_mask <- terra::mask(erosion_tif, brazil)


plot(erosion_tif)
plot(erosion_crop)
plot(erosion_mask)

# Saving
terra::writeRaster(erosion_mask, filename = here("data", "output", "soil",
                                                    "soil_erosion_raw.tif"))


### Mean value inside each municipality
rm(list = ls())

erosion_mask <- terra::rast(here("data", "output", "soil",
                                    "soil_erosion_raw.tif"))

# 1872 municipality borders
mun_1872 <- st_read(here("data", "raw", "mun_borders", "municip_1872",
                         "05-malha municipal 1872.shp"))


# Transforming into terra format
mun_1872 <- terra::vect(mun_1872)

# Reproject raster
terra::crs(mun_1872) <- terra::crs(erosion_mask)


plot(erosion_mask)
plot(mun_1872, add = TRUE)

mun_1872 <- project(mun_1872, crs(erosion_mask))

erosion_avg <- terra::extract(erosion_mask, mun_1872, fun = mean, na.rm = TRUE)


colnames(night_2000_avg) <- c("light_2000", "cod", "mun_name_2000")



erosion_avg <- raster::extract(erosion_mask, mun_1872, fun = mean, na.rm = TRUE, 
                               df=TRUE)


teste <- terra::as.data.frame(erosion_mask)

