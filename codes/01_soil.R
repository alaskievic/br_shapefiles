#Load packages
source("00_load_packages.R")



############################ 1. Read TRI file ##################################

# Read Terrain Ruggedness Index for the whole plane
tri <- raster("C:/Users/Andrei/Desktop/ra_nyu/br_shapefiles/ruggedness/data/tri.txt")

# Read 1872 shapefile from Brazil
mun_1872 <- st_read("C:/Users/Andrei/Desktop/ra_nyu/shared_folders/digitize_brazil/data/raw/shapefiles/municip_1872/malha_municipal_1872.shp")

# Reprojects raster
crs(tri) <- crs(mun_1872)

# Restrict to Brazilian borders
tri_masked <- mask(tri, mun_1872)

plot(tri_masked)




############################ 1. Read Rivers from ANA ###########################

# Read all cursos d'agua
curso <- st_read("C:/Users/Andrei/Desktop/ra_nyu/br_shapefiles/rivers/data/curso_ana/geoft_bho_cursodagua.shp")
curso_dt <- st_set_geometry(curso, NULL)


# Read all rivers (use for merge to obtain names only)
river <- st_read("C:/Users/Andrei/Desktop/ra_nyu/br_shapefiles/rivers/data/river_ana/geoft_bho_rio.shp")
river <- st_set_geometry(river, NULL)


# We are interesed in cocursodag and nuareabacc to define the main rivers
# Remove last two digits
curso %<>% mutate(cod = COCURSODAG) %>% mutate(cod = as.numeric(cod))

river %<>% mutate(cod = CORIO) %>% mutate(cod = substr(cod, 1, nchar(cod)-2)) %>% 
           mutate(cod = as.numeric(cod))

# First dummy - Rivers of otto level 3
curso_d3 <- curso %>% filter(cod <= 999)

# Second dummy - Rivers of otto level 4
curso_d4 <- curso %>% filter(cod <= 9999)

# Third dummy - Rivers with more tha 1.000km2 of nuareabacc (hidrographic contribution)
curso_d1000 <- curso %>% filter(NUAREABACC >= 1000)


curso_d5000 <- curso %>% filter(NUAREABACC >= 5000)


# Take a glimpse of names
curso_d3_names <- inner_join(curso_d3, river, by = "cod")
curso_d4_names <- inner_join(curso_d4, river, by = "cod")
curso_d1000_names <- inner_join(curso_d1000, river, by = "cod")

curso_d5000_names <- inner_join(curso_d5000, river, by = "cod")


plot(st_geometry(curso_d3_names))

plot(st_geometry(curso_d5000_names))


# 1872 municipality borders
mun_1872 <- st_read("C:/Users/Andrei/Desktop/ra_nyu/br_shapefiles/mun_borders/municip_1872/malha_municipal_1872.shp")

plot(st_geometry(mun_1872))


curso_d5000_names <- st_transform(curso_d5000_names, crs = st_crs(mun_1872))


river_d5000 <- as_tibble(st_intersection(curso_d5000_names, mun_1872))


river_d5000 <- teste %>% distinct(codigo) %>% mutate(river_d5000 = 1)


# Merge back with shapefile

river_d5000 <- full_join(mun_1872, river_d5000, by = "codigo")


river_d5000 <- st_set_geometry(river_d5000, NULL)








# Soil shapefile
soil_shp <- st_read(here("data","raw","shapefiles", "geo_shapefiles",
                         "soil_types", "brasil_solos_5m_20201104.shp"))

# 1872 municipality borders
mun_1872 <- st_read(here("data","raw", "shapefiles", "municip_1872", "malha_municipal_1872.shp"))


st_crs(soil_shp)
st_crs(mun_1872)


soil_shp <- st_transform(soil_shp, crs = st_crs(mun_1872))

# Calculate total municipality area
mun_1872$area_m2 <- st_area(mun_1872)

int <- as_tibble(st_intersection(soil_shp, mun_1872))

int %<>% mutate(area_km2 = area_m2/1000000)

# Soil Area for each soiltype
int$area_soil <- st_area(int$geometry)


int %<>% as_tibble() %>% mutate(area_soil = area_soil/1000000)


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

soil_roxa %<>% filter(LEG_SINOT == "LVd - Latossolos Vermelhos Distroficos"|
                      LEG_SINOT == "LVdf - Latossolos Vermelhos Distroferricos"|
                      LEG_SINOT == "LVe - Latossolos Vermelhos Eutroficos"|
                      LEG_SINOT == "LVef - Latossolos Vermelhos Eutroferricoss")

soil_roxa %<>% group_by(codigo) %>% mutate(share_roxa = sum(soil_share)) %>%
  ungroup()

soil_roxa %<>% distinct(codigo, .keep_all = TRUE)


# Joining
soil_final <- full_join(dplyr::select(soil_group_final, c("ORDEM1", "nome", "area_km2",
                                                          "codigo", "soil_share_group")),
                        dplyr::select(soil_roxa, c("codigo", "share_roxa")),
                        by = "codigo")

# Writing to Stata
write_dta(soil_share, path = here("data", "output", "soil", "soil_share_r.dta"))

soil_group_wider %<>% rename("AflorRochas" = "AFLORAMENTOS DE ROCHAS")

write_dta(soil_group_wider, path = here("data", "output", "soil", "soil_share_r_wider.dta"))


# #join tibble to original county polygon shapefile and export as new shapefile
# shp_out <- st_write(merge(counties, tb_ArableByCounty, by = 'County_UA'), "ArablebyCounty.shp")
