#Load packages
source("00_load_packages.R")

######################## 1. Read Biomes from IBGE #############################

# Read biomes
biomes <- st_read(here("shapefiles", "biomes", "data", "mapbiomas_250",
                       "bioma_1milhao_uf2015_250mil_IBGE_albers_v4_revisao_pampa_lagoas.shp"))

# Read vegetation
veget <- st_read(here("shapefiles", "biomes", "data", "vege_area", "vege_area.shp"))

# 1872 municipality borders
mun_1872 <- st_read(here("shapefiles", "mun_borders", "municip_1872", "malha_municipal_1872.shp"))

# 1900 municipality border
mun_1900 <- st_read(here("shapefiles", "mun_borders", "municip_1900", "malha_municipal_1900.shp"))

# 1920 municipality border
mun_1920 <- st_read(here("shapefiles", "mun_borders", "municip_1920", "05-malha_municipal_1920.shp"))

# 1950 municipality border
mun_1950 <- st_read(here("shapefiles", "mun_borders", "municip_1950", "05-malha_municipal_1950.shp"))

# 2000 municipality borders
# mun_2000 <- read_municipality(year=2000)

# 2010 municipality borders
mun_2010 <- st_read(here("shapefiles", "mun_borders", "municip_2010", "municipios_2010.shp"))


# plot(st_geometry(mun_2000))


### Intersecting with municipalities
# Assigning corrdinate system
biomes_1920 <- st_transform(biomes, crs = st_crs(mun_1920))
veget_1920  <- st_transform(veget,  crs = st_crs(mun_1920))

biomes_1950 <- st_transform(biomes, crs = st_crs(mun_1950))
veget_1950  <- st_transform(veget,  crs = st_crs(mun_1950))


# Fixing invalid geometries
veget_1920 %<>% st_make_valid()
veget_1950 %<>% st_make_valid()



# Calculate total municipality area and intersecting
mun_1920$area_m2 <- st_area(mun_1920)
mun_1950$area_m2 <- st_area(mun_1950)

biomes_int_1920 <- as_tibble(st_intersection(biomes_1920, mun_1920))
biomes_int_1920 %<>% mutate(area_km2 = area_m2/1000000)

biomes_int_1950 <- as_tibble(st_intersection(biomes_1950, mun_1950))
biomes_int_1950 %<>% mutate(area_km2 = area_m2/1000000)


veget_int_1920 <- as_tibble(st_intersection(veget_1920, mun_1920))
veget_int_1920 %<>% mutate(area_km2 = area_m2/1000000)

veget_int_1950 <- as_tibble(st_intersection(veget_1950, mun_1950))
veget_int_1950 %<>% mutate(area_km2 = area_m2/1000000)

# Area for each biome
biomes_int_1920$area_biome <- st_area(biomes_int_1920$geometry)
biomes_int_1950$area_biome <- st_area(biomes_int_1950$geometry)


biomes_int_1920 %<>% as_tibble() %>% mutate(area_biome = area_biome/1000000)
biomes_int_1950 %<>% as_tibble() %>% mutate(area_biome = area_biome/1000000)



biomes_int_1920 %<>% group_by(CD_LEGENDA, codigo) %>%
  mutate(biome_share = area_biome/area_km2) %>% ungroup()

biomes_int_1950 %<>% group_by(CD_LEGENDA, codigo) %>%
  mutate(biome_share = area_biome/area_km2) %>% ungroup()


biome_share_1920 <- biomes_int_1920 %>% dplyr::select(CD_LEGENDA, codigo, nome, area_km2, 
                                            area_biome, biome_share)

biome_share_1950 <- biomes_int_1950 %>% dplyr::select(CD_LEGENDA, codigo, nome, area_km2, 
                                                      area_biome, biome_share)

biome_share_1920 %<>% group_by(codigo) %<>% distinct(CD_LEGENDA, .keep_all = TRUE)
biome_share_1950 %<>% group_by(codigo) %<>% distinct(CD_LEGENDA, .keep_all = TRUE)

# Area for each type of vegetation
veget_int_1920$area_veget <- st_area(veget_int_1920$geometry)
veget_int_1950$area_veget <- st_area(veget_int_1950$geometry)


veget_int_1920 %<>% as_tibble() %>% mutate(area_veget = area_veget/1000000)
veget_int_1950 %<>% as_tibble() %>% mutate(area_veget = area_veget/1000000)


veget_int_1920 %<>% group_by(legenda_1, codigo) %>%
  mutate(veget_share = area_veget/area_km2) %>% ungroup()

veget_int_1950 %<>% group_by(legenda_1, codigo) %>%
  mutate(veget_share = area_veget/area_km2) %>% ungroup()


veget_share_1920 <- veget_int_1920 %>% dplyr::select(nm_uveg, leg_uantr, nm_uantr, leg_contat, 
                                           nm_contat, veg_pretet, nm_pretet, leg_sup, 
                                           legenda_1, legenda_2, legenda, codigo,
                                           nome, area_km2, area_veget, veget_share)

veget_share_1950 <- veget_int_1950 %>% dplyr::select(nm_uveg, leg_uantr, nm_uantr, leg_contat, 
                                                nm_contat, veg_pretet, nm_pretet, leg_sup, 
                                                legenda_1, legenda_2, legenda, codigo,
                                                nome, area_km2, area_veget, veget_share)




veget_share_1920 %<>% group_by(legenda_1, codigo) %>%
  mutate(veget_share_group = sum(veget_share)) %>% ungroup()

veget_share_1950 %<>% group_by(legenda_1, codigo) %>%
  mutate(veget_share_group = sum(veget_share)) %>% ungroup()


veget_share_1920 %<>% group_by(codigo) %<>% distinct(legenda_1, .keep_all = TRUE)
veget_share_1950 %<>% group_by(codigo) %<>% distinct(legenda_1, .keep_all = TRUE)



veget_final_1920 <- veget_share_1920 %>% dplyr::select(codigo, nome, veget_share_group, 
                                             area_km2, legenda_1, legenda_2,
                                             legenda, leg_sup, nm_contat, nm_pretet,
                                             veg_pretet, nm_uantr)

veget_final_1950 <- veget_share_1950 %>% dplyr::select(codigo, nome, veget_share_group, 
                                                       area_km2, legenda_1, legenda_2,
                                                       legenda, leg_sup, nm_contat, nm_pretet,
                                                       veg_pretet, nm_uantr)




# Saving
save(veget_final_1920, file = here("output", "biomes", "veget_1920.RData"))
save(veget_final_1950, file = here("output", "biomes", "veget_1950.RData"))

# Transform to wider
biome_share_1920 %<>% pivot_wider(-c("area_biome", "area_km2"), 
                             names_from = CD_LEGENDA, values_from = biome_share)

biome_share_1950 %<>% pivot_wider(-c("area_biome", "area_km2"), 
                             names_from = CD_LEGENDA, values_from = biome_share)


veget_wider_1920 <- veget_final_1920 %>% dplyr::select(codigo, nome, veget_share_group,
                                             legenda_1, area_km2)

veget_wider_1950 <- veget_final_1950 %>% dplyr::select(codigo, nome, veget_share_group,
                                                  legenda_1, area_km2)

# Transform to wider
veget_wider_1920 %<>% pivot_wider(names_from = legenda_1, values_from = veget_share_group)

veget_wider_1950 %<>% pivot_wider(names_from = legenda_1, values_from = veget_share_group)


# Merge
biome_veget_1920 <- inner_join(biome_share_1920, dplyr::select(veget_wider_1920, -nome), by = "codigo")
biome_veget_1950 <- inner_join(biome_share_1950, dplyr::select(veget_wider_1950, -nome), by = "codigo")


colnames(biome_veget_1920) <- c("codigo", "nome", "cerrado", "mata_atlantica",
                              "caatinga", "pampa", "amazonia", "pantanal", 
                              "area_km2", "contato", "corpo_agua", "floresta_semidecidual",
                              "savana", "formacao_pioneira", "floresta_ombrofila_densa",
                              "floresta_ombrofila_mista", "savana_estepica",
                              "estepe", "floresta_decidual","floresta_ombrofila_aberta",
                              "campinarana", "floresta_sempre_verde")

colnames(biome_veget_1950) <- c("codigo", "nome", "mata_atlantica", "cerrado",
                                "caatinga", "pampa", "amazonia", "pantanal", 
                                "area_km2", "floresta_semidecidual", "corpo_agua", "savana",
                                "contato", "formacao_pioneira", "floresta_ombrofila_densa",
                                "floresta_ombrofila_mista", "savana_estepica",
                                "estepe", "floresta_decidual","floresta_ombrofila_aberta",
                                "floresta_sempre_verde", "campinarana")


biome_veget_1920 %<>% mutate(area_atlantica = area_km2 * mata_atlantica) %>%
  mutate_if(is.numeric, as.numeric) %<>% replace(is.na(.), 0) %>% ungroup() %>%
  mutate(tot_brarea = sum(area_km2)) %>% mutate(tot_atlantic = sum(area_atlantica)) %>%
  mutate(share_atlantica_tot = tot_atlantic/tot_brarea)

biome_veget_1950 %<>% mutate(area_atlantica = area_km2 * mata_atlantica) %>%
  mutate_if(is.numeric, as.numeric) %<>% replace(is.na(.), 0) %>% ungroup() %>%
  mutate(tot_brarea = sum(area_km2)) %>% mutate(tot_atlantic = sum(area_atlantica)) %>%
  mutate(share_atlantica_tot = tot_atlantic/tot_brarea)


# Saving
save(biome_veget_1920, file = here("output", "biomes", "biome_veget_1920.RData"))
save(biome_veget_1950, file = here("output", "biomes", "biome_veget_1950.RData"))

# Save to Stata
write_dta(biome_veget_1920, path = here("output", "biomes", "biome_veget_1920.dta"))
write_dta(biome_veget_1950, path = here("output", "biomes", "biome_veget_1950.dta"))


