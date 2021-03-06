#Load packages
source("00_load_packages.R")

######################### 1. Read Biomes from IBGE #############################

# Read biomes
biomes <- st_read(here("shapefiles", "biomes", "data", "mapbiomas_250",
                       "bioma_1milhao_uf2015_250mil_IBGE_albers_v4_revisao_pampa_lagoas.shp"))

# Read vegetation
veget <- st_read(here("shapefiles", "biomes", "data", "vege_area", "vege_area.shp"))

# Create a dataset without geometry for visualization
# biomes_dt <- st_set_geometry(biomes, NULL)
# veget_dt <- st_set_geometry(veget, NULL)

# 1872 municipality borders
mun_1872 <- st_read(here("shapefiles", "mun_borders", "municip_1872", "malha_municipal_1872.shp"))

# 1900 municipality border
mun_1900 <- st_read(here("shapefiles", "mun_borders", "municip_1900", "malha_municipal_1900.shp"))


# 2010 municipality borders
# mun_2010 <- read_municipality(year=2010)


### Intersecting with municipalities
# Assigning coordinate system
biomes_1900 <- st_transform(biomes, crs = st_crs(mun_1900))
veget_1900  <- st_transform(veget,  crs = st_crs(mun_1900))

# teste <- st_transform(mun_2010, crs = st_crs(mun_1900))
# plot(st_geometry(teste))


# biomes <- st_transform(biomes, crs = st_crs(mun_1900))
# veget <- st_transform(veget, crs = st_crs(mun_1900))

# Fixing invalid geometries
veget_1900 %<>% st_make_valid()


# Plotting
# plot(biomes$geometry)
# plot(mun_1900$geometry)
# plot(veget$geometry)


# Calculate total municipality area and intersecting
mun_1900$area_m2 <- st_area(mun_1900)

biomes_int_1900 <- as_tibble(st_intersection(biomes_1900, mun_1900))
biomes_int_1900 %<>% mutate(area_km2 = area_m2/1000000)


veget_int_1900 <- as_tibble(st_intersection(veget_1900, mun_1900))
veget_int_1900 %<>% mutate(area_km2 = area_m2/1000000)


# Area for each biome
biomes_int_1900$area_biome <- st_area(biomes_int_1900$geometry)

biomes_int_1900 %<>% as_tibble() %>% mutate(area_biome = area_biome/1000000)

biomes_int_1900 %<>% group_by(CD_LEGENDA, codigo) %>%
  mutate(biome_share = area_biome/area_km2) %>% ungroup()

biome_share_1900 <- biomes_int_1900 %>% dplyr::select(CD_LEGENDA, codigo, nome, area_km2, 
                                                      area_biome, biome_share)

biome_share_1900 %<>% group_by(codigo) %<>% distinct(CD_LEGENDA, .keep_all = TRUE) %>% ungroup()

# Area for each type of vegetation
veget_int_1900$area_veget <- st_area(veget_int_1900$geometry)

veget_int_1900 %<>% as_tibble() %>% mutate(area_veget = area_veget/1000000)

veget_int_1900 %<>% group_by(legenda_1, codigo) %>% 
  mutate(veget_share = area_veget/area_km2) %>% ungroup()

veget_share_1900 <- veget_int_1900 %>% dplyr::select(nm_uveg, leg_uantr, nm_uantr, leg_contat, 
                                           nm_contat, veg_pretet, nm_pretet, leg_sup, 
                                           legenda_1, legenda_2, legenda, codigo,
                                           nome, area_km2, area_veget, veget_share)

veget_share_1900 %<>% group_by(legenda_1, codigo) %>%
  mutate(veget_share_group = sum(veget_share)) %>% ungroup()

veget_share_1900 %<>% group_by(codigo) %<>% distinct(legenda_1, .keep_all = TRUE) %>% ungroup()

veget_final_1900 <- veget_share_1900 %>% dplyr::select(codigo, nome, veget_share_group, 
                                                       area_km2, legenda_1, legenda_2,
                                                       legenda, leg_sup, nm_contat, nm_pretet,
                                                       veg_pretet, nm_uantr)

# Saving
save(veget_final_1900, file = here("output", "biomes", "veget_1900.RData"))

# Transform to wider
biome_share_1900 %<>% pivot_wider(-c("area_biome", "area_km2"), 
                                     names_from = CD_LEGENDA, values_from = biome_share)

# Let's take a particular look at the Atlantic Forest
biome_share_1900 %<>% mutate(d_atlantic = ifelse(is.na(`MATA ATL�NTICA`) == FALSE,
                                                       1, 0))

# Saving
save(biome_share_1900, file = here("output", "biomes", "biome_1900.RData"))


veget_wider_1900 <- veget_final_1900 %>% dplyr::select(codigo, nome, veget_share_group,
                                             legenda_1, area_km2)

# Transform to wider
veget_wider_1900 %<>% pivot_wider(names_from = legenda_1, values_from = veget_share_group)

# Merge
biome_veget_1900 <- inner_join(biome_share_1900, dplyr::select(veget_wider_1900, -nome),
                               by = "codigo")


# Saving
save(biome_veget_1900, file = here("output", "biomes", "biome_veget_1900.RData"))


# Only S�o Paulo
sp_biome_veget_1900 <- biome_veget_1900 %<>% filter(substr(codigo, 1, 2) == 35)

colnames(sp_biome_veget_1900) <- c("codigo", "nome", "cerrado", "mata_atlantica",
                              "caatinga", "pampa", "amazonia", "pantanal", 
                              "d_atlantic", "area_km2", "corpo_agua", "savana", 
                              "floresta_semidecidual", "contato",
                              "formacao_pioneira", "floresta_ombrofila_densa",
                              "floresta_ombrofila_mista", "savana_estepica",
                              "estepe", "floresta_decidual",
                              "floresta_ombrofila_aberta", "campinarana",
                              "floresta_sempre_verde")

# Percentage of SP in Mata Atlantica
sp_biome_veget_1900 %<>% mutate(area_atlantica = area_km2 * mata_atlantica) %>%
  mutate_if(is.numeric, as.numeric) %<>% replace(is.na(.), 0) %>% ungroup() %>%
  mutate(tot_sparea = sum(area_km2)) %>% mutate(tot_atlantic = sum(area_atlantica)) %>%
  mutate(share_atlantica_tot = tot_atlantic/tot_sparea)

# Save to Stata
write_dta(sp_biome_veget_1900, path = here("output", "biomes", "sp_biome_veget_1900.dta"))



