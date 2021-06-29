#Load packages
source("00_load_packages.R")

############################ 1. Read Biomes from IBGE ##########################

# Read biomes
biomes <- st_read(here("shapefiles", "biomes", "data", "mapbiomas_250", "bioma_1milhao_uf2015_250mil_IBGE_albers_v4_revisao_pampa_lagoas.shp"))

# Read vegetation
veget <- st_read(here("shapefiles", "biomes", "data", "vege_area", "vege_area.shp"))

# Create a dataset without geometry for visualization
biomes_dt <- st_set_geometry(biomes, NULL)
veget_dt <- st_set_geometry(veget, NULL)

# 1872 municipality borders
mun_1872 <- st_read(here("shapefiles", "mun_borders", "municip_1872", "malha_municipal_1872.shp"))

# 1900 municipality border
mun_1900 <- st_read(here("shapefiles", "mun_borders", "municip_1900", "malha_municipal_1900.shp"))


### Intersecting with municipalities
# Assigning corrdinate system
biomes <- st_transform(biomes, crs = st_crs(mun_1872))
veget <- st_transform(veget, crs = st_crs(mun_1872))


biomes <- st_transform(biomes, crs = st_crs(mun_1900))
veget <- st_transform(veget, crs = st_crs(mun_1900))

# Fixing invalid geometries
veget %<>% st_make_valid()


# Plotting
plot(biomes$geometry)
plot(mun_1900$geometry)
# plot(veget$geometry)


# Calculate total municipality area and intersecting
mun_1872$area_m2 <- st_area(mun_1872)

mun_1900$area_m2 <- st_area(mun_1900)

# biomes_int <- as_tibble(st_intersection(biomes, mun_1872))

biomes_int <- as_tibble(st_intersection(biomes, mun_1900))
biomes_int %<>% mutate(area_km2 = area_m2/1000000)

# veget_int <- as_tibble(st_intersection(veget, mun_1872))

veget_int <- as_tibble(st_intersection(veget, mun_1900))
veget_int %<>% mutate(area_km2 = area_m2/1000000)


# Area for each biome
biomes_int$area_biome <- st_area(biomes_int$geometry)

biomes_int %<>% as_tibble() %>% mutate(area_biome = area_biome/1000000)

biomes_int %<>% group_by(CD_LEGENDA, codigo) %>% mutate(biome_share = area_biome/area_km2) %>% ungroup()

biome_share <- biomes_int %>% dplyr::select(CD_LEGENDA, codigo, nome, area_km2, 
                                    area_biome, biome_share)

biome_share %<>% group_by(codigo) %<>% distinct(CD_LEGENDA, .keep_all = TRUE)

# Area for each type of vegetation
veget_int$area_veget <- st_area(veget_int$geometry)

veget_int %<>% as_tibble() %>% mutate(area_veget = area_veget/1000000)

veget_int %<>% group_by(legenda_1, codigo) %>% mutate(veget_share = area_veget/area_km2) %>% ungroup()

veget_share <- veget_int %>% dplyr::select(nm_uveg, leg_uantr, nm_uantr, leg_contat, 
                                           nm_contat, veg_pretet, nm_pretet, leg_sup, 
                                           legenda_1, legenda_2, legenda, codigo,
                                           nome, area_km2, area_veget, veget_share)

veget_share %<>% group_by(legenda_1, codigo) %>%
  mutate(veget_share_group = sum(veget_share)) %>% ungroup()

veget_share %<>% group_by(codigo) %<>% distinct(legenda_1, .keep_all = TRUE)

veget_final <- veget_share %>% dplyr::select(codigo, nome, veget_share_group, 
                                             area_km2, legenda_1, legenda_2,
                                             legenda, leg_sup, nm_contat, nm_pretet,
                                             veg_pretet, nm_uantr)

# Saving
save(veget_final, file = here("output", "biomes", "veget_complete.RData"))

# Transform to wider
biome_share %<>% pivot_wider(-c("area_biome", "area_km2"), 
                                     names_from = CD_LEGENDA, values_from = biome_share)

# Let's take a particular look at the Atlantic Forest
atlantic <- biome_share %>% mutate(d_atlantic = ifelse(is.na(`MATA ATLÂNTICA`) == FALSE,
                                                       1, 0))

# Saving
save(atlantic, file = here("output", "biomes", "atlantic_dummy.RData"))


veget_wider <- veget_final %>% dplyr::select(codigo, nome, veget_share_group,
                                             legenda_1, area_km2)

# Transform to wider
veget_wider %<>% pivot_wider(names_from = legenda_1, values_from = veget_share_group)

# Merge
biome_veget <- inner_join(atlantic, dplyr::select(veget_wider, -nome), by = "codigo")


# Saving
save(biome_veget, file = here("output", "biomes", "biome_veget.RData"))


# Only São Paulo
sp_biome_veget <- biome_veget %<>% filter(substr(codigo, 1, 2) == 35)

colnames(sp_biome_veget) <- c("codigo", "nome", "cerrado", "mata_atlantica",
                              "caatinga", "pampa", "amazonia", "pantanal", 
                              "d_atlantic", "area_km2", "corpo_agua", "savana", 
                              "floresta_semidecidual", "contato",
                              "formacao_pioneira", "floresta_ombrofila_densa",
                              "floresta_ombrofila_mista", "savana_estepica",
                              "estepe", "floresta_decidual",
                              "floresta_ombrofila_aberta", "campinarana",
                              "floresta_sempre_verde")

# Percentage of SP in Mata Atlantica
sp_biome_veget %<>% mutate(area_atlantica = area_km2 * mata_atlantica) %>%
  mutate_if(is.numeric, as.numeric) %<>% replace(is.na(.), 0) %>% ungroup() %>%
  mutate(tot_sparea = sum(area_km2)) %>% mutate(tot_atlantic = sum(area_atlantica)) %>%
  mutate(share_atlantica_tot = tot_atlantic/tot_sparea)

# Save to Stata
write_dta(sp_biome_veget, path = here("output", "biomes", "sp_biome_veget.dta"))



