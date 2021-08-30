#Load packages
source("00_load_packages.R")


########### 1. Calculating Some Geographical Variables (1960 AMCs) #############


########### 1.1 Total Area #############

# 1960 AMCs borders
amc_1960 <- st_read(here("data","amc", "amc_1960_2010.shp"))

amc_1960$area_m2 <- st_area(amc_1960)
amc_1960 %<>% mutate(area_km2 = area_m2/1000000)

amc_1960 %<>% dplyr::select(c("GEOCODIG_M", "UF", "SIGLA", "NOME_MUNIC",
                              "amc_1960_2", "area_km2"))

colnames(amc_1960) <- c("code2010", "uf_amc", "uf_sigla", "name_mun", "amc_1960", 
                        "area_km2", "geometry")

########### 1.2 Latitude and Longitude of Centroids #############

# st_point_on_surface guarantees that the centroid fall inside the AMC borders
amc_1960$coord <- st_coordinates(st_point_on_surface(amc_1960$geometry))

amc_1960$longitude <- amc_1960$coord[, 1]
amc_1960$latitude <- amc_1960$coord[, 2]


amc_1960 <- st_set_geometry(amc_1960, NULL) %>% as_tibble()

amc_1960 %<>% mutate(amc_1960 = as.integer(amc_1960))

########### 1.3 Rainfall and Temperature from Daniel #############

daniel <- read_dta(file = here("data", "rainfall", "daniel", "base_clima_br_1950_2017_13.dta")) %>%
  dplyr::select(codigo_IBGE, uf, ano, v_rain, v_temp) %>% filter(ano <= 1980)

colnames(daniel) <- c("code2010", "uf_daniel", "year", "v_rain", "v_temp")


### Merge with AMCs and collapse

# Get 1960-2000 AMC Crosswalk
cross_1960 <- read_dta(file = here("data", "amc", "crosswalk", "_Crosswalk_final_1960_2000.dta")) %>%
  dplyr::select(uf_amc, final_name, amc, code2010) %>% rename("amc_1960" = "amc") %>%
  mutate(code2010 = str_sub(code2010, 1, -2)) %>% mutate(code2010 = as.integer(code2010))


daniel_aux <- full_join(daniel, cross_1960, by = "code2010")

daniel_aux %<>% group_by(amc_1960, year) %>%
  summarise(temp_mean = mean(v_temp, na.rm = TRUE), rain_mean = mean(v_rain, na.rm = TRUE)) %>%
  ungroup()

# Mean over years
amc_climate <- daniel_aux %>% group_by(amc_1960) %>%
  summarise(temp_mean = mean(temp_mean, na.rm = TRUE), rain_mean = mean(rain_mean, na.rm = TRUE)) %>%
  ungroup()




######################### 2. Merging all datasets ##############################
load(here("output", "slope", "altitude_1960amc.RData"))
load(here("output", "distance_coast", "dist_coast_1960amc.RData"))

altitude_1960 %<>% mutate(amc_1960 = as.integer(amc_1960))
dist_centroid %<>% mutate(amc_1960 = as.integer(amc_1960))


amc_final <- full_join(amc_1960, amc_climate, by = "amc_1960") %>%
             full_join(., altitude_1960, by = "amc_1960") %>%
             full_join(., dist_centroid, by = "amc_1960")

amc_final %<>% dplyr::select(-c("code2010.x", "uf_amc.x", "uf_sigla.x",
                                "code2010.y", "uf_amc.y", "uf_sigla.y"))



write_dta(amc_final, path = here("output", "amc", "amc1960_geo.dta"))




############################## 3. Some Maps ####################################

### 1960 AMCs borders
amc_1960 <- st_read(here("data","amc", "amc_1960_2010.shp"))

amc_1960$coord <- st_coordinates(st_point_on_surface(amc_1960$geometry))


amc_border_1960 <- tm_shape(amc_1960) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.text.size=1.25,
            legend.title.size=1.55,
            legend.position = c("left","bottom"), 
            legend.height=1.0, 
            frame = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 1)


amc_border_1960

# Saving
tmap_save(amc_border_1960, here("output", "amc", "amc_border_1960.png"))


amc_centroid_1960 <- tm_shape(amc_1960) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_dots(col = "black", group = "coord", size = 0.01, alpha = .5) +
  tm_layout(legend.text.size=1.25,
            legend.title.size=1.55,
            legend.position = c("left","bottom"), 
            legend.height=1.0, 
            frame = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 1)


#amc_centroid_1960

# Saving
tmap_save(amc_centroid_1960, here("output", "amc", "amc_border_1960.png"))



### 2010 Municipality borders
mun_2010 <- st_read(here("data","mun_borders", "municip_2010", "municipios_2010.shp"))


mun_border_2010 <- tm_shape(mun_2010) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.text.size=1.25,
            legend.title.size=1.55,
            legend.position = c("left","bottom"), 
            legend.height=1.0, 
            frame = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 1)


#mun_border_2010

# Saving
tmap_save(mun_border_2010, here("output", "amc", "mun_border_2010.png"))




### Presence of BB in 1960 AMCs ###

# Read dta with BB presence
bb_1960 <- read_dta(here("output", "amc", "agri_bb_amc_clean.dta")) %>%
  filter(year <= 1960)


amc_1960 %<>% rename(amc = amc_1960_2) %>% mutate(amc = as.integer(amc))

# Join with shapefile
bb_shp <- inner_join(amc_1960, bb_1960, by = "amc")

bb_shp %<>% mutate(d_bb = as.factor(d_bb))

bb_plot <- tm_shape(bb_shp) +
  tm_fill("d_bb",  palette="RdBu", border.col = "black",
              border.alpha = .3, showNA = TRUE, textNA="No Data",
              title = "Presence of BB \nBranch Before 1975", 
              labels = c("No", "Yes")) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.text.size=1.25,
            legend.title.size=1.55,
            legend.position = c("left","bottom"), 
            legend.height=1.0, 
            frame = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 1) 

bb_plot

# Saving
tmap_save(bb_plot, here("output", "amc", "bb_presence.png"))

