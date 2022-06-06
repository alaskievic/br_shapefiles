#Load packages
source("00_load_packages.R")

########################## 1. Read Land Cover from MapBiomas ###################

mapbiomas <- read_excel(path = here("shapefiles", "map_biomas", "spreadsheet", 
                        "Dados_Cobertura_MapBiomas_5.0_UF-MUN_SITE_v2.xlsx"), 
                        sheet = 3)



map_1985 <- 
  
  

  
map_2010 <- mapbiomas %>% dplyr::select(c("territory_id", "municipality", "state", "level_0",
                            "level_1", "level_2", "level_3", "level_4", "2010")) %>%
  group_by(territory_id) %>% mutate(tot_area = sum(`2010`)) %>% ungroup()

map_2010 %<>% mutate(share_forest = ifelse(level_4 == "Forest Formation", 
                                           `2010`/tot_area, NA))

map_2010 %<>% group_by(state) %>% mutate(tot_area_state = sum(`2010`)) %>%
  mutate(forest_state = ifelse(level_4 == "Forest Formation", sum(`2010`), 0)) %>%
  mutate(forest_share_state = forest_state/tot_area_state)
  ungroup()










### Read Atlantic Forest Remains
atlantic_1985 <- terra::rast(here("shapefiles", "map_biomas", "raster",
                               "COLECAO_5_DOWNLOADS_COLECOES_ANUAL_MATAATLANTICA_MATAATLANTICA-1985.tif"))


atlantic_2010 <- terra::rast(here("shapefiles", "map_biomas", "raster",
                                  "COLECAO_5_DOWNLOADS_COLECOES_ANUAL_MATAATLANTICA_MATAATLANTICA-2010.tif"))


teste <- raster(here("shapefiles", "map_biomas", "raster",
                                  "COLECAO_5_DOWNLOADS_COLECOES_ANUAL_MATAATLANTICA_MATAATLANTICA-2010.tif"))
# Plotting
raster::plot(atlantic_1985)
raster::plot(atlantic_2010)

# 2010 municipality borders
mun_2010 <- read_municipality(year=2010)


# First, to a SpatialPointsDataFrame
teste_pts <- rasterToPoints(teste, spatial = TRUE)
teste_df <- as.data.frame(teste, xy = TRUE)

teste_df  <- data.frame(teste_pts)
rm(teste_pts, teste)
