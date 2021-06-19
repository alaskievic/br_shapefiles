#Load packages
source("00_load_packages.R")


################ 1. Reads Malaria suitability shapefile ########################

# Reading shapefiles
malaria_day_pf <- raster(here("shapefiles", "malaria", "data", "2010_TempSuitability.Pf.AnnualInfectiousDays.1k.global_Decompressed_BRA.tiff"))
malaria_day_pv <- raster(here("shapefiles", "malaria", "data", "2010_TempSuitability.Pv.AnnualInfectiousDays.1k.global_Decompressed_BRA.tiff"))
malaria_index_pf <- raster(here("shapefiles", "malaria", "data", "2010_TempSuitability.Pf.Index.1k.global_Decompressed_BRA.tiff"))
malaria_index_pv <- raster(here("shapefiles", "malaria", "data", "2010_TempSuitability.Pv.Index.1k.global_Decompressed_BRA.tiff"))



# 1872 municipality borders
mun_1872 <- st_read(here("shapefiles","mun_borders", "municip_1872", "malha_municipal_1872.shp"))

# Plotting
plot(malaria_day_pf)
# plot(malaria_index_pf)

# Reprojects shapefile
mun_1872 = st_transform(mun_1872, projection(malaria_day_pf))


# Restrict to 1872 Brazilian borders
malaria_day_pf_mask <- mask(malaria_day_pf, mun_1872)

# Note that Acre is not there anymore
plot(malaria_day_pf_mask)

# Extracting Values
malaria_day_pf_values = raster::extract(x =malaria_day_pf_mask, y = mun_1872, 
                                         sp = TRUE, fun = mean, na.rm = TRUE, 
                                         df = TRUE) 

malaria_day_pf_values_df <- as_tibble(malaria_day_pf_values@data)


colnames(malaria_day_pf_values_df) <- c("codigo", "nome", "days_pf")



# Get the mean value per municipality
malaria_day_pf_mean <- group_by(malaria_day_pf_values, ID) %>% 
  summarize_at(vars(days_pf), list( ~mean(., na.rm = TRUE)))


# Merging back
