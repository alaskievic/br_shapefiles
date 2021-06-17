#Load packages
source("00_load_packages.R")



############################ 1. Read TRI file ##################################

# Read Terrain Ruggedness Index for the whole plane
tri <- raster(here("shapefiles", "slope", "data", "tri.txt"))

# Read 1872 shapefile from Brazil
mun_1872 <- st_read(here("shapefiles","mun_borders", "municip_1872", "malha_municipal_1872.shp"))


brazil <- getData("GADM",country="Brazil",level=0)

# Reprojects raster
crs(tri) <- projection(brazil)

# Restrict to 1872 Brazilian borders
tri_mask <- mask(tri, brazil)

# Plotting
plot(tri)


# Reprojects raster
crs(tri) <- projection(mun_1872)

# Restrict to 1872 Brazilian borders
tri_mask <- mask(tri, mun_1872)

# Note that Acre is not there anymore
plot(tri_mask)


# Extracting Values
malaria_day_pf_values = raster::extract(x =malaria_day_pf_mask, y = mun_1872, 
                                        sp = TRUE, fun = mean, na.rm = TRUE, 
                                        df = TRUE) 

malaria_day_pf_values_df <- as_tibble(malaria_day_pf_values@data)