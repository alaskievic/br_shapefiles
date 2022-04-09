#Load packages
source("00_load_packages.R")


rail_shp <- st_read(here("data", "ferrovia", "BRA_rails.shp"))

plot(rail_shp$geometry)

rail_df <- st_drop_geometry(rail_shp)

# 1872 municipality borders
mun_1872 <- st_read(here("shapefiles","mun_borders", "municip_1872", "malha_municipal_1872.shp"))


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



