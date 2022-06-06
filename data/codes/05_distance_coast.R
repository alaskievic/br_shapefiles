#Load packages
source("00_load_packages.R")


################# 1. Calculates distance to the cost ###########################

# 1872 municipality borders
mun_1872 <- st_read(here("shapefiles","mun_borders", "municip_1872", "malha_municipal_1872.shp"))
                    
# 1872 municipality headquarters
mun_head_1872 <- st_read(here("shapefiles", "mun_borders", "sede_municipal_1872",
                              "03-sede municipal 1872.shp"))

# Calculate centroids (some actual fall outside the mun borders)
centroid_aux_1872 <- st_centroid(mun_1872)

# st_point_on_surface guarantees that the centroid fall inside the mun borders
centroid_1872 <- st_point_on_surface(mun_1872)

# Plotting
plot(mun_1872$geometry)
plot(centroid_1872, add = TRUE)
plot(mun_head_1872, add = TRUE)

# Loading Brazilian coast shapefile
costa <- st_read(here("shapefiles", "linha_costa", "LINHA_DE_COSTA_IMAGEM_GEOCOVER_SIRGAS_2000.shp"))

# Plotting
plot(costa$geometry)

# Assigning same coordinate
# transform to UTM
centroid_1872 <- st_transform(centroid_1872, 3055)
mun_head_1872 <-  st_transform(mun_head_1872, 3055)
costa <-st_transform(costa, 3055)

# Distance Centroids
costa <- st_cast(costa, "MULTILINESTRING")

dist_centroid <- st_distance(costa, centroid_1872)


#create a data.frame with the distance and the coordinates of the points
dist_centroid <- tibble(dist_centroid = as.vector(dist_centroid)/1000,
                 centroid_1872$codigo, centroid_1872$nome)

colnames(dist_centroid) <- c("dist_coast_centroid", "codigo", "nome")


# Distance Headquarters
dist_mun_head <- st_distance(costa, mun_head_1872)

dist_mun_head <- tibble(dist_mun_head = as.vector(dist_mun_head)/1000,
                        mun_head_1872$codigo, mun_head_1872$nome)

colnames(dist_mun_head) <- c("dist_coast_munhead", "codigo", "nome")


# Merging
dist_coast_final <- inner_join(dist_centroid, 
                               dplyr::select(dist_mun_head, -nome), by = "codigo")


# Saving
save(dist_coast_final, file = here("output", "distance_coast", "dist_coast.RData"))




################# 2. Distance to the Coast (1960 AMCs) #########################

# 1960 AMCs borders
amc_1960 <- st_read(here("shapefiles","amc", "amc_1960_2010.shp"))

# Calculate centroids (some actual fall outside the mun borders)
centroid_aux_1960 <- st_centroid(amc_1960)

# st_point_on_surface guarantees that the centroid fall inside the mun borders
centroid_1960 <- st_point_on_surface(amc_1960)

# Plotting
plot(amc_1960$geometry)
plot(centroid_1960, add = TRUE)

# Loading Brazilian coast shapefile
costa <- st_read(here("shapefiles", "linha_costa", "LINHA_DE_COSTA_IMAGEM_GEOCOVER_SIRGAS_2000.shp"))

# Plotting
plot(costa$geometry)

## Assigning same coordinate
#  Transform to UTM
centroid_1960 <- st_transform(centroid_1960, 3055)
costa <-st_transform(costa, 3055)

# Distance to Centroids
costa <- st_cast(costa, "MULTILINESTRING")

dist_centroid <- st_distance(costa, centroid_1960)


# Create a data.frame with the distance and the coordinates of the points
dist_centroid <- tibble(dist_centroid = as.vector(dist_centroid)/1000,
                        centroid_1960$amc_1960_2, centroid_1960$NOME_MUNIC)

colnames(dist_centroid) <- c("dist_coast_centroid", "amc_1960", "nome")


# Saving
save(dist_centroid, file = here("output", "distance_coast", "dist_coast_1960amc.RData"))







# 1950 AMCs borders
amc_1950 <- st_read(here("data","amc", "amc_1950_2010.shp"))

# Calculate centroids (some actual fall outside the mun borders)
centroid_aux_1950 <- st_centroid(amc_1950)

# st_point_on_surface guarantees that the centroid fall inside the mun borders
centroid_1950 <- st_point_on_surface(amc_1950)

# Plotting
plot(amc_1950$geometry)
plot(centroid_1950, add = TRUE)

# Loading Brazilian coast shapefile
costa <- st_read(here("data", "linha_costa", "LINHA_DE_COSTA_IMAGEM_GEOCOVER_SIRGAS_2000.shp"))

# Plotting
plot(costa$geometry)

## Assigning same coordinate
#  Transform to UTM
centroid_1950 <- st_transform(centroid_1950, 3055)
costa <-st_transform(costa, 3055)

# Distance to Centroids
costa <- st_cast(costa, "MULTILINESTRING")

dist_centroid <- st_distance(costa, centroid_1950)


# Create a data.frame with the distance and the coordinates of the points
dist_centroid <- tibble(dist_centroid = as.vector(dist_centroid)/1000,
                        centroid_1950$amc_1950_2, centroid_1950$NOME_MUNIC)

colnames(dist_centroid) <- c("dist_coast_centroid", "amc_1950", "nome")


# Saving
save(dist_centroid, file = here("output", "distance_coast", "dist_coast_1950amc.RData"))


