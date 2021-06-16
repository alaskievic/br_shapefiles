#Load packages
source("00_load_packages.R")

############################ 1. Read Rivers from ANA ###########################

### Read all cursos d'agua

curso <- st_read(here::here("shapefiles", "rivers", "data", "curso_ana", "geoft_bho_cursodagua.shp"))

# Create a dataset without geometry for visualization

curso_dt <- st_set_geometry(curso, NULL)


### Read all rivers (use for merge to obtain names only)

river <- st_read(here("shapefiles", "rivers", "data", "river_ana", "geoft_bho_rio.shp"))
  
# Create a dataset without geometry for visualization

river <- st_set_geometry(river, NULL)


### We are interesed in cocursodag and nuareabacc to define the main rivers
# Remove last two digits

curso %<>% mutate(cod = COCURSODAG) %>% mutate(cod = as.numeric(cod))

river %<>% mutate(cod = CORIO) %>% mutate(cod = substr(cod, 1, nchar(cod)-2)) %>% 
  mutate(cod = as.numeric(cod))

#First dummy - Rivers of otto level 3

curso_d3 <- curso %>% filter(cod <= 999)

#Second dummy - Rivers of otto level 4

curso_d4 <- curso %>% filter(cod <= 9999)

# Third dummy - Rivers with more tha 1.000km2 of nuareabacc (hidrographic contribution)

curso_d1000 <- curso %>% filter(NUAREABACC >= 1000)

# fourth dummy - Rivers with more tha 5.000km2 of nuareabacc (hidrographic contribution)

curso_d5000 <- curso %>% filter(NUAREABACC >= 5000)


# Take a glimpse of names

curso_d3_names <- inner_join(curso_d3, river, by = "cod")
curso_d4_names <- inner_join(curso_d4, river, by = "cod")
curso_d1000_names <- inner_join(curso_d1000, river, by = "cod")
curso_d5000_names <- inner_join(curso_d5000, river, by = "cod")


# Ploting
# plot(st_geometry(curso_d3_names))
# plot(st_geometry(curso_d5000_names))


### 1872 municipality borders

mun_1872 <- st_read(here("shapefiles", "mun_borders", "municip_1872", "malha_municipal_1872.shp"))
                    
                    
# Ploting
# plot(st_geometry(mun_1872))



### Intersecting with municipalities
# Assigning corrdinate system

curso_d3_names <- st_transform(curso_d3_names, crs = st_crs(mun_1872))
curso_d4_names <- st_transform(curso_d4_names, crs = st_crs(mun_1872))
curso_d1000_names <- st_transform(curso_d1000_names, crs = st_crs(mun_1872))
curso_d5000_names <- st_transform(curso_d5000_names, crs = st_crs(mun_1872))


# Intersecting

river_d3 <- as_tibble(st_intersection(curso_d3_names, mun_1872))
river_d3 %<>% distinct(codigo) %>% mutate(river_d3 = 1)

river_d4 <- as_tibble(st_intersection(curso_d4_names, mun_1872))
river_d4 %<>% distinct(codigo) %>% mutate(river_d4 = 1)

river_d1000 <- as_tibble(st_intersection(curso_d1000_names, mun_1872))
river_d1000 %<>% distinct(codigo) %>% mutate(river_d1000 = 1)

river_d5000 <- as_tibble(st_intersection(curso_d5000_names, mun_1872))
river_d5000 %<>% distinct(codigo) %>% mutate(river_d5000 = 1)


# Merge back with shapefile

river_d3 <- full_join(mun_1872, river_d3, by = "codigo")
river_d4 <- full_join(mun_1872, river_d4, by = "codigo")
river_d1000 <- full_join(mun_1872, river_d1000, by = "codigo")
river_d5000 <- full_join(mun_1872, river_d5000, by = "codigo")

# Remove geometry

river_d3 <- st_set_geometry(river_d3, NULL)
river_d4 <- st_set_geometry(river_d4, NULL)
river_d1000 <- st_set_geometry(river_d1000, NULL)
river_d5000 <- st_set_geometry(river_d5000, NULL)


# Replace NAs with zero and count
river_d3 %<>% replace(., is.na(.), 0)
river_d4 %<>% replace(., is.na(.), 0)
river_d1000 %<>% replace(., is.na(.), 0)
river_d5000 %<>% replace(., is.na(.), 0)

# Count

river_d3 %>% count(river_d3 == 1)
river_d4 %>% count(river_d4 == 1)
river_d1000 %>% count(river_d1000 == 1)
river_d5000 %>% count(river_d5000 == 1)


# Final merge and saving

river_dummy <- full_join(river_d3, dplyr::select(river_d4, -nome), by = "codigo") %>%
               full_join(., dplyr::select(river_d1000, -nome)    , by = "codigo") %>% 
               full_join(., dplyr::select(river_d5000, -nome)    , by = "codigo")

save(river_dummy, file = here("output", "rivers", "river_dummy_1872.RData"))
