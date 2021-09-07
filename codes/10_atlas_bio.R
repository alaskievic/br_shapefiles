#Load packages
source("00_load_packages.R")

########### 1. Read Land Productivity Measure From Ramankutty (2002) ###########

### Reads Ramankutty Data
suit <- terra::rast(here("data", "ramankutty", "suit", "dblbnd.adf"))

# Plotting
plot(suit)


# Get Brazilian Borders
brazil <- getData("GADM",country="Brazil",level=0)
brazil <- terra::vect(brazil)

# Reprojects raster
terra::crs(suit) <- terra::crs(brazil)


# Restrict to Brazilian borders
suit_crop <- terra::crop(suit, brazil)

suit_mask <- terra::mask(suit_crop, brazil)

plot(suit_mask)


### Aggregating into AMCs

# 1960-2010 AMCs
amc_1960 <- st_read(here("data","amc", "amc_1960_2010.shp"))


# Mean value inside each AMC

# Transforming into terra format
amc_1960 <- terra::vect(amc_1960)


# Reprojects amc file
terra::crs(amc_1960) <- terra::crs(suit_mask)

suit_avg <- terra::extract(suit_mask, amc_1960, fun = mean, na.rm = TRUE)


suit_avg  %<>% mutate(code2010 =amc_1960$GEOCODIG_M) %>% 
  mutate(amc_name = amc_1960$NOME_MUNIC) %>% mutate(amc = amc_1960$amc_1960_2) %>%
  dplyr::select(-ID) %>% rename()




colnames(night_2010_avg) <- c("light_2010", "cod", "mun_name_2010")


light_join <- inner_join(night_2000_avg, night_2010_avg, by = "cod")







# Reprojects raster
projection(raster) <- projection(amc_1960)

amc_1960 = st_transform(amc_1960, projection(suit))


# Restrict to 1960 AMC borders
suit_mask <- mask(suit, amc_1960)

# Plotting
plot(suit_mask)

# Extracting Values
suit_values = raster::extract(x = suit_mask, y = amc_1960, 
                                        sp = TRUE, fun = mean, na.rm = TRUE, 
                                        df = TRUE)

suit_values_df <- as_tibble(suit_values@data)


colnames(suit_values_df) <- c("codigo", "nome", "days_pf")



# Get the mean value per municipality
malaria_day_pf_mean <- group_by(malaria_day_pf_values, ID) %>% 
  summarize_at(vars(days_pf), list( ~mean(., na.rm = TRUE)))






teste <- suit <- terra::rast(here("data", "ramankutty", "historical_crop", "gl1990_0.5.asc"))


plot(teste)




