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



########### 1.4 Historical Municipality Population #############

mun_pop <- read_excel(here("data", "mun_data", "pop_mun_TUR_1872_a_2010.xls"))


## Create a codes and names file
mun_code_ibge <- mun_pop %>% dplyr::select("código", starts_with("cod"), 
                                            starts_with("Município")) %>%
  rename(code2010 = "código") %>% mutate(code2010 = as.integer(code2010))


save(mun_code_ibge, file = here("output", "mun_data", "mun_code_ibge.RData"))


## Population numbers
mun_pop %<>% janitor::clean_names()

mun_pop %<>% dplyr::select(c("codigo", "municipio", starts_with("pop"))) %>%
  dplyr::select(-c("pop_livre", "pop_escrava")) %>%
  rename(code2010 = "codigo", mun_name = "municipio", pop_tot_1872 = pop_total)



mun_pop %<>% map_dfr(., gsub, pattern = ".", replacement = "", fixed = TRUE)
mun_pop %<>% mutate_at(vars(matches("pop")), as.integer) %>%
  mutate(code2010 = as.integer(code2010))
mun_pop[is.na(mun_pop)] <- 0

mun_pop_lon <- pivot_longer(mun_pop, cols = starts_with("pop_"),
                            names_to = c(".value", "year"),
                            names_pattern = "([^\\.]*)\\.*(\\d{4})")


mun_pop_lon %<>% janitor::clean_names()

### Merge with AMCs and collapse

# Get 1960-2000 AMC Crosswalk
cross_1960 <- read_dta(file = here("data", "amc", "crosswalk", "_Crosswalk_final_1960_2000.dta")) %>%
  dplyr::select(uf_amc, final_name, amc, code2010) %>% rename("amc_1960" = "amc") %>%
   mutate(code2010 = as.integer(code2010))


pop_amc1960 <- full_join(mun_pop_lon, cross_1960, by = "code2010")

pop_amc1960 %<>% group_by(amc_1960, year) %>%
  summarise(pop_tot = sum(pop_tot), pop_urb = sum(pop_urb), pop_rur = sum(pop_rur)) %>%
  ungroup() %>% mutate(year = as.integer(year))


save(pop_amc1960, file = here("output", "mun_data", "pop_amc1960.RData"))



######################### 2. Merging all datasets ##############################
load(here("output", "mun_data", "pop_amc1960.RData"))
load(here("output", "slope", "altitude_1960amc.RData"))
load(here("output", "distance_coast", "dist_coast_1960amc.RData"))

# Voltando atrás xisde
pop_amc_wider <- pivot_wider(pop_amc1960, values_from = c("pop_tot", "pop_urb", "pop_rur"), 
                             names_from = "year")


altitude_1960 %<>% mutate(amc_1960 = as.integer(amc_1960))
dist_centroid %<>% mutate(amc_1960 = as.integer(amc_1960))


amc_final <- full_join(amc_1960, amc_climate, by = "amc_1960") %>%
             full_join(., altitude_1960, by = "amc_1960") %>%
             full_join(., dist_centroid, by = "amc_1960") %>%
             full_join(., pop_amc_wider, by = "amc_1960")

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




### Presence of Caixa in 1960 AMCs ###

# Read dta with BB presence
bb_1960 <- read_dta(here("output", "amc", "agri_bb_amc_clean.dta")) %>%
  filter(year <= 1960)


amc_1960 %<>% rename(amc = amc_1960_2) %>% mutate(amc = as.integer(amc))

# Join with shapefile
caixa_shp <- inner_join(amc_1960, bb_1960, by = "amc")

caixa_shp %<>% mutate(d_caixa = as.factor(d_caixa))

caixa_plot <- tm_shape(caixa_shp) +
  tm_fill("d_caixa",  palette="RdBu", border.col = "black",
          border.alpha = .3, showNA = TRUE, textNA="No Data",
          title = "Presence of Caixa \nBranch Before 1975", 
          labels = c("No", "Yes")) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.text.size=1.25,
            legend.title.size=1.55,
            legend.position = c("left","bottom"), 
            legend.height=1.0, 
            frame = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 1) 

caixa_plot

# Saving
tmap_save(caixa_plot, here("output", "amc", "caixa_presence.png"))



######################## 4. Rural Credit Graphs ################################

rcredit<- read_excel(here("data", "rural_credit", "araujo_credito_rural.xlsx"))

rcredit %<>% janitor::clean_names()

rcredit %<>% mutate(pib_agricola = pib_agricola/1000)

# Pivoting variables
rcredit %<>% pivot_longer(cols = c("custeio_valor", "total_valor"),
                      names_to = "type_value", values_to =  "value")

rcredit %<>% pivot_longer(cols = c("custeio_numero", "total_numero"),
                          names_to = "type_number", values_to =  "number")

rcredit %<>% pivot_longer(cols = c("custeio_percent_pib", "total_percent_pib"),
                          names_to = "type_percent_pib", values_to =  "percent_pib")



rcredit_pib <- ggplot(rcredit, aes(x=ano)) +
  geom_line(data = rcredit, aes(y=pib_agricola), lwd = 1)+
  geom_point(data = rcredit, aes(x=ano, y=pib_agricola), size=2)+
  labs(x = "Year", y = "Agricultural GDP (Billions of R$)") +
  ggtitle("Historical Agricultural GDP") +
  theme_bw(base_size = 16) +
  theme(legend.text=element_text(size=14), legend.position = c(0.80, 0.6), 
        legend.box.background = element_blank(), legend.title = element_blank())

rcredit_pib

ggsave(filename = "rcredit_pib.png", plot = rcredit_pib, path = here("output", "amc"))

# Defining Palette
c25 <- c(
  "dodgerblue2", 
  "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)



rcredit_value <- ggplot(rcredit, aes(x=ano)) +
  geom_line(data = rcredit, aes(y=value, color = type_value, group = type_value), lwd = 1)+
  scale_color_manual(values=c25, 
                     labels = c("Custeio", "Total"))+
  geom_point(data = rcredit, aes(x=ano, y=value, color = type_value), size=2)+
  labs(x = "Year", y = "Rural Credit (Millions of R$)") +
  ggtitle("Value of Rural Credit") +
  theme_bw(base_size = 16) +
  theme(legend.text=element_text(size=14), legend.position = c(0.80, 0.6), 
        legend.box.background = element_blank(), legend.title = element_blank())


rcredit_value

ggsave(filename = "rcredit_value.png", plot = rcredit_value, path = here("output", "amc"))



rcredit_number <- ggplot(rcredit, aes(x=ano)) +
  geom_line(data = rcredit, aes(y=number, color = type_number, group = type_number), lwd = 1)+
  scale_color_manual(values=c25, 
                     labels = c("Custeio", "Total"))+
  geom_point(data = rcredit, aes(x=ano, y=number, color = type_number), size=2)+
  labs(x = "Year", y = "Number of New Loans") +
  ggtitle("Number of New Rural Loans") +
  theme_bw(base_size = 16) +
  theme(legend.text=element_text(size=14), legend.position = c(0.75, 0.6), 
        legend.box.background = element_blank(), legend.title = element_blank())


rcredit_number

ggsave(filename = "rcredit_number.png", plot = rcredit_number, path = here("output", "amc"))


rcredit_percent <- ggplot(rcredit, aes(x=ano)) +
  geom_line(data = rcredit, aes(y=percent_pib, color = type_percent_pib,
                                group = type_percent_pib), lwd = 1)+
  scale_color_manual(values=c25, 
                     labels = c("Custeio", "Total"))+
  geom_point(data = rcredit, aes(x=ano, y=percent_pib, color = type_percent_pib), size=2)+
  labs(x = "Year", y = "Rural Credit (% Agricultural GDP)") +
  ggtitle("Rural Credit Over GDP") +
  theme_bw(base_size = 16) +
  theme(legend.text=element_text(size=14), legend.position = c(0.80, 0.6), 
        legend.box.background = element_blank(), legend.title = element_blank())


rcredit_percent

ggsave(filename = "rcredit_percent.png", plot = rcredit_percent, path = here("output", "amc"))





