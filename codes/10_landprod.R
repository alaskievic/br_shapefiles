#Load packages
source("00_load_packages.R")

########### 1. Read Land Productivity Measure From Ramankutty (2002) ###########

### Read Terrain Ruggedness Index for the whole planet
suit <- terra::rast(here("shapefiles", "ramankutty", "data", "suit", "hdr.adf"))


plot(suit)
