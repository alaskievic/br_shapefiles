# Options
options(digits=10)        # number of digits to show
options(scipen=999)       # disable scientific notation
memory.limit(size=50000)  # allocate more RAM
set.seed(42)              # set seed for replications
options(stringsAsFactors = FALSE)


# Install packages
if (!require("pacman")) install.packages("pacman")

list.of.packages <- c("rgdal", "sf", "ggplot2", "data.table", "tidyverse", "readxl", 
                      "tmap", "fabricatr", "raster", "rgeos",
                      "RColorBrewer", "broom", "sp", "viridis", "grid", "broom",
                      "viridis", "lubridate", "plm", "haven", "RColorBrewer",
                      "foreign", "knitr", "stargazer", "glue",
                      "zoo", "stringi", "devtools", "Rdpack", "installr", "magrittr", 
                      "janitor", "ggpmisc", "ipumsr", "survey", "srvyr", "DBI", 
                      "bigrquery", "here", "terra", "crul")


# In the first time running, turn install = TRUE
pacman::p_load(list.of.packages, character.only = TRUE, install = FALSE)

#Updates packages
update.packages(ask = FALSE)

#Updates R
#updateR()
