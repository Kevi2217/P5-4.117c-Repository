library("dplyr")
library("tidyr")
library("lubridate")
library("openxlsx")
library("ggplot2")
library("leaflet")
library("ggmap")
library("mapview")
library("sp")
library("mapdeck")
library("RColorBrewer")
library("ggcorrplot")

# Definere generelle variable
set.seed(69)

# Henter data (ÆNDRE SELV STIEN)
load("~/Desktop/Projekt P5/stor_homedata.Rda")

# Renser data
source("renset_data.R")

# Test_data tage 10% af observationerne for hver by
test_data <- homedata %>%
  dplyr::group_by(kommunenavn) %>%
  dplyr::sample_n(size = (n() / 10), replace = FALSE) %>%
  dplyr::ungroup()

# Fjerner test_data fra homedata og laver training_data 
training_data <- homedata %>%
  dplyr::anti_join(test_data)

######################################################### ALT ANALYSE HERUNDER #####################################################





























