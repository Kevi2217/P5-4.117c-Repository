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
  dplyr::group_split()
# Fjerner kommunenavn søjlen for alle dataframes i listen
test_data <- lapply(test_data, function(df) df[, !names(df) %in% "kommunenavn", drop = FALSE])


# Fjerner test_data fra homedata og laver training_data 
training_data <- homedata %>%
  dplyr::anti_join(bind_rows(test_data)) %>%
  dplyr::group_by(kommunenavn) %>%
  dplyr::group_split()
# Fjerner kommunenavn søjlen for alle dataframes i listen
training_data <- lapply(training_data, function(df) df[, !names(df) %in% "kommunenavn", drop = FALSE])

######################################### ALT ANALYSE HERUNDER ###########################################
# test data og training_data er inddelt i en liste med længde 3.
# 1 er Aalborg,
# 2 er Aarhus,
# 3 er København.















