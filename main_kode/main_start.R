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
library("MASS")

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
training_data <- lapply(training_data, function(df) df[, !names(df) %in% c("kommunenavn", c("adresse_fuld",
                                                                           "gisy_wgs84", "gisx_wgs84", "salgsaar")), drop = FALSE])

######################################### ALT ANALYSE HERUNDER ###########################################
# test data og training_data er inddelt i en liste med længde 3.
# 1 er Aalborg,
# 2 er Aarhus,
# 3 er København.

# Denne vælger hvilken kategori i hver kategoriske-søjle der skal tage hensyn til når modeller bliver bygget
training_data <- lapply(training_data, function(df) {
  # ejd_altan
  df$ejd_altan <- as.factor(df$ejd_altan)
  df$ejd_altan <- relevel(df$ejd_altan, ref = "Ja")
  # salgsmaaned
  df$salgsmaaned <- as.factor(df$salgsmaaned)
  df$salgsmaaned <- relevel(df$salgsmaaned, ref = "Januar")
  # energimaerke
  df$ejd_energimaerke <- as.factor(df$ejd_energimaerke)
  df$ejd_energimaerke <- relevel(df$ejd_energimaerke, ref = "A")
  # adresse_etage
  df$adresse_etage <- as.factor(df$adresse_etage)
  df$adresse_etage <- relevel(df$adresse_etage, ref = "0")
  # sag_annonceretnettet
  df$sag_annonceretnettet <- as.factor(df$sag_annonceretnettet)
  df$sag_annonceretnettet <- relevel(df$sag_annonceretnettet, ref = "Ja")
  return(df)
})

# Bruger lm() til at lave en model som kigger på pris_salg og alle explanatory variables
model_aal_1 <- lm(pris_salg ~ ., data = training_data[[1]])
# summary(model_aal_1)
# names(coef(model_aal_1))

model_aar_1 <- lm(pris_salg ~ ., data = training_data[[2]])
# summary(model_aar_1)
# names(coef(model_aar_1))


model_kbh_1 <- lm(pris_salg ~ ., data = training_data[[3]])
# summary(model_kbh_1)
# names(coef(model_kbh_1))

# Benytter backwards selection for at bestemme vigtigste explanatory variables
model_aal_1s <- stepAIC(model_aal_1,
                direction = "backward", # Backward selection
                trace = FALSE)
# summary(model_aal_1s)
# names(coef(model_aal_1s))

model_aar_1s <- stepAIC(model_aar_1,
                        direction = "backward", # Backward selection
                        trace = FALSE)
# summary(model_aal_1s)
# names(coef(model_aal_1s))

model_kbh_1s <- stepAIC(model_kbh_1,
                        direction = "backward", # Backward selection
                        trace = FALSE)
# summary(model_aal_1s)
# names(coef(model_aal_1s))




# QQ plots
qqnorm(model_aal_1s$residuals)
qqline(model_aal_1s$residuals)

# COR MATRIX
# Fjerner kategoriske søjler så correlation matrix kan laves med udelukkende numeriske søjler
reduceret_data <- lapply(training_data, function(df) {
  df %>%
    as.data.frame() %>%
    dplyr::select(-sag_annonceretnettet, -ejd_altan, -ejd_energimaerke, -salgsmaaned) %>%
    mutate_all(as.numeric)
})

cor_matrix = round(cor(reduceret_data[[1]]), 2)
ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower",
           lab = TRUE)





















