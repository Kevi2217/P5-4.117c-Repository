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
library("stringr")

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
                                                                           "gisy_wgs84", "gisx_wgs84", "salgsaar")),
                                                       drop = FALSE])

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
  # energimaerke
  df$ejd_energimaerke <- as.factor(df$ejd_energimaerke)
  df$ejd_energimaerke <- relevel(df$ejd_energimaerke, ref = "A")
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
names(coef(model_aal_1s))

model_aar_1s <- stepAIC(model_aar_1,
                        direction = "backward", # Backward selection
                        trace = FALSE)
# summary(model_aar_1s)
names(coef(model_aar_1s))

model_kbh_1s <- stepAIC(model_kbh_1,
                        direction = "backward", # Backward selection
                        trace = FALSE)
# summary(model_kbh_1s)
names(coef(model_kbh_1s))


# COR MATRIX
# Fjerner kategoriske søjler så correlation matrix kan laves med udelukkende numeriske søjler
numerisk_data <- lapply(training_data, function(df) {
  df %>%
    as.data.frame() %>%
    dplyr::select(-sag_annonceretnettet, -ejd_altan,
                  -ejd_energimaerke, -hoejhus) %>%
    mutate_all(as.numeric)
})

# Vi kigger på correlation matrix for de numeriske værdier
# NOTE: Vi fjerne alle høj correlation explanatory variables, og dem som har correlation på under
# 0.15 til pris_salg.
# Værdier vi vil fjerne for nedestående: ejd_antalrum, salgstid, areal_grund, ejd_antalplan
ggcorrplot(round(cor(cbind(pris_salg = training_data[[1]]$pris_salg,
                     areal_bolig = training_data[[1]]$areal_bolig,
                     areal_grund = training_data[[1]]$areal_grund,
                     ejd_antalplan = training_data[[1]]$ejd_antalplan,
                     ejd_antalrum = training_data[[1]]$ejd_antalrum,
                     salgstid = training_data[[1]]$salgstid,
                     dist_skole = training_data[[1]]$dist_skole,
                     alder = training_data[[1]]$alder,
                     dist_raadhus = training_data[[1]]$dist_raadhus)), 2),
           hc.order = TRUE, type = "lower", lab = TRUE)

# Værdier vi vil fjerne for nedestående: ejd_antalrum, salgstid, antal_fremvisninger
ggcorrplot(round(cor(cbind(pris_salg = training_data[[2]]$pris_salg,
                           beloeb_ejerudgift = training_data[[2]]$beloeb_ejerudgift,
                           antalfremvisninger = training_data[[2]]$antalfremvisninger,
                           areal_bolig = training_data[[2]]$areal_bolig,
                           ejd_antalplan = training_data[[2]]$ejd_antalplan,
                           ejd_antalrum = training_data[[2]]$ejd_antalrum,
                           salgstid = training_data[[2]]$salgstid,
                           alder = training_data[[2]]$alder,
                           dist_raadhus = training_data[[2]]$dist_raadhus)), 2),
           hc.order = TRUE, type = "lower", lab = TRUE)

# Værdier vi vil fjerne for nedestående: ejd_antalrum, areal_kaelder, ejd_antalplan, dist_raadhus
ggcorrplot(round(cor(cbind(pris_salg = training_data[[3]]$pris_salg,
                           antalfremvisninger = training_data[[3]]$antalfremvisninger,
                           adresse_etage = training_data[[3]]$adresse_etage,
                           areal_bolig = training_data[[3]]$areal_bolig,
                           areal_grund = training_data[[3]]$areal_grund,
                           areal_kaelder = training_data[[3]]$areal_kaelder,
                           ejd_antalplan = training_data[[3]]$ejd_antalplan,
                           ejd_antalrum = training_data[[3]]$ejd_antalrum,
                           alder = training_data[[3]]$alder,
                           dist_raadhus = training_data[[3]]$dist_raadhus)), 2),
           hc.order = TRUE, type = "lower", lab = TRUE)

# Kigger på model 2 hvor vi ikke har ovenståend fra correlation matrix med, og stadig de kategoriske ting med.
model_aal_2 <- lm(pris_salg ~ areal_bolig + dist_skole +
                    alder + dist_raadhus +
                    ejd_energimaerke + ejd_altan, data = training_data[[1]])
summary(model_aal_2)
# names(coef(model_aal_2))

model_aar_2 <- lm(pris_salg ~ beloeb_ejerudgift + areal_bolig +
                    ejd_antalplan + alder + dist_raadhus +
                    ejd_altan, data = training_data[[2]])
summary(model_aar_2)
# names(coef(model_aar_2))


model_kbh_2 <- lm(pris_salg ~ antalfremvisninger + adresse_etage +
                    areal_bolig + areal_grund + alder +
                    ejd_energimaerke + ejd_altan, data = training_data[[3]])
summary(model_kbh_2)
# names(coef(model_kbh_2))

















