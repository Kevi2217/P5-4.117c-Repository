library(dplyr)
library(tidyr)
library(lubridate)
library(openxlsx)
library(ggplot2)
library(leaflet)
library(ggmap)
library(mapview)
library(sp)
library(mapdeck)
library(RColorBrewer)
library(ggcorrplot)
library(MASS)
library(stringr)

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
# Sætter Aalborg bynavn der refferes til
training_data[[1]]$bynavn <- as.factor(training_data[[1]]$bynavn)
training_data[[1]]$bynavn <- relevel(training_data[[1]]$bynavn, ref = "Aalborg")
# Sætter Aarhus bynavn der refferes til
training_data[[2]]$bynavn <- as.factor(training_data[[2]]$bynavn)
training_data[[2]]$bynavn <- relevel(training_data[[2]]$bynavn, ref = "Aarhus C")
# Sætter Aarhus bynavn der refferes til
training_data[[3]]$bynavn <- as.factor(training_data[[3]]$bynavn)
training_data[[3]]$bynavn <- relevel(training_data[[3]]$bynavn, ref = "København Ø")


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


###### NUMERISK VÆRDIER TIL MODEL 2 #########
# Vi kigger på correlation matrix for de numeriske værdier
# NOTE: Vi fjerne alle høj correlation explanatory variables, og dem som har correlation på under
# 0.1 til pris_salg.

### AAL
ggcorrplot(round(cor(cbind(pris_salg = training_data[[1]]$pris_salg,
                     beloeb_ejerudgift = training_data[[1]]$beloeb_ejerudgift,
                     antalfremvisninger = training_data[[1]]$antalfremvisninger,
                     areal_bolig = training_data[[1]]$areal_bolig,
                     ejd_antalplan = training_data[[1]]$ejd_antalplan,
                     ejd_ombygningsaar = training_data[[1]]$ejd_ombygningsaar,
                     dist_skole = training_data[[1]]$dist_skole,
                     dist_raadhus = training_data[[1]]$dist_raadhus)), 2),
           hc.order = TRUE, type = "lower", lab = TRUE)
# Værdier vi vil fjerne: beloeb_ejerudgift.

###
### AAR
ggcorrplot(round(cor(cbind(pris_salg = training_data[[2]]$pris_salg,
                           beloeb_ejerudgift = training_data[[2]]$beloeb_ejerudgift,
                           areal_bolig = training_data[[2]]$areal_bolig,
                           ejd_antalplan = training_data[[2]]$ejd_antalplan,
                           ejd_opfoerelsesaar = training_data[[2]]$ejd_opfoerelsesaar,
                           dist_skole = training_data[[2]]$dist_skole,
                           dist_raadhus = training_data[[2]]$dist_raadhus)), 2),
           hc.order = TRUE, type = "lower", lab = TRUE)
# Værdier vi vil fjerne: beloeb_ejerudgift, ejd_opfoerelsesaar, dist_skole.
# Vi kigger på plots af disse for at se om correlation giver mening eller ej.
par(mfrow = c(1, 3))

plot(training_data[[2]]$beloeb_ejerudgift,
     training_data[[2]]$pris_salg / 1000000,
     main = "Price vs Cost of Ownership",
     xlab = "Cost of Ownership",
     ylab = "Price (in millions)")

plot(training_data[[2]]$ejd_opfoerelsesaar,
     training_data[[2]]$pris_salg / 1000000,
     main = "Price vs Year of Construction",
     xlab = "Year of Construction",
     ylab = "Price (in millions)")

plot(training_data[[2]]$dist_skole,
     training_data[[2]]$pris_salg / 1000000,
     main = "Price vs School",
     xlab = "Distance to Nearest School",
     ylab = "Price (in millions)")

par(mfrow = c(1, 1))

# Fjerner outliers og plotter igen

training_data[[2]] <- training_data[[2]] %>%
  #beloeb_ejerudgift
  dplyr::filter(beloeb_ejerudgift < 500000) %>%
  #ejd_opfoerelsesaar
  dplyr::filter(ejd_opfoerelsesaar > 1800) %>%
  #dist_skole
  dplyr::filter(dist_skole < 2)

par(mfrow = c(1, 3))
plot(training_data[[2]]$beloeb_ejerudgift,
     training_data[[2]]$pris_salg / 1000000,
     main = "Price vs Cost of Ownership",
     xlab = "Cost of Ownership",
     ylab = "Price (in millions)")

plot(training_data[[2]]$ejd_opfoerelsesaar,
     training_data[[2]]$pris_salg / 1000000,
     main = "Price vs Year of Construction",
     xlab = "Year of Construction",
     ylab = "Price (in millions)")

plot(training_data[[2]]$dist_skole,
     training_data[[2]]$pris_salg / 1000000,
     main = "Price vs School",
     xlab = "Distance to Nearest School",
     ylab = "Price (in millions)")

par(mfrow = c(1, 1))

# Laver correlation-matrix uden outliers
ggcorrplot(round(cor(cbind(pris_salg = training_data[[2]]$pris_salg,
                           beloeb_ejerudgift = training_data[[2]]$beloeb_ejerudgift,
                           areal_bolig = training_data[[2]]$areal_bolig,
                           ejd_antalplan = training_data[[2]]$ejd_antalplan,
                           ejd_opfoerelsesaar = training_data[[2]]$ejd_opfoerelsesaar,
                           dist_skole = training_data[[2]]$dist_skole,
                           dist_raadhus = training_data[[2]]$dist_raadhus)), 2),
           hc.order = TRUE, type = "lower", lab = TRUE)
### KBH
ggcorrplot(round(cor(cbind(pris_salg = training_data[[3]]$pris_salg,
                           beloeb_ejerudgift = training_data[[3]]$beloeb_ejerudgift,
                           adresse_etage = training_data[[3]]$adresse_etage,
                           areal_bolig = training_data[[3]]$areal_bolig,
                           ejd_antalrum = training_data[[3]]$ejd_antalrum,
                           ejd_opfoerelsesaar = training_data[[3]]$ejd_opfoerelsesaar,
                           dist_raadhus = training_data[[3]]$dist_raadhus)), 2),
           hc.order = TRUE, type = "lower", lab = TRUE)
# Værdier vi vil fjerne: ejd_antalrum, beloeb_ejerudgift, dist_raadhus.
# Vi kigger på plots af disse for at se om correlation giver mening eller ej.

plot(training_data[[3]]$dist_raadhus,
     training_data[[3]]$pris_salg / 1000000,
     main = "Price vs Townhall",
     xlab = "Distance to Nearest Townhall",
     ylab = "Price (in millions)")
###


################## KATEGORISKE VÆRDIER TIL MODEL 2 ######################
# AAL (bynanv, energimaerke)
# Fjerner "F" i ejd_energimaerke
par(mfrow = c(1, 2))

plot(training_data[[1]]$bynavn,
     training_data[[1]]$pris_salg / 1000000,
     main = "Price vs Town",
     ylab = "Price (in millions)",
     xlab = "",
     las = 2,
     cex.axis = 0.7)

plot(training_data[[1]]$ejd_energimaerke,
     training_data[[1]]$pris_salg / 1000000,
     main = "Price vs Energy Label",
     xlab = "Energy label",
     ylab = "Price (in millions)")

par(mfrow = c(1, 1))

training_data[[1]] <- training_data[[1]] %>%
  dplyr::filter(ejd_energimaerke != "F")

training_data[[1]]$ejd_energimaerke <- droplevels(training_data[[1]]$ejd_energimaerke)

par(mfrow = c(1, 2))

plot(training_data[[1]]$bynavn,
     training_data[[1]]$pris_salg / 1000000,
     main = "Price vs Town",
     ylab = "Price (in millions)",
     xlab = "",
     las = 2,
     cex.axis = 0.7)

plot(training_data[[1]]$ejd_energimaerke,
     training_data[[1]]$pris_salg / 1000000,
     main = "Price vs Energy Label",
     xlab = "Energy label",
     ylab = "Price (in millions)")

par(mfrow = c(1, 1))
# AAR (bynavn, energimaerke, ejd_altan)
# Fjerner ejd_altan
par(mfrow = c(1, 3))

plot(training_data[[2]]$bynavn,
     training_data[[2]]$pris_salg / 1000000,
     main = "Price vs Town",
     ylab = "Price (in millions)",
     xlab = "",
     las = 2,
     cex.axis = 0.7)

plot(training_data[[2]]$ejd_energimaerke,
     training_data[[2]]$pris_salg / 1000000,
     main = "Price vs Energy Label",
     xlab = "Energy label",
     ylab = "Price (in millions)")

plot(training_data[[2]]$ejd_altan,
     training_data[[2]]$pris_salg / 1000000,
     main = "Price vs Balcony",
     xlab = "Balcony",
     ylab = "Price (in millions)")

par(mfrow = c(1, 1))

# KBH (bynavn, energimaerke, ejd_altan)
# Fjerner ejd_altan
par(mfrow = c(1, 3))

plot(training_data[[3]]$bynavn,
     training_data[[3]]$pris_salg / 1000000,
     main = "Price vs Town",
     ylab = "Price (in millions)",
     xlab = "",
     las = 2,
     cex.axis = 0.7)

plot(training_data[[3]]$ejd_energimaerke,
     training_data[[3]]$pris_salg / 1000000,
     main = "Price vs Energy Label",
     xlab = "Energy label",
     ylab = "Price (in millions)")

plot(training_data[[3]]$ejd_altan,
     training_data[[3]]$pris_salg / 1000000,
     main = "Price vs Balcony",
     xlab = "Balcony",
     ylab = "Price (in millions)")

par(mfrow = c(1, 1))
###






########## LAVER MODEL 2 ##########
# Kigger på model 2 hvor vi ikke har ovenståend fra correlation matrix med, og stadig de kategoriske ting med.
model_aal_2 <- lm(pris_salg ~ bynavn + antalfremvisninger + areal_bolig +
                    ejd_antalplan + ejd_energimaerke + ejd_ombygningsaar +
                    dist_skole + dist_raadhus, data = training_data[[1]])
# summary(model_aal_2)
# names(coef(model_aal_2))

model_aar_2 <- lm(pris_salg ~ bynavn + areal_bolig + ejd_antalplan +
                    ejd_energimaerke + dist_raadhus, data = training_data[[2]])
# summary(model_aar_2)
# names(coef(model_aar_2))


model_kbh_2 <- lm(pris_salg ~ bynavn + antalfremvisninger + adresse_etage +
                    areal_bolig + ejd_energimaerke + ejd_opfoerelsesaar, data = training_data[[3]])
# summary(model_kbh_2)
# names(coef(model_kbh_2))


############################ LOG MODEL 2 ####################################
model_aal_2_log <- lm(log(pris_salg) ~ bynavn + antalfremvisninger + areal_bolig +
                    ejd_antalplan + ejd_energimaerke + ejd_ombygningsaar +
                    dist_skole + dist_raadhus, data = training_data[[1]])
# summary(model_aal_2_log)
# names(coef(model_aal_2_log))

model_aar_2_log <- lm(log(pris_salg) ~ bynavn + areal_bolig + ejd_antalplan +
                    ejd_energimaerke + dist_raadhus, data = training_data[[2]])
# summary(model_aar_2_log)
# names(coef(model_aar_2_log))


model_kbh_2_log <- lm(log(pris_salg) ~ bynavn + antalfremvisninger + adresse_etage +
                    areal_bolig + ejd_energimaerke + ejd_opfoerelsesaar, data = training_data[[3]])
# summary(model_kbh_2_log)
# names(coef(model_kbh_2)_log)


par(mfrow = c(2, 3))
plot(model_aal_2, main = "AAL", which = 1)
plot(model_aar_2, main = "AAR", which = 1)
plot(model_kbh_2, main = "CPH", which = 1)

plot(model_aal_2_log, main = "AAL", which = 1)
plot(model_aar_2_log, main = "AAR", which = 1)
plot(model_kbh_2_log, main = "CPH", which = 1)
par(mfrow = c(1, 1))

par(mfrow = c(2, 3))
plot(model_aal_2, main = "AAL", which = 2)
plot(model_aar_2, main = "AAR", which = 2)
plot(model_kbh_2, main = "CPH", which = 2)

plot(model_aal_2_log, main = "AAL", which = 2)
plot(model_aar_2_log, main = "AAR", which = 2)
plot(model_kbh_2_log, main = "CPH", which = 2)
par(mfrow = c(1, 1))




# confint(model_aar_2, level = 0.95)










