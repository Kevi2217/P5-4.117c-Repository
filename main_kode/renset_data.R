# Data rensning
colnames(homedata) <- tolower(colnames(homedata))

homedata <- homedata %>%
  rename("periode_salg" = "dwid_periode_salg") %>%
  dplyr::filter(sagsstatus != "Under salg") %>%
  # Fjerner meget ukomplette/ligegyldige søjler
  dplyr::select(-ejd_antaltoiletter, -pris_foersteudbud, -dato_aktueludbudpris, -ejd_ombygningsaar,
                -areal_bolig_commercial, -ejd_antalsovevaerelser, -dwid_periode_formid, -dwid_periode_annoncering,
                -dwid_periode_oprettet, -nk_sagid, -sagstype, -sagsstatus,
                -dato_annoncering, -kvhx, -adresse_postnr, -row_id,
                -salgstid, -hoejhus, -dwid_projektsalg, -beloeb_udbetaling,
                -beloeb_mdnetto, -beloeb_mdbrutto, -pris_aktueludbud, -antalstatusmoedersaelger,
                -afstemningsomraade, -pris_ejdvurdering) %>%
  # Beholder ejerlejligheder
  dplyr::filter(ejdtype == "Ejerlejlighed") %>%
  # Renser etage-søjlen så den kun er numerisk
  dplyr::mutate(adresse_etage = ifelse(is.na(adresse_etage) & ejdtype == "Villa", 0,
                                       ifelse(is.na(adresse_etage) & ejdtype == "Fritidshus", 0,
                                       ifelse(adresse_etage == "NULL", 0,
                                       ifelse(adresse_etage == "ST", 0,
                                       ifelse(adresse_etage == "kld.", -1,
                                       ifelse(adresse_etage == "kl", -1, adresse_etage
                                       ))))))) %>%
  # Renser etage-søjlen videre for NA'er hvis etagen står i adresse-søjlen
  dplyr::mutate(adresse_etage = ifelse(rowSums(sapply(c("st\\.", "ST", "st"), grepl, adresse_fuld)) > 0, 0,
                                       ifelse(rowSums(sapply(c("11\\.", "011\\."), grepl, adresse_fuld)) > 0, 11,
                                       ifelse(rowSums(sapply(c("1\\.", "01\\."), grepl, adresse_fuld)) > 0, 1,
                                       ifelse(rowSums(sapply(c("2\\.", "02\\."), grepl, adresse_fuld)) > 0, 2,
                                       ifelse(rowSums(sapply(c("3\\.", "03\\."), grepl, adresse_fuld)) > 0, 3,
                                       ifelse(rowSums(sapply(c("4\\.", "04\\."), grepl, adresse_fuld)) > 0, 4,
                                       ifelse(rowSums(sapply(c("5\\.", "05\\."), grepl, adresse_fuld)) > 0, 5,
                                       ifelse(rowSums(sapply(c("6\\.", "06\\."), grepl, adresse_fuld)) > 0, 6,
                                       ifelse(rowSums(sapply(c("7\\.", "07\\."), grepl, adresse_fuld)) > 0, 7,
                                       ifelse(rowSums(sapply(c("8\\.", "08\\."), grepl, adresse_fuld)) > 0, 8, adresse_etage
                                       ))))))))))) %>%
  mutate(ejd_energimaerke = ifelse(grepl("^A", ejd_energimaerke), "A", 
                                   ifelse(ejd_energimaerke == "Unknown", NA, ejd_energimaerke))) %>%
  # Renser storgrund-søjlen for NA. HOME's definition af storgrund er 1500m^2< 
  dplyr::mutate(storgrund = ifelse(is.na(storgrund) & areal_bolig <= 1500, 0,
                                   ifelse(is.na(storgrund) & areal_bolig > 1500, 1, storgrund))) %>%
  # Laver egen alder-søjle
  dplyr::mutate(alder = as.numeric(format(Sys.Date(), "%Y")) - ejd_opfoerelsesaar) %>%
  dplyr::filter(ejd_opfoerelsesaar > 1700) %>%
  # Runder dist til skole og raadhus af
  dplyr::mutate(dist_skole = round(dist_skole, 3), dist_raadhus = round(dist_raadhus, 3)) %>%
  # For alle fremvisninger der har NA sætter vi 0 (da ingen fremvisninger er skrevet ind)
  dplyr::mutate(antalfremvisninger = ifelse(is.na(antalfremvisninger), 0, antalfremvisninger)) %>%
  # Setter areal_grund til areal_bolig, da der hvor areal_grund = NA er lejligheder
  dplyr::mutate(areal_grund = ifelse(is.na(areal_grund), areal_bolig, areal_grund)) %>%
  # Konverterer salgsåret til kun år for at gruppere senere
  dplyr::mutate(periode_salg = year(periode_salg)) %>%
  # Filtrerer for tre kommuner
  dplyr::filter(kommunenavn == "Aalborg" | kommunenavn == "Aarhus" | kommunenavn == "København") %>%
  # Vælger det år med flest observationer (2015)
  dplyr::filter(periode_salg == 2015) %>%
  # Fjerner yderligere søjler som ikke har betydning når hvor vores problmen er begrænset
  dplyr::select(-ejdtype, -regionnavn, -postnr,
                -bynavn, -sogn, -corona,
                -periode_salg, -ejd_opfoerelsesaar) %>%
  na.omit()


