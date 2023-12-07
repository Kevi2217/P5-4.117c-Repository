# Data rensning
colnames(homedata) <- tolower(colnames(homedata))

homedata <- homedata %>%
  dplyr::rename("periode_salg" = "dwid_periode_salg") %>%
  dplyr::filter(sagsstatus != "Under salg") %>%
  # Fjerner meget ukomplette/ligegyldige søjler
  dplyr::select(-ejd_antaltoiletter, -pris_foersteudbud, -dato_aktueludbudpris,
                -ejd_antalsovevaerelser, -dwid_periode_formid, -dwid_periode_annoncering,
                -dwid_periode_oprettet, -nk_sagid, -sagstype, -sagsstatus,
                -dato_annoncering, -kvhx, -adresse_postnr, -row_id,
                -dwid_projektsalg, -beloeb_udbetaling, -areal_bolig_commercial,
                -beloeb_mdnetto, -beloeb_mdbrutto, -pris_aktueludbud, -antalstatusmoedersaelger,
                -afstemningsomraade, -pris_ejdvurdering, -salgsmaaned, -areal_kaelder, -salgstid,
                -areal_grund, -storgrund, -hoejhus, -areal_garagecarport, -alder) %>%
  # Beholder ejerlejligheder
  dplyr::filter(ejdtype == "Ejerlejlighed") %>%
  # Sætter opbygningsaar til opfoerelsesaar hvis NA
  dplyr::mutate(ejd_ombygningsaar = ifelse(is.na(ejd_ombygningsaar), ejd_opfoerelsesaar,
                                           ejd_ombygningsaar)) %>%
  # Renser etage-søjlen så den kun er numerisk
  dplyr::mutate(adresse_etage = ifelse(is.na(adresse_etage) & ejdtype == "Villa", 0,
                                       ifelse(is.na(adresse_etage) & ejdtype == "Fritidshus", 0,
                                       ifelse(adresse_etage == "NULL" | adresse_etage == "ST", 0,
                                       ifelse(adresse_etage == "kld." | adresse_etage == "kl", -1,
                                    adresse_etage))))) %>%
  dplyr::mutate(adresse_etage = is.numeric(adresse_etage)) %>%
  # Renser etage-søjlen videre for NA'er hvis etagen står i adresse-søjlen
  mutate(adresse_etage = ifelse(grepl("(?i),\\s* kl | (?i),\\s* kld", adresse_fuld), -1,
                                ifelse(grepl("(?i)\\s* st\\.|(?i),\\s* st", adresse_fuld), 0,
                                       ifelse(grepl("(?i),\\s*(\\d+)\\s+", adresse_fuld), 
                                              as.numeric(gsub("\\D","",str_extract(adresse_fuld, "(?i),\\s*(\\d+)\\s+"))),
                                              adresse_etage
                                       )))) %>%
  mutate(ejd_energimaerke = ifelse(grepl("^A", ejd_energimaerke), "A", 
                                   ifelse(ejd_energimaerke == "Unknown", NA, ejd_energimaerke))) %>%
  dplyr::filter(ejd_opfoerelsesaar > 1700) %>%
  # Runder dist til skole og raadhus af
  dplyr::mutate(dist_skole = round(dist_skole, 3), dist_raadhus = round(dist_raadhus, 3)) %>%
  # For alle fremvisninger der har NA sætter vi 0 (da ingen fremvisninger er skrevet ind)
  dplyr::mutate(antalfremvisninger = ifelse(is.na(antalfremvisninger), 0, antalfremvisninger)) %>%
  # Konverterer salgsåret til kun år for at gruppere senere
  dplyr::mutate(periode_salg = year(periode_salg)) %>%
  # Filtrerer for tre kommuner
  dplyr::filter(kommunenavn == "Aalborg" | kommunenavn == "Aarhus" | kommunenavn == "København") %>%
  # Vælger det år med flest observationer (2015)
  dplyr::filter(periode_salg == 2021) %>%
  # Fjerner yderligere søjler som ikke har betydning når hvor vores problmen er begrænset
  dplyr::select(-ejdtype, -regionnavn, -postnr,
                -sogn, -corona,
                -periode_salg) %>%
  na.omit()



## NOTER ########


