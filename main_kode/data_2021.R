
colnames(homedata) <- tolower(colnames(homedata))

homedata2021 <- homedata %>%
  #Filter for observationer i 2021, alt der ikke er under salg og er ejerlejlighed
  rename("periode_salg" = "dwid_periode_salg") %>%
  filter(year(periode_salg) == 2021) %>%
  filter(sagsstatus != "Under salg") %>%
  filter(ejdtype == "Ejerlejlighed") %>%
  
  #Fjerner søjler
  select(-pris_foersteudbud, -dato_aktueludbudpris, -areal_bolig_commercial,
         -dwid_periode_formid, -dwid_periode_annoncering, - regionnavn,
         -dwid_periode_oprettet, -nk_sagid, -sagstype, -sagsstatus, -bynavn,
         -dato_annoncering, -row_id, -dwid_projektsalg, -beloeb_udbetaling,
         -beloeb_mdnetto, -beloeb_mdbrutto, -pris_aktueludbud, -corona,
         -antalstatusmoedersaelger, -afstemningsomraade, -pris_ejdvurdering,
         -storgrund, -sag_annonceretnettet, -kvhx, -salgsaar, -salgsmaaned,
         -hoejhus, -areal_grund, -areal_garagecarport, -ejd_antalsovevaerelser,
         -ejd_antalplan, -adresse_postnr, -ejd_antaltoiletter, -areal_kaelder) %>%
  
  #Divider prisen med 1.000.000 for at aflæse plots nemmere.
  mutate(pris_salg = pris_salg/1000000) %>%
  
  # Sætter kategorisk værdi til kælder
  #mutate(kaelder = as.factor(ifelse(areal_kaelder > 0 , "ja", "nej"))) %>%
  
  mutate(ejd_ombygningsaar = ifelse(is.na(ejd_ombygningsaar), ejd_opfoerelsesaar, ejd_ombygningsaar)) %>%
  
  #Husets alder i 2021
  mutate(alder = 2021 - ejd_opfoerelsesaar) %>%
  
  #Alder på ombygning i 2021
  mutate(alder_ombyg = 2021 - ejd_ombygningsaar) %>%
  # Udfylder tomme indgange og fikser indgange med str i adresse_etage søjlen
  mutate(adresse_etage = ifelse(grepl("(?i),\\s* kl | (?i),\\s* kld", adresse_fuld), -1,
                                ifelse(grepl("(?i)\\s* st\\.|(?i),\\s* st", adresse_fuld), 0,
                                       ifelse(grepl("(?i),\\s*(\\d+)\\s+", adresse_fuld), 
                                              as.numeric(gsub("\\D","",str_extract(adresse_fuld, "(?i),\\s*(\\d+)\\s+"))),
                                              adresse_etage
                                       )))) %>%
  mutate(adresse_etage = ifelse(adresse_etage == "NULL", 0,
                                ifelse(adresse_etage == "kld.", -1, adresse_etage))) %>%
  
  filter(kommunenavn == "Aalborg" | kommunenavn == "Aarhus" | kommunenavn == "København") %>%
  #Omdanner kategoriske værdier til at have en factor
  mutate(ejd_energimaerke = as.factor(ejd_energimaerke),
         kommunenavn = as.factor(kommunenavn),
         postnr = as.factor(postnr),
         ejd_altan = as.factor(ejd_altan)) %>%
  na.omit()
