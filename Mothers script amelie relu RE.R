# ---- Initialisation du projet ----
library(edsRApi)
library(dplyr)
library(knitr)
library(questionr)
library(lubridate)
library(ggplot2)
source("C:\\Users\\savaame\\Desktop\\1000 premier\\read_excel_paths.R")


# ---- Tableau vierge avec les pseudonymPatientNum ----

sexe <- read_excel_paths(keyword = "sexe")


mothers <- sexe %>%
  dplyr::select(pseudonymPatientNum)%>%
  dplyr::group_by(pseudonymPatientNum)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()%>%
  dplyr::arrange(pseudonymPatientNum)


# ---- Sexe mere ----

sexe_filtered <- sexe %>%
  dplyr::group_by(pseudonymPatientNum, labelPermissible)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()%>%
  dplyr::rename(sexe =labelPermissible)%>%
  dplyr::select(pseudonymPatientNum, sexe)


mothers <- mothers %>%
  dplyr::left_join(sexe_filtered, by = "pseudonymPatientNum")


# ---- Ddn mere ----

ddn <- sexe %>%
  dplyr::group_by(pseudonymPatientNum) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(pseudonymPatientNum, startDate)%>%
  dplyr::rename(ddn_mere = startDate)

mothers <- mothers %>%
  dplyr::left_join(ddn, by = "pseudonymPatientNum")  



# ---- Date Accouchement ----

date_accouchement_df <- mothers %>%
  select(pseudonymPatientNum)


## ---- Recuperation des venues des meres ----

observationFilter <- edsRApi::observation_filter_builder() %>%
  edsRApi::add_concept_path(values = c("\\Mouv\\Mouv-rootnode\\Locationlevel-encounter\\Dataelement-encounter_type\\")) %>%
  edsRApi::add_modifier_cd(values = c("@")) %>%
  edsRApi::build()

venues <- edsRApi::get_observations_by_observation_filter(
  conn = conn,
  observationFilter = observationFilter,
  size = 10
)%>%
  dplyr::group_by(pseudonymEncounterNum) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(startDate, endDate, pseudonymEncounterNum) %>%
  dplyr::rename(debut_venue = startDate, fin_venue = endDate)%>%
  dplyr::mutate(debut_venue = as.Date(debut_venue), fin_venue = as.Date(fin_venue))



## ---- Avec dates du PMSI  ----

date_pmsi <- read_excel_paths(keyword = "date_pmsi")


date_pmsi_filtered <- date_pmsi %>%
  dplyr::group_by(pseudonymEncounterNum) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(pseudonymPatientNum, startDate, pseudonymEncounterNum) %>%
  dplyr::rename(date_pmsi = startDate)%>%
  dplyr::mutate(date_pmsi = as.Date(date_pmsi))


date_accouchement_df <- date_accouchement_df %>%
  dplyr::left_join(date_pmsi_filtered, by = c("pseudonymPatientNum"))


## ---- Avec Dxcare date naissance bébé pour info manquante----

date_naissance_bebe <- read_excel_paths(keyword = "date_naissance_bebe")

date_naissance_bebe_filtered <- date_naissance_bebe %>%
  dplyr::left_join(venues, by = c("pseudonymEncounterNum"))%>%
  dplyr::rename(date_naissance_bebe = startDate)%>%
  dplyr::mutate(date_naissance_bebe = as.Date(date_naissance_bebe))%>%
  dplyr::filter(date_naissance_bebe >= debut_venue & date_naissance_bebe <= fin_venue)%>%
  dplyr::group_by(pseudonymPatientNum, date_naissance_bebe) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(pseudonymPatientNum, date_naissance_bebe, pseudonymEncounterNum) 

date_accouchement_df <- date_accouchement_df %>%
  dplyr::left_join(date_naissance_bebe_filtered, by = c("pseudonymPatientNum", "pseudonymEncounterNum"))%>%
  dplyr::mutate(date_accouchement = dplyr::coalesce(date_naissance_bebe, date_pmsi))%>%
  dplyr::select(pseudonymPatientNum, pseudonymEncounterNum, date_accouchement)

date_accouchement_df <- date_accouchement_df %>%
  dplyr::filter(!is.na(date_accouchement))%>%
  dplyr::bind_rows(date_naissance_bebe_filtered %>% filter(!pseudonymPatientNum %in% date_accouchement_df$pseudonymPatientNum)%>% rename(date_accouchement = date_naissance_bebe))%>%
  dplyr::left_join(venues, by = c("pseudonymEncounterNum"))

mothers <- mothers %>%
  dplyr::left_join(date_accouchement_df, by = c("pseudonymPatientNum"))%>%
  dplyr::filter(!is.na(date_accouchement)) %>%
  dplyr::filter(date_accouchement > as.Date("2015-11-01") & date_accouchement < as.Date("2018-06-30"))



# ---- Date début de grossesse DxCare ----
# Récupere dates début grossesse depuis  DxCare
date_grossesse <- read_excel_paths(keyword = "date_grossesse")


# Filtre dates début grossesse
date_grossesse_filtered <- date_grossesse%>%
  dplyr::group_by(pseudonymPatientNum, startDate)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()%>%
  dplyr::select(pseudonymPatientNum, startDate)%>%
  dplyr::rename(date_grossesse = startDate)


# Valide dates grossesse fenêtre
date_grossesse_valide <- mothers %>%
  dplyr::select(pseudonymPatientNum, date_accouchement)%>%
  dplyr::left_join(date_grossesse_filtered, by = "pseudonymPatientNum")%>%
  dplyr::filter(date_grossesse >= date_accouchement - 294 & date_grossesse <= date_accouchement)%>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()%>%
  dplyr::select(date_grossesse, date_accouchement, pseudonymPatientNum)

mothers <- mothers %>%
  dplyr::left_join(date_grossesse_valide, by = c("pseudonymPatientNum","date_accouchement"))


# ---- Terme ----

terme <- read_excel_paths(keyword = "terme") 


terme_filtered <- terme%>%
  dplyr::mutate(terme = substr(observationBlob, 1, 2))%>%
  dplyr::select(terme, pseudonymPatientNum, startDate)


terme_filtered2 <- mothers %>% # Jointure avec données mères et calcul date début grossesse
  dplyr::left_join(terme_filtered, by = "pseudonymPatientNum")%>%
  dplyr::filter(startDate >= (date_accouchement - lubridate::weeks(4)) & startDate <= (date_accouchement + lubridate::weeks(1)))%>%
  dplyr::mutate(date_grossesse_terme = date_accouchement - lubridate::weeks(terme))%>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()%>%
  dplyr::select(pseudonymPatientNum, date_accouchement, date_grossesse_terme, terme)


mothers <- mothers %>% # Mise à jour table mothers
  dplyr::left_join(terme_filtered2, by = c("pseudonymPatientNum", "date_accouchement"))%>%
  dplyr::mutate(date_grossesse = ifelse(is.na(date_grossesse), as.Date(date_grossesse_terme), as.Date(date_grossesse)))%>%
  dplyr::select(-date_grossesse_terme)%>%
  dplyr::mutate(date_grossesse = lubridate::as_date(date_grossesse))

mothers <- mothers %>% # Calcul  du terme en semaines d'aménorrhée
  dplyr::mutate(terme_calcul = round((as.numeric(difftime(date_accouchement, date_grossesse), units = "days") + 14)/ 7 ))%>%
  dplyr::select(-terme)%>%
  dplyr::rename(terme = terme_calcul)



# ---- Age mères à l'accouchement----

mothers <- mothers %>%# Calcul âge
  dplyr::mutate(age_mere = round((as.numeric(difftime(date_accouchement, ddn_mere), units = "days") )/ 365.25))


# ---- Poids debut de grossesse ----

poids_grossesse <- read_excel_paths(keyword = "poids_grossesse")

poids_grossesse_filtered <- poids_grossesse %>%
  dplyr::select(pseudonymPatientNum, nvalNum, startDate) %>%
  dplyr::rename(poids_debut_grossesse = nvalNum, date_consult = startDate) %>%
  dplyr::mutate(date_consult = as.Date(date_consult)) %>%
  dplyr::left_join(mothers %>% select(pseudonymPatientNum, date_accouchement, date_grossesse)) %>%
  dplyr::filter(date_consult >= date_grossesse-7 & date_consult <= date_accouchement+7) %>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement) %>%
  dplyr::arrange(desc(poids_debut_grossesse), .by_group = TRUE) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(-date_consult)

mothers <- mothers %>%
  dplyr::left_join(poids_grossesse_filtered, by = c("pseudonymPatientNum", "date_accouchement", "date_grossesse"))


# ---- Taille des mères ----

taille <- read_excel_paths(keyword = "taille")

taille_filtered <- taille %>%
  dplyr::select(pseudonymPatientNum, startDate, nvalNum) %>%
  dplyr::rename(taille_mere = nvalNum, date_consult = startDate) %>%
  dplyr::mutate(date_consult = as.Date(date_consult)) %>%
  dplyr::left_join(mothers %>% select(pseudonymPatientNum, date_accouchement, date_grossesse)) %>%
  dplyr::filter(date_consult >= date_grossesse & date_consult <= date_accouchement) %>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement) %>%
  dplyr::arrange(desc(taille_mere), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(-date_consult)

mothers <- mothers %>%
  dplyr::left_join(taille_filtered, by = c("pseudonymPatientNum", "date_accouchement", "date_grossesse"))


# ---- Calcul IMC ---- 
# = Poids(kg) / (taille (m)²)
mothers <- mothers %>%
  dplyr::mutate(imc_debut_grossesse = round(as.numeric(poids_debut_grossesse) / ((as.numeric(taille_mere)/100)^2),1))


# ---- Prise poids grossesse ----

prise_poids <- read_excel_paths(keyword = "prise_poids")

# Formatage prise de poids
prise_poids_filtered <- prise_poids %>%
  dplyr::select(pseudonymPatientNum, startDate, nvalNum) %>%
  dplyr::rename(prise_poids = nvalNum, date_consult = startDate)%>%
  dplyr::mutate(date_consult = as.Date(date_consult))%>%
  dplyr::left_join(mothers %>% select(pseudonymPatientNum, date_accouchement, date_grossesse))%>%
  dplyr::filter(date_consult >= date_grossesse - 30 & date_consult <= date_accouchement + 30)%>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement)%>%
  dplyr::arrange(desc(prise_poids), .by_group = TRUE)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()%>%
  dplyr::select(-date_consult)


# Dernière consultation
mothers <- mothers %>%
  dplyr::left_join(prise_poids_filtered, by = c("pseudonymPatientNum", "date_accouchement", "date_grossesse"))

# ----Tabagisme grossesse ----
## Tabagisme grossesse Z72.0 et Tabagisme foetus P04.2 ----
# Chemins DxCare pour tabagisme


tabac_pmsi <- read_excel_paths(keyword = "tabac_pmsi")


# Filtre tabac
tabac_pmsi_filtered <- tabac_pmsi %>%
  dplyr::filter(grepl("tabac|tabagisme|nicotine", labelPermissible, ignore.case = TRUE))%>%
  dplyr::select(pseudonymPatientNum, startDate, labelPermissible)

tabac_pmsi_filtered <- mothers %>%
  dplyr::left_join(tabac_pmsi_filtered, by = "pseudonymPatientNum")%>%
  dplyr::filter(startDate >= (date_grossesse - lubridate::weeks(4)) & startDate <= (date_accouchement + lubridate::weeks(4)))%>%
  dplyr::rename (tabac_pmsi = labelPermissible)%>%
  dplyr::select(pseudonymPatientNum, date_accouchement,tabac_pmsi)%>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()

# Jointure finale
mothers <- mothers %>%
  dplyr::left_join(tabac_pmsi_filtered,  by = c("pseudonymPatientNum", "date_accouchement"))





## ---- Addiction Tabac----
# Données tabac DxCare
addiction <- read_excel_paths(keyword = "addiction")

# Traitement données tabac
addiction_filtre <- addiction %>%
  dplyr::group_by(pseudonymPatientNum, observationBlob)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()%>%
  # Nettoyage texte
  dplyr::mutate(observationBlob2 = tolower(gsub(" ", "", observationBlob)))%>%
  dplyr::mutate(observationBlob2 =iconv(observationBlob2, 
                                        from = "UTF-8", 
                                        to = "ASCII//TRANSLIT"))%>% 
  dplyr::left_join(mothers %>% select(pseudonymPatientNum, date_grossesse, date_accouchement,date_accouchement), by = "pseudonymPatientNum")%>%
  dplyr::filter(startDate >= (date_grossesse - lubridate::weeks(4)) & startDate <= (date_accouchement + lubridate::weeks(4)))%>%
  # Filtres consommation
  dplyr::filter(grepl("parjour|/j|paquet|cig", observationBlob2, ignore.case = TRUE))%>%
  dplyr::filter(!grepl("arret|sevre|stop", observationBlob2, ignore.case = TRUE))%>%
  dplyr::select(observationBlob, pseudonymPatientNum, date_accouchement)%>%
  dplyr::rename(addiction_tabac = observationBlob)%>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()


mothers <- mothers %>%
  dplyr::left_join(addiction_filtre,  by = c("pseudonymPatientNum", "date_accouchement"))


mothers <- mothers %>%
  mutate(tabac_oui = coalesce(tabac_pmsi, addiction_tabac))


# ---- Césarienne ----
# Données PMSI césarienne
pmsi_cesarienne <- read_excel_paths(keyword = "pmsi_cesarienne")

pmsi_cesarienne <- pmsi_cesarienne %>%
  dplyr::select(pseudonymPatientNum, labelPermissible, startDate)


pmsi_cesarienne_filtered <- mothers %>%
  dplyr::left_join(pmsi_cesarienne, by = "pseudonymPatientNum") %>%
  dplyr::filter(startDate >= date_accouchement -15 & startDate <= date_accouchement + 15)%>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::rename(mode_accouchement = labelPermissible)%>%
  dplyr::select(pseudonymPatientNum, date_accouchement, mode_accouchement)

mothers <- mothers %>%
  dplyr::left_join(pmsi_cesarienne_filtered,  by = c("pseudonymPatientNum", "date_accouchement"))


# ---- Allaitement ----

## ---- Artificiel ----

artificiel <- read_excel_paths(keyword = "artificiel")


artificiel_filtered <- artificiel %>%
  dplyr::select(pseudonymPatientNum, startDate, labelPermissible) # Nettoyage des données


artificiel_filtered <- mothers %>% # Filtrage temporel
  dplyr::left_join(artificiel_filtered, by = "pseudonymPatientNum") %>%
  dplyr::filter(startDate >= (date_grossesse - lubridate::weeks(4)) & startDate <= (date_accouchement + lubridate::weeks(4))) %>%
  dplyr::select(pseudonymPatientNum, date_accouchement, labelPermissible) %>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::rename(allaitement_artificiel = labelPermissible)


mothers <- mothers %>% # Ajout à mothers
  dplyr::left_join(artificiel_filtered, by = c("pseudonymPatientNum", "date_accouchement"))


## ---- Maternel ----

maternel <<- read_excel_paths(keyword = "maternel")


maternel <- maternel %>%
  dplyr::select(pseudonymPatientNum, startDate, labelPermissible)# Nettoyage des données

# Lien table mères
allaitement_maternel <- mothers %>%
  dplyr::left_join(maternel, by = "pseudonymPatientNum") %>%
  dplyr::filter(startDate >= (date_grossesse - lubridate::weeks(4)) & startDate <= (date_accouchement + lubridate::weeks(4))) %>%
  dplyr::rename(allaitement_maternel = labelPermissible) %>%
  dplyr::select(pseudonymPatientNum, date_accouchement, allaitement_maternel)

# Création du dataframe final avec colonnes binaires
df_wide <- allaitement_maternel %>%
  dplyr::distinct() %>%
  dplyr::mutate(present = 1) %>%
  tidyr::pivot_wider(
    names_from = allaitement_maternel,
    values_from = present,
    values_fill = 0
  ) %>%
  dplyr::mutate(somme_allaitement = rowSums(select(., -pseudonymPatientNum, -date_accouchement)))

# Ajout à la table mères
mothers <- mothers %>%
  dplyr::left_join(df_wide%>% select(pseudonymPatientNum, date_accouchement, somme_allaitement), by = c("pseudonymPatientNum", "date_accouchement"))


# Création des variables d'allaitement
mothers <- mothers %>%
  dplyr::mutate(mode_allaitement = case_when(
    somme_allaitement >= 1 ~ "Maternel",
    !is.na(allaitement_artificiel)~ "Artificiel",
    TRUE ~ NA_character_
  ))%>%
  dplyr::select(-somme_allaitement)


# ---- Précarité ----

## précarité cochée "OUI" ----
precarite_oui <- read_excel_paths(keyword = "precarite_oui")


precarite_oui_filtered <- precarite_oui %>% # Sélection et renommage des colonnes
  dplyr::select(pseudonymPatientNum, startDate, labelPermissible) %>%
  dplyr::rename(precarite_oui = labelPermissible)%>%
  dplyr::left_join(mothers %>% select(pseudonymPatientNum, date_accouchement, date_grossesse), by = c("pseudonymPatientNum"))%>%
  dplyr::filter(startDate >= date_grossesse-31 & startDate <= date_accouchement + 31)%>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()%>%
  dplyr::select(pseudonymPatientNum, date_accouchement,precarite_oui)

mothers <- mothers %>% # Jointure avec table mothers
  dplyr::left_join(precarite_oui_filtered, by = c("pseudonymPatientNum", "date_accouchement"))



## Récupération des données de précarité CIM10---- 
precarite_cim10 <- read_excel_paths(keyword = "precarite_cim10")

precarite_cim10 <- precarite_cim10 %>% # Sélection et renommage des colonnes
  dplyr::select(pseudonymPatientNum, startDate, labelPermissible) %>%
  dplyr::rename(precarite_cim10 = labelPermissible)%>%
  dplyr::left_join(mothers %>% select(pseudonymPatientNum, date_accouchement, date_grossesse), by = c("pseudonymPatientNum"))%>%
  dplyr::filter(startDate >= date_grossesse-31 & startDate <= date_accouchement + 31)%>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()%>%
  dplyr::select(pseudonymPatientNum, date_accouchement,precarite_cim10)


mothers <- mothers %>% # Jointure avec table mothers
  dplyr::left_join(precarite_cim10,  by = c("pseudonymPatientNum", "date_accouchement"))


## Récupération des données pour chaque code Z55-Z65 (difficultés psychosociales) ----

### Z55: Difficultés liées à l'éducation et l'alphabétisation ----
z55 <- read_excel_paths(keyword = "z55")

# Création d'un tibble vide si pas de données
if (is.null(z55)) {
  z55 <- dplyr::tibble(
    pseudonymPatientNum = character(),
    startDate = as.Date(character()),
    z55 = character()
  )
} else {
  z55 <- z55 %>%
    dplyr::select(pseudonymPatientNum, startDate, labelPermissible) 
}

z55 <- z55 %>%
  dplyr::select(pseudonymPatientNum, startDate, labelPermissible) %>%
  dplyr::rename(z55 = labelPermissible)%>%
  dplyr::left_join(mothers %>% select(pseudonymPatientNum, date_accouchement, date_grossesse), by = c("pseudonymPatientNum"))%>%
  dplyr::filter(startDate >= date_grossesse-31 & startDate <= date_accouchement + 31)%>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()%>%
  dplyr::select(pseudonymPatientNum, date_accouchement,z55)


mothers <- mothers %>%
  dplyr::left_join(z55, by = c("pseudonymPatientNum", "date_accouchement"))

### Z56: Difficultés liées à l'emploi ----
z56 <- read_excel_paths(keyword = "z56")


if (is.null(z56)) {
  z56 <- dplyr::tibble(
    pseudonymPatientNum = character(),
    startDate = as.Date(character()),
    z56 = character()
  )
} else {
  z56 <- z56 %>%
    dplyr::select(pseudonymPatientNum, startDate, labelPermissible) 
}
z56 <- z56 %>%
  dplyr::select(pseudonymPatientNum, startDate, labelPermissible) %>%
  dplyr::rename(z56 = labelPermissible)%>%
  dplyr::left_join(mothers %>% select(pseudonymPatientNum, date_accouchement, date_grossesse), by = c("pseudonymPatientNum"))%>%
  dplyr::filter(startDate >= date_grossesse-31 & startDate <= date_accouchement + 31)%>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()%>%
  dplyr::select(pseudonymPatientNum, date_accouchement,z56)


mothers <- mothers %>%
  dplyr::left_join(z56, by = c("pseudonymPatientNum", "date_accouchement"))

### Z57: Exposition professionnelle aux risques ----
z57 <- read_excel_paths(keyword = "z57")


if (is.null(z57)) {
  z57 <- dplyr::tibble(
    pseudonymPatientNum = character(),
    startDate = as.Date(character()),
    labelPermissible = character()
  )
} else {
  z57 <- z57 %>%
    dplyr::select(pseudonymPatientNum, startDate, labelPermissible) 
}

z57 <- z57 %>%
  dplyr::select(pseudonymPatientNum, startDate, labelPermissible) %>%
  dplyr::rename(z57 = labelPermissible)%>%
  dplyr::left_join(mothers %>% select(pseudonymPatientNum, date_accouchement, date_grossesse), by = c("pseudonymPatientNum"))%>%
  dplyr::filter(startDate >= date_grossesse-31 & startDate <= date_accouchement + 31)%>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()%>%
  dplyr::select(pseudonymPatientNum, date_accouchement,z57)


mothers <- mothers %>%
  dplyr::left_join(z57, by = c("pseudonymPatientNum", "date_accouchement"))

### Z58: Difficultés liées à l'environnement ----
z58 <- read_excel_paths(keyword = "z58")


if (is.null(z58)) {
  z58 <- dplyr::tibble(
    pseudonymPatientNum = character(),
    startDate = as.Date(character()),
    z58 = character()
  )
} else {
  z58 <- z58 %>%
    dplyr::select(pseudonymPatientNum, startDate, labelPermissible) 
}

z58 <- z58 %>%
  dplyr::select(pseudonymPatientNum, startDate, labelPermissible) %>%
  dplyr::rename(z58 = labelPermissible)%>%
  dplyr::left_join(mothers %>% select(pseudonymPatientNum, date_accouchement, date_grossesse), by = c("pseudonymPatientNum"))%>%
  dplyr::filter(startDate >= date_grossesse-31 & startDate <= date_accouchement + 31)%>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()%>%
  dplyr::select(pseudonymPatientNum, date_accouchement,z58)


mothers <- mothers %>%
  dplyr::left_join(z58, by = c("pseudonymPatientNum", "date_accouchement"))

### Z59: Difficultés liées au logement ----
z59 <- read_excel_paths(keyword = "z59")


if (is.null(z59)) {
  z59 <- dplyr::tibble(
    pseudonymPatientNum = character(),
    startDate = as.Date(character()),
    z59 = character()
  )
} else {
  z59 <- z59 %>%
    dplyr::select(pseudonymPatientNum, startDate, labelPermissible) 
}

z59 <- z59 %>%
  dplyr::select(pseudonymPatientNum, startDate, labelPermissible) %>%
  dplyr::rename(z59 = labelPermissible)%>%
  dplyr::left_join(mothers %>% select(pseudonymPatientNum, date_accouchement, date_grossesse), by = c("pseudonymPatientNum"))%>%
  dplyr::filter(startDate >= date_grossesse-31 & startDate <= date_accouchement + 31)%>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()%>%
  dplyr::select(pseudonymPatientNum, date_accouchement,z59)


mothers <- mothers %>%
  dplyr::left_join(z59, by = c("pseudonymPatientNum", "date_accouchement"))

### Z60: Difficultés sociales ----
z60 <- read_excel_paths(keyword = "z60")

if (is.null(z60)) {
  z60 <- dplyr::tibble(
    pseudonymPatientNum = character(),
    startDate = as.Date(character()),
    z60 = character()
  )
} else {
  z60 <- z60 %>%
    dplyr::select(pseudonymPatientNum, startDate, labelPermissible) 
}

z60 <- z60 %>%
  dplyr::select(pseudonymPatientNum, startDate, labelPermissible) %>%
  dplyr::rename(z60 = labelPermissible)%>%
  dplyr::left_join(mothers %>% select(pseudonymPatientNum, date_accouchement, date_grossesse), by = c("pseudonymPatientNum"))%>%
  dplyr::filter(startDate >= date_grossesse-31 & startDate <= date_accouchement + 31)%>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()%>%
  dplyr::select(pseudonymPatientNum, date_accouchement,z60)


mothers <- mothers %>%
  dplyr::left_join(z60, by = c("pseudonymPatientNum", "date_accouchement"))

### Z61: Difficultés liées à l'enfance ----
z61 <- read_excel_paths(keyword = "z61")


if (is.null(z61)) {
  z61 <- dplyr::tibble(
    pseudonymPatientNum = character(),
    startDate = as.Date(character()),
    labelPermissible = character(),
    z61 = character()
  )
} else {
  z61 <- z61 %>%
    dplyr::select(pseudonymPatientNum, startDate, labelPermissible) 
}

z61 <- z61 %>%
  dplyr::select(pseudonymPatientNum, startDate, labelPermissible) %>%
  dplyr::rename(z61 = labelPermissible)%>%
  dplyr::left_join(mothers %>% select(pseudonymPatientNum, date_accouchement, date_grossesse), by = c("pseudonymPatientNum"))%>%
  dplyr::filter(startDate >= date_grossesse-31 & startDate <= date_accouchement + 31)%>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()%>%
  dplyr::select(pseudonymPatientNum, date_accouchement,z61)


mothers <- mothers %>%
  dplyr::left_join(z61, by = c("pseudonymPatientNum", "date_accouchement"))

### Z62: Autres difficultés d'éducation ----
z62 <- read_excel_paths(keyword = "z62")

if (is.null(z62)) {
  z62 <- dplyr::tibble(
    pseudonymPatientNum = character(),
    startDate = as.Date(character()),
    labelPermissible = character(),
    z62 = character()
  )
} else {
  z62 <- z62 %>%
    dplyr::select(pseudonymPatientNum, startDate, labelPermissible) 
}

z62 <- z62 %>%
  dplyr::select(pseudonymPatientNum, startDate, labelPermissible) %>%
  dplyr::rename(z62 = labelPermissible)%>%
  dplyr::left_join(mothers %>% select(pseudonymPatientNum, date_accouchement, date_grossesse), by = c("pseudonymPatientNum"))%>%
  dplyr::filter(startDate >= date_grossesse-31 & startDate <= date_accouchement + 31)%>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()%>%
  dplyr::select(pseudonymPatientNum, date_accouchement,z62)


mothers <- mothers %>%
  dplyr::left_join(z62, by = c("pseudonymPatientNum", "date_accouchement"))

### Z63: Difficultés familiales ----
z63 <- read_excel_paths(keyword = "z63")


if (is.null(z63)) {
  z63 <- dplyr::tibble(
    pseudonymPatientNum = character(),
    startDate = as.Date(character()),
    labelPermissible = character(),
    z63 = character()
  )
} else {
  z63 <- z63 %>%
    dplyr::select(pseudonymPatientNum, startDate, labelPermissible) 
}

z63 <- z63 %>%
  dplyr::select(pseudonymPatientNum, startDate, labelPermissible) %>%
  dplyr::rename(z63 = labelPermissible)%>%
  dplyr::left_join(mothers %>% select(pseudonymPatientNum, date_accouchement, date_grossesse), by = c("pseudonymPatientNum"))%>%
  dplyr::filter(startDate >= date_grossesse-31 & startDate <= date_accouchement + 31)%>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()%>%
  dplyr::select(pseudonymPatientNum, date_accouchement,z63)


mothers <- mothers %>%
  dplyr::left_join(z63, by = c("pseudonymPatientNum", "date_accouchement"))

### Z64: Difficultés psychosociales ----
z64 <- read_excel_paths(keyword = "z64")


if (is.null(z64)) {
  z64 <- dplyr::tibble(
    pseudonymPatientNum = character(),
    startDate = as.Date(character()),
    labelPermissible = character(),
    z64 = character()
  )
} else {
  z64 <- z64 %>%
    dplyr::select(pseudonymPatientNum, startDate, labelPermissible) 
}

z64 <- z64 %>%
  dplyr::select(pseudonymPatientNum, startDate, labelPermissible) %>%
  dplyr::rename(z64 = labelPermissible)%>%
  dplyr::left_join(mothers %>% select(pseudonymPatientNum, date_accouchement, date_grossesse), by = c("pseudonymPatientNum"))%>%
  dplyr::filter(startDate >= date_grossesse-31 & startDate <= date_accouchement + 31)%>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()%>%
  dplyr::select(pseudonymPatientNum, date_accouchement,z64)


mothers <- mothers %>%
  dplyr::left_join(z64, by = c("pseudonymPatientNum", "date_accouchement"))

###  Z65: Autres difficultés psychosociales ----
z65 <- read_excel_paths(keyword = "z65")


if (is.null(z65)) {
  z65 <- dplyr::tibble(
    pseudonymPatientNum = character(),
    startDate = as.Date(character()),
    labelPermissible = character(),
    z65 = character()
  )
} else {
  z65 <- z65 %>%
    dplyr::select(pseudonymPatientNum, startDate, labelPermissible) 
}

z65 <- z65 %>%
  dplyr::select(pseudonymPatientNum, startDate, labelPermissible) %>%
  dplyr::rename(z65 = labelPermissible)%>%
  dplyr::left_join(mothers %>% select(pseudonymPatientNum, date_accouchement, date_grossesse), by = c("pseudonymPatientNum"))%>%
  dplyr::filter(startDate >= date_grossesse-31 & startDate <= date_accouchement + 31)%>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()%>%
  dplyr::select(pseudonymPatientNum, date_accouchement,z65)


mothers <- mothers %>%
  dplyr::left_join(z65, by = c("pseudonymPatientNum", "date_accouchement"))




# Sauvegarde de l'état de la table avant ajout des données de précarité
mothers_save_good_avant_precarite <- mothers

## ---- Précarité CMU ----


# Chemins des différentes valeurs de couverture sociale
CMU <- read_excel_paths(keyword = "CMU")

CMU <- CMU %>%
  dplyr::select(pseudonymPatientNum, startDate, labelPermissible) %>%
  dplyr::rename(CMU = labelPermissible)%>%
  dplyr::left_join(mothers %>% select(pseudonymPatientNum, date_accouchement, date_grossesse), by = c("pseudonymPatientNum"))%>%
  dplyr::filter(startDate >= date_grossesse-31 & startDate <= date_accouchement + 31)%>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()%>%
  dplyr::select(pseudonymPatientNum, date_accouchement,CMU)


mothers <- mothers %>%
  dplyr::left_join(CMU, by = c("pseudonymPatientNum", "date_accouchement"))


## ---- Profession mere et pere ----
# profession mere
profession_mere <- read_excel_paths(keyword = "profession_mere")

# Profession du pere
profession_pere <- read_excel_paths(keyword = "profession_pere")

# Filtrage données mère (a tester)
profession_mere_filtre <- profession_mere %>%
  dplyr::group_by(pseudonymPatientNum, observationBlob)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()%>%
  dplyr::rename(profession_mere = observationBlob)%>%
  dplyr::mutate(profession_mere2 = tolower(gsub(" ", "", profession_mere)))%>%
  dplyr::mutate(profession_mere2 =iconv(profession_mere2, 
                                        from = "UTF-8", 
                                        to = "ASCII//TRANSLIT"))%>% 
  dplyr::left_join(mothers %>% select(pseudonymPatientNum, date_grossesse, date_accouchement), by = "pseudonymPatientNum")%>%
  dplyr::filter(startDate >= (date_grossesse - lubridate::weeks(4)) & startDate <= (date_accouchement + lubridate::weeks(4)))%>%
  dplyr::filter(grepl("^sp|RSA|sansprofession|etudiant|chomage", profession_mere2, ignore.case = TRUE))%>%
  dplyr::select(profession_mere, profession_mere2, pseudonymPatientNum, date_accouchement)

# Filtrage données père
profession_pere_filtre <- profession_pere %>%
  dplyr::group_by(pseudonymPatientNum, observationBlob)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()%>%
  dplyr::rename(profession_pere = observationBlob)%>%
  dplyr::mutate(profession_pere2 = tolower(gsub(" ", "", profession_pere)))%>%
  dplyr::mutate(profession_pere2 =iconv(profession_pere2, 
                                        from = "UTF-8", 
                                        to = "ASCII//TRANSLIT"))%>% 
  dplyr::left_join(mothers %>% select(pseudonymPatientNum, date_grossesse, date_accouchement,date_accouchement), by = "pseudonymPatientNum")%>%
  dplyr::filter(startDate >= (date_grossesse - lubridate::weeks(4)) & startDate <= (date_accouchement + lubridate::weeks(4)))%>%
  dplyr::select(profession_pere, profession_pere2, pseudonymPatientNum, date_accouchement)

# Fusion et précarité
profession_filtre <- profession_mere_filtre %>%
  dplyr::left_join(profession_pere_filtre, by = c("pseudonymPatientNum", "date_accouchement"))%>%
  dplyr::filter(grepl("^sp|RSA|sansprofession|etudiant|chomage", profession_pere2, ignore.case = TRUE))%>%
  dplyr::mutate(precarite_profession = "precarite profession")%>%
  dplyr::select(pseudonymPatientNum, date_accouchement,precarite_profession)%>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()

# Mise à jour mothers
mothers <- mothers %>%
  dplyr::left_join(profession_filtre,  by = c("pseudonymPatientNum", "date_accouchement"))%>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()

## Fusion precarite ----
mothers <- mothers %>%
dplyr::mutate(precarite_tous_critere = coalesce(precarite_oui, precarite_cim10, z55, 
                                                z56, z57, z58, z59, z60, z61, z62, z63, z64, 
                                                z65, precarite_profession,CMU))

# ---- Diabete gestationnel ----

# ## Diabète préalable ----
# diabete_prealable <- edsRApi::get_observations_by_observation_filter(
#   conn = conn,
#   observationFilter = edsRApi::observation_filter_builder() %>%
#     edsRApi::add_concept_path(values = c("\\Diag\\Diag-rootnode\\Concept-icd10\\Concept-iv\\Concept-e10-e14\\")) %>%
#     edsRApi::add_modifier_cd(values = c("@")) %>%
#     edsRApi::build(),
#   size = 100
# ) %>%
#   dplyr::select(pseudonymPatientNum, startDate, labelPermissible) %>%
#   dplyr::rename(diabete_prealable = labelPermissible)
# 
# 
# mothers <- mothers %>%
#   dplyr::left_join(diabete_prealable, by = "pseudonymPatientNum")%>%
#   dplyr::group_by(pseudonymPatientNum, date_accouchement)%>%
#   dplyr::slice(1)%>%
#   dplyr::ungroup()%>%
#   dplyr::select(-startDate)

## Diabète gestationnel sans diabète préalable ----
diabete_gesta_sans_prealable <- read_excel_paths(keyword = "diabete_gesta_sans_prealable")

diabete_gesta_sans_prealable <- diabete_gesta_sans_prealable %>%
  dplyr::select(pseudonymPatientNum, startDate, labelPermissible) %>%
  dplyr::rename(diabete_gesta_sans_prealable = labelPermissible)%>%
  dplyr::left_join(mothers %>% select(pseudonymPatientNum, date_accouchement, date_grossesse), by = c("pseudonymPatientNum"))%>%
  dplyr::filter(startDate >= date_grossesse-31 & startDate <= date_accouchement + 31)%>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()%>%
  dplyr::select(pseudonymPatientNum, date_accouchement, diabete_gesta_sans_prealable)

mothers <- mothers %>%
  dplyr::left_join(diabete_gesta_sans_prealable, by = c("pseudonymPatientNum", "date_accouchement"))


## Diabete gestastionnel dans Pathologie de la grossesse
pathologie_grossesse <- read_excel_paths(keyword = "pathologie_grossesse")


pathologie_grossesse <- pathologie_grossesse %>%
  dplyr::select(pseudonymPatientNum, startDate, labelPermissible) %>%
  dplyr::rename(pathologie_grossesse = labelPermissible)%>%
  dplyr::left_join(mothers %>% select(pseudonymPatientNum, date_accouchement, date_grossesse), by = c("pseudonymPatientNum"))%>%
  dplyr::filter(startDate >= date_grossesse-31 & startDate <= date_accouchement + 31)%>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()%>%
  dplyr::mutate(pathologie_grossesse2 = tolower(gsub(" ", "", pathologie_grossesse)))%>%
  dplyr::mutate(pathologie_grossesse2 =iconv(pathologie_grossesse2, 
                                             from = "UTF-8", 
                                             to = "ASCII//TRANSLIT"))%>% 
  dplyr::filter(grepl("diabete", pathologie_grossesse2, ignore.case = TRUE))%>%
  dplyr::select(pseudonymPatientNum, date_accouchement, pathologie_grossesse)


mothers <- mothers %>%
  dplyr::left_join(pathologie_grossesse, by = c("pseudonymPatientNum", "date_accouchement"))



## Glycémie (HGPO) ----

### HGPO H0----

glycemie_hgpo_0 <- read_excel_paths(keyword = "glycemie_hgpo_h0")

glycemie_hgpo_0_filtered <- glycemie_hgpo_0 %>%
  dplyr::left_join(mothers %>% select(pseudonymPatientNum, date_accouchement, date_grossesse), by = c("pseudonymPatientNum"))%>%
  dplyr::filter(startDate >= date_grossesse-31 & startDate <= date_accouchement + 31)%>%
  dplyr::filter(nvalNum > 0.92)%>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::rename (hgpo_h0_positif = labelPermissible) %>%
  dplyr::select(pseudonymPatientNum, date_accouchement, hgpo_h0_positif)

mothers <- mothers %>%
  dplyr::left_join(glycemie_hgpo_0_filtered, by = c("pseudonymPatientNum", "date_accouchement"))



### HGPO H1----

glycemie_hgpo_h1 <- read_excel_paths(keyword = "glycemie_hgpo_h1")

glycemie_hgpo_h1_filtered <- glycemie_hgpo_h1 %>%
  dplyr::left_join(mothers %>% select(pseudonymPatientNum, date_accouchement, date_grossesse), by = c("pseudonymPatientNum"))%>%
  dplyr::filter(startDate >= date_grossesse-31 & startDate <= date_accouchement + 31)%>%
  dplyr::filter(nvalNum > 1.8)%>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::rename (hgpo_h1_positif = labelPermissible) %>%
  dplyr::select(pseudonymPatientNum, date_accouchement, hgpo_h1_positif)

mothers <- mothers %>%
  dplyr::left_join(glycemie_hgpo_h1_filtered, by = c("pseudonymPatientNum", "date_accouchement"))

### HGPO H2----

glycemie_hgpo_h2 <- read_excel_paths(keyword = "glycemie_hgpo_h2")

glycemie_hgpo_h2_filtered <- glycemie_hgpo_h2 %>%
  dplyr::left_join(mothers %>% select(pseudonymPatientNum, date_accouchement, date_grossesse), by = c("pseudonymPatientNum"))%>%
  dplyr::filter(startDate >= date_grossesse-31 & startDate <= date_accouchement + 31)%>%
  dplyr::filter(nvalNum > 1.53)%>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::rename (hgpo_h2_positif = labelPermissible) %>%
  dplyr::select(pseudonymPatientNum, date_accouchement, hgpo_h2_positif)

mothers <- mothers %>%
  dplyr::left_join(glycemie_hgpo_h2_filtered, by = c("pseudonymPatientNum", "date_accouchement"))

### HGPO com----

glycemie_hgpo_com <- read_excel_paths(keyword = "glycemie_hgpo_com")

glycemie_hgpo_com_filtered <- glycemie_hgpo_com %>%
  dplyr::left_join(mothers %>% select(pseudonymPatientNum, date_accouchement, date_grossesse), by = c("pseudonymPatientNum"))%>%
  dplyr::filter(startDate >= date_grossesse-31 & startDate <= date_accouchement + 31)%>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement) %>%
  dplyr::arrange(desc(startDate), .by_group = TRUE)%>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(observationBlob2 = tolower(gsub(" ", "", observationBlob)))%>%
  dplyr::mutate(observationBlob2 =iconv(observationBlob2, 
                                             from = "UTF-8", 
                                             to = "ASCII//TRANSLIT"))%>% 
  dplyr::filter(grepl("diabete|posit|patho", observationBlob2, ignore.case = TRUE))%>%
  dplyr::rename (hgpo_com_positif = observationBlob) %>%
  dplyr::select(pseudonymPatientNum, date_accouchement, hgpo_com_positif)


mothers <- mothers %>%
  dplyr::left_join(glycemie_hgpo_com_filtered, by = c("pseudonymPatientNum", "date_accouchement"))

### SYnthese diabete gesta
mothers <- mothers %>%
  dplyr::mutate(
    diabete_gesta = coalesce(
      mothers$diabete_gesta_sans_prealable,
      mothers$pathologie_grossesse,
      mothers$hgpo_h0_positif,
      mothers$hgpo_h1_positif,
      mothers$hgpo_h2_positif,
      mothers$hgpo_com_positif
    )
  )

# ---- Poids du bébé à la naissance ----
poids_naissance <- read_excel_paths(keyword = "poids_naissance")


poids_naissance_filtered <- poids_naissance %>%
 dplyr::select(pseudonymPatientNum, nvalNum, startDate)%>%
 dplyr::rename(poids_naissance = nvalNum, poids_naissance_date = startDate) %>%
 dplyr::left_join(mothers %>% select(pseudonymPatientNum, date_accouchement, date_grossesse), by = c("pseudonymPatientNum"))%>%
 dplyr::filter(as.Date(poids_naissance_date) >= date_grossesse-31 & as.Date(poids_naissance_date) <= date_accouchement + 31)%>%
 dplyr::group_by(pseudonymPatientNum, date_accouchement)%>%
 dplyr::slice(1)%>%
 dplyr::ungroup()%>%
 dplyr::select(pseudonymPatientNum, date_accouchement,poids_naissance)
 
 
mothers <- mothers %>%
 dplyr::left_join(poids_naissance_filtered, by = c("pseudonymPatientNum", "date_accouchement"))
 

# ---- Sexe du bébé ----
sexe_bebe <- read_excel_paths(keyword = "sexe_bebe")

sexe_bebe_filtered <- sexe_bebe %>%
  dplyr::select(pseudonymPatientNum, labelPermissible, startDate)%>%
  dplyr::rename(sexe_bebe_col = labelPermissible, sexe_bebe_date = startDate)%>%
  dplyr::left_join(mothers %>% select(pseudonymPatientNum, date_accouchement, date_grossesse), by = c("pseudonymPatientNum"))%>%
  dplyr::filter(as.Date(sexe_bebe_date) >= date_grossesse-31 & as.Date(sexe_bebe_date) <= date_accouchement + 31)%>%
  dplyr::group_by(pseudonymPatientNum, date_accouchement)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()%>%
  dplyr::select(pseudonymPatientNum, date_accouchement,sexe_bebe_col)

mothers <- mothers %>%
  dplyr::left_join(sexe_bebe_filtered, by = c("pseudonymPatientNum", "date_accouchement"))


# ---- Sauvegarde ----
save(list = ls(), file = "Environnement_final_Obesity_230525_Amelie.RData")
