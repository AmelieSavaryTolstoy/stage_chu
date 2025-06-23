# ---- Libraries ----
library(edsRApi)
library(dplyr)
library(knitr)
library(questionr)
library(lubridate)
library(ggplot2)
library(childsds)
library(survival)
library(survminer)

devtools::install_git("https://gitlabeds.chu-bordeaux.fr/iampublic/edsrapi.git")

# ---- ID du projet ----
project_id = "XXXXXXX"

# ---- Connexion aux API centrées sur le projet ----
conn <- edsRApi::connect("XXXXXXX", project_id)

# ---- Tableau vierge, sexe, ddn ----
observationFilter <- edsRApi::observation_filter_builder() %>%
  edsRApi::add_concept_path(values = c("\\Demographic\\Demographic-rootnode\\Dataelement-sex\\")) %>%
  edsRApi::add_modifier_cd(values = c("@")) %>%
  edsRApi::build()

sexe_bebe <- edsRApi::get_observations_by_observation_filter(
  conn = conn,
  observationFilter = observationFilter,
  size = 100
)

children <- sexe_bebe %>%
  dplyr::select(pseudonymPatientNum,labelPermissible,startDate)%>%
  dplyr::group_by(pseudonymPatientNum)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()%>%
  dplyr::arrange(pseudonymPatientNum)%>%
  dplyr::rename(sexe_bebe = labelPermissible, ddn_bebe = startDate)

sexe_ddn_bebe <- children

# ---- Taille ---- 

## ---- DxCare médicaux  taille ----
## Recuperation de tous les concept cd des formulaires Dxcare
observationFilter <- edsRApi::observation_filter_builder() %>%
  edsRApi::add_concept_path(values = c("\\Dxcare_form\\Dxcare_form-rootnode\\Profession-1\\")) %>%
  edsRApi::add_modifier_cd(values = c("@")) %>%
  edsRApi::build()

dxcare_dr_taille <- edsRApi::get_observations_by_observation_filter(
  conn = conn,
  observationFilter = observationFilter,
  size = 20
)

dxcare_dr_taille <- dxcare_dr_taille %>%
  filter(grepl("taille", labelDataElement, ignore.case = TRUE))%>%
  filter(!grepl("Gain|Type|aiguille|mère|père|assis|debout|cible|naissance|tour", labelDataElement, ignore.case = TRUE)) %>%
  dplyr::group_by(conceptCd)%>%
  slice(1)%>%
  ungroup()

concept_cd_dr_taille <- dxcare_dr_taille$conceptCd

## ---- DxCare IDE taille ----
## Recuperation de tous les concept cd des formulaires Dxcare IDE
observationFilter <- edsRApi::observation_filter_builder() %>%
  edsRApi::add_concept_path(values = c("\\Dxcare_form\\Dxcare_form-rootnode\\Profession-2\\")) %>%
  edsRApi::add_modifier_cd(values = c("@")) %>%
  edsRApi::build()

dxcare_ide_taille <- edsRApi::get_observations_by_observation_filter(
  conn = conn,
  observationFilter = observationFilter,
  size = 20
)

dxcare_ide_taille <- dxcare_ide_taille %>%
  filter(grepl("taille", labelDataElement, ignore.case = TRUE))%>%
  filter(!grepl("Gain|Type|aiguille|mère|père|assis|debout|cible|naissance|tour|lame|masque|sonde|VVP", labelDataElement, ignore.case = TRUE)) %>%
  dplyr::group_by(conceptCd)%>%
  slice(1)%>%
  ungroup()

concept_cd_ide_taille <- dxcare_ide_taille$conceptCd

concept_cd_taille <- c(concept_cd_dr_taille, concept_cd_ide_taille)

## ---- Liste concept cd pour la taille ----
cat(paste0('"', concept_cd_taille, '"', collapse = ",\n"))

## ---- Extraction données taille ---- 
observationFilter_taille <- edsRApi::observation_filter_builder() %>%
  edsRApi::add_concept_cd(values = concept_cd_taille) %>%
  edsRApi::build()

taille_obs <- edsRApi::get_observations_by_observation_filter(
  conn = conn,
  observationFilter = observationFilter_taille,
  size = 20
)

taille_enfant <- taille_obs %>%
  filter(grepl("Taille", labelDataElement, ignore.case = TRUE)) %>%
  select(pseudonymPatientNum, pseudonymEncounterNum, startDate, nvalNum) %>%
  mutate(nvalNum = as.numeric(nvalNum)) %>%
  mutate(nvalNum = ifelse(nvalNum < 2, nvalNum * 100, nvalNum)) %>%
  group_by(pseudonymPatientNum, nvalNum) %>%
  arrange(startDate, .by_group = TRUE)%>%
  slice(1)%>%
  ungroup() %>%
  rename(taille_enfant = nvalNum, startDate_taille = startDate)

# ---- Poids ----

## ---- DxCare médicaux poids ----
## Recuperation de tous les concept cd des formulaires Dxcare medicaux
observationFilter <- edsRApi::observation_filter_builder() %>%
  edsRApi::add_concept_path(values = c("\\Dxcare_form\\Dxcare_form-rootnode\\Profession-1\\")) %>%
  edsRApi::add_modifier_cd(values = c("@")) %>%
  edsRApi::build()

dxcare_dr_poids <- edsRApi::get_observations_by_observation_filter(
  conn = conn,
  observationFilter = observationFilter,
  size = 20
)

dxcare_dr_poids <- dxcare_dr_poids %>%
  filter(grepl("poids", labelDataElement, ignore.case = TRUE))%>%
  filter(!grepl("idéal|estimé|grossesse|mère|père|naissance|corrigé|histoire|gain|dernier", labelDataElement, ignore.case = TRUE)) %>%
  dplyr::group_by(conceptCd)%>%
  slice(1)%>%
  ungroup()

concept_cd_dr_poids <- dxcare_dr_poids$conceptCd

## ---- DxCare IDE poids ----
## Recuperation de tous les concept cd des formulaires Dxcare IDE
observationFilter <- edsRApi::observation_filter_builder() %>%
  edsRApi::add_concept_path(values = c("\\Dxcare_form\\Dxcare_form-rootnode\\Profession-2\\")) %>%
  edsRApi::add_modifier_cd(values = c("@")) %>%
  edsRApi::build()

dxcare_ide_poids <- edsRApi::get_observations_by_observation_filter(
  conn = conn,
  observationFilter = observationFilter,
  size = 20
)

dxcare_ide_poids <- dxcare_ide_poids %>%
  filter(grepl("poids", labelDataElement, ignore.case = TRUE))%>%
  filter(!grepl("idéal|estimé|grossesse|mère|père|naissance|corrigé|histoire|gain|dernier|selles|tétée|diurèse|couches|urgences|domicile", labelDataElement, ignore.case = TRUE)) %>%
  dplyr::group_by(conceptCd)%>%
  slice(1)%>%
  ungroup()

concept_cd_ide_poids <- dxcare_ide_poids$conceptCd

concept_cd_poids <- c(concept_cd_dr_poids, concept_cd_ide_poids)

## ---- Liste concept cd pour le poids ----
cat(paste0('"', concept_cd_poids, '"', collapse = ",\n"))

## ---- Extraction données poids ---- 
observationFilter_poids <- edsRApi::observation_filter_builder() %>%
  edsRApi::add_concept_cd(values = concept_cd_poids) %>%
  edsRApi::build()

poids_obs <- edsRApi::get_observations_by_observation_filter(
  conn = conn,
  observationFilter = observationFilter_poids,
  size = 20
)

poids_enfant <- poids_obs %>%
  filter(grepl("Poids", labelDataElement, ignore.case = TRUE)) %>%
  select(pseudonymPatientNum, pseudonymEncounterNum,startDate, nvalNum) %>%
  mutate(nvalNum = as.numeric(nvalNum)) %>%
  mutate(nvalNum = ifelse(nvalNum > 500, nvalNum / 1000, nvalNum)) %>%
  group_by(pseudonymPatientNum, pseudonymEncounterNum) %>%
  slice_max(order_by = nvalNum, with_ties = FALSE) %>%
  ungroup() %>%
  rename(poids_enfant = nvalNum, startDate_poids = startDate)

# ---- Calcul des IMC ----
IMC_enfant <- taille_enfant %>%
  left_join(poids_enfant, by = join_by(pseudonymPatientNum))%>%
  dplyr::filter(startDate_poids >= (startDate_taille - lubridate::weeks(8)) & startDate_poids <= (startDate_taille + lubridate::weeks(8)))%>%
  left_join(children, by = join_by(pseudonymPatientNum))%>%
  select(-pseudonymEncounterNum.x, -pseudonymEncounterNum.y)%>%
  dplyr::mutate(
    age_enfant = round((
    as.numeric(difftime(startDate_poids, ddn_bebe), units = "days"))/ 365.25 ,1
  )
  )%>%
  dplyr::mutate(IMC_enfant = ifelse(!is.na(poids_enfant) & !is.na(taille_enfant), 
                      poids_enfant / (taille_enfant / 100)^2, 
                      NA_real_))%>%
  dplyr::mutate(IMC_percentile = childsds::sds(value = IMC_enfant, 
                                        age = age_enfant, 
                                        sex = sexe_bebe, 
                                        male = "Sexe masculin", 
                                        female = "Sexe féminin",
                                        ref = kro.ref, type = "perc", item = "bmi"))%>%
  # mutate(IMC_percentile = format(IMC_percentile, scientific = FALSE))%>%
  mutate(IMC_percentile = round(IMC_percentile,2))%>%
  dplyr::filter(taille_enfant > 60)%>%
  filter(IMC_enfant > 10)

IMC_enfant <- IMC_enfant%>%
  dplyr::rename(pseudonymPatientNum_bebe = pseudonymPatientNum)

children <- IMC_enfant

# ---- Lien mère-enfant ----
Projet_EXPO_1000_Lien_pseudonyme_Mère_Enfant <- readxl::read_excel("Projet EXPO 1000 - Lien pseudonyme Mère Enfant.xlsx")

children <- children %>%
  left_join(Projet_EXPO_1000_Lien_pseudonyme_Mère_Enfant, by = "pseudonymPatientNum_bebe")

# ---- Classification des patients surpoids/obèses ----
patient_non_surpoids <- children %>%
  filter(age_enfant>=3)%>%
  group_by(pseudonymPatientNum_bebe)%>%
  mutate(percentile_max = max(IMC_percentile))%>%
  ungroup()%>%
  filter(percentile_max<0.97)%>%
  group_by(pseudonymPatientNum_bebe)%>%
  arrange(desc(age_enfant))%>%
  slice(1)%>%
  ungroup()%>%
  mutate(event = 0)

patient_surpoids <- children %>%
  filter(age_enfant>=3)%>%
  filter(IMC_percentile>=0.97)%>%
  group_by(pseudonymPatientNum_bebe)%>%
  arrange((age_enfant))%>%
  slice(1)%>%
  ungroup()%>%
  mutate(event = 1)
  
children_survie <- patient_non_surpoids %>%
  bind_rows(patient_surpoids)

# ---- Kaplan Meier ----
library(survival)
library(survminer)

# children_survie_km <- children_survie %>%
#   mutate(
#     time = age_enfant,
#     status = ifelse(IMC_percentile >= 0.97, 1, 0),
#     group = sexe_bebe
#   )
# 
# surv_obj <- Surv(time = children_survie_km$time, event = children_survie_km$status)
# 
# fit <- survfit(surv_obj ~ group, data = children_survie_km)
# 
# ggsurvplot(
#   fit,
#   data = children_survie_km,
#   pval = TRUE,
#   conf.int = TRUE,
#   risk.table = TRUE,
#   legend.title = "Sexe",
#   legend.labs = unique(children_survie_km$group),
#   xlab = "Âge de l'enfant (années)",
#   ylab = "Probabilité de non-survenue de surpoids",
#   palette = c("#F4A7B9", "#A7D3F4")
# )
 
# # Enfants à risque ou non ----
# children_risk <- children_survie %>%
#   select(pseudonymPatientNum_bebe,
#          sexe_bebe,
#          ddn_bebe,
#          age_enfant,
#          IMC_enfant,
#          IMC_percentile,
#          pseudonymPatientNum)

children_survie_km <- children_survie %>%
  left_join(obesity_risk_elfe %>% select(pseudonymPatientNum, score_total_eds_elfe,score_total_these_elfe), 
            by = "pseudonymPatientNum") 

children_survie_km <- children_survie_km %>%
  mutate(risque_eds = case_when(
    score_total_eds_elfe >= 5 ~ "A risque",
    TRUE ~ "Non à risque"
  ))

children_survie_km <- children_survie_km %>%
  mutate(risque_these = case_when(
    score_total_these_elfe >= 5 ~ "A risque",
    TRUE ~ "Non à risque"
  ))

km_fit <- survfit(Surv(age_enfant, event) ~ risque_eds, data = children_survie_km)

ggsurvplot(
  km_fit,
  data = children_survie_km,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = TRUE,
  xlab = "Âge (années)",
  ylab = "Probabilité de non-survenue de surpoids",
  legend.title = "Statut de risque",
  legend.labs = c("À risque", "Non à risque"),
  palette = c("#FFA50080", "#90EE9090")  # Orange clair, Vert clair avec transparence
)

# ---- Sauvegarder tout l'environnement de travail ----
save(list = ls(), file = "Maeva_global_environment_220525.RData")
