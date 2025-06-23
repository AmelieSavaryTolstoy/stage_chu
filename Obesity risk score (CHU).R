library(dplyr)
library(knitr)
library(questionr)
library(lubridate)
library(ggplot2)


 
# # ---- Score ----
## ---- Macrosomie - hypotrophie ----
audipog_normes <- readxl::read_excel("C:\\Users\\savaame\\Desktop\\1000 premier\\xlsx\\audipog_normes.xlsx") %>%
  dplyr::select(sexe_bebe, terme, percentile_10e, percentile_90e)

score_chu <- mothers %>%
  left_join(audipog_normes, by = c("terme", "sexe_bebe"))%>%
  dplyr::mutate(
    score_macrosomie = case_when(
      poids_naissance >= percentile_90e ~ 3,
      TRUE ~ 0
    ),
    score_hypotrophie = case_when(
      poids_naissance <= percentile_10e ~ 1,
      TRUE ~ 0
    )
  )


## ---- Obésité ----


score_chu <- score_chu %>%
  dplyr::mutate(score_obesite = case_when(
    is.na(imc_debut_grossesse) ~ NA_real_,             
    imc_debut_grossesse >= 30 ~ 3,                      
    TRUE ~ 0                       
  ))

## ---- Surpoids ----
score_chu <- score_chu %>%
  dplyr::mutate(score_surpoids = case_when(
    is.na(imc_debut_grossesse) ~ NA_real_,                     
    imc_debut_grossesse >= 25 & imc_debut_grossesse < 30 ~ 2,   
    TRUE ~ 0                                                     
  ))

## ---- Prise de poids excessive ----
score_chu <- score_chu %>%
  dplyr::mutate(score_ppe = case_when(
    is.na(imc_debut_grossesse) | is.na(prise_poids) ~ NA_real_,  
    imc_debut_grossesse < 18.5 & prise_poids > 18 ~ 2,              
    imc_debut_grossesse >= 18.5 & imc_debut_grossesse < 25 & prise_poids > 16 ~ 2,
    imc_debut_grossesse >= 25 & imc_debut_grossesse < 30 & prise_poids > 11.5 ~ 2,  
    imc_debut_grossesse >= 30 & prise_poids > 9 ~ 2,                
    TRUE ~ 0                                                                 
  ))

## ---- Accouchement césarienne ----
score_chu <- score_chu %>%
  dplyr::mutate(score_cesarienne = case_when(
    is.na(mode_accouchement) ~ 0,                     
    TRUE ~ 2                                                 
  ))

## ---- Tabagisme ----
score_chu <- score_chu %>%
  dplyr::mutate(score_tabac = case_when(
    is.na(tabac_oui) ~ 0,                         
    TRUE ~ 1                                               
  ))

## ---- Allaitement artificiel ----
score_chu <- score_chu %>%
  dplyr::mutate(score_allaitement = case_when(
    !is.na(allaitement_artificiel) ~ 1,
    TRUE ~ 0                                                              
  ))

## ---- Diabete gestationnel ----
score_chu <- score_chu %>%
  dplyr::mutate(score_diabete_gesta = case_when(
    is.na(diabete_gesta) ~ 0,
    # !is.na("diabete_gesta_sans_prealable") | !is.na("pathologie_grossesse") ~ 1,
    TRUE ~ 1
  ))

## ---- Précarité ----
score_chu <- score_chu %>%
  dplyr::mutate(score_precarite = case_when(
    is.na(precarite_tous_critere) ~ 0,
    TRUE ~ 1
  ))


# ---- Score de risque CHU de Bdx ----
score_chu <- score_chu %>%
  dplyr::select(
    pseudonymPatientNum,
    date_accouchement,
    score_obesite,
    score_surpoids,
    score_ppe,
    score_cesarienne,
    score_diabete_gesta,
    score_tabac,
    score_allaitement,
    score_precarite,
    score_macrosomie,
    score_hypotrophie
  ) %>%
  dplyr::mutate(
    score_total = rowSums(across(starts_with("score_")), na.rm = TRUE)
  )


# ---- Tableau des scores de risque CHU ----
obesity_risk_chu <- score_chu %>%
  dplyr::select(c("pseudonymPatientNum",
                  "date_accouchement",
                  "score_obesite", 
                  "score_surpoids", 
                  "score_ppe", 
                  "score_cesarienne", 
                  "score_tabac", 
                  "score_allaitement", 
                  "score_diabete_gesta",
                  "score_precarite",
                  "score_macrosomie",
                  "score_hypotrophie",
                  "score_total"))

obesity_risk_chu <- obesity_risk_chu %>%
  rename(score_obesite_eds_chu = score_obesite, 
         score_surpoids_eds_chu = score_surpoids, 
         score_ppe_eds_chu = score_ppe, 
         score_cesarienne_eds_chu = score_cesarienne, 
         score_tabac_eds_chu = score_tabac, 
         score_allaitement_eds_chu = score_allaitement, 
         score_diabete_gesta_eds_chu = score_diabete_gesta,
         score_precarite_eds_chu = score_precarite,
         score_macrosomie_eds_chu = score_macrosomie,
         score_hypotrophie_eds_chu = score_hypotrophie,
         score_total_eds_chu = score_total)
