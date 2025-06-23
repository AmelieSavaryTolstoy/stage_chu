library(dplyr)
library(readxl)

# ---- Importation données fichier excel ----
score_these <- read_excel("C:\\Users\\savaame\\Desktop\\1000 premier\\xlsx\\Obesity risk score.xlsx", 
                                   col_types = c("text", "date", "numeric", 
                                                 "date", "date", "numeric", "numeric", 
                                                 "text", "numeric", "numeric", "text", 
                                                 "text", "text", "numeric", 
                                                 "text", "text", "text", "text", 
                                                 "text", "text", "text", 
                                                 "text", "text", "text"))%>%
  dplyr::filter(!pseudonymPatientNum %in% c("0000000092", "0000000103", "0000000154", "0000000158", "0000000248", "0000000252",
                                            "0000000333", "0000000405", "0000000587", "0000000687", "0000000979", "0000001222",
                                            "0000001285", "0000001296", "0000001359", "0000001852", "0000001941", "0000001961"))

score_these <- score_these %>%
  dplyr::filter(!pseudonymPatientNum %in% c("0000001664", "0000001311", "0000000655", "0000000628", "0000000510")) # patients avec diabete prealable

# ---- Score ----
## ---- obésité ----
score_these <- score_these %>%
  mutate(score_obesite_these_chu = case_when(
    is.na(IMC) ~ NA_real_,
    IMC >= 30 ~ 3,
    TRUE ~ 0
  ))

## ---- surpoids ----
score_these <- score_these %>%
  mutate(score_surpoids_these_chu = case_when(
    is.na(IMC) ~ NA_real_,                     
    IMC >= 25 & IMC < 30 ~ 2,   
    TRUE ~ 0                                                     
  ))

## ---- prise de poids excessive ----
score_these <- score_these %>%
  mutate(score_ppe_these_chu = case_when(
    is.na(IMC) | is.na(`Prise de poids`) ~ NA_real_,
    IMC < 18.5 & `Prise de poids` > 18 ~ 2,
    IMC >= 18.5 & IMC < 25 & `Prise de poids` > 16 ~ 2,
    IMC >= 25 & IMC < 30 & `Prise de poids` > 11.5 ~ 2,
    IMC >= 30 & `Prise de poids` > 9 ~ 2,
    TRUE ~ 0
  ))

## ---- accouchement césarienne ----
score_these <- score_these %>%
  mutate(score_cesarienne_these_chu = case_when(
    is.na(`Mode d'accouchement`) ~ NA_real_,
    grepl("cesarienne", `Mode d'accouchement`, ignore.case = TRUE) ~ 2,
    TRUE ~ 0
  ))

## ---- tabagisme ----
score_these <- score_these %>%
  mutate(score_tabac_these_chu = case_when(
    is.na(Tabagisme) ~ NA_real_,
    Tabagisme == 1 ~ 1,
    Tabagisme == 0 ~ 0,
    TRUE ~ NA_real_
  ))

## ---- allaitement artificiel ----
score_these <- score_these %>%
  mutate(score_allaitement_these_chu = case_when(
    grepl("artificiel", `Type d'allaitement`, ignore.case = TRUE) ~ 1,
    grepl("maternel", `Type d'allaitement`, ignore.case = TRUE) ~ 0,
    TRUE ~ NA_real_
  ))

## ---- diabete gestationnel ----
score_these <- score_these %>%
  mutate(score_diabete_gesta_these_chu = case_when(
    is.na(`Diabète gestationnel`) ~ NA_real_,
    `Diabète gestationnel` == 1 ~ 1,
    `Diabète gestationnel` == 0 ~ 0,
    TRUE ~ NA_real_
  ))

## ---- précarité ----
score_these <- score_these %>%
  mutate(score_precarite_these_chu = case_when(
    is.na(Précarité) ~ NA_real_,
    Précarité == 1 ~ 1,
    Précarité == 0 ~ 0,
    TRUE ~ NA_real_
  ))

## ---- macrosomie ----
score_these <- score_these %>%
  mutate(score_macrosomie_these_chu = case_when(
    is.na(Macrosomie) ~ NA_real_,
    Macrosomie == 1 ~ 3,
    Macrosomie == 0 ~ 0,
    TRUE ~ NA_real_
  ))

## ---- hypotrophie ----
score_these <- score_these %>%
  mutate(score_hypotrophie_these_chu = case_when(
    is.na(Hypotrophie) ~ NA_real_,
    Hypotrophie == 1 ~ 1,
    Hypotrophie == 0 ~ 0,
    TRUE ~ NA_real_
  ))

score_these <- score_these %>%
  dplyr::mutate(
  score_total_these_chu = rowSums(across(ends_with("_these_chu")), na.rm = TRUE)
)


# ---- Tableau final CHU ----
comparaison_obesity_risk_chu <- obesity_risk_chu %>%
  left_join(score_these %>% 
              select(
                pseudonymPatientNum,
                `Date d'accouchement`,
                score_obesite_these_chu,
                score_surpoids_these_chu,
                score_ppe_these_chu,
                score_cesarienne_these_chu,
                score_tabac_these_chu,
                score_allaitement_these_chu,
                score_diabete_gesta_these_chu,
                score_precarite_these_chu,
                score_macrosomie_these_chu,
                score_hypotrophie_these_chu,
                score_total_these_chu
              ), 
            by = "pseudonymPatientNum")%>%
  filter(date_accouchement >= as.Date(`Date d'accouchement`)-31 & date_accouchement <= as.Date(`Date d'accouchement`)+31)%>%
  dplyr::group_by(pseudonymPatientNum)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()




# ---- Comparaison des scores ----
comparaison_obesity_risk_chu <- comparaison_obesity_risk_chu %>%
  mutate(
    comparaison_obesite_chu = case_when(
      is.na(score_obesite_eds_chu) & !is.na(score_obesite_these_chu) ~ "Absence EDS",
      !is.na(score_obesite_eds_chu) & is.na(score_obesite_these_chu) ~ "Absence Thèse",
      is.na(score_obesite_eds_chu) & is.na(score_obesite_these_chu) ~ "Absence totale",
      score_obesite_eds_chu == score_obesite_these_chu ~ "Concordance",
      TRUE ~ "Divergence"
    ),
    comparaison_surpoids_chu = case_when(
      is.na(score_surpoids_eds_chu) & !is.na(score_surpoids_these_chu) ~ "Absence EDS",
      !is.na(score_surpoids_eds_chu) & is.na(score_surpoids_these_chu) ~ "Absence Thèse",
      is.na(score_surpoids_eds_chu) & is.na(score_surpoids_these_chu) ~ "Absence totale",
      score_surpoids_eds_chu == score_surpoids_these_chu ~ "Concordance",
      TRUE ~ "Divergence"
    ),
    comparaison_ppe_chu = case_when(
      is.na(score_ppe_eds_chu) & !is.na(score_ppe_these_chu) ~ "Absence EDS",
      !is.na(score_ppe_eds_chu) & is.na(score_ppe_these_chu) ~ "Absence Thèse",
      is.na(score_ppe_eds_chu) & is.na(score_ppe_these_chu) ~ "Absence totale",
      score_ppe_eds_chu == score_ppe_these_chu ~ "Concordance",
      TRUE ~ "Divergence"
    ),
    comparaison_cesarienne_chu = case_when(
      is.na(score_cesarienne_eds_chu) & !is.na(score_cesarienne_these_chu) ~ "Absence EDS",
      !is.na(score_cesarienne_eds_chu) & is.na(score_cesarienne_these_chu) ~ "Absence Thèse",
      is.na(score_cesarienne_eds_chu) & is.na(score_cesarienne_these_chu) ~ "Absence totale",
      score_cesarienne_eds_chu == score_cesarienne_these_chu ~ "Concordance",
      TRUE ~ "Divergence"
    ),
    comparaison_tabac_chu = case_when(
      is.na(score_tabac_eds_chu) & !is.na(score_tabac_these_chu) ~ "Absence EDS",
      !is.na(score_tabac_eds_chu) & is.na(score_tabac_these_chu) ~ "Absence Thèse",
      is.na(score_tabac_eds_chu) & is.na(score_tabac_these_chu) ~ "Absence totale",
      score_tabac_eds_chu == score_tabac_these_chu ~ "Concordance",
      TRUE ~ "Divergence"
    ),
    comparaison_allaitement_chu = case_when(
      is.na(score_allaitement_eds_chu) & !is.na(score_allaitement_these_chu) ~ "Absence EDS",
      !is.na(score_allaitement_eds_chu) & is.na(score_allaitement_these_chu) ~ "Absence Thèse",
      is.na(score_allaitement_eds_chu) & is.na(score_allaitement_these_chu) ~ "Absence totale",
      score_allaitement_eds_chu == score_allaitement_these_chu ~ "Concordance",
      TRUE ~ "Divergence"
    ),
    comparaison_diabete_gesta_chu = case_when(
      is.na(score_diabete_gesta_eds_chu) & !is.na(score_diabete_gesta_these_chu) ~ "Absence EDS",
      !is.na(score_diabete_gesta_eds_chu) & is.na(score_diabete_gesta_these_chu) ~ "Absence Thèse",
      is.na(score_diabete_gesta_eds_chu) & is.na(score_diabete_gesta_these_chu) ~ "Absence totale",
      score_diabete_gesta_eds_chu == score_diabete_gesta_these_chu ~ "Concordance",
      TRUE ~ "Divergence"
    ),
    comparaison_precarite_chu = case_when(
      is.na(score_precarite_eds_chu) & !is.na(score_precarite_these_chu) ~ "Absence EDS",
      !is.na(score_precarite_eds_chu) & is.na(score_precarite_these_chu) ~ "Absence Thèse",
      is.na(score_precarite_eds_chu) & is.na(score_precarite_these_chu) ~ "Absence totale",
      score_precarite_eds_chu == score_precarite_these_chu ~ "Concordance",
      TRUE ~ "Divergence"
    ),
    comparaison_macrosomie_chu = case_when(
      is.na(score_macrosomie_eds_chu) & !is.na(score_macrosomie_these_chu) ~ "Absence EDS",
      !is.na(score_macrosomie_eds_chu) & is.na(score_macrosomie_these_chu) ~ "Absence Thèse",
      is.na(score_macrosomie_eds_chu) & is.na(score_macrosomie_these_chu) ~ "Absence totale",
      score_macrosomie_eds_chu == score_macrosomie_these_chu ~ "Concordance",
      TRUE ~ "Divergence"
    ),
    comparaison_hypotrophie_chu = case_when(
      is.na(score_hypotrophie_eds_chu) & !is.na(score_hypotrophie_these_chu) ~ "Absence EDS",
      !is.na(score_hypotrophie_eds_chu) & is.na(score_hypotrophie_these_chu) ~ "Absence Thèse",
      is.na(score_hypotrophie_eds_chu) & is.na(score_hypotrophie_these_chu) ~ "Absence totale",
      score_hypotrophie_eds_chu == score_hypotrophie_these_chu ~ "Concordance",
      TRUE ~ "Divergence"
    ),
    comparaison_score_total_chu = case_when(
      is.na(score_total_eds_chu) & !is.na(score_total_these_chu) ~ "Absence EDS",
      !is.na(score_total_eds_chu) & is.na(score_total_these_chu) ~ "Absence Thèse",
      is.na(score_total_eds_chu) & is.na(score_total_these_chu) ~ "Absence totale",
      score_total_eds_chu == score_total_these_chu ~ "Concordance",
      TRUE ~ "Divergence"
    )
  )

# #-------------------------------------------------------------------------------
# 
# 
# # ---- Tableau des scores de risque ELFE----
# obesity_risk_elfe <- mothers %>%
#   group_by(pseudonymPatientNum, date_accouchement)%>%
#   slice(1)%>%
#   ungroup()%>%
#   dplyr::select(c("pseudonymPatientNum",
#                   "date_accouchement",
#                   "score_obesite_elfe", 
#                   "score_surpoids_elfe", 
#                   "score_ppe_elfe", 
#                   "score_cesarienne_elfe", 
#                   "score_tabac_elfe", 
#                   "score_precarite_elfe",
#                   "score_macrosomie_elfe"))
# 
# obesity_risk_elfe <- obesity_risk_elfe %>%
#   rename(score_obesite_eds_elfe = score_obesite_elfe, 
#          score_surpoids_eds_elfe = score_surpoids_elfe, 
#          score_ppe_eds_elfe = score_ppe_elfe, 
#          score_cesarienne_eds_elfe = score_cesarienne_elfe, 
#          score_tabac_eds_elfe = score_tabac_elfe, 
#          score_precarite_eds_elfe = score_precarite_elfe,
#          score_macrosomie_eds_elfe = score_macrosomie_elfe)
# 
# obesity_risk_elfe <- obesity_risk_elfe %>%
#   dplyr::mutate(
#     score_total_eds_elfe = rowSums(across(starts_with("score_")), na.rm = TRUE)
#   )
# 
# # ---- Score ----
# ## ---- obésité ----
# score_these <- score_these %>%
#   mutate(score_obesite_these_elfe = case_when(
#     is.na(IMC) ~ NA_real_,
#     IMC >= 30 ~ 3,
#     TRUE ~ 0
#   ))
# 
# ## ---- surpoids ----
# score_these <- score_these %>%
#   mutate(score_surpoids_these_elfe = case_when(
#     is.na(IMC) ~ NA_real_,                     
#     IMC >= 25 & IMC < 30 ~ 2,   
#     TRUE ~ 0                                                     
#   ))
# 
# ## ---- prise de poids excessive ----
# score_these <- score_these %>%
#   mutate(score_ppe_these_elfe = case_when(
#     is.na(IMC) | is.na(`Prise de poids`) ~ NA_real_,
#     IMC < 18.5 & `Prise de poids` > 18 ~ 1,
#     IMC >= 18.5 & IMC < 25 & `Prise de poids` > 16 ~ 1,
#     IMC >= 25 & IMC < 30 & `Prise de poids` > 11.5 ~ 1,
#     IMC >= 30 & `Prise de poids` > 9 ~ 1,
#     TRUE ~ 0
#   ))
# 
# ## ---- accouchement césarienne ----
# score_these <- score_these %>%
#   mutate(score_cesarienne_these_elfe = case_when(
#     is.na(`Mode d'accouchement`) ~ NA_real_,
#     grepl("cesarienne", `Mode d'accouchement`, ignore.case = TRUE) ~ 1,
#     TRUE ~ 0
#   ))
# 
# ## ---- tabagisme ----
# score_these <- score_these %>%
#   mutate(score_tabac_these_elfe = case_when(
#     is.na(Tabagisme) ~ NA_real_,
#     Tabagisme == 1 ~ 2,
#     Tabagisme == 0 ~ 0,
#     TRUE ~ NA_real_
#   ))
# 
# ## ---- précarité ----
# score_these <- score_these %>%
#   mutate(score_precarite_these_elfe = case_when(
#     is.na(Précarité) ~ NA_real_,
#     Précarité == 1 ~ 1,
#     Précarité == 0 ~ 0,
#     TRUE ~ NA_real_
#   ))
# 
# ## ---- macrosomie ----
# score_these <- score_these %>%
#   mutate(score_macrosomie_these_elfe = case_when(
#     is.na(Macrosomie) ~ NA_real_,
#     Macrosomie == 1 ~ 2,
#     Macrosomie == 0 ~ 0,
#     TRUE ~ NA_real_
#   ))
# 
# score_these <- score_these %>%
#   dplyr::mutate(
#     score_total_these_elfe = rowSums(across(ends_with("_these_elfe")), na.rm = TRUE)
#   )
# 
# # ---- Tableau final ELFE ----
# obesity_risk_elfe <- obesity_risk_elfe %>%
#   left_join(score_these %>% 
#               select(
#                 pseudonymPatientNum,
#                 `Date d'accouchement`,
#                 score_obesite_these_elfe,
#                 score_surpoids_these_elfe,
#                 score_ppe_these_elfe,
#                 score_cesarienne_these_elfe,
#                 score_tabac_these_elfe,
#                 score_precarite_these_elfe,
#                 score_macrosomie_these_elfe,
#                 score_total_these_elfe
#               ), 
#             by = "pseudonymPatientNum")%>%
#   dplyr::filter(`Date d'accouchement` >= (date_accouchement - lubridate::weeks(4)) & `Date d'accouchement` <= (date_accouchement + lubridate::weeks(4)))
# 
# # ---- Comparaison des scores ----
# obesity_risk_elfe <- obesity_risk_elfe %>%
#   mutate(
#     comparaison_obesite_elfe = case_when(
#       is.na(score_obesite_eds_elfe) & !is.na(score_obesite_these_elfe) ~ "Absence EDS",
#       !is.na(score_obesite_eds_elfe) & is.na(score_obesite_these_elfe) ~ "Absence Thèse",
#       is.na(score_obesite_eds_elfe) & is.na(score_obesite_these_elfe) ~ "Absence totale",
#       score_obesite_eds_elfe == score_obesite_these_elfe ~ "Concordance",
#       TRUE ~ "Divergence"
#     ),
#     comparaison_surpoids_elfe = case_when(
#       is.na(score_surpoids_eds_elfe) & !is.na(score_surpoids_these_elfe) ~ "Absence EDS",
#       !is.na(score_surpoids_eds_elfe) & is.na(score_surpoids_these_elfe) ~ "Absence Thèse",
#       is.na(score_surpoids_eds_elfe) & is.na(score_surpoids_these_elfe) ~ "Absence totale",
#       score_surpoids_eds_elfe == score_surpoids_these_elfe ~ "Concordance",
#       TRUE ~ "Divergence"
#     ),
#     comparaison_ppe_elfe = case_when(
#       is.na(score_ppe_eds_elfe) & !is.na(score_ppe_these_elfe) ~ "Absence EDS",
#       !is.na(score_ppe_eds_elfe) & is.na(score_ppe_these_elfe) ~ "Absence Thèse",
#       is.na(score_ppe_eds_elfe) & is.na(score_ppe_these_elfe) ~ "Absence totale",
#       score_ppe_eds_elfe == score_ppe_these_elfe ~ "Concordance",
#       TRUE ~ "Divergence"
#     ),
#     comparaison_cesarienne_elfe = case_when(
#       is.na(score_cesarienne_eds_elfe) & !is.na(score_cesarienne_these_elfe) ~ "Absence EDS",
#       !is.na(score_cesarienne_eds_elfe) & is.na(score_cesarienne_these_elfe) ~ "Absence Thèse",
#       is.na(score_cesarienne_eds_elfe) & is.na(score_cesarienne_these_elfe) ~ "Absence totale",
#       score_cesarienne_eds_elfe == score_cesarienne_these_elfe ~ "Concordance",
#       TRUE ~ "Divergence"
#     ),
#     comparaison_tabac_elfe = case_when(
#       is.na(score_tabac_eds_elfe) & !is.na(score_tabac_these_elfe) ~ "Absence EDS",
#       !is.na(score_tabac_eds_elfe) & is.na(score_tabac_these_elfe) ~ "Absence Thèse",
#       is.na(score_tabac_eds_elfe) & is.na(score_tabac_these_elfe) ~ "Absence totale",
#       score_tabac_eds_elfe == score_tabac_these_elfe ~ "Concordance",
#       TRUE ~ "Divergence"
#     ),
#     comparaison_precarite_elfe = case_when(
#       is.na(score_precarite_eds_elfe) & !is.na(score_precarite_these_elfe) ~ "Absence EDS",
#       !is.na(score_precarite_eds_elfe) & is.na(score_precarite_these_elfe) ~ "Absence Thèse",
#       is.na(score_precarite_eds_elfe) & is.na(score_precarite_these_elfe) ~ "Absence totale",
#       score_precarite_eds_elfe == score_precarite_these_elfe ~ "Concordance",
#       TRUE ~ "Divergence"
#     ),
#     comparaison_macrosomie_elfe = case_when(
#       is.na(score_macrosomie_eds_elfe) & !is.na(score_macrosomie_these_elfe) ~ "Absence EDS",
#       !is.na(score_macrosomie_eds_elfe) & is.na(score_macrosomie_these_elfe) ~ "Absence Thèse",
#       is.na(score_macrosomie_eds_elfe) & is.na(score_macrosomie_these_elfe) ~ "Absence totale",
#       score_macrosomie_eds_elfe == score_macrosomie_these_elfe ~ "Concordance",
#       TRUE ~ "Divergence"
#     ),
#     comparaison_score_total_elfe = case_when(
#       is.na(score_total_eds_elfe) & !is.na(score_total_these_elfe) ~ "Absence EDS",
#       !is.na(score_total_eds_elfe) & is.na(score_total_these_elfe) ~ "Absence Thèse",
#       is.na(score_total_eds_elfe) & is.na(score_total_these_elfe) ~ "Absence totale",
#       score_total_eds_elfe == score_total_these_elfe ~ "Concordance",
#       TRUE ~ "Divergence"
#     )
#   )

# ---- Résultats gtsummary ----
## ---- Score de risque CHU ----
table_obesity_risk_chu <-comparaison_obesity_risk_chu %>%
  dplyr::filter(`Date d'accouchement` >= (date_accouchement - lubridate::weeks(4)) & `Date d'accouchement` <= (date_accouchement + lubridate::weeks(4)))%>%
  dplyr::select(comparaison_obesite_chu, 
                comparaison_surpoids_chu, 
                comparaison_ppe_chu, 
                comparaison_cesarienne_chu, 
                comparaison_tabac_chu, 
                comparaison_allaitement_chu, 
                comparaison_diabete_gesta_chu, 
                comparaison_precarite_chu,
                comparaison_macrosomie_chu,
                comparaison_hypotrophie_chu,
                comparaison_score_total_chu)%>%
  gtsummary::tbl_summary()

table_obesity_risk_chu 



# table_obesity_risk_chu %>%
#   gtsummary::as_gt()%>%
#   gt::gtsave(filename = "table_obesity_risk_chu.png")
# 
# ## ---- Score de risque ELFE ----
# table_obesity_risk_elfe <-obesity_risk_elfe %>%
#   dplyr::select(comparaison_obesite_elfe, 
#                 comparaison_surpoids_elfe, 
#                 comparaison_ppe_elfe, 
#                 comparaison_cesarienne_elfe, 
#                 comparaison_tabac_elfe, 
#                 comparaison_precarite_elfe,
#                 comparaison_macrosomie_elfe,
#                 comparaison_score_total_elfe)%>%
#   gtsummary::tbl_summary()
# 
# table_obesity_risk_elfe
# 
# table_obesity_risk_elfe %>%
#   gtsummary::as_gt()%>%
#   gt::gtsave(filename = "table_obesity_risk_elfe.png")
