# ---- Résultats ----
# ---- Table 1 : Caractéristiques ----
library(dplyr)
library(gt)
library(gtsummary)
library(forcats)
library(tidyr)
library(flextable)
library(officer)
library(tidyr)
library(survival)
library(survminer)

score_these <- score_these %>%
  mutate(`Corpulence en début de grossesse` = forcats::fct_relevel(
    `Corpulence en début de grossesse`,
    "insuffisance pondérale",
    "normale",
    "surpoids",          
    "obesite I",
    "obesite II",
    "obesite III"
  ))

table1 <- score_these %>%
  select(
    `Âge`,
    `Taille (cm)`,
    `Poids début de grossesse`,
    `Corpulence en début de grossesse`,
    `IMC`,
    `Prise de poids`,
    `Mode d'accouchement`,
    `Type d'allaitement`
  )

table1_summary <- table1 %>%
  tbl_summary(
    by = NULL,
    label = list(
      `Âge` ~ "Âge maternel (ans)",
      `Taille (cm)` ~ "Taille (cm)",
      `Poids début de grossesse` ~ "Poids en début de grossesse (kg)",
      `Corpulence en début de grossesse` ~ "Corpulence en début de grossesse",
      `IMC` ~ "Indice de Masse Corporelle (IMC)",
      `Prise de poids` ~ "Prise de poids pendant la grossesse (kg)",
      `Mode d'accouchement` ~ "Mode d'accouchement",
      `Type d'allaitement` ~ "Type d’allaitement"
    ),
    digits = all_continuous() ~ 1
  ) %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Total**")%>%
  gtsummary::bold_labels()

table1_summary

table1_summary %>%
  gtsummary::as_gt()%>%
  gt::gtsave(filename = "Table 1 - Characteristic.png")

table1_summary_word <- table1_summary %>%
  as_flex_table()%>%
  autofit()

save_as_docx(table1_summary_word, path = "table1.docx")

# ---- Table 2 : Concordance score CHU ----
variables_score <- c(
  "comparaison_obesite_chu",
  "comparaison_surpoids_chu",
  "comparaison_ppe_chu",
  "comparaison_cesarienne_chu",
  "comparaison_tabac_chu",
  "comparaison_allaitement_chu",
  "comparaison_diabete_gesta_chu",
  "comparaison_precarite_chu",
  "comparaison_macrosomie_chu",
  "comparaison_hypotrophie_chu"
)

table2 <- obesity_risk_chu %>%
  summarise(across(all_of(variables_score), ~ sum(. == "Concordance", na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Effectif") %>%
  mutate(
    Pourcentage = round(Effectif / nrow(obesity_risk_chu) * 100, 1),
    Variable = gsub("comparaison_", "", Variable),
    Variable = gsub("_chu", "", Variable)
  ) %>%
  pivot_longer(cols = c(Effectif, Pourcentage), names_to = "Type", values_to = "Valeur") %>%
  pivot_wider(names_from = Variable, values_from = Valeur) %>%
  gt() %>%
  tab_header(title = "Concordance des données du score de risque CHU") %>%
  tab_stubhead(label = "") %>% 
  cols_label(
    obesite = "Obésité",
    surpoids = "Surpoids",
    ppe = "Prise de poids excessive",
    cesarienne = "Accouchement par césarienne",
    tabac = "Tabagisme pendant la grossesse",
    allaitement = "Allaitement artificiel",
    diabete_gesta = "Diabète gestationnel",
    precarite = "Situation de précarité",
    macrosomie = "Macrosomie",
    hypotrophie = "Hypotrophie"
  ) %>%
  fmt_number(columns = everything(), decimals = 1) %>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = list(
      cells_stub(),              
      cells_column_labels()
    )
  )

table2

table2_word <- table2 %>%
  as_flex_table()%>%
  autofit()
save_as_docx(table2_word, path = "table2.docx")

#Table 2.0 ----
table2_concordance <- obesity_risk_chu %>%
  group_by(pseudonymPatientNum, date_accouchement)%>%
  slice(1)%>%
  ungroup()%>%
  select(
    comparaison_obesite_chu, 
    comparaison_surpoids_chu, 
    comparaison_ppe_chu, 
    comparaison_cesarienne_chu, 
    comparaison_tabac_chu, 
    comparaison_allaitement_chu, 
    comparaison_diabete_gesta_chu, 
    comparaison_precarite_chu, 
    comparaison_macrosomie_chu, 
    comparaison_hypotrophie_chu
  ) %>%
  tbl_summary(
    by = NULL,
    missing = "no",
    label = list(
      comparaison_obesite_chu = "Obésité",
      comparaison_surpoids_chu = "Surpoids",
      comparaison_ppe_chu = "Prise de poids excessive",
      comparaison_cesarienne_chu = "Accouchement par césarienne",
      comparaison_tabac_chu = "Tabagisme pendant la grossesse",
      comparaison_allaitement_chu = "Allaitement artificiel",
      comparaison_diabete_gesta_chu = "Diabète gestationnel",
      comparaison_precarite_chu = "Situation de précarité",
      comparaison_macrosomie_chu = "Macrosomie",
      comparaison_hypotrophie_chu = "Hypotrophie"
    )
  ) %>%
  modify_header(label ~ "**Variable de comparaison**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Total (n, %)**") %>%
  bold_labels()


table2_concordance

table2_concordance_word <- table2_concordance %>%
  as_flex_table()%>%
  autofit()
save_as_docx(table2_concordance_word, path = "table2.concordance.0.docx")

# ---- Table 3 : Score de risque ----
obesity_risk_elfe_2 <- obesity_risk_elfe %>%
  mutate(risque_eds = case_when(
    score_total_eds_elfe >= 5 ~ "A risque",
    TRUE ~ "Non à risque"
  ))%>%
  mutate(risque_these = case_when(
    score_total_these_elfe >= 5 ~ "A risque",
    TRUE ~ "Non à risque"
  ))%>%
  mutate(pseudonymPatientNum2 = paste0(pseudonymPatientNum,date_accouchement))

table3_data <- obesity_risk_elfe_2 %>%
  mutate(groupe = "placeholder")

table3_long <- table3_data %>%
  transmute(
    pseudonymPatientNum2,
    `Score total` = score_total_eds_elfe,
    `Patient à risque ou non` = risque_eds,
    `Comparaison du score total` = comparaison_score_total_elfe,
    Source = "EDS"
  ) %>%
  bind_rows(
    table3_data %>%
      transmute(
        pseudonymPatientNum2,
        `Score total` = score_total_these_elfe,
        `Patient à risque ou non` = risque_these,
        `Comparaison du score total` = comparaison_score_total_elfe,
        Source = "Thèse"
      )
  )
table3_summary <- table3_long %>%
  select(Source,
         `Score total`,
         `Patient à risque ou non`,
         `Comparaison du score total`) %>%
  tbl_summary(
    by = Source,
    label = list(
      `Score total` ~ "Score de risque",
      `Patient à risque ou non` ~ "Statut de risque de l'enfant",
      `Comparaison du score total` ~ "Nombre de scores concordants"
    ),
    statistic = list(
      all_continuous() ~ "{median} [{p25}, {p75}]",
      all_categorical() ~ "{n} ({p}%)"
    )
  ) %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Source des données**") %>%
  bold_labels()

table3_summary

table3_summary_word <- table3_summary %>%
  as_flex_table()%>%
  autofit()
save_as_docx(table3_summary_word, path = "table3.docx")


# ---- Table 4 : Kaplan Meier ----
Projet_EXPO_1000_Lien_pseudonyme_Mère_Enfant <- readxl::read_excel("Projet EXPO 1000 - Lien pseudonyme Mère Enfant.xlsx")

children <- children %>%
  left_join(Projet_EXPO_1000_Lien_pseudonyme_Mère_Enfant, by = "pseudonymPatientNum_bebe")

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
  palette = c("#FFA50080", "#90EE9090") 
)
