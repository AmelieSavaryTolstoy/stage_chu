# ---- Score obésité EFLE ----
mothers <- mothers %>%
  mutate(score_obesite_elfe = case_when(
    is.na(imc_debut_grossesse) ~ NA_real_,
    imc_debut_grossesse >= 30 ~ 3,
    TRUE ~ 0
  ))

# ---- Score surpoids EFLE ----
mothers <- mothers %>%
  mutate(score_surpoids_elfe = case_when(
    is.na(imc_debut_grossesse) ~ NA_real_,
    imc_debut_grossesse >= 25 & imc_debut_grossesse < 30 ~ 2,
    TRUE ~ 0
  ))

# ---- Score tabac EFLE ----
mothers <- mothers %>%
  mutate(score_tabac_elfe = case_when(
    is.na(path_tabac_pmsi) ~ 0,
    grepl("tabac", path_tabac_pmsi, ignore.case = TRUE) ~ 2,
    TRUE ~ 0
  ))

# ---- Score césarienne EFLE ----
mothers <- mothers %>%
  mutate(score_cesarienne_elfe = case_when(
    is.na(mode_accouchement) ~ NA_real_,
    grepl("césarienne", mode_accouchement, ignore.case = TRUE) ~ 1,
    TRUE ~ 0
  ))

# ---- Score prise de poids excessive EFLE ----
mothers <- mothers %>%
  mutate(score_ppe_elfe = case_when(
    is.na(imc_debut_grossesse) | is.na(prise_poids) ~ NA_real_,
    imc_debut_grossesse < 18.5 & prise_poids > 18 ~ 1,
    imc_debut_grossesse >= 18.5 & imc_debut_grossesse < 25 & prise_poids > 16 ~ 1,
    imc_debut_grossesse >= 25 & imc_debut_grossesse < 30 & prise_poids > 11.5 ~ 1,
    imc_debut_grossesse >= 30 & prise_poids > 9 ~ 1,
    TRUE ~ 0
  ))


# ---- Score précarité ----
mothers <- mothers %>%
  mutate(score_precarite = case_when(
    !is.na(precarite_oui) | !is.na(precarite_cim10) | !is.na(CMU) | !is.na(profession_mere) | !is.na(profession_pere)~ 1,
    TRUE ~ 0
  ))

mothers <- mothers %>%
  dplyr::mutate(
    score_macrosomie_elfe = case_when(
      poids_naissance >= percentile_90e ~ 2,
      TRUE ~ 0
    ))
    
# ---- Score de risque ELFE ----
score_elfe <- mothers %>%
  dplyr::select(
    pseudonymPatientNum,
    date_accouchement,
    score_obesite_elfe,
    score_surpoids_elfe,
    score_tabac_elfe,
    score_cesarienne_elfe,
    score_ppe_elfe,
    score_precarite_elfe,
    score_macrosomie_elfe
  ) %>%
  dplyr::mutate(
    score_total_elfe = rowSums(across(starts_with("score_")), na.rm = TRUE)
  )
