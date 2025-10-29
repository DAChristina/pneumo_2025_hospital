# patients per-hospital
df_epi_clean %>% 
  dplyr::group_by(hospital) %>% 
  dplyr::summarise(count = n()) %>% 
  # view() %>% 
  glimpse()

# samples recap
df_epi_clean %>% 
  dplyr::mutate(
    sample_blood_id = ifelse(is.na(sample_blood_id), FALSE, TRUE)
  ) %>% 
  dplyr::group_by(hospital, sample_blood_id) %>% 
  dplyr::summarise(n_blood = n()) %>% 
  dplyr::filter(sample_blood_id == TRUE) %>% 
  dplyr::select(hospital, n_blood) %>% 
  dplyr::left_join(
    df_epi_clean %>% 
      dplyr::mutate(
        sample_sputum_id = ifelse(is.na(sample_sputum_id), FALSE, TRUE)
      ) %>% 
      dplyr::group_by(hospital, sample_sputum_id) %>% 
      dplyr::summarise(n_sputum = n()) %>% 
      dplyr::filter(sample_sputum_id == TRUE) %>% 
      dplyr::select(hospital, n_sputum)
    ,
    by = "hospital"
  ) %>% 
  dplyr::left_join(
    df_epi_clean %>% 
      dplyr::mutate(
        sample_lcs_id = ifelse(is.na(sample_lcs_id), FALSE, TRUE)
      ) %>% 
      dplyr::group_by(hospital, sample_lcs_id) %>% 
      dplyr::summarise(n_lcs = n()) %>% 
      dplyr::filter(sample_lcs_id == TRUE) %>% 
      dplyr::select(hospital, n_lcs)
    ,
    by = "hospital"
  ) %>% 
  dplyr::left_join(
    df_epi_clean %>% 
      dplyr::mutate(
        sample_np_id = ifelse(is.na(sample_np_id), FALSE, TRUE)
      ) %>% 
      dplyr::group_by(hospital, sample_np_id) %>% 
      dplyr::summarise(n_np = n()) %>% 
      dplyr::filter(sample_np_id == TRUE) %>% 
      dplyr::select(hospital, n_np)
    ,
    by = "hospital"
  ) %>% 
  view() %>% 
  glimpse()

# early diagnosis per-hospital and age
df_epi_clean %>% 
  dplyr::mutate(
    diag_2signs_bacteraemia = ifelse(diag_2signs_bacteraemia == 1, "bacteremia",
                                 NA),
    diag_2signs_pneumonia = ifelse(diag_2signs_pneumonia == 1, "pneumonia",
                                 NA),
    diag_2signs_meningitis = ifelse(diag_2signs_meningitis == 1, "meningitis",
                                 NA),
    diag_2signs_pleural_effusion = ifelse(diag_2signs_pleural_effusion == 1, "pleural effusion",
                                    NA),
  ) %>% 
  tidyr::pivot_longer(
    .,
    cols = c(
      diag_2signs_bacteraemia,
      diag_2signs_pneumonia,
      diag_2signs_meningitis,
      diag_2signs_pleural_effusion
    ),
    values_to = "early_signs"
  ) %>% 
  # dplyr::distinct(id, .keep_all = T) %>% 
  dplyr::group_by(hospital, early_signs) %>% 
  dplyr::summarise(count = n()) %>% 
  dplyr::filter(!is.na(early_signs)) %>% # counts has already include complications
  # view() %>%
  glimpse()

# age distribution per-hospital
df_epi_clean %>% 
  dplyr::group_by(hospital, age_group_2vaccine) %>% 
  dplyr::summarise(count = n()) %>% 
  view() %>% 
  glimpse()


# PICU-NICU per-hospital
df_epi_clean %>% 
  dplyr::group_by(hospital, patient_room_origin) %>% 
  dplyr::summarise(count = n()) %>% 
  view() %>% 
  glimpse()

