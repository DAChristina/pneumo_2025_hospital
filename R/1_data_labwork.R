# load df_epi_clean first to get id
# NP refer to np_swb_id in cleaned major database

# load lab data; focused only to NP coz' other samples are negative for pneumo.
sheet_names <- readxl::excel_sheets(
  "raw_data/Database Pneumokokus MSD Anak Isolat Rumah Sakit (RSUD NTB, RSUD Sorong, RSUP Kandou).xlsx"
) %>% 
  grep("_NP", ., value = TRUE) %>% 
  glimpse()

bind_NP_data <- data.frame()
for(s in sheet_names){
read_NP <- readxl::read_excel("raw_data/Database Pneumokokus MSD Anak Isolat Rumah Sakit (RSUD NTB, RSUD Sorong, RSUP Kandou).xlsx",
                              sheet = s) %>% 
  janitor::clean_names() %>% 
  dplyr::transmute(
    id = specimen_id,
    labWork_culture = s_pneumoniae_culture_result,
    temporary_area = s)

bind_NP_data <- dplyr::bind_rows(bind_NP_data, read_NP)
bind_NP_data
}

lab_data <- bind_NP_data %>% 
  dplyr::filter(!is.na(id)) %>% 
  dplyr::transmute(
    labWork_id = gsub("[ -]", "_", id), # instead of using " |-"),
    labWork_culture = labWork_culture,
    labWork_checkArea = temporary_area
  ) %>% 
  dplyr::mutate(across(where(is.character), tolower)) %>% 
  glimpse()


# combine lab_data with metadata
test_labWork <- df_epi_clean %>% 
  dplyr::full_join(
    lab_data %>% 
      dplyr::select(labWork_id, labWork_culture)
    ,
    by = c("sample_np_id" = "labWork_id")
  ) %>% 
  dplyr::select(id, sample_np_id,
                hospital,
                age_group_2vaccine,
                patient_prognosis,
                labWork_culture,
                contains("_bacteria")) %>% 
  view() %>% 
  glimpse()

