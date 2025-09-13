library(tidyverse)

# Data cleaning process for hospital data ######################################
df_epi_hospital <- readxl::read_excel("raw_data/temporary_df_epi_hospital_combine_row_cleaned_27Aug2025.xlsx") %>% 
  # dplyr::rename_all(~stringr::str_replace_all(., " ", "_")) %>% 
  # rename_with(~ gsub("[\\. ]", "_", tolower(.))) %>%
  janitor::clean_names() %>% 
  dplyr::mutate(SPECIMEN_ID = gsub(" ", "_", no_id_subjek),
                # across(where(is.character), ~ na_if(., "N/A")),
                # across(where(is.character), ~ if_else(. == "-", "tidak", .)),
                usia_bulan_dc = round(usia_bulan_dc, 0),
                
                # some age grouping
                group_age = case_when(
                  usia_bulan_dc < 1 ~ "0 months",
                  usia_bulan_dc >= 1 & usia_bulan_dc < 7 ~ "1-6 months",
                  usia_bulan_dc >= 7 & usia_bulan_dc < 13 ~ "7-12 months",
                  usia_bulan_dc >= 13 & usia_bulan_dc < 19 ~ "13-18 months",
                  usia_bulan_dc >= 19 & usia_bulan_dc < 25 ~ "19-24 months",
                  
                  # usia_bulan_dc <= 24 ~ as.character(round(usia_bulan_dc, 0)),
                  usia_tahun_dc >= 2 & usia_tahun_dc < 6 ~ "2-5 years",
                  usia_tahun_dc >= 6 & usia_tahun_dc < 11 ~ "6-10 years",
                  usia_tahun_dc >= 11 ~ "10+ years",
                ),
                group_age = factor(group_age,
                                   levels = c("0 months",
                                              "1-6 months",
                                              "7-12 months",
                                              "13-18 months",
                                              "19-24 months",
                                              "2-5 years",
                                              "6-10 years",
                                              "10+ years"
                                              )),
                
                group_age_vaccine = case_when(
                  usia_tahun_dc < 1 ~ "0 years",
                  usia_tahun_dc >= 1 & usia_tahun_dc < 3 ~ "1-2 years",
                  usia_tahun_dc >= 3 & usia_tahun_dc < 6 ~ "3-5 years",
                  usia_tahun_dc >= 6 & usia_tahun_dc < 11 ~ "6-10 years",
                  usia_tahun_dc >= 11 ~ "11-18 years",
                  
                ),
                group_age_vaccine = factor(group_age_vaccine,
                                   levels = c("0 years",
                                              "1-2 years",
                                              "3-5 years",
                                              "6-10 years",
                                              "11-18 years"
                                   )),
                
                # immunised vs. unimmunised
                pcv13_group = case_when(
                  is.na(jumlah_dosis_vaksin_pcv_13) ~ "unimmunised",
                  TRUE ~ "immunised"
                ),
                
                # combine prognosis when "Membaik" == "Sembuh"
                bagaimanakah_prognosis_pasien = case_when(
                  bagaimanakah_prognosis_pasien == "Membaik" | bagaimanakah_prognosis_pasien == "Sembuh" ~ "Membaik/Sembuh",
                  TRUE ~ bagaimanakah_prognosis_pasien
                )
                
                ) %>%
  glimpse()


# Detect duplicated IDs; 35 IDs are duplicated but they seems come from different people
df_epi_hospital_duplicated_ids <- df_epi_hospital %>% 
  dplyr::count(SPECIMEN_ID) %>% 
  dplyr::filter(SPECIMEN_ID > 1) %>% 
  dplyr::mutate(category = case_when(
    n == 2 ~ "Duplicated",
    n == 3 ~ "Triplicated",
    n == 4 ~ "Quadruplicated",
    n > 4 ~ "More than Quadruplicated"
  )) %>% 
  dplyr::filter(n > 1) %>% 
  # view() %>% 
  glimpse()

# quick viz to see data distribution
hist(df_epi_hospital$age_hari_dc)
hist(df_epi_hospital$usia_bulan_dc)
hist(df_epi_hospital$usia_tahun_dc)

# usia all data
ggplot(df_epi_hospital %>% 
         group_by(group_age) %>% 
         summarise(count = n())
       ,
       aes(x = group_age, y = count, fill = group_age)) +
  geom_bar(stat = "identity") +
  # geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, size = 3) +
  # scale_y_log10() +
  # scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  # labs(x = " ", y = "Percentage", 
  #      # title = "All Serotypes"
  # ) +
  # scale_fill_manual(values = c(col_map)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = c(0.02, 0.75),
        legend.direction = "horizontal",
        legend.justification = c("left", "top"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        legend.margin = margin(t = -50),
        legend.spacing.y = unit(-0.3, "cm"))

# trial piechart
# piechart based on usia_tahun_dc
ggplot(df_epi_hospital %>% 
         mutate(usia_tahun_dc = as.character(round(usia_tahun_dc, 0))) %>% 
         group_by(usia_tahun_dc) %>% 
         summarise(count = n()) %>%
         mutate(prop = round(count/sum(count)*100, 1),
                usia_tahun_dc = factor(usia_tahun_dc,
                                       levels = c("0", "1", "2", "3", "4", "5",
                                                  "6", "7", "8", "9", "10",
                                                  "11", "12", "13", "14", "15",
                                                  "16", "17", "18", "19", "20"
                                       ))
                ) %>% 
         arrange(desc(usia_tahun_dc)) %>%
         mutate(label_coor = cumsum(prop)- 0.5*prop,
                )
       ,
       aes(x = "", y = prop, fill = usia_tahun_dc)) +
  geom_bar(stat = "identity", width = 1, , color="white") +
  coord_polar("y", start=0) +
  geom_text(aes(y = label_coor, label = paste0(prop, "%")), color = "grey10", size=3) +
  # scale_fill_brewer(palette="Set1") +
  guides(fill = guide_legend(ncol = 3)) +
  theme_void()

# piechart based on group_age
ggplot(df_epi_hospital %>% 
         group_by(group_age) %>% 
         summarise(count = n()) %>%
         mutate(prop = round(count/sum(count)*100, 1)) %>% 
         arrange(desc(group_age)) %>%
         mutate(label_coor = cumsum(prop)- 0.5*prop)
       ,
       aes(x = "", y = prop, fill = group_age)) +
  geom_bar(stat = "identity", width = 1, , color="white") +
  coord_polar("y", start=0) +
  geom_text(aes(y = label_coor, label = paste0(prop, "%")), color = "grey10", size=3) +
  scale_fill_brewer(palette="Set1") +
  theme_void()

# piechart based on group_age_vaccine
ggplot(df_epi_hospital %>% 
         group_by(group_age_vaccine) %>% 
         summarise(count = n()) %>%
         mutate(prop = round(count/sum(count)*100, 1)) %>% 
         arrange(desc(group_age_vaccine)) %>%
         mutate(label_coor = cumsum(prop)- 0.5*prop)
       ,
       aes(x = "", y = prop, fill = group_age_vaccine)) +
  geom_bar(stat = "identity", width = 1, , color="white") +
  coord_polar("y", start=0) +
  geom_text(aes(y = label_coor, label = paste0(prop, "%")), color = "grey10", size=3) +
  scale_fill_brewer(palette="Set1") +
  theme_void()

# seasonality based on group_age
ggplot(df_epi_hospital %>% 
         mutate(
           iso_week = paste0(year(tanggal_masuk_rs), "-W", sprintf("%02d", week(tanggal_masuk_rs)), "-1"),
           yearWeek =ISOweek::ISOweek2date(iso_week),
           
           yearMonth = floor_date(as.Date(tanggal_masuk_rs), unit = "month"),
         ) %>% 
         group_by(yearMonth, group_age
                  ) %>% 
         summarise(count = n())
       ,
       aes(x = yearMonth, y = count, fill = group_age)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  # scale_x_date(date_labels = "%Y", date_breaks = "1 year",
  #              limits = c(as.Date(min(df_epi_hospital$tanggal_masuk_rs)),
  #                         as.Date(max(df_epi_hospital$tanggal_masuk_rs)))
  # ) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month",
               limits = c(as.Date("2023-06-01"),
                          as.Date("2025-02-01"))
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "bottom", #c(0.02, 0.75),
        legend.direction = "horizontal",
        # legend.justification = c("left", "top"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        #legend.margin = margin(t = -50),
        legend.spacing.y = unit(-0.3, "cm"))

# usia grouped by area
ggplot(df_epi_hospital %>% 
         group_by(group_age_vaccine, area) %>% 
         summarise(count = n())
       ,
       aes(x = group_age_vaccine, y = count, fill = area)) +
  geom_bar(stat = "identity") +
  # geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, size = 3) +
  # scale_y_log10() +
  # scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  # labs(x = " ", y = "Percentage", 
  #      # title = "All Serotypes"
  # ) +
  # scale_fill_manual(values = c(col_map)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = c(0.02, 0.75),
        legend.direction = "horizontal",
        legend.justification = c("left", "top"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        legend.margin = margin(t = -50),
        legend.spacing.y = unit(-0.3, "cm"))

# usia grouped by pcv13 & area
ggplot(df_epi_hospital %>% 
         group_by(group_age_vaccine, area, jumlah_dosis_vaksin_pcv_13) %>% 
         summarise(count = n())
       ,
       aes(x = group_age_vaccine, y = count, fill = area)) +
  geom_bar(stat = "identity") +
  # geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, size = 3) +
  # scale_y_log10() +
  # scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  # labs(x = " ", y = "Percentage", 
  #      # title = "All Serotypes"
  # ) +
  # scale_fill_manual(values = c(col_map)) +
  theme_bw() +
  facet_grid(jumlah_dosis_vaksin_pcv_13 ~ area) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = c(0.02, 0.75),
        legend.direction = "horizontal",
        legend.justification = c("left", "top"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        legend.margin = margin(t = -50),
        legend.spacing.y = unit(-0.3, "cm"))

# usia grouped by bagaimanakah_prognosis_pasien
ggplot(df_epi_hospital %>% 
         group_by(group_age_vaccine, bagaimanakah_prognosis_pasien) %>% 
         summarise(count = n())
       ,
       aes(x = group_age_vaccine, y = count, fill = bagaimanakah_prognosis_pasien)) +
  geom_bar(stat = "identity") +
  # geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, size = 3) +
  # scale_y_log10() +
  # scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  # labs(x = " ", y = "Percentage", 
  #      # title = "All Serotypes"
  # ) +
  # scale_fill_manual(values = c(col_map)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "bottom", #c(0.02, 0.75),
        legend.direction = "horizontal",
        # legend.justification = c("left", "top"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        # legend.margin = margin(t = -50),
        legend.spacing.y = unit(-0.3, "cm"))


# usia grouped by pcv13 & bagaimanakah_prognosis_pasien
ggplot(df_epi_hospital %>% 
         filter(!is.na(bagaimanakah_prognosis_pasien),
                bagaimanakah_prognosis_pasien != ""
                ) %>% 
         group_by(group_age_vaccine, bagaimanakah_prognosis_pasien, jumlah_dosis_vaksin_pcv_13) %>% 
         summarise(count = n())
       ,
       aes(x = group_age_vaccine, y = count, fill = bagaimanakah_prognosis_pasien)) +
  geom_bar(stat = "identity") +
  # geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, size = 3) +
  # scale_y_log10() +
  # scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  # labs(x = " ", y = "Percentage", 
  #      # title = "All Serotypes"
  # ) +
  # scale_fill_manual(values = c(col_map)) +
  theme_bw() +
  facet_grid(jumlah_dosis_vaksin_pcv_13 ~ bagaimanakah_prognosis_pasien) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        # legend.position = c(-0.02, 0.75),
        legend.direction = "horizontal",
        # legend.justification = c("left", "top"),
        # legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        # legend.margin = margin(t = -50),
        # legend.spacing.y = unit(-0.3, "cm")
        )


# usia grouped by pcv13_group & bagaimanakah_prognosis_pasien
# count
ggplot(
  df_epi_hospital %>% 
    group_by(bagaimanakah_prognosis_pasien, pcv13_group, group_age_vaccine) %>% 
    summarise(count_vaccinated = n(), .groups = "drop") %>% 
    filter(
      !is.na(bagaimanakah_prognosis_pasien),
      bagaimanakah_prognosis_pasien != ""
    )
  ,
  aes(x = group_age_vaccine, y = count_vaccinated, fill = pcv13_group)
) +
  geom_bar(stat = "identity") +
  theme_bw() +
  facet_wrap(~ bagaimanakah_prognosis_pasien) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.direction = "horizontal",
    legend.title = element_blank()
  )

# chisq or fisher's exact test test for children below 5 or 2
contingency_table <- df_epi_hospital %>% 
  select(bagaimanakah_prognosis_pasien,
         group_age_vaccine,
         pcv13_group) %>% 
  mutate(
    bagaimanakah_prognosis_pasien = case_when(
      bagaimanakah_prognosis_pasien == "Memburuk (Rujuk luar)" | bagaimanakah_prognosis_pasien == "Memburuk (komplikasi/ sekuele)" ~ "Memburuk",
      TRUE ~ bagaimanakah_prognosis_pasien
    )
  ) %>% 
  filter(
    !is.na(bagaimanakah_prognosis_pasien),
    bagaimanakah_prognosis_pasien != "",
    group_age_vaccine == "0 years" | group_age_vaccine == "1-2 years" | group_age_vaccine == "3-5 years"
  ) %>%
  count(bagaimanakah_prognosis_pasien, pcv13_group) %>%
  pivot_wider(
    names_from = pcv13_group,
    values_from = n,
    values_fill = 0
  ) %>%
  column_to_rownames("bagaimanakah_prognosis_pasien") %>%
  # glimpse() %>% 
  as.matrix()

contingency_table

# Step 2: Run Fisher's Exact Test
fisher.test(contingency_table)


# proportion
ggplot(df_epi_hospital %>% 
         group_by(bagaimanakah_prognosis_pasien, pcv13_group, group_age_vaccine) %>% 
         summarise(count_vaccinated = n()) %>% 
         left_join(
           df_epi_hospital %>% 
             group_by(bagaimanakah_prognosis_pasien, group_age_vaccine) %>% 
             summarise(count_prognosis = n())
           ,
           by = c("group_age_vaccine", "bagaimanakah_prognosis_pasien")
         ) %>% 
         mutate(prop = round(count_vaccinated/count_prognosis*100, 1)) %>% 
         filter(!is.na(bagaimanakah_prognosis_pasien),
                       bagaimanakah_prognosis_pasien != "",
         )
       ,
       aes(x = group_age_vaccine, y = prop, fill = pcv13_group)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  facet_wrap(~ bagaimanakah_prognosis_pasien) +
  # facet_grid(pcv13_group ~ bagaimanakah_prognosis_pasien) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        # legend.position = c(-0.02, 0.75),
        legend.direction = "horizontal",
        # legend.justification = c("left", "top"),
        # legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        # legend.margin = margin(t = -50),
        # legend.spacing.y = unit(-0.3, "cm")
  )


################################################################################
# Combinations analysis
# source: https://www.epirhandbook.com/en/new_pages/combination_analysis.html
# library(UpSetR)
# library(ggupset)
# 
# # trial ggupset (better to use UpSetR)
# early_diagnosis <- df_epi_hospital %>% 
#   dplyr::mutate(
#     bacteremia = ifelse(diagnosis_bakteremia == 1, "bacteremia", NA),
#     pneumonia = ifelse(diagnosis_pneumonia == 1, "pneumonia", NA),
#     meningitis = ifelse(diagnosis_meningitis == 1, "meningitis", NA),
#     pleural_effusion = ifelse(diagnosis_efusi_pleura == 1, "pleural effusion", NA),
#     # diagnosis_tambahan_saat_masuk = ifelse(!is.na(diagnosis_tambahan_saat_masuk), "others", NA),
#   ) %>% 
#   tidyr::unite(
#     col = "early_diagnosis",
#     c(bacteremia, pneumonia, meningitis, pleural_effusion),
#     sep = "; ",
#     remove = T,
#     na.rm = T
#   ) %>% 
#   dplyr::mutate(
#     early_diagnosis_list = as.list(strsplit(early_diagnosis, "; "))
#   ) %>% 
#   glimpse()
#   
# ggplot(early_diagnosis,
#   mapping = aes(x = early_diagnosis_list)) +
#   geom_bar() +
#   ggupset::scale_x_upset(
#     reverse = FALSE,
#     n_intersections = 15, # 15 combinations; C(n,k)= binom(n, k)= fracnk(n−k)
#     sets = c("bacteremia", "pneumonia", "pleural_effusion"))+
#   labs(
#     title = "Signs & symptoms",
#     subtitle = "10 most frequent combinations of signs and symptoms",
#     caption = "Caption here.",
#     x = "Symptom combination",
#     y = "Frequency in dataset")

# trial UpSetR
df_epi_hospital %>% 
  dplyr::mutate(
    # have already in 1 and 0
    bacteremia = diagnosis_bakteremia,
    pneumonia = diagnosis_pneumonia,
    meningitis = diagnosis_meningitis,
    "pleural effusion" = diagnosis_efusi_pleura,
    # diagnosis_tambahan_saat_masuk = ifelse(!is.na(diagnosis_tambahan_saat_masuk), 1, 0),
  ) %>% 
  UpSetR::upset(
    sets = c("bacteremia", "pneumonia", "meningitis", "pleural effusion"),
    order.by = "freq",
    empty.intersections = "on",
    number.angles = 0,
    point.size = 3.5,
    line.size = 2, 
    mainbar.y.label = "Symptoms Combinations",
    sets.x.label = "Patients with Symptom")


# trial UpSetR for pneumonia symptoms
df_epi_hospital %>% 
  dplyr::select(contains("manakah_tanda_tanda_pneumonia_yang_ditemukan_")) %>% 
  dplyr::rename_with(~ str_remove(., "manakah_tanda_tanda_pneumonia_yang_ditemukan_")
  ) %>% 
  dplyr::transmute(
    cough = batuk,
    `rapid breathing according to age` = nafas_cepat_menurut_usia,
    `chest wall retraction` = tarikan_dinding_dada,
    `central cyanosis` = sianosis_sentral,
    grunting = merintih,
    `unable to drink` = tidak_dapat_minum,
    lethargy = letargi,
    `decreased consciousness` = penurunan_kesadaran,
    seizures = kejang,
    `pleurisy (chest pain)` = pleuritis_nyeri_dada
  ) %>%
  dplyr::mutate(across(everything(), as.numeric)) %>%
  as.data.frame() %>% 
  UpSetR::upset(
    sets = c("cough",
             "rapid breathing according to age",
             "chest wall retraction",
             "central cyanosis",
             "grunting",
             "unable to drink",
             "lethargy",
             "decreased consciousness",
             "seizures",
             "pleurisy (chest pain)"
             ),
    order.by = "freq",
    empty.intersections = "off",
    number.angles = 0,
    point.size = 3.5,
    line.size = 2, 
    mainbar.y.label = "Symptoms Combinations",
    sets.x.label = "Patients with Symptom")


# trial UpSetR for antibiotics usage
df_epi_hospital %>% 
  dplyr::select(contains("jika_ya_manakah_antibiotik_di_bawah_ini_yang_diberikan_")) %>% 
  dplyr::rename_with(~ str_remove(., "jika_ya_manakah_antibiotik_di_bawah_ini_yang_diberikan_")
  ) %>% 
  dplyr::transmute(
    # have already in 1 and 0
    "amoxicillin" = amoksisilin,
    "amoxicillin-clavulanic acid" = amoksisilin_as_klavulanat,
    "ampicillin" = ampisilin,
    "ampicillin-sulbactam" = ampisilin_sulbaktam,
    "azithromycin" = azitromisin,
    "cefotaxime" = sefotaksim,
    "ceftriaxone" = seftriakson,
    "erythromycin" = eritromisin,
    "trimethoprim-sulfamethoxazole" = trimetoprim_sulfametoksazol,
    "other" = lainnya
    
  ) %>% 
  # need to be discussed: NA in antibiotics usage
  dplyr::filter(!is.na(amoxicillin)
                ) %>% 
  dplyr::mutate(across(everything(), as.numeric)) %>%
  as.data.frame() %>% 
  UpSetR::upset(
    sets = c("amoxicillin",
             "amoxicillin-clavulanic acid",
             "ampicillin",
             "ampicillin-sulbactam",
             "azithromycin",
             "cefotaxime",
             "ceftriaxone",
             "erythromycin",
             "trimethoprim-sulfamethoxazole",
             "other"
    ),
    order.by = "freq",
    empty.intersections = "off",
    number.angles = 0,
    point.size = 3.5,
    line.size = 2, 
    mainbar.y.label = "Antibiotics Combinations",
    sets.x.label = "Patients with Antibiotics")

# trial UpSetR for FULL antibiotics usage



# trial UpSetR for other chronic conditions
# I ID from Sorong got diabetes while born premature (?)
df_epi_hospital %>% 
  dplyr::select(contains("diantara_daftar_nama_penyakit_di_bawah_ini_manakah_penyakit_medis_atau_kondisi_kronik_yang_mendasari_pasien_")) %>% 
  dplyr::rename_with(~ str_remove(., "diantara_daftar_nama_penyakit_di_bawah_ini_manakah_penyakit_medis_atau_kondisi_kronik_yang_mendasari_pasien_")
  ) %>% 
  dplyr::mutate(
    # have already in 1 and 0
    "heart" = jantung,
    "lung(s)" = paru,
    "liver" = hati,
    "kidney(s)" = ginjal,
    "prematurity" = prematuritas,
    "immunodeficiency" = imunodefisiensi,
    "systemic steroid use" = penggunaan_streoid_sistemik,
    "neoplastic disease (cancer)" = penyakit_neoplastik_kanker,
    "reactive airway disease (asthma)" = penyakit_saluran_nafas_reaktif_asma,
    "diabetes" = diabetes,
    "sickle cell thalassemia" = sickle_cell_thalasemia,
    "hemoglobinopathy" = hemoglobinopati,
    "leukemia, lymphoma, other blood malignancies" = leukimia_limfoma_keganasan_darah_lainnya,
    "no history of diseases mentioned above" = tidak_ada_riwayat_penyakit_yang_disebut_diatas
    
  ) %>% 
  UpSetR::upset(
    sets = c("heart",
             "lung(s)",
             "liver",
             "kidney(s)",
             "prematurity",
             "immunodeficiency",
             "systemic steroid use",
             "neoplastic disease (cancer)",
             "reactive airway disease (asthma)",
             "diabetes",
             "sickle cell thalassemia",
             "hemoglobinopathy",
             "leukemia, lymphoma, other blood malignancies"
             #"no history of diseases mentioned above"
    ),
    order.by = "freq",
    empty.intersections = "on",
    number.angles = 0,
    point.size = 3.5,
    line.size = 2, 
    mainbar.y.label = "Chronic Condition Combinations",
    sets.x.label = "Patients with Chronic Conditions")



