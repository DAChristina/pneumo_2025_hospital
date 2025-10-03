library(tidyverse)

# Data cleaning process for hospital data ######################################
df_epi_hospital <- readxl::read_excel("raw_data/temporary_df_epi_hospital_combine_row_cleaned_22Sept2025.xlsx") %>% 
  janitor::clean_names() %>% 
  detoxdats::lowerval_except(., exclude = c("no_id_subjek")) %>%
  dplyr::mutate(
    id = gsub("[ -]", "_", no_id_subjek), # instead of using " |-")
    ) %>% 
  glimpse()


# Detect duplicated IDs; 12 IDs are duplicated but they seems come from different people
report_duplicated_id <- detoxdats::report_duplicate(df = df_epi_hospital,
                                                    column_id = id,
                                                    other_iden = tanggal_lahir_pasien) %>% 
  # view() %>% 
  glimpse()

# test unique values
report_unique_values <- detoxdats::report_uniqval(df = df_epi_hospital) %>%
  # view() %>% 
  glimpse()


# clean up interesting columns #################################################
# including english translation
df_epi_clean <- df_epi_hospital %>% 
  dplyr::transmute(
    id = id,
    area = area,
    hospital = asal_rumah_sakit,
    start = start,
    end = end,
    
    # additional data for descriptive analysis
    patient_room_origin = asal_ruangan_pasien,
    patient_care_type = jenis_perawatan,
    patient_gender = jenis_kelamin_pasien,
    patient_surgery = apakah_dilakukan_pembedahan_selama_di_rawat,
    patient_care_unit = asal_rumah_sakit,
    
    # prognosis
    patient_prognosis = case_when(
      bagaimanakah_prognosis_pasien == "sembuh" ~ "recovered",
      bagaimanakah_prognosis_pasien == "membaik" ~ "improving",
      bagaimanakah_prognosis_pasien == "memburuk (rujuk luar)" | bagaimanakah_prognosis_pasien == "memburuk (komplikasi/ sekuele)" ~ "worsened",
      bagaimanakah_prognosis_pasien == "meninggal" ~ "deceased",
      TRUE ~ bagaimanakah_prognosis_pasien
    ),
    patient_prognosis = factor(patient_prognosis,
                               levels = c("recovered", "improving", "worsened", "deceased")
                               ), # recovered is baseline
    
    # epidemiological data
    epi_daycare = apakah_anak_berada_di_tempat_penitipan_anak,
    epi_household_size = berapa_banyak_orang_termasuk_pasien_yang_tinggal_di_rumah_yang_ditempati_pasien,
    epi_household_children_under5 = berapa_banyak_anak_berusia_5_tahun_yang_tinggal_satu_rumah_dengan_pasien_jika_tidak_ada_jawab_0,
    epi_household_smoker = apakah_ada_orang_di_rumah_yang_merokok,
    
    age_month = round(usia_bulan_dc, 0),
    age_year = as.numeric(round(usia_tahun_dc, 0)),
    # some age grouping
    age_group_1semester = case_when(
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
    age_group_1semester = factor(age_group_1semester,
                                 levels = c("0 months",
                                            "1-6 months",
                                            "7-12 months",
                                            "13-18 months",
                                            "19-24 months",
                                            "2-5 years",
                                            "6-10 years",
                                            "10+ years"
                                 )),
    age_group_2vaccine = case_when(
      usia_tahun_dc < 1 ~ "0 years",
      usia_tahun_dc >= 1 & usia_tahun_dc < 3 ~ "1-2 years",
      usia_tahun_dc >= 3 & usia_tahun_dc < 6 ~ "3-5 years",
      usia_tahun_dc >= 6 & usia_tahun_dc < 11 ~ "6-10 years",
      usia_tahun_dc >= 11 ~ "11-18 years",
      
    ),
    age_group_2vaccine = factor(age_group_2vaccine,
                                levels = c("0 years",
                                           "1-2 years",
                                           "3-5 years",
                                           "6-10 years",
                                           "11-18 years"
                                )),
    
    LOS_days = lama_di_rs_hari_dc,
    
    # pneumonia diagnosis
    diag_1xray_infiltration = jika_mengarah_ke_pneumonia_pilih_salah_satu_dari_pilihan_gambaran_di_bawah_ini_infiltrat,
    diag_1xray_consolidation = jika_mengarah_ke_pneumonia_pilih_salah_satu_dari_pilihan_gambaran_di_bawah_ini_konsolidasi,
    diag_1xray_effusion = jika_mengarah_ke_pneumonia_pilih_salah_satu_dari_pilihan_gambaran_di_bawah_ini_efusi,
    
    diag_2signs_bacteraemia = diagnosis_bakteremia,
    diag_2signs_pneumonia = diagnosis_pneumonia,
    diag_2signs_meningitis = diagnosis_meningitis,
    diag_2signs_pleural_effusion = diagnosis_efusi_pleura,
    diag_2signs_cough = manakah_tanda_tanda_pneumonia_yang_ditemukan_batuk,
    diag_2signs_tachypnea = manakah_tanda_tanda_pneumonia_yang_ditemukan_nafas_cepat_menurut_usia,
    diag_2signs_chest_in_drawing = manakah_tanda_tanda_pneumonia_yang_ditemukan_tarikan_dinding_dada,
    diag_2signs_central_cyanosis = manakah_tanda_tanda_pneumonia_yang_ditemukan_sianosis_sentral,
    diag_2signs_grunting = manakah_tanda_tanda_pneumonia_yang_ditemukan_merintih,
    diag_2signs_cannot_drink = manakah_tanda_tanda_pneumonia_yang_ditemukan_tidak_dapat_minum,
    diag_2signs_lethargy = manakah_tanda_tanda_pneumonia_yang_ditemukan_letargi,
    diag_2signs_decreased_consciousness = manakah_tanda_tanda_pneumonia_yang_ditemukan_penurunan_kesadaran,
    diag_2signs_seizures = manakah_tanda_tanda_pneumonia_yang_ditemukan_kejang,
    diag_2signs_pleuritis_chest_pain = manakah_tanda_tanda_pneumonia_yang_ditemukan_pleuritis_nyeri_dada,
    
    diag_3chronic_heart_disease = diantara_daftar_nama_penyakit_di_bawah_ini_manakah_penyakit_medis_atau_kondisi_kronik_yang_mendasari_pasien_jantung,
    diag_3chronic_lung_disease = diantara_daftar_nama_penyakit_di_bawah_ini_manakah_penyakit_medis_atau_kondisi_kronik_yang_mendasari_pasien_paru,
    diag_3chronic_liver_disease = diantara_daftar_nama_penyakit_di_bawah_ini_manakah_penyakit_medis_atau_kondisi_kronik_yang_mendasari_pasien_hati,
    diag_3chronic_kidney_disease = diantara_daftar_nama_penyakit_di_bawah_ini_manakah_penyakit_medis_atau_kondisi_kronik_yang_mendasari_pasien_ginjal,
    diag_3chronic_prematurity = diantara_daftar_nama_penyakit_di_bawah_ini_manakah_penyakit_medis_atau_kondisi_kronik_yang_mendasari_pasien_prematuritas,
    diag_3chronic_immunodeficiency = diantara_daftar_nama_penyakit_di_bawah_ini_manakah_penyakit_medis_atau_kondisi_kronik_yang_mendasari_pasien_imunodefisiensi,
    diag_3chronic_systemic_steroid_use = diantara_daftar_nama_penyakit_di_bawah_ini_manakah_penyakit_medis_atau_kondisi_kronik_yang_mendasari_pasien_penggunaan_streoid_sistemik,
    diag_3chronic_malignancy = diantara_daftar_nama_penyakit_di_bawah_ini_manakah_penyakit_medis_atau_kondisi_kronik_yang_mendasari_pasien_penyakit_neoplastik_kanker,
    diag_3chronic_reactive_airway_disease = diantara_daftar_nama_penyakit_di_bawah_ini_manakah_penyakit_medis_atau_kondisi_kronik_yang_mendasari_pasien_penyakit_saluran_nafas_reaktif_asma,
    diag_3chronic_diabetes = diantara_daftar_nama_penyakit_di_bawah_ini_manakah_penyakit_medis_atau_kondisi_kronik_yang_mendasari_pasien_diabetes,
    diag_3chronic_sickle_cell_thalassemia = diantara_daftar_nama_penyakit_di_bawah_ini_manakah_penyakit_medis_atau_kondisi_kronik_yang_mendasari_pasien_sickle_cell_thalasemia,
    diag_3chronic_hemoglobinopathy = diantara_daftar_nama_penyakit_di_bawah_ini_manakah_penyakit_medis_atau_kondisi_kronik_yang_mendasari_pasien_hemoglobinopati,
    diag_3chronic_leukemia_lymphoma_other_blood_cancer = diantara_daftar_nama_penyakit_di_bawah_ini_manakah_penyakit_medis_atau_kondisi_kronik_yang_mendasari_pasien_leukimia_limfoma_keganasan_darah_lainnya,
    diag_3chronic_asplenia = diantara_daftar_nama_penyakit_di_bawah_ini_manakah_penyakit_medis_atau_kondisi_kronik_yang_mendasari_pasien_asplenia_anatomis_fungsional,
    
    # antibiotics consumption
    abx_amoxicillin = jika_ya_manakah_antibiotik_di_bawah_ini_yang_diberikan_amoksisilin,
    abx_amox_clav = jika_ya_manakah_antibiotik_di_bawah_ini_yang_diberikan_amoksisilin_as_klavulanat,
    abx_ampicillin = jika_ya_manakah_antibiotik_di_bawah_ini_yang_diberikan_ampisilin,
    abx_ampicillin_sulbactam = jika_ya_manakah_antibiotik_di_bawah_ini_yang_diberikan_ampisilin_sulbaktam,
    abx_azithromycin = jika_ya_manakah_antibiotik_di_bawah_ini_yang_diberikan_azitromisin,
    abx_cefotaxime = jika_ya_manakah_antibiotik_di_bawah_ini_yang_diberikan_sefotaksim,
    abx_ceftriaxone = jika_ya_manakah_antibiotik_di_bawah_ini_yang_diberikan_seftriakson,
    abx_erythromycin = jika_ya_manakah_antibiotik_di_bawah_ini_yang_diberikan_eritromisin,
    abx_trimeth_sulfamethoxazole = jika_ya_manakah_antibiotik_di_bawah_ini_yang_diberikan_trimetoprim_sulfametoksazol,
    # other antibiotics will be combined below
    
    # vaccination status
    vacc_pcv13_status = apakah_pasien_telah_menerima_vaksin_pcv13_sebelumnya,
    vacc_pcv13_doses = case_when(
      is.na(jumlah_dosis_vaksin_pcv_13) ~ 0,
      TRUE ~ jumlah_dosis_vaksin_pcv_13
    ),
    
    # immunised vs. unimmunised
    vacc_pcv13_group = case_when(
      is.na(vacc_pcv13_doses) ~ "unimmunised",
      TRUE ~ "immunised"
    ),
    
    # samples (focused on bacteria species and fungi; to be continued)
    sample_blood_bacteria_day = jika_ya_jelaskan_positif_pada_hari_keberapa, # some data are in dates
    sample_blood_bacteria = jika_bukan_bakteri_streptococcus_pneumoniae_jelaskan_jenis_bakterinya,
    sample_blood_yeast = hasil_pengecatan_gram_yeast,
    sample_blood_hyphae = hasil_pengecatan_gram_tidak_ditemukan_bentukan_kuman_yeast_hifa,
    
    # sample_sputum_bacteria_day = jika_ya_jelaskan_positif_pada_hari_keberapa, # not available
    sample_sputum_bacteria = jika_bukan_bakteri_streptococcus_pneumoniae_tuliskan_bakterinya,
    sample_sputum_yeast =  hasil_pengecatan_gram_yeast13,
    sample_sputum_hyphae = hasil_pengecatan_gram_hifa,
    
    sample_lcs_bacteria_day = jika_ya_tuliskan_pada_hari_keberapa_kah_baktek_positif,
    sample_lcs_bacteria = jika_bukan_bakteri_streptococcus_pneumoniae_tuliskan_bakteri_nya,
    sample_lcs_yeast = hasil_pengecatan_gram_yeast21,
    sample_lcs_hyphae = hasil_pengecatan_gram_hifa22,
    
    sample_pleural_bacteria_day = jika_ya_tuliskan_pada_hari_keberapa_kah_baktek_positif_34,
    sample_pleural_bacteria = jika_bukan_bakteri_streptococcus_pneumoniae_tuliskan_bakteri_nya_2,
    sample_pleural_yeast = hasil_pengecatan_gram_yeast30,
    sample_pleural_hyphae = hasil_pengecatan_gram_hifa31,
    
    
    # np_swab_taken = apakah_dilakukan_pengambilan_np_swab,
    
    
    # numerical data
    vital_oxygen_saturation = saturasi_oksigen_jika_tidak_dilakukan_tuliskan_tidak_dilakukan_jika_dilakukan_tuliskan_berupa_angka_yang_tertera_di_oxymeter_contoh_90,
    vital_respiratory_rate = laju_pernafasan_x_mnt,
    vital_pulse_rate = denyut_nadi_x_mnt,
    vital_axillary_temp = suhu_ketiak_derajat_celcius,
    
    # clarification needed:
    count_leucocyte = nilai_leukosit_u_l, # some have > 30x10e3/uL
    count_NLR = nilai_hitung_jenis_neutrofil_batang_dan_segmen_percent/nilai_hitung_jenis_limfosit_percent, # extreme values when NLR < 1 or NLR > 20
    
    # diag_WHO_pneumonia = x
  ) %>% 
  glimpse()

# data cleaning framework:
# https://bmjopen.bmj.com/content/12/6/e057957.abstract

# clean up multiple values in: #################################################
# 1. other antibiotics
# 2. bacteria & antimicrobial assessment for:
# 2.1. blood,
# 2.2. sputum,
# 2.3. lcs and
# 2.4. pleural

# 1. other antibiotics #########################################################
# list of all other antibiotics
name_abx <- c(
  "amikasin" = "amikacin",
  "ainophylin" = "aminophylline",
  "aminophylin" = "aminophylline",
  "aninophylin" = "aminophylline",
  
  "cefazoline" = "cefazolin",
  "cefixim" = "cefixime",
  "cefixime oral" = "cefixime",
  "cefobactam" = "cefoperazone_sulbactam", # cefoperazone + sulbactam
  "cefoperazone" = "cefoperazone_sulbactam", # different but no individual cefoperazone detected
  "cefoperazol sulbactam" = "cefoperazone_sulbactam",
  "cefoperazol" = "cefoperazone_sulbactam",
  "cefotaxim" = "cefotaxime",
  "ceftadizime" = "ceftazidime",
  "kotrimoxazol" = "cotrimoxazole",
  
  "genta" = "gentamicin",
  "gentamicyn" = "gentamicin",
  "gentamisin" = "gentamicin",
  "gentamycin" = "gentamicin",
  "gentanycin" = "gentamicin",
  "levoflixacin" = "levofloxacin",
  "meropenem injeksi" = "meropenem",
  "metrinidazole" = "metronidazole",
  "metro" = "metronidazole",
  
  "oat (rifampisin, inh, pirazinamid, etambutol)" = "OAT (Rifampicin, Isoniazid, Pyrazinamide, Ethambutol)",
  "rifampisin" = "rifampicin",
  "inh" = "isoniazid",
  "pirazinamid" = "pyrazinamide",
  "etambutol" = "ethambutol",
  
  "vancomicin" = "vancomycin",
  "vicillin" = "vicilin"
)

weird_abx <- c("oral", "injeksi", "dan",
               "oat",
               "sulbactam", "sulbaktam"
)

cleaned_other_abx <- df_epi_hospital %>%
  dplyr::rename(other_antibiotics = jika_lainnya_sebutkan_nama_antibiotik_nya) %>%
  dplyr::rowwise() %>% # crucial!
  dplyr::mutate(other_antibiotics_cleaned = detoxdats::detox_multitext(text = other_antibiotics,
                                                                       dict = name_abx,
                                                                       exclude = weird_abx)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    no_id_subjek,
    other_antibiotics,
    other_antibiotics_cleaned
  ) %>%
  # view() %>%
  glimpse()

all_unique_abx <- cleaned_other_abx %>%
  dplyr::pull(other_antibiotics_cleaned) %>%
  stringr::str_split(", ") %>%
  unlist() %>%
  unique() %>%
  sort() %>% 
  # view() %>% 
  glimpse()

df_epi_clean <- df_epi_clean %>% 
  left_join(
    cleaned_other_abx %>%
      dplyr::select(-other_antibiotics) %>% 
      dplyr::filter(!is.na(other_antibiotics_cleaned)) %>%
      tidyr::separate_rows(other_antibiotics_cleaned, sep = ", ") %>%
      dplyr::mutate(value = as.numeric(1)) %>%
      tidyr::pivot_wider(
        id_cols = no_id_subjek,
        names_from = other_antibiotics_cleaned,
        names_prefix = "abx_",
        values_from = value,
        values_fill = 0,
        values_fn = length
      ) %>%
      dplyr::mutate(
        across(where(is.list), ~ as.numeric(.)),
        # across(where(is.list), ~ ifelse(map_lgl(., is.null), 0, .)),
        across(where(is.numeric), ~ replace_na(., 0))
      )
    ,
    by = c("id" = "no_id_subjek")
  ) %>% 
  glimpse()












