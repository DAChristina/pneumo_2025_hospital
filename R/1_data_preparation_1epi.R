library(tidyverse)

# Data cleaning process for hospital data ######################################
df_epi_hospital <- readxl::read_excel("raw_data/temporary_df_epi_hospital_combine_row_cleaned_22Sept2025_Sorong_duplicatedID_cleaned.xlsx") %>% 
  janitor::clean_names() %>% 
  detoxdats::lowerval_except(., exclude = c("no_id_subjek",
                                            "asal_rumah_sakit",
                                            "asal_ruangan_pasien")) %>%
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
    # fix hospital name
    hospital = case_when(
      asal_rumah_sakit == "RSUD Sorong" ~ "RSUD JP Wanane",
      asal_rumah_sakit == "RSUD Provinsi NTB" ~ "RSUD Provinsi NTB",
      asal_rumah_sakit == "RSUP Prof. Dr. Kandau Manado" ~ "RSUP Prof. Dr. R.D. Kandou",
      TRUE ~ NA
    ),
    
    # dates
    date_start = start,
    date_end = end,
    date_hospitalised = tanggal_masuk_rs,
    date_hospitalised_season = case_when(
      as.numeric(format(as.Date(date_hospitalised), "%m")) >= 4 &
        as.numeric(format(as.Date(date_hospitalised), "%m")) < 10
      ~ "Dry",
      TRUE ~ "Rainy"
    ),
    
    # additional data for descriptive analysis
    patient_room_origin = case_when(
      asal_ruangan_pasien == "Ruang rawat inap anak" ~ "Paediatric inpatient room",
      TRUE ~ asal_ruangan_pasien
      ),
    patient_care_type = jenis_perawatan,
    patient_gender = case_when(
      jenis_kelamin_pasien == "laki-laki" ~ "male",
      jenis_kelamin_pasien == "perempuan" ~ "female"
      ),
    # patient_weight = berat_dc, # banyak yang kosong
    # patient_height = tinggi_dc, # banyak yang kosong
    patient_surgery = apakah_dilakukan_pembedahan_selama_di_rawat,
    patient_care_unit = asal_rumah_sakit,
    
    # samples availability
    sample_blood_id = bila_ya_tuliskan_kode_spesimen,
    sample_sputum_id = bila_ya_tuliskan_kode_spesimen7,
    sample_csf_id = jika_ya_tuliskan_kode_spesimen,
    sample_pleural_id = id_spesimen_pleura_dc, # need confirmation
    sample_np_id = jika_ya_tuliskan_tanggal_pengambilan_spesimennya,
    
    # prognosis
    patient_prognosis = case_when(
      bagaimanakah_prognosis_pasien == "sembuh" ~ "recovered",
      bagaimanakah_prognosis_pasien == "membaik" ~ "improving",
      bagaimanakah_prognosis_pasien == "memburuk (rujuk luar)" | 
        bagaimanakah_prognosis_pasien == "memburuk (komplikasi/ sekuele)" 
      ~ "worsened",
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
      usia_bulan_dc < 2 ~ "<2 months",
      usia_bulan_dc >= 2 & usia_bulan_dc < 60 ~ "2-59 months",
      usia_bulan_dc >= 60 ~ "5-18 years",
      
    ),
    age_group_2vaccine = factor(age_group_2vaccine,
                                levels = c("<2 months",
                                           "2-59 months",
                                           "5-18 years"
                                )),
    
    # age_group_3pneumoFramework = case_when(
    #   "0-1 mo (Neonatal)"
    #   "2-59 mo (WHO)"
    #   ">5 yr"
    # ),
    
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
    diag_2signs_fever = ifelse(suhu_ketiak_derajat_celcius >= 37.5, 1, 0),
    # diag_2signs_tachypnea = manakah_tanda_tanda_pneumonia_yang_ditemukan_nafas_cepat_menurut_usia,
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
    # diag_3chronic_prematurity = diantara_daftar_nama_penyakit_di_bawah_ini_manakah_penyakit_medis_atau_kondisi_kronik_yang_mendasari_pasien_prematuritas,
    diag_3chronic_immunodeficiency = diantara_daftar_nama_penyakit_di_bawah_ini_manakah_penyakit_medis_atau_kondisi_kronik_yang_mendasari_pasien_imunodefisiensi,
    diag_3chronic_systemic_steroid_use = diantara_daftar_nama_penyakit_di_bawah_ini_manakah_penyakit_medis_atau_kondisi_kronik_yang_mendasari_pasien_penggunaan_streoid_sistemik,
    diag_3chronic_malignancy = diantara_daftar_nama_penyakit_di_bawah_ini_manakah_penyakit_medis_atau_kondisi_kronik_yang_mendasari_pasien_penyakit_neoplastik_kanker,
    diag_3chronic_reactive_airway_disease = diantara_daftar_nama_penyakit_di_bawah_ini_manakah_penyakit_medis_atau_kondisi_kronik_yang_mendasari_pasien_penyakit_saluran_nafas_reaktif_asma,
    diag_3chronic_diabetes = diantara_daftar_nama_penyakit_di_bawah_ini_manakah_penyakit_medis_atau_kondisi_kronik_yang_mendasari_pasien_diabetes,
    diag_3chronic_sickle_cell_thalassemia = diantara_daftar_nama_penyakit_di_bawah_ini_manakah_penyakit_medis_atau_kondisi_kronik_yang_mendasari_pasien_sickle_cell_thalasemia,
    diag_3chronic_hemoglobinopathy = diantara_daftar_nama_penyakit_di_bawah_ini_manakah_penyakit_medis_atau_kondisi_kronik_yang_mendasari_pasien_hemoglobinopati,
    diag_3chronic_leukemia_lymphoma_other_blood_cancer = diantara_daftar_nama_penyakit_di_bawah_ini_manakah_penyakit_medis_atau_kondisi_kronik_yang_mendasari_pasien_leukimia_limfoma_keganasan_darah_lainnya,
    diag_3chronic_asplenia = diantara_daftar_nama_penyakit_di_bawah_ini_manakah_penyakit_medis_atau_kondisi_kronik_yang_mendasari_pasien_asplenia_anatomis_fungsional,
    
    # numerical data
    vital_oxygen_saturation = saturasi_oksigen_jika_tidak_dilakukan_tuliskan_tidak_dilakukan_jika_dilakukan_tuliskan_berupa_angka_yang_tertera_di_oxymeter_contoh_90,
    vital_respiratory_rate = laju_pernafasan_x_mnt,
    vital_pulse_rate = denyut_nadi_x_mnt,
    vital_axillary_temp = suhu_ketiak_derajat_celcius,
    
    # clarification needed:
    count_leucocyte = nilai_leukosit_u_l, # some have >30x10e3/uL
    count_NLR = nilai_hitung_jenis_neutrofil_batang_dan_segmen_percent/nilai_hitung_jenis_limfosit_percent, # extreme values when NLR < 1 or NLR > 20
    
    # redefine tachypnea from age_month
    diag_2signs_tachypnea = case_when(
      age_month >= 0 & age_month < 2 & vital_respiratory_rate > 60 ~ 1,
      age_month >= 2 & age_month < 12 & vital_respiratory_rate > 50 ~ 1,
      age_month >= 12 & age_month < 60 & vital_respiratory_rate > 40 ~ 1,
      age_month >= 60 & vital_respiratory_rate >= 30 ~ 1,  # ATS adult threshold
      TRUE ~ 0
    ),
    
    # redefine prematurity based on months
    diag_3chronic_prematurity = case_when(
      diantara_daftar_nama_penyakit_di_bawah_ini_manakah_penyakit_medis_atau_kondisi_kronik_yang_mendasari_pasien_prematuritas == 1  & age_month < 37 ~ 1,
      TRUE ~ 0
    ),
    
    diag_4pneumo_final = case_when(
      age_month >= 0 & age_month < 2 &
        ( # x-ray is holy grail for < 2 mo; not all IDs have x-ray data
          diag_1xray_infiltration == 1 | is.na(diag_1xray_infiltration) |
            diag_1xray_consolidation == 1 | is.na(diag_1xray_consolidation) |
            diag_1xray_effusion == 1 | is.na(diag_1xray_effusion)) |
        (diag_2signs_tachypnea == 1 |
           diag_2signs_grunting == 1 |
           diag_2signs_cannot_drink == 1 |
           diag_2signs_lethargy == 1 |
           diag_2signs_cough == 1
        ) ~ 1,
      
      age_month >= 2 & age_month < 60 &
        (diag_2signs_cough == 1) &
        # not all children have x-rays
        (diag_1xray_infiltration == 1 |
           diag_1xray_consolidation == 1 |
           diag_1xray_effusion == 1 |
           diag_2signs_tachypnea == 1 |
           diag_2signs_grunting == 1 |
           diag_2signs_chest_in_drawing == 1 |
           
           diag_2signs_central_cyanosis == 1) ~ 1,
      
      age_month >= 60 &
        (diag_2signs_cough == 1 |
           diag_2signs_pleuritis_chest_pain == 1 |
           diag_2signs_bacteraemia == 1 |
           diag_2signs_pneumonia == 1) &
        (diag_1xray_infiltration == 1 |
           diag_1xray_consolidation == 1 |
           diag_1xray_effusion == 1 |
           vital_respiratory_rate >= 30 |
           vital_oxygen_saturation < 92 |
           diag_2signs_decreased_consciousness == 1 |
           diag_2signs_bacteraemia == 1
        ) ~ 1,
      
      TRUE ~ 0
    ),

    # diag_4pneumo_severity = case_when(
    #   age_month < 2 &
    #     (diag_1xray_infiltration == 1 |
    #        diag_1xray_consolidation == 1 |
    #        diag_2signs_pneumonia == 1
    #     ) ~ "Severe (Neonate)",
    #   age_month >= 2 & age_month < 60 &
    #     (diag_2signs_cough == 1 | diag_2signs_pneumonia == 1) &
    #     (diag_2signs_tachypnea == 1 |
    #        diag_1xray_infiltration == 1 | 
    #        diag_1xray_consolidation == 1 |
    #        diag_1xray_effusion == 1) &
    #     (diag_2signs_chest_in_drawing != 1 &
    #        diag_2signs_grunting != 1 &
    #        diag_2signs_central_cyanosis != 1 &
    #        diag_2signs_cannot_drink != 1 &
    #        diag_2signs_lethargy != 1 &
    #        diag_2signs_bacteraemia != 1 &
    #        diag_2signs_meningitis != 1 &
    #        diag_2signs_decreased_consciousness != 1 &
    #        diag_2signs_seizures != 1
    #     ) ~ "Pneumonia (Non-severe, WHO)",
    #   age_month >= 2 & age_month < 60 &
    #     (diag_2signs_cough == 1 | diag_2signs_pneumonia == 1) &
    #     (diag_2signs_chest_in_drawing == 1 |
    #        diag_2signs_grunting == 1 |
    #        diag_2signs_central_cyanosis == 1 |
    #        diag_2signs_cannot_drink == 1 |
    #        diag_2signs_lethargy == 1
    #     ) ~ "Severe Pneumonia (WHO)",
    #   age_month >= 2 & age_month < 60 &
    #     (diag_2signs_cough == 1 | diag_2signs_pneumonia == 1) &
    #     (diag_2signs_bacteraemia == 1 |
    #        diag_2signs_meningitis == 1 |
    #        diag_2signs_decreased_consciousness == 1 |
    #        diag_2signs_seizures == 1 |
    #        diag_1xray_effusion == 1
    #     ) ~ "Very Severe Pneumonia (WHO)",
    #   age_month >= 60 &
    #     (diag_2signs_cough == 1 | diag_2signs_pleuritis_chest_pain == 1 | diag_2signs_pneumonia == 1) &
    #     (diag_1xray_infiltration == 1 |
    #        diag_1xray_consolidation == 1 |
    #        diag_1xray_effusion == 1) &
    #     (vital_respiratory_rate < 30 &
    #        vital_oxygen_saturation >= 92 &
    #        diag_2signs_decreased_consciousness != 1 &
    #        diag_2signs_bacteraemia != 1
    #     ) ~ "Mild CAP (ATS/IDSA)",
    #   
    #   age_month >= 60 &
    #     (diag_2signs_cough == 1 | diag_2signs_pleuritis_chest_pain == 1 | diag_2signs_pneumonia == 1) &
    #     (diag_1xray_infiltration == 1 |
    #        diag_1xray_consolidation == 1 |
    #        diag_1xray_effusion == 1) &
    #     (vital_respiratory_rate >= 30 |
    #        vital_oxygen_saturation < 92 |
    #        diag_2signs_decreased_consciousness == 1 |
    #        diag_2signs_bacteraemia == 1
    #     ) ~ "Severe CAP (ATS/IDSA)",
    #   TRUE ~ "No Pneumonia"
    # ),
    
    # vaccination status
    vacc_pcv13_status = apakah_pasien_telah_menerima_vaksin_pcv13_sebelumnya,
    vacc_pcv13_doses = case_when(
      is.na(jumlah_dosis_vaksin_pcv_13) ~ 0,
      TRUE ~ jumlah_dosis_vaksin_pcv_13
    ),
    
    # immunised vs. unimmunised
    vacc_pcv13_group = case_when(
      is.na(vacc_pcv13_doses) |
        vacc_pcv13_doses == 0 ~ "0 (not yet)",
      vacc_pcv13_doses == 1 | 
        vacc_pcv13_doses == 2 ~ "1-2 (mandatory)",
      vacc_pcv13_doses == 3 | 
        vacc_pcv13_doses == 4 ~ "3-4 (booster)"
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
    
    sample_csf_bacteria_day = jika_ya_tuliskan_pada_hari_keberapa_kah_baktek_positif,
    sample_csf_bacteria = jika_bukan_bakteri_streptococcus_pneumoniae_tuliskan_bakteri_nya,
    sample_csf_yeast = hasil_pengecatan_gram_yeast21,
    sample_csf_hyphae = hasil_pengecatan_gram_hifa22,
    
    sample_pleural_bacteria_day = jika_ya_tuliskan_pada_hari_keberapa_kah_baktek_positif_34,
    sample_pleural_bacteria = jika_bukan_bakteri_streptococcus_pneumoniae_tuliskan_bakteri_nya_2,
    sample_pleural_yeast = hasil_pengecatan_gram_yeast30,
    sample_pleural_hyphae = hasil_pengecatan_gram_hifa31,
    
    
    # np_swab_taken = apakah_dilakukan_pengambilan_np_swab,
    sample_np_id = np_swb_dc_id,
    
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
  ) %>% 
  # view() %>% 
  glimpse()

# data cleaning framework:
# https://bmjopen.bmj.com/content/12/6/e057957.abstract

# clean up multiple values in: #################################################
# 1. other antibiotics
# 2. bacteria & antimicrobial assessment for:
# 2.1. blood,
# 2.2. sputum,
# 2.3. csf and
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
  dplyr::left_join(
    cleaned_other_abx %>%
      dplyr::select(-other_antibiotics,
      ) %>%
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
      dplyr::select(-abx_cefotaxime) %>%  # more accurate compared to "others"
      dplyr::mutate(
        across(where(is.list), ~ as.numeric(.)),
        # across(where(is.list), ~ ifelse(map_lgl(., is.null), 0, .)),
        across(where(is.numeric), ~ replace_na(., 0))
      )
    ,
    by = c("id" = "no_id_subjek")
  ) %>% 
  glimpse()


# 2. bacteria & antimicrobial assessment #######################################
# cleanup bacteria names
# list of all bacteria
compiled_names <- sort(unique(
  c(
    df_epi_clean$sample_blood_bacteria,
    df_epi_clean$sample_sputum_bacteria,
    df_epi_clean$sample_csf_bacteria,
    df_epi_clean$sample_pleural_bacteria
  )))

name_bac <- c(
  "achromobacter xylosoxidans" = "Achromobacter xylosoxidans",
  "aciba" = "Acinetobacter baumannii",
  "aciba (mdr)" = "Acinetobacter baumannii (MDR)",
  "acinetobacter baumannii" = "Acinetobacter baumannii",
  "acinetobacter baumannii (mdr)" = "Acinetobacter baumannii (MDR)",
  "acinetobacter sp" = "Acinetobacter sp.",
  "aerococcus viridans" = "Aerococcus viridans",
  "bakteri gram positif unidentified" = "Unidentified Gram-positive bacterium",
  
  "cedecea sp (cre-mdr)" = "Cedecea sp. (Carbapenem-resistant, MDR)",
  "cromobacterium violaceum" = "Chromobacterium violaceum",
  "e.coli" = "Escherichia coli",
  "enterobacter cloacae" = "Enterobacter cloacae",
  "enterobacter cloacae (mdr)" = "Enterobacter cloacae (MDR)",
  "enterobacter cloacae complex" = "Enterobacter cloacae complex",
  "enterobcter cloacae (mdr)" = "Enterobacter cloacae (MDR)",
  "escherichia coli" = "Escherichia coli",
  "ewingela americana (mdr)" = "Ewingella americana (MDR)",
  "klebsiella pneumoniae" = "Klebsiella pneumoniae",
  "klebsiella pneumoniae (carbapenem resistant)" = "Klebsiella pneumoniae (Carbapenem-resistant)",
  "klebsiella pneumoniae (mdr) (mrs)" = "Klebsiella pneumoniae (MDR)", # Klebsiella not MRS
  "kocuria kristinae" = "Kocuria kristinae",
  "micrococcus luteus" = "Micrococcus luteus",
  "moraxella sp" = "Moraxella sp.",
  "pseudomonas aeruginosa" = "Pseudomonas aeruginosa",
  "raoutella ornithinolytica (cre-mdr), staph aureus (mrs) (mdr)" = 
    "Raoultella ornithinolytica (Carbapenem-resistant, MDR), Staphylococcus aureus (MRS, MDR)",
  "staph aureus (mrs) (mdr), raoutella ornythinolytica (cre) (mdr)" =
    "Raoultella ornithinolytica (Carbapenem-resistant, MDR), Staphylococcus aureus (MRS, MDR)",
  "rodentibacter pneumotropicus" = "Rodentibacter pneumotropicus",
  "serratia ficaria (mdr)" = "Serratia ficaria (MDR)",
  
  # Staphylococcus entries
  "stap.aureus" = "Staphylococcus aureus",
  "stap. aureus" = "Staphylococcus aureus",
  "staph. aureus" = "Staphylococcus aureus",
  "staphylococcus aureus" = "Staphylococcus aureus",
  "staphylococcus aureus (mrsa)" = "Staphylococcus aureus (MRSA)",
  "stap.aureus (mdr)(mrs)" = "Staphylococcus aureus (MRSA, MDR)",
  "stap.aureus(mdr)(mrs)" = "Staphylococcus aureus (MRSA, MDR)",
  "stap. aureus (mdr) (mrs)" = "Staphylococcus aureus (MRSA, MDR)",
  "staph aureus (mrs) (mdr)" = "Staphylococcus aureus (MRSA, MDR)",
  "staph aureus (mrsa) (mdr)" = "Staphylococcus aureus (MRSA, MDR)",
  "staph aureus (mrs)(mdr)" = "Staphylococcus aureus (MRSA, MDR)",
  "staphylococcus aureus (mdr) (mrs)" = "Staphylococcus aureus (MRSA, MDR)",
  "staphylococus aureus (mrsa) (mdr)" = "Staphylococcus aureus (MRSA, MDR)",
  
  # Coagulase-negative staphylococci (MRS, not MRSA)
  "staph epidermidis" = "Staphylococcus epidermidis",
  "stap. epidermidis" = "Staphylococcus epidermidis",
  "staphylococcus epidermidis" = "Staphylococcus epidermidis",
  "stap.epidermidis (mdr)" = "Staphylococcus epidermidis (MDR)",
  "stap. epidermidis (mdr)(mrs)" = "Staphylococcus epidermidis (MRS, MDR)",
  "stap.epidermidis (mdr, mrs)" = "Staphylococcus epidermidis (MRS, MDR)",
  "stap.epidermidis (mdr)(mrs)" = "Staphylococcus epidermidis (MRS, MDR)",
  "stap.epidermidis(mdr)(mrs)" = "Staphylococcus epidermidis (MRS, MDR)",
  "staph epidermidis (mrsa) (mdr)" = "Staphylococcus epidermidis (MRS, MDR)",
  "staph epidermidis (mdr)" = "Staphylococcus epidermidis (MDR)",
  "staph epidermidis (mrs) (mdr)" = "Staphylococcus epidermidis (MRS, MDR)",
  "staph. epidermidis (mrs) (mdr)" = "Staphylococcus epidermidis (MRS, MDR)",
  "staphylococcus epidermidis (mdr) (mrsa)" = "Staphylococcus epidermidis (MRS, MDR)",
  
  "staph. haemolyticus" = "Staphylococcus haemolyticus",
  "staphylococcus haemolyticus" = "Staphylococcus haemolyticus",
  "staphylococcus haemolitycus" = "Staphylococcus haemolyticus",
  "staphylococcus haemoliticus" = "Staphylococcus haemolyticus",
  "staphylococcus haemolitycus" = "Staphylococcus haemolyticus",
  "stap.haemolyticus (mdr)(mrs)" = "Staphylococcus haemolyticus (MRS, MDR)",
  
  "staphylococcus hominis" = "Staphylococcus hominis",
  "stap.hominis subsp.hominis" = "Staphylococcus hominis subsp. hominis",
  "stap.hominis subsp.homonis" = "Staphylococcus hominis subsp. hominis",
  "staph hominis subs hominis (mrsa)" = "Staphylococcus hominis subsp. hominis (MRS)",
  "stap.hominis subsp.hominis (mrs)" = "Staphylococcus hominis subsp. hominis (MRS)",
  "staph hominis subs hominis (mrs) (mdr)" = "Staphylococcus hominis subsp. hominis (MRS, MDR)",
  "staph hominis susbs hominis (mrs) (mdr)" = "Staphylococcus hominis subsp. hominis (MRS, MDR)",
  "stap.hominis subsp.hominis (mdr)(mrs)" = "Staphylococcus hominis subsp. hominis (MRS, MDR)",
  "staph. hominis subs hominis (mdr) (mrs)" = "Staphylococcus hominis subsp. hominis (MRS, MDR)",
  "staph hominis subs hominis (mrsa) (mdr)" = "Staphylococcus hominis subsp. hominis (MRS, MDR)",
  "staph. hominis subs hominis (mrsa) (mdr)" = "Staphylococcus hominis subsp. hominis (MRS, MDR)",
  "stap.hominis subps.hominis (mdr)(mrs)" = "Staphylococcus hominis subsp. hominis (MRS, MDR)",
  "stap.hominis subsp.hominis (mdr,mrs)" = "Staphylococcus hominis subsp. hominis (MRS, MDR)",
  "staph hominis subs hominis (mdr) (mrs)" = "Staphylococcus hominis subsp. hominis (MRS, MDR)",
  "stap.hominis subsp.homonis (mdr)(mrs)" = "Staphylococcus hominis subsp. hominis (MRS, MDR)",
  "staphylococcus hominis subs hominis (mrs) (mdr)" = "Staphylococcus hominis subsp. hominis (MRS, MDR)",
  
  "staphylococcus sciuri (mdr) (mrs)" = "Staphylococcus sciuri (MRS, MDR)",
  "staphylococcus saprophyticus" = "Staphylococcus saprophyticus",
  "staph sp" = "Staphylococcus sp.",
  "staph. sp" = "Staphylococcus sp.",
  "staphylococcus sp" = "Staphylococcus sp.",
  "staphylococcus sp." = "Staphylococcus sp.",
  "staphylococcus capitis" = "Staphylococcus capitis",
  "stap.chonii subsp. urialyticus"= "Staphylococcus cohnii subsp. urealyticus",
  "stap.chonnii subsp. uryalyticus" = "Staphylococcus cohnii subsp. urealyticus",
  "stap. intermedius" = "Staphylococcus intermedius",
  
  # Streptococcus and others
  "streptococcus acidominus" = "Streptococcus acidominus",
  "streptococcus arlettae" = "Streptococcus arlettae",
  "streptococcus mitis" = "Streptococcus mitis",
  "streptococcus oralis" = "Streptococcus oralis",
  "streptococcus salivarus" = "Streptococcus salivarius",
  "streptococus bovis" = "Streptococcus bovis",
  "streptococus oralis" = "Streptococcus oralis",
  "staphylococcus pneumoniae" = "Streptococcus pneumoniae",
  
  # Others
  "stenotrophomonas maltophilia" = "Stenotrophomonas maltophilia",
  
  # bacteria only, yeast have already classified (checked)
  "yeast" = "No Growth",
  "yeast cell" = "No Growth",
  "candida albicans" = "No Growth",
  "candida rugosa" = "No Growth",
  "candida tropicalis" = "No Growth",
  
  "tidak ada pertumbuhan" = "No Growth"
)

cleaned_other_bac <- df_epi_clean %>%
  dplyr::rowwise() %>% # crucial!
  dplyr::mutate(sample_blood_bacteria_cleaned = detoxdats::detox_singletext(text = sample_blood_bacteria,
                                                                            dict = name_bac,
                                                                            exclude = NA),
                sample_sputum_bacteria_cleaned = detoxdats::detox_singletext(text = sample_sputum_bacteria,
                                                                             dict = name_bac,
                                                                             exclude = NA),
                sample_csf_bacteria_cleaned = detoxdats::detox_singletext(text = sample_csf_bacteria,
                                                                          dict = name_bac,
                                                                          exclude = NA),
                sample_pleural_bacteria_cleaned = detoxdats::detox_singletext(text = sample_pleural_bacteria,
                                                                              dict = name_bac,
                                                                              exclude = NA),
  ) %>%
  dplyr::ungroup() %>%
  # check cleaned bacteria species per-id
  dplyr::select(
    id,
    sample_blood_bacteria,
    sample_blood_bacteria_cleaned,
    sample_sputum_bacteria,
    sample_sputum_bacteria_cleaned,
    sample_csf_bacteria,
    sample_csf_bacteria_cleaned,
    sample_pleural_bacteria,
    sample_pleural_bacteria_cleaned
  ) %>%
  # view() %>%
  glimpse()

# compile all bacteria names
all_unique_bac <- cleaned_other_bac %>%
  dplyr::select(contains("_cleaned")) %>%
  unlist() %>%
  stringr::str_split(", ") %>%
  unlist() %>%
  unique() %>%
  sort() %>%
  # view() %>% 
  glimpse()

# combine data
df_epi_clean <- df_epi_clean %>% 
  dplyr::select(-sample_blood_bacteria,
                -sample_sputum_bacteria,
                -sample_csf_bacteria,
                -sample_pleural_bacteria) %>% 
  dplyr::left_join(
    cleaned_other_bac %>%
      dplyr::select(id, 
                    contains("_cleaned")) %>% 
      dplyr::rename(
        sample_blood_bacteria = sample_blood_bacteria_cleaned,
        sample_sputum_bacteria = sample_sputum_bacteria_cleaned,
        sample_csf_bacteria = sample_csf_bacteria_cleaned,
        sample_pleural_bacteria = sample_pleural_bacteria_cleaned
      )
    ,
    by = "id"
  ) %>% 
  # define No Growth if sample exist but species absent
  dplyr::mutate(
    sample_blood_bacteria = case_when(
      !is.na(sample_blood_id) & is.na(sample_blood_bacteria)
      ~ "No Growth",
    TRUE ~ sample_blood_bacteria
    ),
    sample_sputum_bacteria = case_when(
      !is.na(sample_sputum_id) & is.na(sample_sputum_bacteria)
      ~ "No Growth",
      TRUE ~ sample_sputum_bacteria
    ),
    sample_csf_bacteria = case_when(
      !is.na(sample_csf_id) & is.na(sample_csf_bacteria)
      ~ "No Growth",
      TRUE ~ sample_csf_bacteria
    ),
    sample_pleural_bacteria = case_when(
      !is.na(sample_pleural_id) & is.na(sample_pleural_bacteria)
      ~ "No Growth",
      TRUE ~ sample_pleural_bacteria
    ),
  ) %>% 
  # specified to blood data
  dplyr::mutate(
    sample_blood_bacteria_genus = case_when(
      sample_blood_bacteria != "No Growth" ~ str_extract(sample_blood_bacteria,
                                                         "^[A-Za-z]+"),
      sample_blood_bacteria == "No Growth" ~ "No Growth",
      TRUE ~ NA
    ),
    sample_blood_bacteria_species = case_when(
      sample_blood_bacteria != "No Growth"
      ~ paste0(sample_blood_bacteria_genus, " ",
               str_extract(sample_blood_bacteria,
                           "(?<=\\s)[a-z]+")
      ),
      sample_blood_bacteria == "No Growth" ~ "No Growth",
      TRUE ~ NA
    ),
    sample_blood_bacteria_resistance = case_when(
      str_detect(sample_blood_bacteria, regex("MDR")) ~ "MDR",
      str_detect(sample_blood_bacteria, regex("MRSA")) ~ "MRSA",
      str_detect(sample_blood_bacteria, regex("MRS")) ~ "MRS",
      str_detect(sample_blood_bacteria, regex("Carbapenem")) ~ "Carbapenem-resistant",
      !is.na(sample_blood_bacteria) & sample_blood_bacteria != "No Growth" ~ "Not specified",
      TRUE ~ NA
    ),
    sample_blood_bacteria_resistance_simplified = case_when(
      sample_blood_bacteria_resistance == "MRSA" |
        sample_blood_bacteria_resistance == "MRS" ~ "Methicillin-resistant",
      TRUE ~ sample_blood_bacteria_resistance
    ),
    sample_blood_bacteria_gram = case_when(
      sample_blood_bacteria_genus %in% c("Achromobacter", "Acinetobacter",
                                         "Cedecea", "Chromobacterium",
                                         "Enterobacter", "Escherichia",
                                         "Ewingella", "Klebsiella",
                                         "Pseudomonas", "Raoultella",
                                         "Rodentibacter", "Serratia",
                                         "Stenotrophomonas") 
      ~ "Gram-negative rods",
      sample_blood_bacteria_genus %in% c("Staphylococcus", "Streptococcus",
                                         "Aerococcus", "Micrococcus", "Kocuria")
      ~ "Gram-positive cocci",
      sample_blood_bacteria_genus %in% c("Moraxella")
      ~ "Gram-negative cocci",
      sample_blood_bacteria == "No Growth" ~ "No Growth",
      TRUE ~ NA
    ),
  ) %>% 
  # specified to sputum data
  dplyr::mutate(
    sample_sputum_bacteria_genus = case_when(
      sample_sputum_bacteria != "No Growth" ~ str_extract(sample_sputum_bacteria,
                                                          "^[A-Za-z]+"),
      sample_sputum_bacteria == "No Growth" ~ "No Growth",
      TRUE ~ NA
    ),
    sample_sputum_bacteria_species = case_when(
      sample_sputum_bacteria != "No Growth"
      ~ paste0(sample_sputum_bacteria_genus, " ",
               str_extract(sample_sputum_bacteria,
                           "(?<=\\s)[a-z]+")
      ),
      sample_sputum_bacteria == "No Growth" ~ "No Growth",
      TRUE ~ NA
    ),
    sample_sputum_bacteria_resistance = case_when(
      str_detect(sample_sputum_bacteria, regex("MDR")) ~ "MDR",
      str_detect(sample_sputum_bacteria, regex("MRSA")) ~ "MRSA",
      str_detect(sample_sputum_bacteria, regex("MRS")) ~ "MRS",
      str_detect(sample_sputum_bacteria, regex("Carbapenem")) ~ "Carbapenem-resistant",
      !is.na(sample_sputum_bacteria) & sample_sputum_bacteria != "No Growth" ~ "Not specified",
      TRUE ~ NA
    ),
    sample_sputum_bacteria_resistance_simplified = case_when(
      sample_sputum_bacteria_resistance == "MRSA" |
        sample_sputum_bacteria_resistance == "MRS" ~ "Methicillin-resistant",
      TRUE ~ sample_sputum_bacteria_resistance
    ),
    sample_sputum_bacteria_gram = case_when(
      sample_sputum_bacteria_genus %in% c("Achromobacter", "Acinetobacter",
                                          "Cedecea", "Chromobacterium",
                                          "Enterobacter", "Escherichia",
                                          "Ewingella", "Klebsiella",
                                          "Pseudomonas", "Raoultella",
                                          "Rodentibacter", "Serratia",
                                          "Stenotrophomonas") 
      ~ "Gram-negative rods",
      sample_sputum_bacteria_genus %in% c("Staphylococcus", "Streptococcus",
                                          "Aerococcus", "Micrococcus", "Kocuria")
      ~ "Gram-positive cocci",
      sample_sputum_bacteria_genus %in% c("Moraxella")
      ~ "Gram-negative cocci",
      sample_sputum_bacteria == "No Growth" ~ "No Growth",
      TRUE ~ NA
    ),
  ) %>% 
  # specified to csf data
  dplyr::mutate(
    sample_csf_bacteria_genus = case_when(
      sample_csf_bacteria != "No Growth" ~ str_extract(sample_csf_bacteria,
                                                       "^[A-Za-z]+"),
      sample_csf_bacteria == "No Growth" ~ "No Growth",
      TRUE ~ NA
    ),
    sample_csf_bacteria_species = case_when(
      sample_csf_bacteria != "No Growth"
      ~ paste0(sample_csf_bacteria_genus, " ",
               str_extract(sample_csf_bacteria,
                           "(?<=\\s)[a-z]+")
      ),
      sample_csf_bacteria == "No Growth" ~ "No Growth",
      TRUE ~ NA
    ),
    sample_csf_bacteria_resistance = case_when(
      str_detect(sample_csf_bacteria, regex("MDR")) ~ "MDR",
      str_detect(sample_csf_bacteria, regex("MRSA")) ~ "MRSA",
      str_detect(sample_csf_bacteria, regex("MRS")) ~ "MRS",
      str_detect(sample_csf_bacteria, regex("Carbapenem")) ~ "Carbapenem-resistant",
      !is.na(sample_csf_bacteria) & sample_csf_bacteria != "No Growth" ~ "Not specified",
      TRUE ~ NA
    ),
    sample_csf_bacteria_resistance_simplified = case_when(
      sample_csf_bacteria_resistance == "MRSA" |
        sample_csf_bacteria_resistance == "MRS" ~ "Methicillin-resistant",
      TRUE ~ sample_csf_bacteria_resistance
    ),
    sample_csf_bacteria_gram = case_when(
      sample_csf_bacteria_genus %in% c("Achromobacter", "Acinetobacter",
                                       "Cedecea", "Chromobacterium",
                                       "Enterobacter", "Escherichia",
                                       "Ewingella", "Klebsiella",
                                       "Pseudomonas", "Raoultella",
                                       "Rodentibacter", "Serratia",
                                       "Stenotrophomonas") 
      ~ "Gram-negative rods",
      sample_csf_bacteria_genus %in% c("Staphylococcus", "Streptococcus",
                                       "Aerococcus", "Micrococcus", "Kocuria")
      ~ "Gram-positive cocci",
      sample_csf_bacteria_genus %in% c("Moraxella")
      ~ "Gram-negative cocci",
      sample_csf_bacteria == "No Growth" ~ "No Growth",
      TRUE ~ NA
    ),
  ) %>% 
  # specified to pleural data
  dplyr::mutate(
    sample_pleural_bacteria_genus = case_when(
      sample_pleural_bacteria != "No Growth" ~ str_extract(sample_pleural_bacteria,
                                                           "^[A-Za-z]+"),
      sample_pleural_bacteria == "No Growth" ~ "No Growth",
      TRUE ~ NA
    ),
    sample_pleural_bacteria_species = case_when(
      sample_pleural_bacteria != "No Growth"
      ~ paste0(sample_pleural_bacteria_genus, " ",
               str_extract(sample_pleural_bacteria,
                           "(?<=\\s)[a-z]+")
      ),
      sample_pleural_bacteria == "No Growth" ~ "No Growth",
      TRUE ~ NA
    ),
    sample_pleural_bacteria_resistance = case_when(
      str_detect(sample_pleural_bacteria, regex("MDR")) ~ "MDR",
      str_detect(sample_pleural_bacteria, regex("MRSA")) ~ "MRSA",
      str_detect(sample_pleural_bacteria, regex("MRS")) ~ "MRS",
      str_detect(sample_pleural_bacteria, regex("Carbapenem")) ~ "Carbapenem-resistant",
      !is.na(sample_pleural_bacteria) & sample_pleural_bacteria != "No Growth" ~ "Not specified",
      TRUE ~ NA
    ),
    sample_pleural_bacteria_resistance_simplified = case_when(
      sample_pleural_bacteria_resistance == "MRSA" |
        sample_pleural_bacteria_resistance == "MRS" ~ "Methicillin-resistant",
      TRUE ~ sample_pleural_bacteria_resistance
    ),
    sample_pleural_bacteria_gram = case_when(
      sample_pleural_bacteria_genus %in% c("Achromobacter", "Acinetobacter",
                                           "Cedecea", "Chromobacterium",
                                           "Enterobacter", "Escherichia",
                                           "Ewingella", "Klebsiella",
                                           "Pseudomonas", "Raoultella",
                                           "Rodentibacter", "Serratia",
                                           "Stenotrophomonas") 
      ~ "Gram-negative rods",
      sample_pleural_bacteria_genus %in% c("Staphylococcus", "Streptococcus",
                                           "Aerococcus", "Micrococcus", "Kocuria")
      ~ "Gram-positive cocci",
      sample_pleural_bacteria_genus %in% c("Moraxella")
      ~ "Gram-negative cocci",
      sample_pleural_bacteria == "No Growth" ~ "No Growth",
      TRUE ~ NA
    ),
  ) %>% 
  # pending: cleanup blood, hyphae & yeast tabulation for 0, 1 and NA
  # view() %>% 
  glimpse()

# test (cleaned data previously called df_epi_clean2)
# dplyr::left_join(
#   df_epi_clean %>% 
#     select(id,
#            sample_blood_bacteria_id,
#            sample_blood_bacteria)
#   ,
#   df_epi_clean2 %>% 
#     select(id, contains("sample_"))
#   ,
#   by = "id"
# ) %>% 
#   view()

# test yeast and hyphae (cleaned data previously called df_epi_clean2)
# df_epi_clean2 %>% 
#   select(id,
#          sample_blood_bacteria,
#          sample_sputum_bacteria,
#          sample_csf_bacteria,
#          sample_pleural_bacteria,
#          contains(c("yeast", "hyphae"))) %>% 
#   view()

write.csv(df_epi_clean, "inputs/df_epi_hospital_cleaned.csv", row.names = F)

