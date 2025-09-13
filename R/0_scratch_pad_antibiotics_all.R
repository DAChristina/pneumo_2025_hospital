library(tidyverse)
source("global/fun.R")

# cleanup other antibiotics data
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
  dplyr::mutate(other_antibiotics_cleaned = clean_values(text = other_antibiotics,
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

test <- cleaned_other_abx %>% 
  dplyr::filter(stringr::str_detect(other_antibiotics_cleaned, "cefopera")) %>% 
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

df_one_hot_encoded <- cleaned_other_abx %>%
  dplyr::select(-other_antibiotics) %>% 
  dplyr::filter(!is.na(other_antibiotics_cleaned)) %>%
  tidyr::separate_rows(other_antibiotics_cleaned, sep = ", ") %>%
  dplyr::mutate(value = as.numeric(1)) %>%
  tidyr::pivot_wider(
    id_cols = no_id_subjek,
    names_from = other_antibiotics_cleaned,
    values_from = value,
    values_fill = 0,
    values_fn = length
  ) %>%
  dplyr::mutate(
    across(where(is.list), ~ as.numeric(.)),
    # across(where(is.list), ~ ifelse(map_lgl(., is.null), 0, .)),
    across(where(is.numeric), ~ replace_na(., 0))
  ) %>%
  # view() %>% 
  glimpse()

abx_major <- df_one_hot_encoded %>%
  summarise(across(everything(), ~ sum(. == 1, na.rm = TRUE))) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "antibiotic",
    values_to = "count"
  ) %>%
  arrange(desc(count)) %>% 
  filter(count >= 2) %>% 
  pull(antibiotic)

dplyr::left_join(
  df_epi_hospital %>% 
    dplyr::select(
      no_id_subjek,
      contains("jika_ya_manakah_antibiotik_di_bawah_ini_yang_diberikan_")
    ) %>% 
    dplyr::rename_with(~ str_remove(., "jika_ya_manakah_antibiotik_di_bawah_ini_yang_diberikan_")
    )
  ,
  df_one_hot_encoded
  ,
  by = "no_id_subjek"
) %>% 
  dplyr::select(-no_id_subjek,
                -lainnya) %>% 
  dplyr::mutate(
    across(everything(), ~ as.numeric(.)),
    across(where(is.numeric), ~ replace_na(., 0))
  ) %>%
  dplyr::rename(
    # have already in 1 and 0
    "amoxicillin" = amoksisilin,
    "amoxicillin-clavulanic acid" = amoksisilin_as_klavulanat,
    "ampicillin" = ampisilin,
    "ampicillin-sulbactam" = ampisilin_sulbaktam,
    "azithromycin" = azitromisin,
    "cefotaxime main" = sefotaksim,
    "ceftriaxone" = seftriakson,
    "erythromycin" = eritromisin,
    "trimethoprim-sulfamethoxazole" = trimetoprim_sulfametoksazol
  ) %>% 
  # need to be discussed: NA in antibiotics usage
  dplyr::filter(!is.na(amoxicillin)
  ) %>% 
  dplyr::mutate(
    across(everything(), ~ as.numeric(.)),
    across(where(is.numeric), ~ replace_na(., 0))
  ) %>% 
  as.data.frame() %>% 
  UpSetR::upset(
    sets = abx_major,
    order.by = "freq",
    empty.intersections = "off",
    number.angles = 0,
    point.size = 3.5,
    line.size = 2, 
    mainbar.y.label = "Antibiotics Combinations",
    sets.x.label = "Patients with Antibiotics")
