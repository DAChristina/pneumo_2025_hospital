# load df_epi_clean first

library(tidyverse)

# I will define tachypnea based on respiratory rate not diag_2signs_tachypnea
df_epi_clean %>% 
  select(age_year, diag_2signs_tachypnea, vital_respiratory_rate) %>% 
  ggplot(., aes(x = vital_respiratory_rate,
                fill = as.character(diag_2signs_tachypnea))) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ age_year, scales = "free") +
  theme_bw()


# I will add X-rays as a part of pneumonia diagnosis
# centralisation for xrays data --> I can't define x-rays as mandatory for neo
df_epi_clean %>% 
  select(age_year, contains("diag_1xray_")) %>% 
  mutate(xray_availability = case_when(
    diag_1xray_infiltration == 1 |
      diag_1xray_consolidation == 1 |
      diag_1xray_effusion == 1 ~ 1
    ,
    TRUE ~ 0
  )
  ) %>% 
  # pivot_longer(cols = contains("diag_1xray_"),
  #              names_to = "xray_type",
  #              values_to = "xray_availability") %>%
  ggplot(., aes(x = age_year,
                fill = as.character(xray_availability))) +
  geom_density(alpha = 0.4) +
  # facet_wrap(~ xray_type, scales = "free_y") +
  theme_bw()

df_epi_clean %>% 
  select(id, age_month,
         diag_2signs_pneumonia,
         diag_4pneumo_final,
         contains("diag_")) %>% 
  view()

# central_cyanosis based on oxygen_saturation (?)
df_epi_clean %>% 
  select(age_year, diag_2signs_central_cyanosis, vital_oxygen_saturation) %>% 
  ggplot(., aes(x = vital_oxygen_saturation,
                fill = as.character(age_year))) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ diag_2signs_central_cyanosis, scales = "free_y") +
  theme_bw()

df_epi_clean %>% 
  select(age_year, diag_2signs_central_cyanosis, vital_oxygen_saturation) %>% 
  ggplot(., aes(x = vital_oxygen_saturation,
                fill = as.character(diag_2signs_central_cyanosis))) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ age_year, scales = "free_y") +
  theme_bw()

