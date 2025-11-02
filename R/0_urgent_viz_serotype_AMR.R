library(tidyverse)
source("global/fun.R")


# data viz just only for serotype & AMR ########################################
df_epi_gen_pneumo <- read.csv("inputs/genData_pneumo_with_epiData_with_final_pneumo_decision.csv") %>% 
  dplyr::filter(workWGS_species_pw == "Streptococcus pneumoniae") %>% 
  dplyr::mutate(
    serotype_final_decision = case_when(
      serotype_final_decision == "mixed serotypes/serogroups" ~ "mixed serogroups",
      TRUE ~ serotype_final_decision
    ),
    serotype_final_decision = factor(serotype_final_decision,
                                     levels = c(
                                       # VT
                                       # "3", "6A/6B", "6A/6B/6C/6D", "serogroup 6",
                                       # "14", "17F", "18C/18B", "19A", "19F", "23F",
                                       "1", "3", "4", "5", "7F",
                                       "6A", "6B", "9V", "14", "18C",
                                       "19A", "19F", "23F",
                                       # NVT
                                       # "7C", "10A", "10B", "11A/11D", "13", "15A", "15B/15C",
                                       # "16F", "19B", "20", "23A", "23B", "24F/24B", "25F/25A/38",
                                       # "28F/28A", "31", "34", "35A/35C/42", "35B/35D", "35F",
                                       # "37", "39", "mixed serogroups",
                                       "serogroup 6", "6C", "7C",
                                       "10", "10A", "10B", "11A", "13",
                                       "15A", "15B", "15C","15B/15C", "16F",
                                       "17F", "18A", "18B", "19B", "20", "20B",
                                       "21", "23A", "23B", "23B1",
                                       "24F", "24B/C/F", "24B/24C/24F", "serogroup 24",
                                       "25B", "25F",
                                       "28A", "31", "33B", "33G",
                                       "34", "35A", "35B", "35C", "35F", "37",
                                       "37F", "38", "39",
                                       "nontypeable")),
    serotype_classification_PCV13_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                          levels = c("VT", "NVT", "nontypeable")),
    serotype_classification_PCV15_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                          levels = c("VT", "NVT", "nontypeable"))
  ) %>%
  glimpse()

table(df_epi_gen_pneumo$hospital)

# test classification df
test_classification <- df_epi_gen_pneumo %>% 
  dplyr::select(workWGS_serotype,
                serotype_final_decision,
                serotype_classification_PCV13_final_decision) %>% 
  # view() %>% 
  glimpse()


df_serotype_summary <- df_epi_gen_pneumo %>% 
  dplyr::count(serotype_final_decision) %>%
  dplyr::mutate(percentage = n / sum(n) * 100) %>% 
  dplyr::left_join(
    df_epi_gen_pneumo %>% 
      dplyr::select(serotype_final_decision,
                    serotype_classification_PCV13_final_decision)
    ,
    by = "serotype_final_decision"
  ) %>% 
  dplyr::distinct() %>% 
  # view() %>% 
  glimpse()

df_serotype_classification_summary <- df_epi_gen_pneumo %>% 
  dplyr::count(serotype_classification_PCV13_final_decision) %>%
  dplyr::mutate(percentage = n / sum(n) * 100) %>% 
  glimpse()

df_serotype_classification_perhospital_summary <- df_epi_gen_pneumo %>% 
  dplyr::count(serotype_classification_PCV13_final_decision, hospital) %>%
  dplyr::mutate(percentage = n / sum(n) * 100) %>% 
  glimpse()

# compiled VT percentage per hospital ##############################################
df_compiled_VT_percentage <- dplyr::left_join(
  df_epi_gen_pneumo %>% 
    dplyr::group_by(hospital, serotype_classification_PCV13_final_decision) %>% 
    dplyr::summarise(count_vacc = n(), .groups = "drop") %>% 
    dplyr::mutate(
      count_vacc = case_when(
        is.na(count_vacc) ~ 0,
        TRUE ~ count_vacc
      )
    )
  ,
  df_epi_gen_pneumo %>% 
    dplyr::group_by(hospital) %>% 
    dplyr::summarise(count_hospital = n(), .groups = "drop") %>% 
    dplyr::mutate(
      count_hospital = case_when(
        is.na(count_hospital) ~ 0,
        TRUE ~ count_hospital
      )
    )
  ,
  by = "hospital"
) %>% 
  dplyr::mutate(
    percent = round(count_vacc/count_hospital*100, 1),
    report_vaccination = paste0(count_vacc, " (", percent, "%)"),
    # vacc_pcv13_group = case_when(
    #   is.na(vacc_pcv13_group) ~ "0 not yet",
    #   T ~ vacc_pcv13_group
    # ),
    hospital = factor(hospital,
                  levels = c("lombok", "sumbawa", "manado", "sorong"))
  ) %>% 
  # view() %>% 
  glimpse()

png(file = "pictures/genData_serotypes_vaccineGroup.png",
    width = 29, height = 20, unit = "cm", res = 600)
ggplot(df_compiled_VT_percentage, aes(x = serotype_classification_PCV13_final_decision,
                                      y = percent,
                                      fill = hospital)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            vjust = -0.5, size = 3,
            angle = 0,
            position = position_dodge(width = 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  # facet_grid(~ hospital,
  #            scales = "free_x",
  #            space = "free_x"
  # ) +
  scale_fill_manual(values = c(col_map)) +
  labs(x = "PCV13 Vaccine Group", y = "Percentage", 
       # title = "All Serotypes"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
        legend.position = c(0.02, 0.75),
        legend.direction = "vertical",
        legend.justification = c("left", "top"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        legend.margin = margin(t = -50),
        legend.spacing.y = unit(-0.3, "cm")) # +
# facet_wrap(~ serotype_classification_PCV13_final_decision, nrow = 1, scales = "free_x")
dev.off()


# serotypes per-hospital ###########################################################
ser1 <- ggplot(df_serotype_summary %>% 
                 dplyr::mutate(percentage_label = ifelse(percentage < 3, NA,
                                                         paste0(round(percentage, 1), "%")))
               ,
               aes(x = serotype_final_decision, y = percentage,
                   fill = serotype_classification_PCV13_final_decision)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percentage_label),
            vjust = -0.5, size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(x = " ", y = "Percentage", 
       # title = "All Serotypes"
  ) +
  scale_fill_manual(values = c(col_map)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = c(0.22, 0.75),
        legend.direction = "vertical",
        legend.justification = c("left", "top"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        legend.margin = margin(t = -50),
        legend.spacing.y = unit(-0.3, "cm"))
ser1

# Compute percentage by age_group_2vaccine
df_serotype_age_group_2vaccine_summary <- df_epi_gen_pneumo %>% 
  dplyr::group_by(age_group_2vaccine, serotype_final_decision) %>%
  dplyr::summarise(count = n(), .groups = "drop") %>%
  dplyr::group_by(age_group_2vaccine) %>% 
  # percentage is calculated from count_serotype per age_group_2vaccine
  dplyr::mutate(percentage = count / sum(count) * 100,
                # age_group_2vaccine = factor(age_group_2vaccine,
                #                   levels = c("1", "2", "3", "4", "5")),
                serotype_classification_PCV13_final_decision = case_when(
                  serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                                 "6A", "6B", "9V", "14", "18C",
                                                 "19A", "19F", "23F") ~ "VT",
                  serotype_final_decision == "nontypeable" ~ "nontypeable",
                  TRUE ~ "NVT"
                ),
                serotype_classification_PCV15_final_decision = case_when(
                  serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                                 "6A", "6B", "9V", "14", "18C",
                                                 "19A", "19F", "23F",
                                                 "22F", "33F") ~ "VT",
                  serotype_final_decision == "nontypeable" ~ "nontypeable",
                  TRUE ~ "NVT"
                ),
                serotype_classification_PCV13_final_decision = case_when(
                  serotype_classification_PCV13_final_decision == "nontypeable" ~ " ",
                  TRUE ~ serotype_classification_PCV13_final_decision
                ),
                serotype_classification_PCV13_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                                      levels = c("VT", "NVT", " ")),
  )

ser_age <- ggplot(df_serotype_age_group_2vaccine_summary, aes(x = serotype_final_decision,
                                                    y = percentage,
                                                    fill = age_group_2vaccine)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  # geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  facet_grid(~ serotype_classification_PCV13_final_decision,
             scales = "free_x",
             space = "free_x"
  ) +
  scale_fill_manual(values = c(col_map)) +
  labs(x = "Category", y = "Percentage",
       # title = "All Serotypes"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = c(0.02, 0.75),
        legend.direction = "vertical",
        legend.justification = c("left", "top"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        legend.margin = margin(t = -50),
        legend.spacing.y = unit(-0.3, "cm")) # +
# facet_wrap(~ serotype_classification_PCV13_final_decision, nrow = 1, scales = "free_x")
ser_age

png(file = "pictures/genData_serotypes_per_age_groups.png",
    width = 29, height = 23, unit = "cm", res = 600)
print(ser_age)
dev.off()

# Compute percentage by vacc_pcv13_group
df_serotype_vacc_pcv13_group_summary <- df_epi_gen_pneumo %>% 
  dplyr::group_by(vacc_pcv13_group, serotype_final_decision) %>%
  dplyr::summarise(count = n(), .groups = "drop") %>%
  dplyr::group_by(vacc_pcv13_group) %>% 
  # percentage is calculated from count_serotype per vacc_pcv13_group
  dplyr::mutate(percentage = count / sum(count) * 100,
                # vacc_pcv13_group = factor(vacc_pcv13_group,
                #                   levels = c("0", "1", "2", "3", "4", "5")),
                serotype_classification_PCV13_final_decision = case_when(
                  serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                                 "6A", "6B", "9V", "14", "18C",
                                                 "19A", "19F", "23F") ~ "VT",
                  serotype_final_decision == "nontypeable" ~ "nontypeable",
                  TRUE ~ "NVT"
                ),
                serotype_classification_PCV15_final_decision = case_when(
                  serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                                 "6A", "6B", "9V", "14", "18C",
                                                 "19A", "19F", "23F",
                                                 "22F", "33F") ~ "VT",
                  serotype_final_decision == "nontypeable" ~ "nontypeable",
                  TRUE ~ "NVT"
                ),
                serotype_classification_PCV13_final_decision = case_when(
                  serotype_classification_PCV13_final_decision == "nontypeable" ~ " ",
                  TRUE ~ serotype_classification_PCV13_final_decision
                ),
                serotype_classification_PCV13_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                                      levels = c("VT", "NVT", " ")),
  )

ser_vacc <- ggplot(df_serotype_vacc_pcv13_group_summary, aes(x = serotype_final_decision,
                                                             y = percentage,
                                                             fill = vacc_pcv13_group)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  # geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  facet_grid(~ serotype_classification_PCV13_final_decision,
             scales = "free_x",
             space = "free_x"
  ) +
  # scale_fill_manual(values = c(col_map)) +
  labs(x = "Category", y = "Percentage",
       # title = "All Serotypes"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = c(0.02, 0.75),
        legend.direction = "vertical",
        legend.justification = c("left", "top"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        legend.margin = margin(t = -50),
        legend.spacing.y = unit(-0.3, "cm")) # +
# facet_wrap(~ serotype_classification_PCV13_final_decision, nrow = 1, scales = "free_x")
ser_vacc

# Compute percentage by hospital
df_serotype_hospital_summary <- df_epi_gen_pneumo %>% 
  dplyr::group_by(hospital, serotype_final_decision) %>%
  dplyr::summarise(count = n(), .groups = "drop") %>%
  dplyr::group_by(hospital) %>% 
  # percentage is calculated from count_serotype per hospital
  dplyr::mutate(percentage = count / sum(count) * 100,
                hospital = factor(hospital,
                              levels = c("lombok", "sumbawa", "manado", "sorong")),
                serotype_classification_PCV13_final_decision = case_when(
                  serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                                 "6A", "6B", "9V", "14", "18C",
                                                 "19A", "19F", "23F") ~ "VT",
                  serotype_final_decision == "nontypeable" ~ "nontypeable",
                  TRUE ~ "NVT"
                ),
                serotype_classification_PCV15_final_decision = case_when(
                  serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                                 "6A", "6B", "9V", "14", "18C",
                                                 "19A", "19F", "23F",
                                                 "22F", "33F") ~ "VT",
                  serotype_final_decision == "nontypeable" ~ "nontypeable",
                  TRUE ~ "NVT"
                ),
                serotype_classification_PCV13_final_decision = case_when(
                  serotype_classification_PCV13_final_decision == "nontypeable" ~ " ",
                  TRUE ~ serotype_classification_PCV13_final_decision
                ),
                serotype_classification_PCV13_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                                      levels = c("VT", "NVT", " ")),
  )

ser2 <- ggplot(df_serotype_hospital_summary, aes(x = serotype_final_decision,
                                             y = percentage,
                                             fill = hospital)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  # geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  facet_grid(~ serotype_classification_PCV13_final_decision,
             scales = "free_x",
             space = "free_x"
  ) +
  scale_fill_manual(values = c(col_map)) +
  labs(x = "Category", y = "Percentage", 
       # title = "All Serotypes"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = c(0.22, 0.75),
        legend.direction = "vertical",
        legend.justification = c("left", "top"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        legend.margin = margin(t = -50),
        legend.spacing.y = unit(-0.3, "cm")) # +
# facet_wrap(~ serotype_classification_PCV13_final_decision, nrow = 1, scales = "free_x")
ser2

png(file = "pictures/genData_serotypes_classification_filterPneumo.png",
    width = 29, height = 23, unit = "cm", res = 600)
cowplot::plot_grid(ser1, ser2,
                   nrow = 2,
                   labels = c("A", "B"))
dev.off()


# additional visualisation of serotype percentage per hospital & age ###############
# additional visualisation of serotype percentage per hospital & age
# levels = c("lombok", "sumbawa", "manado", "sorong"))
hospital <- unique(df_epi_gen_pneumo$hospital)
plotStore_hospital <- list()

for(a in hospital){
  plot <- df_epi_gen_pneumo %>% 
    dplyr::filter(hospital == a) %>% 
    dplyr::group_by(age_group_2vaccine, serotype_final_decision) %>%
    dplyr::summarise(count = n(), .groups = "drop") %>%
    dplyr::group_by(age_group_2vaccine) %>% 
    # percentage is calculated from count_serotype per hospital
    dplyr::mutate(percentage = count / sum(count) * 100,
                  age_group_2vaccine = factor(age_group_2vaccine,
                                    levels = c("< 1 year old",
                                               "1-2 years old",
                                               "3-5 years old")),
                  serotype_classification_PCV13_final_decision = case_when(
                    serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                                   "6A", "6B", "9V", "14", "18C",
                                                   "19A", "19F", "23F") ~ "VT",
                    serotype_final_decision == "nontypeable" ~ "nontypeable",
                    TRUE ~ "NVT"
                  ),
                  serotype_classification_PCV15_final_decision = case_when(
                    serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                                   "6A", "6B", "9V", "14", "18C",
                                                   "19A", "19F", "23F",
                                                   "22F", "33F") ~ "VT",
                    serotype_final_decision == "nontypeable" ~ "nontypeable",
                    TRUE ~ "NVT"
                  ),
                  serotype_classification_PCV13_final_decision = case_when(
                    serotype_classification_PCV13_final_decision == "nontypeable" ~ " ",
                    TRUE ~ serotype_classification_PCV13_final_decision
                  ),
                  serotype_classification_PCV13_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                                        levels = c("VT", "NVT", " ")),
    ) %>% 
    ggplot(.,
           aes(x = serotype_final_decision,
               y = percentage,
               fill = age_group_2vaccine)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    # geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 3) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    facet_grid(~ serotype_classification_PCV13_final_decision,
               scales = "free_x",
               space = "free_x"
    ) +
    scale_fill_manual(values = c(col_map)) +
    labs(x = "Category", y = "Percentage", 
         # title = "All Serotypes"
    ) +
    theme_bw() +
    ggtitle(a) +
    labs(x = NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          # legend.position = "none",
          legend.direction = "vertical",
          # legend.justification = "bottom",
          legend.background = element_rect(fill = NA, color = NA),
          legend.title = element_blank(),
          legend.margin = margin(t = -50),
          legend.spacing.y = unit(-0.3, "cm"))
  
  # redefine plots
  plotStore_hospital[[a]] <- plot
}

png(file = "pictures/genData_serotypes_classification_filterPneumo_hospital.png",
    width = 29, height = 46, unit = "cm", res = 600)
cowplot::plot_grid(plotStore_hospital[[1]],
                   plotStore_hospital[[3]],
                   plotStore_hospital[[4]],
                   plotStore_hospital[[2]],
                   nrow = 4)
dev.off()

# additional visualisation of serotype percentage per PCV13-implemented hospital & age
# levels = c("PCV13-implemented hospital", "Not yet implemented hospital"))
vaccination_status_hospital <- c("PCV13-implemented hospital (Lombok & Sumbawa)",
                             "Not yet implemented hospital (Manado & Sorong)")
plotStore_vacchospital <- list()

for(a in vaccination_status_hospital){
  plot <- df_epi_gen_pneumo %>% 
    dplyr::mutate(vaccination_status_hospital = case_when(
      hospital == "lombok" |
        hospital == "sumbawa" ~ "PCV13-implemented hospital (Lombok & Sumbawa)",
      TRUE ~ "Not yet implemented hospital (Manado & Sorong)"
    )
    ) %>% 
    dplyr::filter(vaccination_status_hospital == a) %>% 
    dplyr::group_by(age_group_2vaccine, serotype_final_decision) %>%
    dplyr::summarise(count = n(), .groups = "drop") %>%
    dplyr::group_by(age_group_2vaccine) %>% 
    # percentage is calculated from count_serotype per hospital
    dplyr::mutate(percentage = count / sum(count) * 100,
                  age_group_2vaccine = factor(age_group_2vaccine,
                                            levels = c("< 1 year old",
                                                       "1-2 years old",
                                                       "3-5 years old")),
                  serotype_classification_PCV13_final_decision = case_when(
                    serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                                   "6A", "6B", "9V", "14", "18C",
                                                   "19A", "19F", "23F") ~ "VT",
                    serotype_final_decision == "nontypeable" ~ "nontypeable",
                    TRUE ~ "NVT"
                  ),
                  serotype_classification_PCV15_final_decision = case_when(
                    serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                                   "6A", "6B", "9V", "14", "18C",
                                                   "19A", "19F", "23F",
                                                   "22F", "33F") ~ "VT",
                    serotype_final_decision == "nontypeable" ~ "nontypeable",
                    TRUE ~ "NVT"
                  ),
                  serotype_classification_PCV13_final_decision = case_when(
                    serotype_classification_PCV13_final_decision == "nontypeable" ~ " ",
                    TRUE ~ serotype_classification_PCV13_final_decision
                  ),
                  serotype_classification_PCV13_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                                        levels = c("VT", "NVT", " ")),
    ) %>% 
    ggplot(.,
           aes(x = serotype_final_decision,
               y = percentage,
               fill = age_group_2vaccine)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    # geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 3) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    facet_grid(~ serotype_classification_PCV13_final_decision,
               scales = "free_x",
               space = "free_x"
    ) +
    scale_fill_manual(values = c(col_map)) +
    labs(x = "Category", y = "Percentage", 
         # title = "All Serotypes"
    ) +
    theme_bw() +
    ggtitle(a) +
    labs(x = NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          # legend.position = "none",
          legend.direction = "vertical",
          # legend.justification = "bottom",
          legend.background = element_rect(fill = NA, color = NA),
          legend.title = element_blank(),
          legend.margin = margin(t = -50),
          legend.spacing.y = unit(-0.3, "cm"))
  
  # redefine plots
  plotStore_vacchospital[[a]] <- plot
}

png(file = "pictures/genData_serotypes_classification_filterPneumo_vacchospital.png",
    width = 29, height = 23, unit = "cm", res = 600)
cowplot::plot_grid(plotlist = plotStore_vacchospital,
                   nrow = 2)
dev.off()


# additional visualisation of serotype percentage per hospital & age (year) ########
# additional visualisation of serotype percentage per hospital & age
# levels = c("lombok", "sumbawa", "manado", "sorong"))
hospital <- unique(df_epi_gen_pneumo$hospital)
plotStore_hospital <- list()

for(a in hospital){
  plot <- df_epi_gen_pneumo %>% 
    dplyr::filter(hospital == a) %>% 
    dplyr::group_by(age_year, serotype_final_decision) %>%
    dplyr::summarise(count = n(), .groups = "drop") %>%
    dplyr::group_by(age_year) %>% 
    # percentage is calculated from count_serotype per hospital
    dplyr::mutate(percentage = count / sum(count) * 100,
                  age_year = factor(age_year,
                                    levels = c("0", "1", "2", "3", "4", "5")),
                  serotype_classification_PCV13_final_decision = case_when(
                    serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                                   "6A", "6B", "9V", "14", "18C",
                                                   "19A", "19F", "23F") ~ "VT",
                    serotype_final_decision == "nontypeable" ~ "nontypeable",
                    TRUE ~ "NVT"
                  ),
                  serotype_classification_PCV15_final_decision = case_when(
                    serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                                   "6A", "6B", "9V", "14", "18C",
                                                   "19A", "19F", "23F",
                                                   "22F", "33F") ~ "VT",
                    serotype_final_decision == "nontypeable" ~ "nontypeable",
                    TRUE ~ "NVT"
                  ),
                  serotype_classification_PCV13_final_decision = case_when(
                    serotype_classification_PCV13_final_decision == "nontypeable" ~ " ",
                    TRUE ~ serotype_classification_PCV13_final_decision
                  ),
                  serotype_classification_PCV13_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                                        levels = c("VT", "NVT", " ")),
    ) %>% 
    ggplot(.,
           aes(x = serotype_final_decision,
               y = percentage,
               fill = age_year)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    # geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 3) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    facet_grid(~ serotype_classification_PCV13_final_decision,
               scales = "free_x",
               space = "free_x"
    ) +
    scale_fill_manual(values = c(col_map)) +
    labs(x = "Category", y = "Percentage", 
         # title = "All Serotypes"
    ) +
    theme_bw() +
    ggtitle(a) +
    labs(x = NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          # legend.position = "none",
          legend.direction = "vertical",
          # legend.justification = "bottom",
          legend.background = element_rect(fill = NA, color = NA),
          legend.title = element_blank(),
          legend.margin = margin(t = -50),
          legend.spacing.y = unit(-0.3, "cm"))
  
  # redefine plots
  plotStore_hospital[[a]] <- plot
}

png(file = "pictures/genData_serotypes_classification_filterPneumo_hospital_year.png",
    width = 29, height = 46, unit = "cm", res = 600)
cowplot::plot_grid(plotStore_hospital[[1]],
                   plotStore_hospital[[3]],
                   plotStore_hospital[[4]],
                   plotStore_hospital[[2]],
                   nrow = 4)
dev.off()

# additional visualisation of serotype percentage per PCV13-implemented hospital & age
# levels = c("PCV13-implemented hospital", "Not yet implemented hospital"))
vaccination_status_hospital <- c("PCV13-implemented hospital (Lombok & Sumbawa)",
                             "Not yet implemented hospital (Manado & Sorong)")
plotStore_vacchospital <- list()

for(a in vaccination_status_hospital){
  plot <- df_epi_gen_pneumo %>% 
    dplyr::mutate(vaccination_status_hospital = case_when(
      hospital == "lombok" |
        hospital == "sumbawa" ~ "PCV13-implemented hospital (Lombok & Sumbawa)",
      TRUE ~ "Not yet implemented hospital (Manado & Sorong)"
    )
    ) %>% 
    dplyr::filter(vaccination_status_hospital == a) %>% 
    dplyr::group_by(age_year, serotype_final_decision) %>%
    dplyr::summarise(count = n(), .groups = "drop") %>%
    dplyr::group_by(age_year) %>% 
    # percentage is calculated from count_serotype per hospital
    dplyr::mutate(percentage = count / sum(count) * 100,
                  age_year = factor(age_year,
                                    levels = c("0", "1", "2", "3", "4", "5")),
                  serotype_classification_PCV13_final_decision = case_when(
                    serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                                   "6A", "6B", "9V", "14", "18C",
                                                   "19A", "19F", "23F") ~ "VT",
                    serotype_final_decision == "nontypeable" ~ "nontypeable",
                    TRUE ~ "NVT"
                  ),
                  serotype_classification_PCV15_final_decision = case_when(
                    serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                                   "6A", "6B", "9V", "14", "18C",
                                                   "19A", "19F", "23F",
                                                   "22F", "33F") ~ "VT",
                    serotype_final_decision == "nontypeable" ~ "nontypeable",
                    TRUE ~ "NVT"
                  ),
                  serotype_classification_PCV13_final_decision = case_when(
                    serotype_classification_PCV13_final_decision == "nontypeable" ~ " ",
                    TRUE ~ serotype_classification_PCV13_final_decision
                  ),
                  serotype_classification_PCV13_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                                        levels = c("VT", "NVT", " ")),
    ) %>% 
    ggplot(.,
           aes(x = serotype_final_decision,
               y = percentage,
               fill = age_year)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    # geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 3) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    facet_grid(~ serotype_classification_PCV13_final_decision,
               scales = "free_x",
               space = "free_x"
    ) +
    scale_fill_manual(values = c(col_map)) +
    labs(x = "Category", y = "Percentage", 
         # title = "All Serotypes"
    ) +
    theme_bw() +
    ggtitle(a) +
    labs(x = NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          # legend.position = "none",
          legend.direction = "vertical",
          # legend.justification = "bottom",
          legend.background = element_rect(fill = NA, color = NA),
          legend.title = element_blank(),
          legend.margin = margin(t = -50),
          legend.spacing.y = unit(-0.3, "cm"))
  
  # redefine plots
  plotStore_vacchospital[[a]] <- plot
}

png(file = "pictures/genData_serotypes_classification_filterPneumo_vacchospital_year.png",
    width = 29, height = 23, unit = "cm", res = 600)
cowplot::plot_grid(plotlist = plotStore_vacchospital,
                   nrow = 2)
dev.off()


# AMR analyses & viz ###########################################################
# load df_epi_gen_pneumo first
# AMR and virulence factors use pass qc samples (n = 314)
df_epi_gen_pneumo <- read.csv("inputs/genData_pneumo_with_epiData_with_final_pneumo_decision.csv") %>% 
  # dplyr::filter(workPoppunk_qc == "pass_qc") %>%
  dplyr::filter(workWGS_species_pw == "Streptococcus pneumoniae") %>% 
  dplyr::mutate(
    serotype_final_decision = case_when(
      serotype_final_decision == "mixed serotypes/serogroups" ~ "mixed serogroups",
      TRUE ~ serotype_final_decision
    ),
    serotype_final_decision = factor(serotype_final_decision,
                                     levels = c(
                                       # VT
                                       # "3", "6A/6B", "6A/6B/6C/6D", "serogroup 6",
                                       # "14", "17F", "18C/18B", "19A", "19F", "23F",
                                       "1", "3", "4", "5", "7F",
                                       "6A", "6B", "9V", "14", "18C",
                                       "19A", "19F", "23F",
                                       # NVT
                                       # "7C", "10A", "10B", "11A/11D", "13", "15A", "15B/15C",
                                       # "16F", "19B", "20", "23A", "23B", "24F/24B", "25F/25A/38",
                                       # "28F/28A", "31", "34", "35A/35C/42", "35B/35D", "35F",
                                       # "37", "39", "mixed serogroups",
                                       "serogroup 6", "6C", "7C",
                                       "10", "10A", "10B", "11A", "13",
                                       "15A", "15B", "15C","15B/15C", "16F",
                                       "17F", "18A", "18B", "19B", "20", "20B",
                                       "21", "23A", "23B", "23B1",
                                       "24F", "24B/C/F", "24B/24C/24F", "serogroup 24",
                                       "25B", "25F",
                                       "28A", "31", "33B", "33G",
                                       "34", "35A", "35B", "35C", "35F", "37",
                                       "37F", "38", "39",
                                       "nontypeable")),
    serotype_classification_PCV13_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                          levels = c("VT", "NVT", "nontypeable")),
    serotype_classification_PCV15_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                          levels = c("VT", "NVT", "nontypeable"))
  ) %>%
  glimpse()

# cumulative AMR per-PCV13 vaccine type
amr_pcv13_long <- df_epi_gen_pneumo %>% 
  dplyr::select(serotype_classification_PCV13_final_decision,
                contains("workWGS_AMR_logic_class"),
                -workWGS_AMR_logic_class_counts
  ) %>% 
  tidyr::pivot_longer(
    cols = contains("workWGS_AMR_logic_class"),
    names_to = "class",
    values_to = "logic"
  ) %>% 
  dplyr::group_by(serotype_classification_PCV13_final_decision, class, logic) %>%
  dplyr::summarise(count = n(),
                   .groups = "drop") %>% 
  dplyr::left_join(
    df_epi_gen_pneumo %>% 
      dplyr::select(serotype_classification_PCV13_final_decision,
                    contains("workWGS_AMR_logic_class"),
                    -workWGS_AMR_logic_class_counts
      ) %>% 
      tidyr::pivot_longer(
        cols = contains("workWGS_AMR_logic_class"),
        names_to = "class",
        values_to = "logic"
      ) %>% 
      dplyr::group_by(serotype_classification_PCV13_final_decision, class) %>%
      dplyr::summarise(count_class = n(),
                       .groups = "drop")
    ,
    by = c("serotype_classification_PCV13_final_decision", "class")
  ) %>% 
  dplyr::mutate(
    percent = count/count_class*100
  ) %>% 
  dplyr::filter(
    logic == "TRUE"
  ) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(
    class = gsub("workWGS_AMR_logic_class_", "", class)
  ) %>% 
  # view() %>% 
  glimpse()

amr_grouped <- amr_pcv13_long %>% 
  # dplyr::filter(logic == "TRUE") %>% 
  dplyr::group_by(class, logic) %>% 
  dplyr::summarise(count = sum(count)) %>% 
  dplyr::ungroup() %>% 
  glimpse()

amr1 <- ggplot(amr_pcv13_long, aes(x = serotype_classification_PCV13_final_decision,
                                   y = percent,
                                   fill = class)) +
  # geom_line(size = 1.5) +
  geom_bar(stat = "identity", position = position_stack()) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  # geom_text(aes(label = paste0(round(percentage, 2), "%")),
  #           position = position_dodge(width = 0.9),
  #           vjust = -0.3) +
  # scale_fill_manual(values = c(col_map)) +
  labs(x = " ", y = "Percentage", 
       # title = "All Serotypes"
  ) +
  theme_bw() +
  theme(legend.position = "none")
# theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 10),
#       legend.position = "bottom", # c(0.02, 0.75),
#       legend.direction = "horizontal",
#       legend.justification = c("centre", "top"),
#       legend.background = element_rect(fill = NA, color = NA),
#       legend.title = element_blank(),
#       legend.margin = margin(t = -10),
#       legend.spacing.y = unit(-0.3, "cm"))


# cumulative AMR per-serotype
amr_ser_long <- df_epi_gen_pneumo %>% 
  dplyr::select(serotype_final_decision,
                contains("workWGS_AMR_logic_class"),
                -workWGS_AMR_logic_class_counts
  ) %>% 
  tidyr::pivot_longer(
    cols = contains("workWGS_AMR_logic_class"),
    names_to = "class",
    values_to = "logic"
  ) %>% 
  dplyr::group_by(serotype_final_decision, class, logic) %>%
  dplyr::summarise(count = n(),
                   .groups = "drop") %>% 
  dplyr::left_join(
    df_epi_gen_pneumo %>% 
      dplyr::select(serotype_final_decision,
                    contains("workWGS_AMR_logic_class"),
                    -workWGS_AMR_logic_class_counts
      ) %>% 
      tidyr::pivot_longer(
        cols = contains("workWGS_AMR_logic_class"),
        names_to = "class",
        values_to = "logic"
      ) %>% 
      dplyr::group_by(serotype_final_decision, class) %>%
      dplyr::summarise(count_class = n(),
                       .groups = "drop")
    ,
    by = c("serotype_final_decision", "class")
  ) %>% 
  dplyr::mutate(
    percent = count/count_class*100
  ) %>% 
  dplyr::filter(
    logic == "TRUE"
  ) %>% 
  # dplyr::group_by(serotype_final_decision, class) %>%
  # dplyr::summarise(percent = count/sum(count)*100) %>% 
  dplyr::left_join(
    df_epi_gen_pneumo %>% 
      dplyr::select(serotype_final_decision,
                    serotype_classification_PCV13_final_decision) %>% 
      dplyr::mutate(
        # slightly change classifications
        serotype_classification_PCV13_final_decision = case_when(
          serotype_classification_PCV13_final_decision == "nontypeable" ~ " ",
          TRUE ~ serotype_classification_PCV13_final_decision
        ),
        serotype_classification_PCV13_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                              levels = c("VT", "NVT", " "))
      )
    ,
    by = "serotype_final_decision"
    ,
    relationship = "many-to-many"
  ) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(
    class = gsub("workWGS_AMR_logic_class_", "", class)
  ) %>% 
  # view() %>% 
  glimpse()

amr2 <- ggplot(amr_ser_long, aes(x = serotype_final_decision,
                                 y = percent,
                                 fill = class)) +
  # geom_line(size = 1.5) +
  geom_bar(stat = "identity", position = position_stack()) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  facet_grid(~ serotype_classification_PCV13_final_decision,
             scales = "free_x",
             space = "free_x"
  ) +
  # geom_text(aes(label = paste0(round(percentage, 2), "%")),
  #           position = position_dodge(width = 0.9),
  #           vjust = -0.3) +
  # scale_fill_manual(values = c(col_map)) +
  labs(x = " ", y = "Percentage", 
       # title = "All Serotypes"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "bottom", # c(0.02, 0.75),
        legend.direction = "horizontal",
        legend.justification = c("centre", "top"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        legend.margin = margin(t = -25),
        legend.spacing.y = unit(-0.3, "cm"))


png(file = "pictures/genData_amr_filterPneumo.png",
    width = 29, height = 25, unit = "cm", res = 600)
cowplot::plot_grid(amr1, amr2,
                   nrow = 2,
                   labels = c("A", "B"))
dev.off()



# numeric AMR counts ###########################################################
df_amr_counts_summary <- df_epi_gen_pneumo %>% 
  dplyr::group_by(workWGS_AMR_logic_class_counts, serotype_final_decision) %>%
  dplyr::summarise(count = n()) %>% 
  dplyr::left_join(
    df_epi_gen_pneumo %>% 
      dplyr::group_by(serotype_final_decision) %>%
      dplyr::summarise(count_serotype = n())
    ,
    by = "serotype_final_decision"
  ) %>% 
  dplyr::mutate(
    percentage = ifelse(workWGS_AMR_logic_class_counts == "MDR",
                        count/count_serotype*100,
                        NA_real_),
    y_pos = count_serotype + 2
  ) %>% 
  dplyr::left_join(
    df_epi_gen_pneumo %>%
      dplyr::select(serotype_final_decision,
                    serotype_classification_PCV13_final_decision) %>%
      dplyr::mutate(
        # slightly change classifications
        serotype_classification_PCV13_final_decision = case_when(
          serotype_classification_PCV13_final_decision == "nontypeable" ~ " ",
          TRUE ~ serotype_classification_PCV13_final_decision
        ),
        serotype_classification_PCV13_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                              levels = c("VT", "NVT", " "))
      )
    ,
    by = "serotype_final_decision"
  ) %>%
  dplyr::mutate(
    workWGS_AMR_logic_class_counts = as.character(workWGS_AMR_logic_class_counts),
    workWGS_AMR_logic_class_counts = factor(workWGS_AMR_logic_class_counts,
                                            levels = c("7", "6", "5", "4", "3",
                                                       "2", "1", "0"))
  ) %>% 
  dplyr::distinct() %>% 
  # view() %>%
  glimpse()

# test plot
mdr1 <- ggplot(df_amr_counts_summary, aes(x = serotype_final_decision,
                                          y = count,
                                          fill = workWGS_AMR_logic_class_counts)) +
  # geom_line(size = 1.5) +
  geom_bar(stat = "identity", position = position_stack()) +
  # scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  facet_grid(~ serotype_classification_PCV13_final_decision,
             scales = "free_x",
             space = "free_x"
  ) +
  scale_fill_manual(values = c(col_map)) +
  labs(x = " ", y = "Count", 
       # title = "All Serotypes"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = c(0.32, 0.75),
        legend.direction = "vertical",
        legend.justification = c("left", "top"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        legend.margin = margin(t = -50),
        legend.spacing.y = unit(-0.3, "cm")) +
  guides(fill = guide_legend(ncol = 2))

df_amr_counts_classification_summary <- df_epi_gen_pneumo %>% 
  dplyr::count(workWGS_AMR_logic_class_counts) %>%
  dplyr::mutate(percentage = n / sum(n) * 100) %>% 
  glimpse()

df_amr_counts_classification_perhospital_summary <- df_epi_gen_pneumo %>% 
  dplyr::count(workWGS_AMR_logic_class_counts, hospital) %>%
  dplyr::mutate(percentage = n / sum(n) * 100) %>% 
  glimpse()



# MDR flag
df_amr_mdr_summary <- df_epi_gen_pneumo %>% 
  dplyr::group_by(workWGS_AMR_MDR_flag, serotype_final_decision) %>%
  dplyr::summarise(count = n()) %>% 
  dplyr::left_join(
    df_epi_gen_pneumo %>% 
      dplyr::group_by(serotype_final_decision) %>%
      dplyr::summarise(count_serotype = n())
    ,
    by = "serotype_final_decision"
  ) %>% 
  dplyr::mutate(
    percentage = ifelse(workWGS_AMR_MDR_flag == "MDR",
                        count/count_serotype*100,
                        NA_real_),
    y_pos = count_serotype + 0.2
  ) %>% 
  dplyr::left_join(
    df_epi_gen_pneumo %>%
      dplyr::select(serotype_final_decision,
                    serotype_classification_PCV13_final_decision) %>%
      dplyr::mutate(
        # slightly change classifications
        serotype_classification_PCV13_final_decision = case_when(
          serotype_classification_PCV13_final_decision == "nontypeable" ~ " ",
          TRUE ~ serotype_classification_PCV13_final_decision
        ),
        serotype_classification_PCV13_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                              levels = c("VT", "NVT", " "))
      )
    ,
    by = "serotype_final_decision"
  ) %>%
  dplyr::distinct() %>% 
  # view() %>%
  glimpse()

# test plot
mdr2 <- ggplot(df_amr_mdr_summary, aes(x = serotype_final_decision,
                                       y = count,
                                       fill = workWGS_AMR_MDR_flag)) +
  # geom_line(size = 1.5) +
  geom_bar(stat = "identity", position = position_stack()) +
  # scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  facet_grid(~ serotype_classification_PCV13_final_decision,
             scales = "free_x",
             space = "free_x"
  ) +
  geom_text(aes(x = serotype_final_decision,
                y = y_pos,
                label = ifelse(is.na(percentage),
                               NA,
                               paste0(round(percentage, 1), "%"))),
            # position = position_stack(vjust = 0),
            angle = 90,
            check_overlap = TRUE
  ) +
  scale_fill_manual(values = c(col_map)) +
  labs(x = " ", y = "Count", 
       # title = "All Serotypes"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = c(0.32, 0.75),
        legend.direction = "vertical",
        legend.justification = c("left", "top"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        legend.margin = margin(t = -50),
        legend.spacing.y = unit(-0.3, "cm"))

png(file = "pictures/genData_mdr_filterPneumo.png",
    width = 29, height = 25, unit = "cm", res = 600)
cowplot::plot_grid(mdr1, mdr2,
                   nrow = 2,
                   rel_heights = c(1, 1.3),
                   labels = c("A", "B"))
dev.off()


# deep dive of MDR in hospital #################################################
hospital <- unique(df_epi_gen_pneumo$hospital)
plotStore_hospital <- list()

for(a in hospital){
  plot <- df_epi_gen_pneumo %>%
    dplyr::filter(hospital == a) %>% 
    dplyr::group_by(workWGS_AMR_MDR_flag, serotype_final_decision) %>%
    dplyr::summarise(count = n()) %>% 
    dplyr::left_join(
      df_epi_gen_pneumo %>% 
        dplyr::filter(hospital == a) %>% 
        dplyr::group_by(serotype_final_decision) %>%
        dplyr::summarise(count_serotype = n())
      ,
      by = "serotype_final_decision"
    ) %>% 
    dplyr::mutate(
      percentage = ifelse(workWGS_AMR_MDR_flag == "MDR",
                          count/count_serotype*100,
                          NA_real_),
      y_pos = count_serotype
    ) %>% 
    dplyr::left_join(
      df_epi_gen_pneumo %>%
        dplyr::select(serotype_final_decision,
                      serotype_classification_PCV13_final_decision) %>%
        dplyr::mutate(
          # slightly change classifications
          serotype_classification_PCV13_final_decision = case_when(
            serotype_classification_PCV13_final_decision == "nontypeable" ~ " ",
            TRUE ~ serotype_classification_PCV13_final_decision
          ),
          serotype_classification_PCV13_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                                levels = c("VT", "NVT", " "))
        )
      ,
      by = "serotype_final_decision"
    ) %>%
    dplyr::distinct() %>% 
    ggplot(.,
           aes(x = serotype_final_decision,
               y = count,
               fill = workWGS_AMR_MDR_flag)) +
    # geom_line(size = 1.5) +
    geom_bar(stat = "identity", position = position_stack()) +
    # scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    facet_grid(~ serotype_classification_PCV13_final_decision,
               scales = "free_x",
               space = "free_x"
    ) +
    geom_text(aes(x = serotype_final_decision,
                  y = y_pos,
                  label = ifelse(is.na(percentage),
                                 NA,
                                 paste0(round(percentage, 1), "%"))),
              # position = position_stack(vjust = 1.3),
              angle = 90,
              # vjust = -3,
              check_overlap = TRUE
    ) +
    scale_fill_manual(values = c(col_map)) +
    labs(x = " ", y = "Count", 
         # title = "All Serotypes"
    ) +
    theme_bw() +
    ggtitle(a) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          legend.position = "none",
          legend.direction = "vertical",
          legend.justification = c("left", "top"),
          legend.background = element_rect(fill = NA, color = NA),
          legend.title = element_blank(),
          legend.margin = margin(t = -50),
          legend.spacing.y = unit(-0.3, "cm"))
  
  # redefine plots
  plotStore_hospital[[a]] <- plot
}


png(file = "pictures/genData_mdr_filterPneumo_hospital.png",
    width = 29, height = 46, unit = "cm", res = 600)
cowplot::plot_grid(plotStore_hospital[[1]],
                   plotStore_hospital[[3]],
                   plotStore_hospital[[4]],
                   plotStore_hospital[[2]],
                   nrow = 4)
dev.off()


# deep dive of MDR in PCV13-implemented hospital ###############################
vaccination_status_hospital <- c("PCV13-implemented hospital (Lombok & Sumbawa)",
                             "Not yet implemented hospital (Manado & Sorong)")
plotStore_vacchospital <- list()

for(a in vaccination_status_hospital){
  plot <- df_epi_gen_pneumo %>%
    dplyr::mutate(vaccination_status_hospital = case_when(
      hospital == "lombok" |
        hospital == "sumbawa" ~ "PCV13-implemented hospital (Lombok & Sumbawa)",
      TRUE ~ "Not yet implemented hospital (Manado & Sorong)"
    )
    ) %>% 
    dplyr::filter(vaccination_status_hospital == a) %>% 
    dplyr::group_by(workWGS_AMR_MDR_flag, serotype_final_decision) %>%
    dplyr::summarise(count = n()) %>% 
    dplyr::left_join(
      df_epi_gen_pneumo %>% 
        dplyr::mutate(vaccination_status_hospital = case_when(
          hospital == "lombok" |
            hospital == "sumbawa" ~ "PCV13-implemented hospital (Lombok & Sumbawa)",
          TRUE ~ "Not yet implemented hospital (Manado & Sorong)"
        )
        ) %>% 
        dplyr::filter(vaccination_status_hospital == a) %>% 
        dplyr::group_by(serotype_final_decision) %>%
        dplyr::summarise(count_serotype = n())
      ,
      by = "serotype_final_decision"
    ) %>% 
    dplyr::mutate(
      percentage = ifelse(workWGS_AMR_MDR_flag == "MDR",
                          count/count_serotype*100,
                          NA_real_),
      y_pos = count_serotype
    ) %>% 
    dplyr::left_join(
      df_epi_gen_pneumo %>%
        dplyr::select(serotype_final_decision,
                      serotype_classification_PCV13_final_decision) %>%
        dplyr::mutate(
          # slightly change classifications
          serotype_classification_PCV13_final_decision = case_when(
            serotype_classification_PCV13_final_decision == "nontypeable" ~ " ",
            TRUE ~ serotype_classification_PCV13_final_decision
          ),
          serotype_classification_PCV13_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                                levels = c("VT", "NVT", " "))
        )
      ,
      by = "serotype_final_decision"
    ) %>%
    dplyr::distinct() %>% 
    ggplot(.,
           aes(x = serotype_final_decision,
               y = count,
               fill = workWGS_AMR_MDR_flag)) +
    # geom_line(size = 1.5) +
    geom_bar(stat = "identity", position = position_stack()) +
    # scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    facet_grid(~ serotype_classification_PCV13_final_decision,
               scales = "free_x",
               space = "free_x"
    ) +
    geom_text(aes(x = serotype_final_decision,
                  y = y_pos,
                  label = ifelse(is.na(percentage),
                                 NA,
                                 paste0(round(percentage, 1), "%"))),
              # position = position_stack(vjust = 1.3),
              angle = 90,
              # vjust = -3,
              check_overlap = TRUE
    ) +
    scale_fill_manual(values = c(col_map)) +
    labs(x = " ", y = "Count", 
         # title = "All Serotypes"
    ) +
    theme_bw() +
    ggtitle(a) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          legend.position = "none",
          legend.direction = "vertical",
          legend.justification = c("left", "top"),
          legend.background = element_rect(fill = NA, color = NA),
          legend.title = element_blank(),
          legend.margin = margin(t = -50),
          legend.spacing.y = unit(-0.3, "cm"))
  
  # redefine plots
  plotStore_vacchospital[[a]] <- plot
}

png(file = "pictures/genData_mdr_filterPneumo_vacchospital.png",
    width = 29, height = 23, unit = "cm", res = 600)
cowplot::plot_grid(plotlist = plotStore_vacchospital,
                   nrow = 2)
dev.off()