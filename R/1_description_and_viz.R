library(tidyverse)
library(ComplexUpset)
library(ggsankey)
source("global/fun.R")

# Load df_epi_clean (from 1_data_preparation_1epi.R) first
df_epi_clean <- read.csv("inputs/df_epi_hospital_cleaned.csv") %>% 
  dplyr::filter(
    diag_3chronic_immunodeficiency != 1,
    diag_3chronic_systemic_steroid_use != 1,
    diag_3chronic_malignancy != 1,
    diag_3chronic_leukemia_lymphoma_other_blood_cancer != 1,
    diag_3chronic_asplenia != 1
  ) %>% 
  dplyr::mutate(
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
    age_group_2vaccine = factor(age_group_2vaccine,
                                levels = c("<2 months",
                                           "2-59 months",
                                           "5-18 years"
                                )),
  ) %>% 
  glimpse()

# quick viz to see data distribution
hist(df_epi_clean$age_month)
hist(df_epi_clean$age_year)
barplot(table(df_epi_clean$age_group_1semester))
barplot(table(df_epi_clean$age_group_2vaccine))

# usia all data ################################################################
age_all <- df_epi_clean %>% 
  dplyr::group_by(age_group_2vaccine) %>% 
  dplyr::summarise(count = n()) %>% 
  dplyr::mutate(percentage = round(count/sum(count)*100, 1),
                report = paste0(count, " (", percentage, "%)")) %>% 
  ggplot(.
         ,
         aes(x = age_group_2vaccine, y = count, fill = age_group_2vaccine)) +
  geom_bar(stat = "identity") +
  # geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, size = 3) +
  # scale_y_log10() +
  # scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  # labs(x = " ", y = "Percentage", 
  #      # title = "All Serotypes"
  # ) +
  # scale_fill_manual(values = c(col_map)) +
  theme_bw() +
  geom_text(aes(label = report), size = 3, vjust = -0.1) + 
  scale_fill_manual(values=col_map) +
  labs(x = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = c(0.75, 0.75),
        legend.direction = "vertical",
        legend.justification = c("left", "top"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        legend.margin = margin(t = -50),
        legend.spacing.y = unit(-0.3, "cm"))
age_all

# piechart age per-hospital
hospitals <- unique(df_epi_clean$hospital)
plotStore_hospital <- list()

for(h in hospitals){
  p <- df_epi_clean %>% 
    dplyr::filter(hospital == h) %>% 
    dplyr::group_by(age_group_2vaccine) %>% 
    dplyr::summarise(count = n()) %>%
    dplyr::mutate(prop = round(count/sum(count)*100, 1),
                  report = paste0(count, "\n(", prop, "%)")
    ) %>% 
    dplyr::arrange(desc(age_group_2vaccine)) %>%
    dplyr::mutate(label_coor = cumsum(prop)-0.5*prop, # arrange first & mutate
    ) %>% 
    ggplot(
      .,
      aes(x = "", y = prop, fill = age_group_2vaccine)
    ) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start=0) +
    ggtitle(h) +
    geom_text(aes(y = label_coor,
                  label = report),
              colour = "white", size = 2) +
    guides(fill = guide_legend(nrow = 3)) +
    scale_fill_manual(values=col_map) +
    theme_void() +
    theme(#legend.title = element_blank(),
          legend.position = "none",
          plot.title = element_text(size = 10)
          )
  
  # redefine plots
  plotStore_hospital[[h]] <- p
}

age_hospital <- cowplot::plot_grid(plotlist = plotStore_hospital,
                                   nrow = 3)
age_hospital

age_total <- cowplot::plot_grid(age_all, age_hospital,
                                ncol = 2,
                                rel_widths = c(2, 1),
                                labels = c("A", "B"))
age_total

png(file = "pictures/epi_age_consensus.png", width = 23, height = 12,
    unit = "cm", res = 600)
print(age_total)
dev.off()


# seasonality based on age_group_2vaccine ######################################
season_month <- df_epi_clean %>% 
  dplyr::mutate(
    iso_week = paste0(year(date_hospitalised), "-W", sprintf("%02d", week(date_hospitalised)), "-1"),
    yearWeek =ISOweek::ISOweek2date(iso_week),
    yearMonth = floor_date(as.Date(date_hospitalised), unit = "month"),
  ) %>% 
  group_by(yearMonth, age_group_2vaccine
  ) %>% 
  summarise(count = n()) %>% 
  ggplot(.
         ,
         aes(x = yearMonth, y = count, fill = age_group_2vaccine)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  # scale_x_date(date_labels = "%Y", date_breaks = "1 year",
  #              limits = c(as.Date(min(df_epi_clean$date_hospitalised)),
  #                         as.Date(max(df_epi_clean$date_hospitalised)))
  # ) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month",
               limits = c(as.Date("2023-06-01"),
                          as.Date("2025-02-01"))
  ) +
  scale_fill_manual(values=col_map) +
  labs(x = "Hospitalisation by Month",
       y = "Number of Patients") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "none", # "bottom", #c(0.02, 0.75),
        # legend.direction = "horizontal",
        # legend.justification = c("left", "top"),
        # legend.background = element_rect(fill = NA, color = NA),
        # legend.title = element_blank(),
        #legend.margin = margin(t = -50),
        # legend.spacing.y = unit(-0.3, "cm")
        )
season_month

# dry vs. rainy season
season_season <- df_epi_clean %>% 
  group_by(date_hospitalised_season, age_group_2vaccine) %>% 
  summarise(count = n()) %>%
  glimpse() %>% 
  ggplot(.
         ,
         aes(x = date_hospitalised_season,
             y = count,
             fill = age_group_2vaccine)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  scale_fill_manual(values=col_map) +
  labs(x = "Hospitalisation by Season",
       y = "Number of Patients") +
  theme(axis.text.x = element_text(size = 10),
        legend.position = "bottom", #c(0.02, 0.75),
        legend.direction = "horizontal",
        # legend.justification = c("left", "top"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        #legend.margin = margin(t = -50),
        legend.spacing.y = unit(-0.3, "cm"))
season_season

season_total <- cowplot::plot_grid(season_month, season_season,
                                   ncol = 1,
                                   rel_heights = c(1, 1),
                                   labels = c("A", "C"))
season_total

season_month2 <- df_epi_clean %>% 
  dplyr::mutate(
    iso_week = paste0(year(date_hospitalised), "-W",
                      sprintf("%02d", week(date_hospitalised)), "-1"),
    yearWeek =ISOweek::ISOweek2date(iso_week),
    yearMonth = floor_date(as.Date(date_hospitalised), unit = "month"),
  ) %>% 
  group_by(yearMonth, age_group_2vaccine, hospital
  ) %>% 
  summarise(count = n()) %>% 
  ggplot(.
         ,
         aes(x = yearMonth, y = count, fill = age_group_2vaccine)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  # scale_x_date(date_labels = "%Y", date_breaks = "1 year",
  #              limits = c(as.Date(min(df_epi_clean$date_hospitalised)),
  #                         as.Date(max(df_epi_clean$date_hospitalised)))
  # ) +
  facet_wrap(~ hospital, ncol = 1, scales = "free_y") +
  scale_x_date(date_labels = "%m", date_breaks = "3 month",
               limits = c(as.Date("2023-06-01"),
                          as.Date("2025-02-01"))
  ) +
  scale_fill_manual(values=col_map) +
  labs(x = "Hospitalisation by Month",
       y = "Number of Patients") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "none", # "bottom", #c(0.02, 0.75),
        # legend.direction = "horizontal",
        # legend.justification = c("left", "top"),
        # legend.background = element_rect(fill = NA, color = NA),
        # legend.title = element_blank(),
        #legend.margin = margin(t = -50),
        # legend.spacing.y = unit(-0.3, "cm")
  )
season_month2

# dry vs. rainy season
season_season2 <- df_epi_clean %>% 
  group_by(date_hospitalised_season, age_group_2vaccine, hospital) %>% 
  summarise(count = n()) %>%
  glimpse() %>% 
  ggplot(.
         ,
         aes(x = date_hospitalised_season,
             y = count,
             fill = age_group_2vaccine)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  scale_fill_manual(values=col_map) +
  labs(x = "Hospitalisation by Season",
       y = "Number of Patients") +
  facet_wrap(~ hospital, ncol = 1, scales = "free_y") +
  theme(axis.text.x = element_text(size = 10),
        legend.position = "none", #c(0.02, 0.75),
        # legend.direction = "horizontal",
        # legend.justification = c("left", "top"),
        # legend.background = element_rect(fill = NA, color = NA),
        # legend.title = element_blank(),
        # legend.margin = margin(t = -50),
        # legend.spacing.y = unit(-0.3, "cm")
        )
season_season2

season_total2 <- cowplot::plot_grid(season_month2, season_season2,
                                    ncol = 1,
                                    rel_heights = c(1, 1),
                                    labels = c("B", "D"))
season_total2

season_total_count <- cowplot::plot_grid(season_total, season_total2,
                                         ncol = 2,
                                         rel_widths = c(2,1))
season_total_count

count_total_all <- cowplot::plot_grid(season_total, season_total2,
                                      ncol = 2,
                                      rel_widths = c(2,1))
count_total_all

# boxplots of dry-rainy season
boxplot_age_gender <- df_epi_clean %>% 
  group_by(date_hospitalised_season,
           age_group_2vaccine, hospital, patient_gender) %>% 
  summarise(count = n()) %>% 
  ggplot(., aes(x = date_hospitalised_season,
                y = count)
                ) +
  geom_boxplot(alpha = 0.7, width = 0.6, fill = "skyblue") +
  geom_jitter(aes(colour = hospital), width = 0.1,
              size = 2, alpha = 0.8) +
  scale_colour_manual(values=col_map) +
  facet_grid(rows = vars(patient_gender), cols = vars(age_group_2vaccine),
             scales = "free_y") +
  labs(
    x = "Season",
    y = "Hospitalisations",
  ) +
  theme_bw(base_size = 13) +
  labs(x = NULL) +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank())
boxplot_age_gender

boxplot_age_hospital <- df_epi_clean %>% 
  group_by(date_hospitalised_season,
           age_group_2vaccine, hospital, patient_gender) %>% 
  summarise(count = n()) %>% 
  ggplot(., aes(x = date_hospitalised_season,
                y = count)) +
  geom_boxplot(alpha = 0.7, width = 0.6, fill = "skyblue") +
  geom_jitter(aes(colour = patient_gender),
              width = 0.1, size = 2, alpha = 0.8) +
  scale_colour_manual(values=col_map) +
  facet_grid(rows = vars(hospital), cols = vars(age_group_2vaccine),
             scales = "free_y") +
  # facet_wrap(~ patient_gender, scales = "free_y") +
  labs(
    x = "Season",
    y = "Hospitalisations",
  ) +
  theme_bw(base_size = 13) +
  labs(x = NULL) +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank(),
        strip.text.y = element_text(size = 5.1) # minimise facet_grid row names
  )
boxplot_age_hospital

boxplot_total_all <- cowplot::plot_grid(boxplot_age_gender,
                                        boxplot_age_hospital,
                                        ncol = 2,
                                        labels = c("E", "F"))
boxplot_total_all

count_plus_boxplot <- cowplot::plot_grid(count_total_all, boxplot_total_all,
                                         nrow = 2,
                                         rel_heights = c(1.5,1))
count_plus_boxplot

png(file = "pictures/epi_seasonality_consensus.png", width = 23, height = 29,
    unit = "cm", res = 600)
print(count_plus_boxplot)
dev.off()

# seasonality based on patient_gender ##########################################
season_month <- df_epi_clean %>% 
  dplyr::mutate(
    iso_week = paste0(year(date_hospitalised), "-W", sprintf("%02d", week(date_hospitalised)), "-1"),
    yearWeek =ISOweek::ISOweek2date(iso_week),
    yearMonth = floor_date(as.Date(date_hospitalised), unit = "month"),
  ) %>% 
  group_by(yearMonth, patient_gender
  ) %>% 
  summarise(count = n()) %>% 
  ggplot(.
         ,
         aes(x = yearMonth, y = count, fill = patient_gender)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  # scale_x_date(date_labels = "%Y", date_breaks = "1 year",
  #              limits = c(as.Date(min(df_epi_clean$date_hospitalised)),
  #                         as.Date(max(df_epi_clean$date_hospitalised)))
  # ) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month",
               limits = c(as.Date("2023-06-01"),
                          as.Date("2025-02-01"))
  ) +
  scale_fill_manual(values=col_map) +
  labs(x = "Hospitalisation by Month",
       y = "Number of Patients") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "none", # "bottom", #c(0.02, 0.75),
        # legend.direction = "horizontal",
        # legend.justification = c("left", "top"),
        # legend.background = element_rect(fill = NA, color = NA),
        # legend.title = element_blank(),
        #legend.margin = margin(t = -50),
        # legend.spacing.y = unit(-0.3, "cm")
  )
season_month

# dry vs. rainy season
season_season <- df_epi_clean %>% 
  group_by(date_hospitalised_season, patient_gender) %>% 
  summarise(count = n()) %>%
  glimpse() %>% 
  ggplot(.
         ,
         aes(x = date_hospitalised_season,
             y = count,
             fill = patient_gender)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  scale_fill_manual(values=col_map) +
  labs(x = "Hospitalisation by Season",
       y = "Number of Patients") +
  theme(axis.text.x = element_text(size = 10),
        legend.position = "bottom", #c(0.02, 0.75),
        legend.direction = "horizontal",
        # legend.justification = c("left", "top"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        #legend.margin = margin(t = -50),
        legend.spacing.y = unit(-0.3, "cm"))
season_season

season_total <- cowplot::plot_grid(season_month, season_season,
                                   ncol = 1,
                                   rel_heights = c(1, 1),
                                   labels = c("A", "B"))
season_total

png(file = "pictures/epi_seasonality_gender.png", width = 23, height = 23,
    unit = "cm", res = 600)
print(season_total)
dev.off()


# usia grouped by patient_prognosis
ggplot(df_epi_clean %>% 
         group_by(age_group_2vaccine, patient_prognosis) %>% 
         summarise(count = n())
       ,
       aes(x = age_group_2vaccine, y = count, fill = patient_prognosis)) +
  geom_bar(stat = "identity") +
  # geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, size = 3) +
  # scale_y_log10() +
  # scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  # labs(x = " ", y = "Percentage", 
  #      # title = "All Serotypes"
  # ) +
  # scale_fill_manual(values = c(col_map)) +
  theme_bw() +
  # scale_fill_manual(values=col_map) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "bottom", #c(0.02, 0.75),
        legend.direction = "horizontal",
        # legend.justification = c("left", "top"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        # legend.margin = margin(t = -50),
        legend.spacing.y = unit(-0.3, "cm"))


# usia grouped by pcv13 & hospital
# compiled vaccination coverage per hospital
df_compiled_vaccination_coverage <- dplyr::left_join(
  df_epi_clean %>% 
    dplyr::filter(age_group_2vaccine == "2-59 months") %>% 
    dplyr::group_by(hospital, vacc_pcv13_group) %>% 
    dplyr::summarise(count_vacc = n(), .groups = "drop") %>% 
    dplyr::mutate(
      count_vacc = case_when(
        is.na(count_vacc) ~ 0,
        TRUE ~ count_vacc
      )
    )
  ,
  df_epi_clean %>% 
    dplyr::filter(age_group_2vaccine == "2-59 months") %>% 
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
    hospital = factor(hospital,
                      levels = c(
                        "RSUD Kabupaten Sorong",
                        "RSUP Prof. Dr. R.D. Kandou",
                        "RSUP NTB"
                      ))
  ) %>% 
  # view() %>% 
  glimpse()

ggplot(df_compiled_vaccination_coverage, aes(x = vacc_pcv13_group,
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
  labs(x = "PCV13 vaccination in children aged 2-59 months", y = "Percentage", 
       # title = "All Serotypes"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
        legend.position = c(0.65, 0.75),
        legend.direction = "vertical",
        legend.justification = c("left", "top"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        legend.margin = margin(t = -50),
        legend.spacing.y = unit(-0.3, "cm"))

# # usia grouped by pcv13_group & patient_prognosis
# # count
# ggplot(
#   df_epi_clean %>% 
#     group_by(patient_prognosis, pcv13_group, age_group_2vaccine) %>% 
#     summarise(count_vaccinated = n(), .groups = "drop") %>% 
#     filter(
#       !is.na(patient_prognosis),
#       patient_prognosis != ""
#     )
#   ,
#   aes(x = age_group_2vaccine, y = count_vaccinated, fill = pcv13_group)
# ) +
#   geom_bar(stat = "identity") +
#   theme_bw() +
#   facet_wrap(~ patient_prognosis) +
#   theme(
#     legend.position = "bottom",
#     axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
#     legend.direction = "horizontal",
#     legend.title = element_blank()
#   )
# 
# # chisq or fisher's exact test test for children below 5 or 2
# contingency_table <- df_epi_clean %>% 
#   select(patient_prognosis,
#          age_group_2vaccine,
#          pcv13_group) %>% 
#   mutate(
#     patient_prognosis = case_when(
#       patient_prognosis == "Memburuk (Rujuk luar)" | patient_prognosis == "Memburuk (komplikasi/ sekuele)" ~ "Memburuk",
#       TRUE ~ patient_prognosis
#     )
#   ) %>% 
#   filter(
#     !is.na(patient_prognosis),
#     patient_prognosis != "",
#     age_group_2vaccine == "0 years" | age_group_2vaccine == "1-2 years" | age_group_2vaccine == "3-5 years"
#   ) %>%
#   count(patient_prognosis, pcv13_group) %>%
#   pivot_wider(
#     names_from = pcv13_group,
#     values_from = n,
#     values_fill = 0
#   ) %>%
#   column_to_rownames("patient_prognosis") %>%
#   # glimpse() %>% 
#   as.matrix()
# 
# contingency_table
# 
# # Step 2: Run Fisher's Exact Test
# fisher.test(contingency_table)
# 
# 
# # proportion
# ggplot(df_epi_clean %>% 
#          group_by(patient_prognosis, pcv13_group, age_group_2vaccine) %>% 
#          summarise(count_vaccinated = n()) %>% 
#          left_join(
#            df_epi_clean %>% 
#              group_by(patient_prognosis, age_group_2vaccine) %>% 
#              summarise(count_prognosis = n())
#            ,
#            by = c("age_group_2vaccine", "patient_prognosis")
#          ) %>% 
#          mutate(prop = round(count_vaccinated/count_prognosis*100, 1)) %>% 
#          filter(!is.na(patient_prognosis),
#                        patient_prognosis != "",
#          )
#        ,
#        aes(x = age_group_2vaccine, y = prop, fill = pcv13_group)) +
#   geom_bar(stat = "identity") +
#   theme_bw() +
#   facet_wrap(~ patient_prognosis) +
#   # facet_grid(pcv13_group ~ patient_prognosis) +
#   theme(legend.position = "bottom",
#         axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
#         # legend.position = c(-0.02, 0.75),
#         legend.direction = "horizontal",
#         # legend.justification = c("left", "top"),
#         # legend.background = element_rect(fill = NA, color = NA),
#         legend.title = element_blank(),
#         # legend.margin = margin(t = -50),
#         # legend.spacing.y = unit(-0.3, "cm")
#   )


# Length of Stay ###############################################################
hist(df_epi_clean$LOS_days)

# LOS calculations overall/per-ageGroups/per-area
# must clean up patient_room_originity & complications first!
LOS_age_sex <- df_epi_clean %>%
  group_by(age_group_2vaccine, patient_room_origin, patient_gender) %>% 
  summarise(
    median_los = median(LOS_days, na.rm = TRUE),
    # iqr_los = IQR(LOS_days, na.rm = TRUE),
    q1 = quantile(LOS_days, 0.25, na.rm = TRUE),
    q3 = quantile(LOS_days, 0.75, na.rm = TRUE)
  ) %>% 
  mutate(
    report = paste0(median_los, " (", q1, ", ", q3, ")"),
    patient_room_origin = factor(patient_room_origin,
                                 levels = c("NICU",
                                            "PICU",
                                            "Paediatric inpatient room"))
  ) %>% 
  # view() %>% 
  glimpse() %>% 
  ggplot(., aes(x = patient_room_origin)) +
  geom_point(aes(y = median_los), colour = "steelblue", size = 2) +
  geom_errorbar(aes(
    ymin = q1,
    ymax = q3,
    width = .1),
    colour = "steelblue") +
  theme_bw() +
  # facet_wrap(~age_group_2vaccine) +
  facet_grid(rows = vars(patient_gender), cols = vars(age_group_2vaccine),
             scales = "free_y") +
  labs(colour = NULL, x = NULL, y = "Length of Stay (Median (Q1-Q3))") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "bottom")
LOS_age_sex

LOS_age_hospital <- df_epi_clean %>%
  group_by(age_group_2vaccine, patient_room_origin, hospital) %>% 
  summarise(
    median_los = median(LOS_days, na.rm = TRUE),
    # iqr_los = IQR(LOS_days, na.rm = TRUE),
    q1 = quantile(LOS_days, 0.25, na.rm = TRUE),
    q3 = quantile(LOS_days, 0.75, na.rm = TRUE)
  ) %>% 
  mutate(
    report = paste0(median_los, " (", q1, ", ", q3, ")"),
    patient_room_origin = factor(patient_room_origin,
                                 levels = c("NICU",
                                            "PICU",
                                            "Paediatric inpatient room"))
  ) %>% 
  # view() %>% 
  glimpse() %>% 
  ggplot(., aes(x = patient_room_origin)) +
  geom_point(aes(y = median_los), colour = "steelblue", size = 2) +
  geom_errorbar(aes(
    ymin = q1,
    ymax = q3,
    width = .1),
    colour = "steelblue") +
  theme_bw() +
  # facet_wrap(~age_group_2vaccine) +
  facet_grid(rows = vars(hospital), cols = vars(age_group_2vaccine),
             scales = "free_y") +
  labs(colour = NULL, x = NULL, y = "Length of Stay (Median (Q1-Q3))") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "bottom",
        strip.text.y = element_text(size = 5.1))
LOS_age_hospital

LOS_age_sex_hospital <- cowplot::plot_grid(LOS_age_sex, LOS_age_hospital,
                                           labels = c("A", "B"))
LOS_age_sex_hospital

LOS_prematurity_all <- df_epi_clean %>%
  filter(age_group_2vaccine != "5-18 years") %>% 
  group_by(age_group_2vaccine, diag_3chronic_prematurity) %>% 
  summarise(
    median_los = median(LOS_days, na.rm = TRUE),
    q1 = quantile(LOS_days, 0.25, na.rm = TRUE),
    q3 = quantile(LOS_days, 0.75, na.rm = TRUE)
  ) %>% 
  mutate(
    report = paste0(median_los, " (", q1, ", ", q3, ")"),
    diag_3chronic_prematurity = ifelse(diag_3chronic_prematurity == 1, "Premature", "Normal"),
    diag_3chronic_prematurity = factor(diag_3chronic_prematurity,
                                       levels = c("Premature",
                                                  "Normal"))
  ) %>% 
  # view() %>% 
  glimpse() %>% 
  ggplot(., aes(x = diag_3chronic_prematurity)) +
  geom_point(aes(y = median_los), colour = "darkgreen", size = 2) +
  geom_errorbar(aes(
    ymin = q1,
    ymax = q3,
    width = .1),
    colour = "darkgreen") +
  theme_bw() +
  facet_wrap(~age_group_2vaccine) +
  labs(colour = NULL, x = NULL, y = "Length of Stay (Median (Q1-Q3))") +
  theme(legend.position = "bottom")
LOS_prematurity_all

LOS_prematurity_hospital <- df_epi_clean %>%
  filter(age_group_2vaccine != "5-18 years") %>% 
  group_by(age_group_2vaccine, diag_3chronic_prematurity, hospital) %>% 
  summarise(
    median_los = median(LOS_days, na.rm = TRUE),
    q1 = quantile(LOS_days, 0.25, na.rm = TRUE),
    q3 = quantile(LOS_days, 0.75, na.rm = TRUE)
  ) %>% 
  mutate(
    report = paste0(median_los, " (", q1, ", ", q3, ")"),
    diag_3chronic_prematurity = ifelse(diag_3chronic_prematurity == 1, "Premature", "Normal"),
    diag_3chronic_prematurity = factor(diag_3chronic_prematurity,
                                       levels = c("Premature",
                                                  "Normal"))
  ) %>% 
  # view() %>% 
  glimpse() %>% 
  ggplot(., aes(x = diag_3chronic_prematurity)) +
  geom_point(aes(y = median_los), colour = "darkgreen", size = 2) +
  geom_errorbar(aes(
    ymin = q1,
    ymax = q3,
    width = .1),
    colour = "darkgreen") +
  theme_bw() +
  # facet_wrap(~age_group_2vaccine) +
  facet_grid(rows = vars(hospital), cols = vars(age_group_2vaccine),
             scales = "free_y") +
  labs(colour = NULL, x = NULL, y = "Length of Stay (Median (Q1-Q3))") +
  theme(legend.position = "bottom",
        strip.text.y = element_text(size = 5.1))
LOS_prematurity_hospital

LOS_prematurity_combined <- cowplot::plot_grid(LOS_prematurity_all,
                                               LOS_prematurity_hospital)
LOS_prematurity_combined

LOS_final <- cowplot::plot_grid(LOS_age_sex_hospital,
                                LOS_prematurity_combined,
                                nrow = 2,
                                rel_heights = c(1.5, 1),
                                labels = c("", "C"))
LOS_final

png(file = "pictures/epi_LOS.png", width = 23, height = 23,
    unit = "cm", res = 600)
print(LOS_final)
dev.off()


################################################################################
# Combinations analysis
# source:
# https://www.epirhandbook.com/en/new_pages/combination_analysis.html
# https://krassowski.github.io/complex-upset/articles/Examples_Python.html
# 
# better to use ComplexUpset instead of UpSetR
# early signs
df_signs <- df_epi_clean %>%
  dplyr::transmute(
    hospital = hospital,
    age = age_group_2vaccine,
    bacteremia = diag_2signs_bacteraemia,
    pneumonia = diag_2signs_pneumonia,
    meningitis = diag_2signs_meningitis,
    pleural_effusion = diag_2signs_pleural_effusion
  ) %>%
  dplyr::mutate(across(c(bacteremia, pneumonia,
                         meningitis, pleural_effusion),
                       as.numeric)) %>%
  as.data.frame() %>% 
  glimpse()

png(file = "pictures/epi_combinations_1pneumo_all.png", width = 29, height = 15,
    unit = "cm", res = 600)
ComplexUpset::upset(
  df_signs,
  intersect = c("bacteremia",
                "pneumonia",
                "meningitis",
                "pleural_effusion"),
  # encode_sets=FALSE, # for annotate() to select the set by name disable encoding
  base_annotations = list(
    "Hospital\n(count)" = intersection_size(
      counts = FALSE,
      mapping = aes(fill=hospital),
      text = list(
        check_overlap=TRUE,
        vjust = -0.1,
        hjust = -0.1
      )) +
      scale_fill_manual(values=col_map)),
  annotations = list(
    "Age\n(%)" = ggplot(mapping=aes(fill=age)) +
      geom_bar(stat='count', position='fill') +
      scale_fill_manual(values=col_map) +
      scale_y_continuous(labels=scales::percent_format())
  ),
  # queries=list(
  #   upset_query(
  #     intersect=c('pneumonia', 'pleural_effusion'),
  #     color='red',
  #     fill='red',
  #     only_components=c('intersections_matrix', 'Intersection size')
  #   ),
  # ),
  wrap = F,
  mode = "distinct",
  width_ratio=0.1,
  min_degree=1, # min combination
  set_sizes=upset_set_size(mapping = aes(),
                           geom = geom_bar(aes(fill=hospital, x=group),
                                           width = 0.6),
                           position = "left",
                           filter_intersections = T) +
    scale_fill_manual(values=col_map) +
    geom_text(aes(label=..count..), hjust=1.1, stat='count') +
    expand_limits(y=800) +
    theme(legend.position = "none",
          axis.text.x=element_text(angle=90))
)
dev.off()

# early signs separated by hospital but filled by age groups
hospitals <- unique(df_epi_clean$hospital)
plotStore_hospital <- list()

for(h in hospitals){
  df_signs <- df_epi_clean %>%
    dplyr::transmute(
      hospital = hospital,
      age = age_group_2vaccine,
      bacteremia = diag_2signs_bacteraemia,
      pneumonia = diag_2signs_pneumonia,
      meningitis = diag_2signs_meningitis,
      pleural_effusion = diag_2signs_pleural_effusion
    ) %>%
    dplyr::filter(hospital == h) %>% 
    dplyr::mutate(across(c(bacteremia, pneumonia,
                           meningitis, pleural_effusion),
                         as.numeric)) %>%
    as.data.frame()
  
  p <- ComplexUpset::upset(
    df_signs,
    intersect = c("bacteremia",
                  "pneumonia",
                  "meningitis",
                  "pleural_effusion"),
    # encode_sets=FALSE, # for annotate() to select the set by name disable encoding
    base_annotations = list(
      "Age\n(count)" = intersection_size(
        counts = FALSE,
        mapping = aes(fill=age),
        text = list(
          check_overlap=TRUE,
          vjust = -0.1,
          hjust = -0.1
        )) +
        scale_fill_manual(values=col_map)),
    wrap = F,
    mode = "distinct",
    width_ratio=0.1,
    min_degree=1, # min combination
    set_sizes=upset_set_size(mapping = aes(),
                             geom = geom_bar(aes(fill=age, x=group),
                                             width = 0.6),
                             position = "left",
                             filter_intersections = T) +
      scale_fill_manual(values=col_map) +
      geom_text(aes(label=..count..), hjust=1.1, stat='count') +
      expand_limits(y=500) +
      theme(legend.position = "none",
            # axis.text.x=element_text(angle=90)
            axis.text.x = element_blank())
  ) +
    ggtitle(h)
  
  # redefine plots
  plotStore_hospital[[h]] <- p
}

png(file = "pictures/epi_combinations_1pneumo_RS.png", width = 29, height = 22,
    unit = "cm", res = 600)
cowplot::plot_grid(plotlist = plotStore_hospital,
                   nrow = 3)
dev.off()

# pivot longer symptoms
df_epi_clean %>% 
  dplyr::mutate(
    chosen_bacteraemia = ifelse(diag_2signs_bacteraemia == 1, "bacteremia", NA),
    chosen_pneumonia = ifelse(diag_2signs_pneumonia == 1, "pneumonia", NA),
    chosen_meningitis = ifelse(diag_2signs_meningitis == 1, "meningitis", NA),
    chosen_pleural_effusion = ifelse(diag_2signs_pleural_effusion == 1, "pleural_effusion", NA),
  ) %>% 
  pivot_longer(
    cols = contains("chosen_"),
    names_to = "symptoms"
  ) %>% 
  dplyr::group_by(hospital, value) %>% 
  dplyr::summarise(count = n()) %>% 
  glimpse()

# test pneumo only per hospital
df_epi_clean %>% 
  dplyr::mutate(peumoOnly = case_when(
    diag_2signs_pneumonia == 1 &
      diag_2signs_bacteraemia == 0 &
      diag_2signs_meningitis ==  0 &
      diag_2signs_pleural_effusion == 0
    ~ "pneumonia only",
    TRUE ~ NA
    )
  ) %>% 
  dplyr::group_by(hospital, peumoOnly) %>% 
  dplyr::summarise(count = n()) %>% 
  dplyr::filter(peumoOnly == "pneumonia only") %>% 
  dplyr::glimpse()
  

# UpSetR for pneumonia symptoms
# using ComplexUpset
df_signs <- df_epi_clean %>%
  dplyr::transmute(
    hospital = hospital,
    age = age_group_2vaccine,
    
    # bacteremia = diag_2signs_bacteraemia,
    # pneumonia = diag_2signs_pneumonia,
    # meningitis = diag_2signs_meningitis,
    # pleural_effusion = diag_2signs_pleural_effusion,
    
    cough = diag_2signs_cough,
    fever = diag_2signs_fever,
    `rapid breathing according to age` = diag_2signs_tachypnea,
    `chest wall retraction` = diag_2signs_chest_in_drawing,
    `central cyanosis` = diag_2signs_central_cyanosis,
    grunting = diag_2signs_grunting,
    `unable to drink` = diag_2signs_cannot_drink,
    lethargy = diag_2signs_lethargy,
    `decreased consciousness` = diag_2signs_decreased_consciousness,
    seizures = diag_2signs_seizures,
    `pleurisy (chest pain)` = diag_2signs_pleuritis_chest_pain
  ) %>%
  dplyr::mutate(across(3:ncol(.), as.numeric)) %>%
  as.data.frame() %>% 
  glimpse()

png(file = "pictures/epi_combinations_2pneumoSymp_all.png", width = 29, height = 20,
    unit = "cm", res = 600)
ComplexUpset::upset(
  df_signs,
  intersect = c("cough",
                "fever",
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
  # encode_sets=FALSE, # for annotate() to select the set by name disable encoding
  base_annotations = list(
    "Hospital\n(count)" = intersection_size(
      counts = FALSE,
      mapping = aes(fill=hospital),
      text = list(
        check_overlap=TRUE,
        vjust = -0.1,
        hjust = -0.1
      )) +
      scale_fill_manual(values=col_map)),
  annotations = list(
    "Age\n(%)" = ggplot(mapping=aes(fill=age)) +
      geom_bar(stat='count', position='fill') +
      scale_fill_manual(values=col_map) +
      scale_y_continuous(labels=scales::percent_format())
  ),
  # queries=list(
  #   upset_query(
  #     intersect=c('pneumonia', 'pleural_effusion'),
  #     color='red',
  #     fill='red',
  #     only_components=c('intersections_matrix', 'Intersection size')
  #   ),
  # ),
  wrap = F,
  mode = "distinct",
  width_ratio=0.1,
  min_degree=1, # min combination
  set_sizes=upset_set_size(mapping = aes(),
                           geom = geom_bar(aes(fill=hospital, x=group),
                                           width = 0.6),
                           position = "left",
                           filter_intersections = T) +
    scale_fill_manual(values=col_map) +
    geom_text(aes(label=..count..), hjust=1.1, stat='count') +
    expand_limits(y=800) +
    theme(legend.position = "none",
          axis.text.x=element_text(angle=90))
)
dev.off()

# pneumonia symptoms separated by hospital but filled by age groups
hospitals <- unique(df_epi_clean$hospital)
plotStore_hospital <- list()

for(h in hospitals){
  df_signs <- df_epi_clean %>%
    dplyr::transmute(
      hospital = hospital,
      age = age_group_2vaccine,
      
      # bacteremia = diag_2signs_bacteraemia,
      # pneumonia = diag_2signs_pneumonia,
      # meningitis = diag_2signs_meningitis,
      # pleural_effusion = diag_2signs_pleural_effusion,
      
      cough = diag_2signs_cough,
      fever = diag_2signs_fever,
      `rapid breathing according to age` = diag_2signs_tachypnea,
      `chest wall retraction` = diag_2signs_chest_in_drawing,
      `central cyanosis` = diag_2signs_central_cyanosis,
      grunting = diag_2signs_grunting,
      `unable to drink` = diag_2signs_cannot_drink,
      lethargy = diag_2signs_lethargy,
      `decreased consciousness` = diag_2signs_decreased_consciousness,
      seizures = diag_2signs_seizures,
      `pleurisy (chest pain)` = diag_2signs_pleuritis_chest_pain
    ) %>%
    dplyr::filter(hospital == h) %>% 
    dplyr::mutate(across(3:ncol(.), as.numeric)) %>%
    as.data.frame()
  
  p <- ComplexUpset::upset(
    df_signs,
    intersect = c("cough",
                  "fever",
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
    # encode_sets=FALSE, # for annotate() to select the set by name disable encoding
    base_annotations = list(
      "Age\n(count)" = intersection_size(
        counts = FALSE,
        mapping = aes(fill=age),
        text = list(
          check_overlap=TRUE,
          vjust = -0.1,
          hjust = -0.1
        )) +
        scale_fill_manual(values=col_map)),
    wrap = F,
    mode = "distinct",
    width_ratio=0.1,
    min_degree=1, # min combination
    set_sizes=upset_set_size(mapping = aes(),
                             geom = geom_bar(aes(fill=age, x=group),
                                             width = 0.6),
                             position = "left",
                             filter_intersections = T) +
      scale_fill_manual(values=col_map) +
      geom_text(aes(label=..count..), hjust=1.1, stat='count') +
      expand_limits(y=400) +
      theme(legend.position = "none",
            # axis.text.x=element_text(angle=90)
            axis.text.x = element_blank())
  ) +
    ggtitle(h)
  
  # redefine plots
  plotStore_hospital[[h]] <- p
}

png(file = "pictures/epi_combinations_2pneumoSymp_RS.png", width = 29, height = 30,
    unit = "cm", res = 600)
cowplot::plot_grid(plotlist = plotStore_hospital,
                   nrow = 3)
dev.off()

# trial UpSetR for chronic/comorbidity status ##################################
# ommited:
# diag_3chronic_immunodeficiency
# diag_3chronic_systemic_steroid_use
# diag_3chronic_malignancy
# diag_3chronic_leukemia_lymphoma_other_blood_cancer
# diag_3chronic_asplenia

df_signs <- df_epi_clean %>%
  dplyr::transmute(
    hospital = hospital,
    age = age_group_2vaccine,
    prematurity = diag_3chronic_prematurity,
    `heart disease` = diag_3chronic_heart_disease,
    `lung disease` = diag_3chronic_lung_disease,
    `liver disease` = diag_3chronic_liver_disease,
    `kidney disease` = diag_3chronic_kidney_disease,
    `reactive airway disease` = diag_3chronic_reactive_airway_disease,
    `diabetes` = diag_3chronic_diabetes,
    `sickle cell thalassemia` = diag_3chronic_sickle_cell_thalassemia,
    `hemoglobinopathy` = diag_3chronic_hemoglobinopathy
  ) %>%
  dplyr::mutate(across(c(prematurity,
                         "heart disease",
                         "lung disease",
                         "liver disease",
                         "kidney disease",
                         "reactive airway disease",
                         "diabetes",
                         "sickle cell thalassemia",
                         "hemoglobinopathy"),
                       as.numeric)) %>%
  as.data.frame() %>% 
  glimpse()

png(file = "pictures/epi_combinations_3chronic_all.png", width = 29, height = 15,
    unit = "cm", res = 600)
ComplexUpset::upset(
  df_signs,
  intersect = c("prematurity",
                "heart disease",
                "lung disease",
                "liver disease",
                "kidney disease",
                "reactive airway disease",
                "diabetes",
                "sickle cell thalassemia",
                "hemoglobinopathy"),
  # encode_sets=FALSE, # for annotate() to select the set by name disable encoding
  base_annotations = list(
    "Hospital\n(count)" = intersection_size(
      counts = FALSE,
      mapping = aes(fill=hospital),
      text = list(
        check_overlap=TRUE,
        vjust = -0.1,
        hjust = -0.1
      )) +
      scale_fill_manual(values=col_map)),
  annotations = list(
    "Age\n(%)" = ggplot(mapping=aes(fill=age)) +
      geom_bar(stat='count', position='fill') +
      scale_fill_manual(values=col_map) +
      scale_y_continuous(labels=scales::percent_format())
  ),
  # queries=list(
  #   upset_query(
  #     intersect=c('pneumonia', 'pleural_effusion'),
  #     color='red',
  #     fill='red',
  #     only_components=c('intersections_matrix', 'Intersection size')
  #   ),
  # ),
  wrap = F,
  mode = "distinct",
  width_ratio=0.1,
  min_degree=1, # min combination
  set_sizes=upset_set_size(mapping = aes(),
                           geom = geom_bar(aes(fill=hospital, x=group),
                                           width = 0.6),
                           position = "left",
                           filter_intersections = T) +
    scale_fill_manual(values=col_map) +
    geom_text(aes(label=..count..), hjust=1.1, stat='count') +
    expand_limits(y=100) +
    theme(legend.position = "none",
          axis.text.x=element_text(angle=90))
)
dev.off()

# early signs separated by hospital but filled by age groups
hospitals <- unique(df_epi_clean$hospital)
plotStore_hospital <- list()

for(h in hospitals){
  df_signs <- df_epi_clean %>%
    dplyr::transmute(
      hospital = hospital,
      age = age_group_2vaccine,
      prematurity = diag_3chronic_prematurity,
      `heart disease` = diag_3chronic_heart_disease,
      `lung disease` = diag_3chronic_lung_disease,
      `liver disease` = diag_3chronic_liver_disease,
      `kidney disease` = diag_3chronic_kidney_disease,
      `reactive airway disease` = diag_3chronic_reactive_airway_disease,
      `diabetes` = diag_3chronic_diabetes,
      `sickle cell thalassemia` = diag_3chronic_sickle_cell_thalassemia,
      `hemoglobinopathy` = diag_3chronic_hemoglobinopathy
    ) %>%
    dplyr::filter(hospital == h) %>% 
    dplyr::mutate(across(c(prematurity,
                           "heart disease",
                           "lung disease",
                           "liver disease",
                           "kidney disease",
                           "reactive airway disease",
                           "diabetes",
                           "sickle cell thalassemia",
                           "hemoglobinopathy"),
                         as.numeric)) %>%
    as.data.frame()
  
  p <- ComplexUpset::upset(
    df_signs,
    intersect = c("prematurity",
                  "heart disease",
                  "lung disease",
                  "liver disease",
                  "kidney disease",
                  "reactive airway disease",
                  "diabetes",
                  "sickle cell thalassemia",
                  "hemoglobinopathy"),
    # encode_sets=FALSE, # for annotate() to select the set by name disable encoding
    base_annotations = list(
      "Age\n(count)" = intersection_size(
        counts = FALSE,
        mapping = aes(fill=age),
        text = list(
          check_overlap=TRUE,
          vjust = -0.1,
          hjust = -0.1
        )) +
        scale_fill_manual(values=col_map)),
    wrap = F,
    mode = "distinct",
    width_ratio=0.1,
    min_degree=1, # min combination
    set_sizes=upset_set_size(mapping = aes(),
                             geom = geom_bar(aes(fill=age, x=group),
                                             width = 0.6),
                             position = "left",
                             filter_intersections = T) +
      scale_fill_manual(values=col_map) +
      geom_text(aes(label=..count..), hjust=1.1, stat='count') +
      expand_limits(y=70) +
      theme(legend.position = "none",
            # axis.text.x=element_text(angle=90)
            axis.text.x = element_blank())
  ) +
    ggtitle(h)
  
  # redefine plots
  plotStore_hospital[[h]] <- p
}

png(file = "pictures/epi_combinations_3chronic_RS.png", width = 29, height = 22,
    unit = "cm", res = 600)
cowplot::plot_grid(plotlist = plotStore_hospital,
                   nrow = 3)
dev.off()

# trial UpSetR for antibiotics usage ###########################################
# UpSetR for all abx usage
# using ComplexUpset
df_signs <- df_epi_clean %>%
  dplyr::transmute(
    hospital = hospital,
    age = age_group_2vaccine,
    
    amoxicillin = abx_amoxicillin,
    "amoxicillin-clavulanic acid" = abx_amox_clav,
    ampicillin = abx_ampicillin,
    "ampicillin-sulbactam" = abx_ampicillin_sulbactam,
    azithromycin = abx_azithromycin,
    cefotaxime = abx_cefotaxime,
    ceftriaxone = abx_ceftriaxone,
    erythromycin = abx_erythromycin,
    "trimethoprim-sulfamethoxazole" = abx_trimeth_sulfamethoxazole,
    meropenem = abx_meropenem,
    vancomycin = abx_vancomycin,
    amikacin = abx_amikacin,
    ceftazidime = abx_ceftazidime,
    gentamicin = abx_gentamicin,
    metronidazole = abx_metronidazole,
    cefixime = abx_cefixime,
    "cefoperazone-sulbactam" = abx_cefoperazone_sulbactam,
    cefazolin = abx_cefazolin,
    ethambutol = abx_ethambutol,
    isoniazid = abx_isoniazid,
    pyrazinamide = abx_pyrazinamide,
    rifampicin = abx_rifampicin,
    levofloxacin = abx_levofloxacin,
    ciprofloxacin = abx_ciprofloxacin,
    cotrimoxazole = abx_cotrimoxazole,
    aminophylline = abx_aminophylline,
    vicilin = abx_vicilin
  ) %>%
  dplyr::mutate(across(3:ncol(.), as.numeric)) %>%
  as.data.frame() %>% 
  glimpse()

png(file = "pictures/epi_combinations_4abx_all.png", width = 29, height = 30,
    unit = "cm", res = 600)
ComplexUpset::upset(
  df_signs,
  intersect = c("amoxicillin",
                "amoxicillin-clavulanic acid",
                "ampicillin",
                "ampicillin-sulbactam",
                "azithromycin",
                "cefotaxime",
                "ceftriaxone",
                "erythromycin",
                "trimethoprim-sulfamethoxazole",
                "meropenem",
                "vancomycin",
                "amikacin",
                "ceftazidime",
                "gentamicin",
                "metronidazole",
                "cefixime",
                "cefoperazone-sulbactam",
                "cefazolin",
                "ethambutol",
                "isoniazid",
                "pyrazinamide",
                "rifampicin",
                "levofloxacin",
                "ciprofloxacin",
                "cotrimoxazole",
                "aminophylline",
                "vicilin"
  ),
  # encode_sets=FALSE, # for annotate() to select the set by name disable encoding
  base_annotations = list(
    "Hospital\n(count)" = intersection_size(
      counts = FALSE,
      mapping = aes(fill=hospital),
      text = list(
        check_overlap=TRUE,
        vjust = -0.1,
        hjust = -0.1
      )) +
      scale_fill_manual(values=col_map)),
  annotations = list(
    "Age\n(%)" = ggplot(mapping=aes(fill=age)) +
      geom_bar(stat='count', position='fill') +
      scale_fill_manual(values=col_map) +
      scale_y_continuous(labels=scales::percent_format())
  ),
  # queries=list(
  #   upset_query(
  #     intersect=c('pneumonia', 'pleural_effusion'),
  #     color='red',
  #     fill='red',
  #     only_components=c('intersections_matrix', 'Intersection size')
  #   ),
  # ),
  wrap = F,
  mode = "distinct",
  width_ratio=0.1,
  min_degree=1, # min combination
  set_sizes=upset_set_size(mapping = aes(),
                           geom = geom_bar(aes(fill=hospital, x=group),
                                           width = 0.6),
                           position = "left",
                           filter_intersections = T) +
    scale_fill_manual(values=col_map) +
    geom_text(aes(label=..count..), hjust=1.1, stat='count') +
    expand_limits(y=200) +
    theme(legend.position = "none",
          axis.text.x=element_text(angle=90))
)
dev.off()

# abx usage separated by hospital but filled by age groups
hospitals <- unique(df_epi_clean$hospital)
plotStore_hospital <- list()

for(h in hospitals){
  df_signs <- df_epi_clean %>%
    dplyr::transmute(
      hospital = hospital,
      age = age_group_2vaccine,
      
      amoxicillin = abx_amoxicillin,
      "amoxicillin-clavulanic acid" = abx_amox_clav,
      ampicillin = abx_ampicillin,
      "ampicillin-sulbactam" = abx_ampicillin_sulbactam,
      azithromycin = abx_azithromycin,
      cefotaxime = abx_cefotaxime,
      ceftriaxone = abx_ceftriaxone,
      erythromycin = abx_erythromycin,
      "trimethoprim-sulfamethoxazole" = abx_trimeth_sulfamethoxazole,
      meropenem = abx_meropenem,
      vancomycin = abx_vancomycin,
      amikacin = abx_amikacin,
      ceftazidime = abx_ceftazidime,
      gentamicin = abx_gentamicin,
      metronidazole = abx_metronidazole,
      cefixime = abx_cefixime,
      "cefoperazone-sulbactam" = abx_cefoperazone_sulbactam,
      cefazolin = abx_cefazolin,
      ethambutol = abx_ethambutol,
      isoniazid = abx_isoniazid,
      pyrazinamide = abx_pyrazinamide,
      rifampicin = abx_rifampicin,
      levofloxacin = abx_levofloxacin,
      ciprofloxacin = abx_ciprofloxacin,
      cotrimoxazole = abx_cotrimoxazole,
      aminophylline = abx_aminophylline,
      vicilin = abx_vicilin
    ) %>%
    dplyr::filter(hospital == h) %>% 
    dplyr::mutate(across(3:ncol(.), as.numeric)) %>%
    as.data.frame()
  
  p <- ComplexUpset::upset(
    df_signs,
    intersect = c("amoxicillin",
                  "amoxicillin-clavulanic acid",
                  "ampicillin",
                  "ampicillin-sulbactam",
                  "azithromycin",
                  "cefotaxime",
                  "ceftriaxone",
                  "erythromycin",
                  "trimethoprim-sulfamethoxazole",
                  "meropenem",
                  "vancomycin",
                  "amikacin",
                  "ceftazidime",
                  "gentamicin",
                  "metronidazole",
                  "cefixime",
                  "cefoperazone-sulbactam",
                  "cefazolin",
                  "ethambutol",
                  "isoniazid",
                  "pyrazinamide",
                  "rifampicin",
                  "levofloxacin",
                  "ciprofloxacin",
                  "cotrimoxazole",
                  "aminophylline",
                  "vicilin"
    ),
    # encode_sets=FALSE, # for annotate() to select the set by name disable encoding
    base_annotations = list(
      "Age\n(count)" = intersection_size(
        counts = FALSE,
        mapping = aes(fill=age),
        text = list(
          check_overlap=TRUE,
          vjust = -0.1,
          hjust = -0.1
        )) +
        scale_fill_manual(values=col_map)),
    wrap = F,
    mode = "distinct",
    width_ratio=0.1,
    min_degree=1, # min combination
    # min_size = 5,
    set_sizes=upset_set_size(mapping = aes(),
                             geom = geom_bar(aes(fill=age, x=group),
                                             width = 0.6),
                             position = "left",
                             filter_intersections = T) +
      scale_fill_manual(values=col_map) +
      geom_text(aes(label=..count..), hjust=1.1, stat='count') +
      expand_limits(y=100) +
      theme(legend.position = "none",
            # axis.text.x=element_text(angle=90)
            axis.text.x = element_blank())
  ) +
    ggtitle(h)
  
  # redefine plots
  plotStore_hospital[[h]] <- p
}

png(file = "pictures/epi_combinations_4abx_RS.png", width = 29, height = 50,
    unit = "cm", res = 600)
cowplot::plot_grid(plotlist = plotStore_hospital,
                   nrow = 3)
dev.off()

# sankey plot(s) for bacterial infection #######################################
bac_blood <- df_epi_clean %>%
  dplyr::filter(!is.na(sample_blood_bacteria_species) & sample_blood_bacteria_species != "No Growth") %>% 
  dplyr::rename(
    prognosis = patient_prognosis,
    `age group` = age_group_2vaccine,
    resistance = sample_blood_bacteria_resistance,
    `species\n(blood samples)` = sample_blood_bacteria_species
  ) %>% 
  ggsankey::make_long(prognosis,
                      resistance,
                      `species\n(blood samples)`,
                      hospital,
                      `age group`) %>% 
  glimpse() %>% 
  ggplot(., aes(x = x, 
                next_x = next_x, 
                node = node, 
                next_node = next_node,
                fill = factor(node),
                label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1,
              width = 0.001) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  labs(x = NULL) +
  theme_sankey(base_size = 16) +
  theme(legend.position = "none")
bac_blood

bac_sputum <- df_epi_clean %>%
  dplyr::filter(!is.na(sample_sputum_bacteria_species) & sample_sputum_bacteria_species != "No Growth") %>% 
  dplyr::rename(
    prognosis = patient_prognosis,
    `age group` = age_group_2vaccine,
    resistance = sample_sputum_bacteria_resistance,
    `species\n(sputum samples)` = sample_sputum_bacteria_species
  ) %>% 
  ggsankey::make_long(prognosis,
                      resistance,
                      `species\n(sputum samples)`,
                      hospital,
                      `age group`) %>% 
  glimpse() %>% 
  ggplot(., aes(x = x, 
                next_x = next_x, 
                node = node, 
                next_node = next_node,
                fill = factor(node),
                label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1,
              width = 0.001) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  labs(x = NULL) +
  theme_sankey(base_size = 16) +
  theme(legend.position = "none")
bac_sputum


png(file = "pictures/sankey_bacteria_blood_sputum.png",
    width = 29, height = 37,
    unit = "cm", res = 600)
cowplot::plot_grid(bac_blood, bac_sputum,
                   nrow = 2,
                   rel_heights = c(2, 1),
                   labels = c("A", "B"))
dev.off()

# test flow with not sampled
bac_flow_not_sampled <- df_epi_clean %>%
  dplyr::select(contains("_species")) %>% 
  dplyr::mutate(
    filter_group = case_when(
      is.na(sample_blood_bacteria_species) &
        is.na(sample_sputum_bacteria_species) &
        is.na(sample_csf_bacteria_species) &
        is.na(sample_pleural_bacteria_species)
      ~ 0,
      TRUE ~ 1
    )
  ) %>% 
  dplyr::filter(filter_group == 1) %>% 
  dplyr::mutate(
    across(everything(), ~ replace_na(., "Not Sampled"))
  ) %>% 
  dplyr::rename(
    blood = sample_blood_bacteria_species,
    sputum = sample_sputum_bacteria_species,
    pleural = sample_pleural_bacteria_species,
    CSF = sample_csf_bacteria_species
  ) %>% 
  ggsankey::make_long(sputum,
                      blood,
                      pleural,
                      CSF
  ) %>% 
  glimpse() %>% 
  ggplot(., aes(x = x, 
                next_x = next_x, 
                node = node, 
                next_node = next_node,
                fill = factor(node),
                label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1,
              width = 0.001) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  labs(x = NULL) +
  theme_sankey(base_size = 16) +
  theme(legend.position = "none")
bac_flow_not_sampled

bac_flow_filtered <- df_epi_clean %>%
  dplyr::select(contains("_species")) %>% 
  dplyr::mutate(
    filter_group = case_when(
      is.na(sample_blood_bacteria_species) &
        is.na(sample_sputum_bacteria_species) &
        is.na(sample_csf_bacteria_species) &
        is.na(sample_pleural_bacteria_species)
      ~ 0,
      TRUE ~ 1
    )
  ) %>% 
  dplyr::filter(filter_group == 1) %>% 
  dplyr::mutate(
    across(everything(), ~ replace_na(., "Not Sampled")),
    filter_group = case_when(
      (sample_blood_bacteria_species == "Not Sampled" |
         sample_blood_bacteria_species == "No Growth") & 
        (sample_sputum_bacteria_species == "Not Sampled" |
           sample_sputum_bacteria_species == "No Growth") & 
        (sample_csf_bacteria_species == "Not Sampled" |
           sample_csf_bacteria_species == "No Growth") &
        (sample_pleural_bacteria_species == "Not Sampled" |
           sample_pleural_bacteria_species == "No Growth")
      ~ 0,
      TRUE ~ 1
    )
  ) %>% 
  dplyr::filter(filter_group == 1) %>% 
  dplyr::rename(
    blood = sample_blood_bacteria_species,
    sputum = sample_sputum_bacteria_species,
    pleural = sample_pleural_bacteria_species,
    CSF = sample_csf_bacteria_species
  ) %>% 
  ggsankey::make_long(sputum,
                      blood,
                      pleural,
                      CSF
  ) %>% 
  glimpse() %>% 
  ggplot(., aes(x = x, 
                next_x = next_x, 
                node = node, 
                next_node = next_node,
                fill = factor(node),
                label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1,
              width = 0.001) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  labs(x = NULL) +
  theme_sankey(base_size = 16) +
  theme(legend.position = "none")
bac_flow_filtered

# test flow using NP data (load df_epi_gen_pneumo) #############################
bac_flow_not_sampled <- read.csv("inputs/genData_pneumo_with_epiData_with_final_pneumo_decision.csv") %>% 
  dplyr::select(contains("_species")) %>% 
  dplyr::mutate(
    filter_group = case_when(
      is.na(sample_blood_bacteria_species) &
        is.na(sample_sputum_bacteria_species) &
        is.na(sample_csf_bacteria_species) &
        is.na(sample_pleural_bacteria_species) &
        is.na(workWGS_species_pw)
      ~ 0,
      TRUE ~ 1
    )
  ) %>% 
  dplyr::filter(filter_group == 1) %>% 
  dplyr::mutate(
    across(everything(), ~ replace_na(., "Not Sampled")),
    workWGS_species_pw = ifelse(workWGS_species_pw == "Not Sampled",
                                "Not Sampled/Analysed",
                                workWGS_species_pw)
  ) %>% 
  dplyr::rename(
    blood = sample_blood_bacteria_species,
    `nasopharynx (WGS)` = workWGS_species_pw,
    sputum = sample_sputum_bacteria_species,
    pleural = sample_pleural_bacteria_species,
    CSF = sample_csf_bacteria_species
  ) %>% 
  ggsankey::make_long(sputum,
                      `nasopharynx (WGS)`,
                      blood,
                      pleural,
                      CSF
  ) %>% 
  glimpse() %>% 
  ggplot(., aes(x = x, 
                next_x = next_x, 
                node = node, 
                next_node = next_node,
                fill = factor(node),
                label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1,
              width = 0.001) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  labs(x = NULL) +
  theme_sankey(base_size = 16) +
  theme(legend.position = "none")
bac_flow_not_sampled

bac_flow_filtered <- read.csv("inputs/genData_pneumo_with_epiData_with_final_pneumo_decision.csv") %>% 
  dplyr::select(contains("_species")) %>% 
  dplyr::mutate(
    filter_group = case_when(
      is.na(sample_blood_bacteria_species) &
        is.na(sample_sputum_bacteria_species) &
        is.na(sample_csf_bacteria_species) &
        is.na(sample_pleural_bacteria_species) &
        is.na(workWGS_species_pw)
      ~ 0,
      TRUE ~ 1
    )
  ) %>% 
  dplyr::filter(filter_group == 1) %>% 
  dplyr::mutate(
    across(everything(), ~ replace_na(., "Not Sampled")),
    workWGS_species_pw = ifelse(workWGS_species_pw == "Not Sampled",
                                "Not Sampled/Analysed",
                                workWGS_species_pw),
    filter_group = case_when(
      (sample_blood_bacteria_species == "Not Sampled" |
         sample_blood_bacteria_species == "No Growth") & 
        (sample_sputum_bacteria_species == "Not Sampled" |
           sample_sputum_bacteria_species == "No Growth") & 
        (sample_csf_bacteria_species == "Not Sampled" |
           sample_csf_bacteria_species == "No Growth") &
        (sample_pleural_bacteria_species == "Not Sampled" |
           sample_pleural_bacteria_species == "No Growth")
      ~ 0,
      TRUE ~ 1
    )
  ) %>% 
  dplyr::filter(filter_group == 1) %>% 
  dplyr::rename(
    blood = sample_blood_bacteria_species,
    `nasopharynx (WGS)` = workWGS_species_pw,
    sputum = sample_sputum_bacteria_species,
    pleural = sample_pleural_bacteria_species,
    CSF = sample_csf_bacteria_species
  ) %>% 
  ggsankey::make_long(sputum,
                      `nasopharynx (WGS)`,
                      blood,
                      pleural,
                      CSF
  ) %>% 
  glimpse() %>% 
  ggplot(., aes(x = x, 
                next_x = next_x, 
                node = node, 
                next_node = next_node,
                fill = factor(node),
                label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1,
              width = 0.001) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  labs(x = NULL) +
  theme_sankey(base_size = 16) +
  theme(legend.position = "none")
bac_flow_filtered

png(file = "pictures/sankey_bacteria_all_flow_filtered.png",
    width = 29, height = 18,
    unit = "cm", res = 600)
print(bac_flow_filtered)
dev.off()

