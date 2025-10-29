



# tobecontinued
df_long <- df %>%
  pivot_longer(cols = starts_with("tanggal_"), names_to = "tanggal_type", values_to = "tanggal_")

no_id_subjek_order <- df_long %>%
  group_by(no_id_subjek) %>%
  summarise(min_tanggal_ = min(tanggal_, na.rm = TRUE)) %>%
  arrange(min_tanggal_) %>% # earlier date
  pull(no_id_subjek)

df_long <- df_long %>%
  mutate(no_id_subjek = factor(no_id_subjek, levels = no_id_subjek_order)) %>% 
  glimpse()

df_segments <- df_long %>%
  group_by(no_id_subjek) %>%
  arrange(tanggal_) %>%
  mutate(next_date = lead(tanggal_)) %>%
  filter(!is.na(next_date))  # Remove last point

ggplot(df_long, aes(x = as.Date(tanggal_), y = no_id_subjek, color = tanggal_type)) +
  geom_point(size = 3) +
  geom_segment(
    data = df_segments,
    aes(x = as.Date(tanggal_), xend = as.Date(next_date),
        y = no_id_subjek, yend = no_id_subjek),
    arrow = arrow(length = unit(0.2, "cm")),
    color = "steelblue",
    size = 0.2,
    inherit.aes = FALSE
  ) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year",
               # limits = c(as.Date("2023-06-01"), as.Date(max(df_long$tanggal_)))
               ) +
  labs(x = "Date", y = "ID", color = "Date Type")
  # theme(#axis.text.y = element_text(size = 10),
  #       #axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
  #       # legend.position = c(0.02, 0.75),
  #       # legend.direction = "horizontal",
  #       # legend.justification = c("left", "top"),
  #       legend.background = element_rect(fill = NA, color = NA),
  #       legend.title = element_blank(),
  #       legend.margin = margin(t = -50),
  #       legend.spacing.y = unit(-0.3, "cm"))



# additional arrow:
df_arrow <- df %>%
  select(no_id_subjek, tanggal_masuk_rs, tanggal_keluar_rs) %>%
  filter(!is.na(tanggal_masuk_rs), !is.na(tanggal_keluar_rs)) %>%
  mutate(
    tanggal_masuk_rs = as.Date(tanggal_masuk_rs),
    tanggal_keluar_rs = as.Date(tanggal_keluar_rs)
  )

# Sort by earliest masuk
df_arrow <- df_arrow %>%
  mutate(no_id_subjek = factor(no_id_subjek, levels = df_arrow %>%
                                 group_by(no_id_subjek) %>%
                                 summarise(min_date = min(tanggal_masuk_rs)) %>%
                                 arrange(min_date) %>%
                                 pull(no_id_subjek)))



ggplot(df_arrow, aes(x = tanggal_masuk_rs, xend = tanggal_keluar_rs, 
                     y = no_id_subjek, yend = no_id_subjek)) +
  geom_segment(arrow = arrow(length = unit(0.2, "cm")), color = "tomato", size = 1) +
  geom_point(aes(x = tanggal_masuk_rs), color = "darkgreen", size = 2) +
  geom_point(aes(x = tanggal_keluar_rs), color = "steelblue", size = 2) +
  theme_minimal() +
  labs(x = "Date", y = "ID", title = "Hospital Stay (Masuk-Keluar, filter >= 50 hari)") +
  theme(axis.text.y = element_text(size = 7))
