df_epi_hospital2 <- df_epi_hospital %>% 
  rename(age_year = usia_tahun_dc,
         pcv13 = apakah_pasien_telah_menerima_vaksin_pcv13_sebelumnya,
         hospital = asal_rumah_sakit,
         prognosis = bagaimanakah_prognosis_pasien)


library(nnet)
fit_linear <- multinom(patient_prognosis ~ age_year + vacc_pcv13_doses + hospital,
                       data = df_epi_clean)
library(splines)
fit_spline <- multinom(patient_prognosis ~ ns(age_year, df = 3) + vacc_pcv13_doses + hospital,
                       data = df_epi_clean)

anova(fit_linear, fit_spline, test = "Chisq")
# If p < 0.05, the spline model fits significantly better → age is non-linear (use spline or age groups).
# If p ≥ 0.05, the linear assumption is fine → age can stay continuous.

library(effects)
plot(allEffects(fit_spline), multiline = TRUE)

