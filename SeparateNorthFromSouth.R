###############################################################
# North Delta vs South / Upper Egypt Analysis
# Suggested by Senthold meeting discussion
# Author: Ahmed Kheir workflow
###############################################################

rm(list = ls())

# ============================================================
# 1. Packages
# ============================================================

packages <- c(
  "tidyverse", "readr", "readxl", "janitor", "lubridate",
  "broom", "patchwork", "ggrepel", "ggpubr",
  "FactoMineR", "factoextra", "openxlsx",
  "plotrix", "hydroGOF", "scales"
)

new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) install.packages(new_packages)

invisible(lapply(packages, library, character.only = TRUE))

theme_set(theme_classic(base_size = 14))

# ============================================================
# 2. Paths
# ============================================================

base_dir <- "D:/Asseng_paper2024/Updated/Asseng_updated/40YRSSIMULATIONS/ForShawn/21072025/21072025/Update27032026/CIMMYTSimulations/CIMMYT_ClimateMetrics_From_WTH_LocCodes"

out_dir <- file.path(base_dir, "North_South_Strong_Analysis")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

record_file <- file.path(base_dir, "CIMMYT_RecordLevel_ClimateMetrics_Last90Days.csv")
yearloc_file <- file.path(base_dir, "CIMMYT_YearLocation_ClimateMetrics_Last90Days.csv")
obs_file    <- file.path(base_dir, "CIMMYT_Observed_Matched_To_CIMMYTLocs_Codes.csv")
sim_file    <- file.path(base_dir, "CIMMYTSimulatedMME.xlsx")

# ============================================================
# 3. Main settings
# ============================================================

break_year <- 1998
north_lat_cutoff <- 30

# years discussed as suspicious / needing annotation
highlight_years <- c(1995, 2008, 2009, 2012, 2019)

# optional: flag geographically weak matches
max_allowed_distance_km <- 50

# ============================================================
# 4. Load data
# ============================================================

record <- read_csv(record_file, show_col_types = FALSE) |> clean_names()
yearloc <- read_csv(yearloc_file, show_col_types = FALSE) |> clean_names()
obs <- read_csv(obs_file, show_col_types = FALSE) |> clean_names()
sim <- read_excel(sim_file) |> clean_names()

# ============================================================
# 5. Harmonize region classification
# ============================================================

assign_region <- function(lat) {
  case_when(
    lat >= north_lat_cutoff ~ "North_Delta",
    lat < north_lat_cutoff ~ "South_Upper_Egypt",
    TRUE ~ NA_character_
  )
}

record <- record |>
  mutate(
    region = assign_region(matched_loc_lat),
    period = ifelse(year <= break_year, "Pre_1998", "Post_1998"),
    yield_cimmyt_t_ha = yield_cimmyt / 1000,
    distance_flag_50km = match_distance_km > max_allowed_distance_km,
    suspicious_year = year %in% highlight_years
  )

yearloc <- yearloc |>
  mutate(
    region = assign_region(matched_loc_lat),
    period = ifelse(year <= break_year, "Pre_1998", "Post_1998"),
    suspicious_year = year %in% highlight_years
  )

obs <- obs |>
  mutate(
    region = assign_region(matched_loc_lat),
    period = ifelse(year <= break_year, "Pre_1998", "Post_1998"),
    yield_cimmyt_t_ha = yield_cimmyt / 1000,
    distance_flag_50km = match_distance_km > max_allowed_distance_km,
    suspicious_year = year %in% highlight_years
  )

sim <- sim |>
  mutate(
    region = assign_region(lat),
    period = ifelse(year <= break_year, "Pre_1998", "Post_1998"),
    sim_yield_mme_t_ha = sim_yield_mme / 1000
  )

# ============================================================
# 6. Remove / flag distant matches
# ============================================================

record_clean <- record |>
  filter(!distance_flag_50km | is.na(distance_flag_50km))

obs_clean <- obs |>
  filter(!distance_flag_50km | is.na(distance_flag_50km))

# ============================================================
# 7. Build regional annual dataset
# ============================================================

regional_annual <- record_clean |>
  group_by(region, year, period) |>
  summarise(
    n_records = n(),
    n_locations = n_distinct(matched_weather_code),
    yield_median = median(yield_cimmyt_t_ha, na.rm = TRUE),
    yield_mean = mean(yield_cimmyt_t_ha, na.rm = TRUE),
    yield_p05 = quantile(yield_cimmyt_t_ha, 0.05, na.rm = TRUE),
    yield_p20 = quantile(yield_cimmyt_t_ha, 0.20, na.rm = TRUE),
    yield_p40 = quantile(yield_cimmyt_t_ha, 0.40, na.rm = TRUE),
    yield_p60 = quantile(yield_cimmyt_t_ha, 0.60, na.rm = TRUE),
    yield_p80 = quantile(yield_cimmyt_t_ha, 0.80, na.rm = TRUE),
    yield_p95 = quantile(yield_cimmyt_t_ha, 0.95, na.rm = TRUE),
    sim_yield_mean = mean(sim_yield_mme_t_ha, na.rm = TRUE),
    sim_sd_model = mean(stdev_models, na.rm = TRUE),
    tmax = mean(tmax_mean, na.rm = TRUE),
    tmin = mean(tmin_mean, na.rm = TRUE),
    tmean = mean(tmean_mean, na.rm = TRUE),
    heat_days_32 = mean(heat_days_tmax_gt32, na.rm = TRUE),
    srad = mean(srad_mean, na.rm = TRUE),
    srad_sum = mean(srad_sum, na.rm = TRUE),
    vpd = mean(vpd_mean, na.rm = TRUE),
    hdw_days = mean(hdw_days, na.rm = TRUE),
    rain = mean(rain_sum, na.rm = TRUE),
    wind = mean(wind_mean, na.rm = TRUE),
    rhum = mean(rhum_mean, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    yield_gap = sim_yield_mean - yield_mean,
    relative_yield_gap = 100 * (sim_yield_mean - yield_mean) / yield_mean,
    suspicious_year = year %in% highlight_years
  )

national_annual <- record_clean |>
  group_by(year) |>
  summarise(
    region = "National",
    period = ifelse(first(year) <= break_year, "Pre_1998", "Post_1998"),
    n_records = n(),
    n_locations = n_distinct(matched_weather_code),
    yield_median = median(yield_cimmyt_t_ha, na.rm = TRUE),
    yield_mean = mean(yield_cimmyt_t_ha, na.rm = TRUE),
    yield_p05 = quantile(yield_cimmyt_t_ha, 0.05, na.rm = TRUE),
    yield_p20 = quantile(yield_cimmyt_t_ha, 0.20, na.rm = TRUE),
    yield_p40 = quantile(yield_cimmyt_t_ha, 0.40, na.rm = TRUE),
    yield_p60 = quantile(yield_cimmyt_t_ha, 0.60, na.rm = TRUE),
    yield_p80 = quantile(yield_cimmyt_t_ha, 0.80, na.rm = TRUE),
    yield_p95 = quantile(yield_cimmyt_t_ha, 0.95, na.rm = TRUE),
    sim_yield_mean = mean(sim_yield_mme_t_ha, na.rm = TRUE),
    sim_sd_model = mean(stdev_models, na.rm = TRUE),
    tmax = mean(tmax_mean, na.rm = TRUE),
    tmin = mean(tmin_mean, na.rm = TRUE),
    tmean = mean(tmean_mean, na.rm = TRUE),
    heat_days_32 = mean(heat_days_tmax_gt32, na.rm = TRUE),
    srad = mean(srad_mean, na.rm = TRUE),
    srad_sum = mean(srad_sum, na.rm = TRUE),
    vpd = mean(vpd_mean, na.rm = TRUE),
    hdw_days = mean(hdw_days, na.rm = TRUE),
    rain = mean(rain_sum, na.rm = TRUE),
    wind = mean(wind_mean, na.rm = TRUE),
    rhum = mean(rhum_mean, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    yield_gap = sim_yield_mean - yield_mean,
    relative_yield_gap = 100 * (sim_yield_mean - yield_mean) / yield_mean,
    suspicious_year = year %in% highlight_years
  )

all_annual <- bind_rows(national_annual, regional_annual)

write.xlsx(all_annual, file.path(out_dir, "Annual_National_North_South_Dataset.xlsx"))

# ============================================================
# 8. North vs South climate gradient summary
# ============================================================

gradient_summary <- regional_annual |>
  group_by(region) |>
  summarise(
    n_years = n(),
    mean_yield = mean(yield_mean, na.rm = TRUE),
    mean_sim_yield = mean(sim_yield_mean, na.rm = TRUE),
    mean_gap = mean(yield_gap, na.rm = TRUE),
    mean_relative_gap = mean(relative_yield_gap, na.rm = TRUE),
    mean_tmax = mean(tmax, na.rm = TRUE),
    mean_tmin = mean(tmin, na.rm = TRUE),
    mean_tmean = mean(tmean, na.rm = TRUE),
    mean_heat_days_32 = mean(heat_days_32, na.rm = TRUE),
    mean_srad = mean(srad, na.rm = TRUE),
    mean_vpd = mean(vpd, na.rm = TRUE),
    mean_hdw_days = mean(hdw_days, na.rm = TRUE),
    .groups = "drop"
  )

north_south_difference <- regional_annual |>
  select(region, year, yield_mean, sim_yield_mean, yield_gap, relative_yield_gap,
         tmax, tmin, tmean, heat_days_32, srad, vpd, hdw_days) |>
  pivot_wider(
    names_from = region,
    values_from = c(yield_mean, sim_yield_mean, yield_gap, relative_yield_gap,
                    tmax, tmin, tmean, heat_days_32, srad, vpd, hdw_days)
  ) |>
  mutate(
    diff_yield_north_minus_south =
      yield_mean_North_Delta - yield_mean_South_Upper_Egypt,
    diff_sim_yield_north_minus_south =
      sim_yield_mean_North_Delta - sim_yield_mean_South_Upper_Egypt,
    diff_tmax_south_minus_north =
      tmax_South_Upper_Egypt - tmax_North_Delta,
    diff_heat_days_south_minus_north =
      heat_days_32_South_Upper_Egypt - heat_days_32_North_Delta,
    diff_srad_south_minus_north =
      srad_South_Upper_Egypt - srad_North_Delta,
    diff_hdw_south_minus_north =
      hdw_days_South_Upper_Egypt - hdw_days_North_Delta
  )

write.xlsx(
  list(
    GradientSummary = gradient_summary,
    AnnualNorthSouthDifference = north_south_difference
  ),
  file.path(out_dir, "North_South_Gradient_Summary.xlsx")
)

# ============================================================
# 9. Segmented regressions by region
# ============================================================

segmented_regression <- all_annual |>
  filter(region %in% c("National", "North_Delta", "South_Upper_Egypt")) |>
  pivot_longer(
    cols = c(yield_mean, sim_yield_mean, tmax, heat_days_32, srad, vpd, hdw_days),
    names_to = "variable",
    values_to = "value"
  ) |>
  drop_na(value) |>
  group_by(region, period, variable) |>
  group_modify(~ broom::tidy(lm(value ~ year, data = .x))) |>
  ungroup() |>
  filter(term == "year") |>
  rename(slope = estimate, p_value = p.value) |>
  mutate(significant = ifelse(p_value < 0.05, "Yes", "No"))

write.xlsx(segmented_regression, file.path(out_dir, "Segmented_Regression_North_South.xlsx"))

# ============================================================
# 10. Interaction models: does North differ from South?
# ============================================================

interaction_data <- regional_annual |>
  filter(region %in% c("North_Delta", "South_Upper_Egypt")) |>
  mutate(
    region = factor(region),
    period = factor(period),
    year_centered = year - mean(year, na.rm = TRUE)
  )

interaction_models <- list(
  yield_year_region = lm(yield_mean ~ year_centered * region, data = interaction_data),
  sim_yield_year_region = lm(sim_yield_mean ~ year_centered * region, data = interaction_data),
  tmax_year_region = lm(tmax ~ year_centered * region, data = interaction_data),
  heat_days_year_region = lm(heat_days_32 ~ year_centered * region, data = interaction_data),
  srad_year_region = lm(srad ~ year_centered * region, data = interaction_data),
  vpd_year_region = lm(vpd ~ year_centered * region, data = interaction_data),
  hdw_year_region = lm(hdw_days ~ year_centered * region, data = interaction_data),
  yield_climate_region =
    lm(yield_mean ~ tmax * region + heat_days_32 * region + srad * region + vpd * region, data = interaction_data)
)

interaction_tables <- lapply(interaction_models, broom::tidy)

write.xlsx(interaction_tables, file.path(out_dir, "Interaction_Models_North_vs_South.xlsx"))

# ============================================================
# 11. Figure 1: strong meeting-style panel
# Yield, Tmax, heat days >32, solar radiation
# ============================================================

p_yield <- regional_annual |>
  ggplot(aes(year, yield_median, color = region, fill = region)) +
  geom_ribbon(aes(ymin = yield_p20, ymax = yield_p80), alpha = 0.18, color = NA) +
  geom_point(aes(shape = suspicious_year), size = 2.8) +
  geom_line(linewidth = 0.8, alpha = 0.7) +
  geom_line(aes(y = sim_yield_mean), linewidth = 1.1, linetype = "dashed") +
  geom_ribbon(
    aes(ymin = sim_yield_mean - sim_sd_model,
        ymax = sim_yield_mean + sim_sd_model),
    alpha = 0.10,
    color = NA
  ) +
  geom_smooth(aes(group = interaction(region, period)), method = "lm", se = FALSE, linewidth = 1.1) +
  geom_vline(xintercept = break_year, linetype = "dashed") +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 8)) +
  labs(
    title = "A. Observed CIMMYT attainable yield vs simulated MME yield",
    subtitle = "Points = observed median; shaded band = 20–80% observed range; dashed line = simulated MME",
    x = NULL,
    y = "Yield (t ha-1)",
    color = "Region",
    fill = "Region",
    shape = "Highlighted year"
  )

p_tmax <- regional_annual |>
  ggplot(aes(year, tmax, color = region)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_smooth(aes(group = interaction(region, period)), method = "lm", se = FALSE, linewidth = 1.1) +
  geom_vline(xintercept = break_year, linetype = "dashed") +
  labs(
    title = "B. Mean Tmax during last 90 days",
    x = NULL,
    y = "Tmax (°C)",
    color = "Region"
  )

p_heat <- regional_annual |>
  ggplot(aes(year, heat_days_32, color = region)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_smooth(aes(group = interaction(region, period)), method = "lm", se = FALSE, linewidth = 1.1) +
  geom_vline(xintercept = break_year, linetype = "dashed") +
  labs(
    title = "C. Days with Tmax > 32°C during last 90 days",
    x = NULL,
    y = "Days > 32°C",
    color = "Region"
  )

p_srad <- regional_annual |>
  ggplot(aes(year, srad, color = region)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_smooth(aes(group = interaction(region, period)), method = "lm", se = FALSE, linewidth = 1.1) +
  geom_vline(xintercept = break_year, linetype = "dashed") +
  labs(
    title = "D. Mean solar radiation during last 90 days",
    x = "Year",
    y = "SRAD (MJ m-2 day-1)",
    color = "Region"
  )

fig1 <- (p_yield / p_tmax / p_heat / p_srad) +
  plot_annotation(
    title = "North Delta vs South/Upper Egypt: yield plateau, warming, heat exposure, and radiation",
    subtitle = "Regression lines are split at the 1998 breakpoint as suggested in the meeting"
  )

ggsave(
  file.path(out_dir, "Figure1_North_South_Yield_Climate_4Panel.png"),
  fig1, width = 14, height = 18, dpi = 600
)

# ============================================================
# 12. Figure 2: annual North–South difference
# ============================================================

diff_plot_data <- north_south_difference |>
  select(
    year,
    diff_yield_north_minus_south,
    diff_sim_yield_north_minus_south,
    diff_tmax_south_minus_north,
    diff_heat_days_south_minus_north,
    diff_srad_south_minus_north,
    diff_hdw_south_minus_north
  ) |>
  pivot_longer(-year, names_to = "metric", values_to = "difference")

p_diff <- ggplot(diff_plot_data, aes(year, difference)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dotted") +
  facet_wrap(~metric, scales = "free_y", ncol = 2) +
  geom_vline(xintercept = break_year, linetype = "dashed") +
  labs(
    title = "Annual North–South contrasts",
    subtitle = "Positive yield difference means North > South; positive climate difference means South > North",
    x = "Year",
    y = "Difference"
  )

ggsave(
  file.path(out_dir, "Figure2_Annual_North_South_Differences.png"),
  p_diff, width = 14, height = 10, dpi = 600
)

# ============================================================
# 13. Figure 3: climate-yield relationships by region
# ============================================================

clim_yield_data <- regional_annual |>
  select(region, year, yield_mean, sim_yield_mean, tmax, tmin, tmean,
         heat_days_32, srad, vpd, hdw_days) |>
  pivot_longer(
    cols = c(tmax, tmin, tmean, heat_days_32, srad, vpd, hdw_days),
    names_to = "climate_variable",
    values_to = "climate_value"
  )

p_clim_yield <- ggplot(clim_yield_data, aes(climate_value, yield_mean, color = region)) +
  geom_point(size = 2.5, alpha = 0.85) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~climate_variable, scales = "free_x", ncol = 3) +
  labs(
    title = "Climate–yield relationships differ between North Delta and South/Upper Egypt",
    x = "Climate variable",
    y = "Observed CIMMYT yield (t ha-1)",
    color = "Region"
  )

ggsave(
  file.path(out_dir, "Figure3_Climate_Yield_Relationships_ByRegion.png"),
  p_clim_yield, width = 15, height = 10, dpi = 600
)

# ============================================================
# 14. Regression stats for climate-yield relationships
# ============================================================

climate_vars <- c("tmax", "tmin", "tmean", "heat_days_32", "srad", "vpd", "hdw_days")

climate_yield_stats <- map_dfr(climate_vars, function(v) {
  regional_annual |>
    group_by(region) |>
    group_modify(~ {
      fit <- lm(as.formula(paste("yield_mean ~", v)), data = .x)
      broom::tidy(fit) |>
        filter(term == v) |>
        mutate(
          r2 = broom::glance(fit)$r.squared,
          n = nobs(fit),
          climate_variable = v
        )
    }) |>
    ungroup()
})

write.xlsx(
  climate_yield_stats,
  file.path(out_dir, "Climate_Yield_Regression_Stats_ByRegion.xlsx")
)

# ============================================================
# 15. Figure 4: outlier / suspicious years investigation
# ============================================================

outlier_plot <- regional_annual |>
  mutate(label = ifelse(suspicious_year, as.character(year), "")) |>
  ggplot(aes(year, yield_mean, color = region)) +
  geom_line(linewidth = 1) +
  geom_point(aes(size = suspicious_year), alpha = 0.9) +
  geom_text_repel(aes(label = label), size = 4, show.legend = FALSE) +
  geom_smooth(aes(group = interaction(region, period)), method = "lm", se = FALSE) +
  geom_vline(xintercept = break_year, linetype = "dashed") +
  scale_size_manual(values = c(`FALSE` = 2, `TRUE` = 5)) +
  labs(
    title = "Highlighted high-yield / suspicious years by region",
    subtitle = "These years should be retained but annotated unless experimental error is confirmed",
    x = "Year",
    y = "Observed CIMMYT yield (t ha-1)",
    color = "Region",
    size = "Highlighted year"
  )

ggsave(
  file.path(out_dir, "Figure4_Highlighted_Suspicious_Years.png"),
  outlier_plot, width = 13, height = 8, dpi = 600
)

# ============================================================
# 16. Taylor diagrams by region
# ============================================================

taylor_stats <- list()

for (reg in c("North_Delta", "South_Upper_Egypt")) {
  
  td <- obs_clean |>
    filter(region == reg) |>
    select(yield_cimmyt_t_ha, sim_yield_mme_t_ha) |>
    drop_na()
  
  if (nrow(td) > 5) {
    
    png(
      file.path(out_dir, paste0("Taylor_Diagram_", reg, ".png")),
      width = 3000, height = 2500, res = 300
    )
    
    taylor.diagram(
      ref = td$yield_cimmyt_t_ha,
      model = td$sim_yield_mme_t_ha,
      main = paste0("Taylor diagram: ", reg),
      col = "blue",
      pch = 19,
      pos.cor = TRUE,
      normalize = FALSE
    )
    
    dev.off()
    
    taylor_stats[[reg]] <- tibble(
      region = reg,
      n = nrow(td),
      correlation = cor(td$yield_cimmyt_t_ha, td$sim_yield_mme_t_ha, use = "complete.obs"),
      rmse = hydroGOF::rmse(td$sim_yield_mme_t_ha, td$yield_cimmyt_t_ha),
      mae = hydroGOF::mae(td$sim_yield_mme_t_ha, td$yield_cimmyt_t_ha),
      bias = mean(td$sim_yield_mme_t_ha - td$yield_cimmyt_t_ha, na.rm = TRUE),
      sd_observed = sd(td$yield_cimmyt_t_ha, na.rm = TRUE),
      sd_simulated = sd(td$sim_yield_mme_t_ha, na.rm = TRUE)
    )
  }
}

write.xlsx(
  bind_rows(taylor_stats),
  file.path(out_dir, "Taylor_Performance_North_South.xlsx")
)

# ============================================================
# 17. PCA: North vs South climate-yield separation
# ============================================================

pca_data <- regional_annual |>
  select(region, year, yield_mean, sim_yield_mean, yield_gap,
         tmax, tmin, tmean, heat_days_32, srad, vpd, hdw_days, rain, wind, rhum) |>
  drop_na()

pca_numeric <- pca_data |>
  select(-region, -year)

pca_res <- PCA(pca_numeric, scale.unit = TRUE, graph = FALSE)

p_pca <- fviz_pca_biplot(
  pca_res,
  habillage = pca_data$region,
  addEllipses = TRUE,
  repel = TRUE,
  title = "PCA separation of North Delta and South/Upper Egypt"
) +
  theme_classic(base_size = 14)

ggsave(
  file.path(out_dir, "Figure5_PCA_North_South_Biplot.png"),
  p_pca, width = 11, height = 8, dpi = 600
)

p_eig <- fviz_eig(
  pca_res,
  addlabels = TRUE,
  title = "PCA explained variance"
) +
  theme_classic(base_size = 14)

ggsave(
  file.path(out_dir, "Figure5b_PCA_ExplainedVariance.png"),
  p_eig, width = 9, height = 7, dpi = 600
)

write.xlsx(
  list(
    Eigenvalues = as.data.frame(pca_res$eig),
    Variables = as.data.frame(pca_res$var$coord),
    Individuals = bind_cols(pca_data |> select(region, year), as.data.frame(pca_res$ind$coord))
  ),
  file.path(out_dir, "PCA_North_South_Outputs.xlsx")
)

# ============================================================
# 18. Plateau test: pre vs post 1998 yield means
# ============================================================

plateau_test <- regional_annual |>
  group_by(region, period) |>
  summarise(
    mean_yield = mean(yield_mean, na.rm = TRUE),
    mean_sim_yield = mean(sim_yield_mean, na.rm = TRUE),
    mean_tmax = mean(tmax, na.rm = TRUE),
    mean_heat_days_32 = mean(heat_days_32, na.rm = TRUE),
    mean_srad = mean(srad, na.rm = TRUE),
    mean_hdw_days = mean(hdw_days, na.rm = TRUE),
    n_years = n(),
    .groups = "drop"
  )

plateau_diff <- plateau_test |>
  pivot_wider(
    names_from = period,
    values_from = c(mean_yield, mean_sim_yield, mean_tmax,
                    mean_heat_days_32, mean_srad, mean_hdw_days, n_years)
  ) |>
  mutate(
    yield_change_post_minus_pre = mean_yield_Post_1998 - mean_yield_Pre_1998,
    sim_yield_change_post_minus_pre = mean_sim_yield_Post_1998 - mean_sim_yield_Pre_1998,
    tmax_change_post_minus_pre = mean_tmax_Post_1998 - mean_tmax_Pre_1998,
    heat_days_change_post_minus_pre = mean_heat_days_32_Post_1998 - mean_heat_days_32_Pre_1998,
    srad_change_post_minus_pre = mean_srad_Post_1998 - mean_srad_Pre_1998,
    hdw_change_post_minus_pre = mean_hdw_days_Post_1998 - mean_hdw_days_Pre_1998
  )

write.xlsx(
  list(
    PlateauPeriodMeans = plateau_test,
    PlateauDifferences = plateau_diff
  ),
  file.path(out_dir, "Plateau_Pre_Post_1998_North_South.xlsx")
)

# ============================================================
# 19. Final publication-style summary plot
# ============================================================

summary_long <- plateau_diff |>
  select(
    region,
    yield_change_post_minus_pre,
    sim_yield_change_post_minus_pre,
    tmax_change_post_minus_pre,
    heat_days_change_post_minus_pre,
    srad_change_post_minus_pre,
    hdw_change_post_minus_pre
  ) |>
  pivot_longer(-region, names_to = "metric", values_to = "change")

p_summary <- ggplot(summary_long, aes(region, change, fill = region)) +
  geom_col(width = 0.65) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~metric, scales = "free_y", ncol = 2) +
  labs(
    title = "Post-1998 change relative to pre-1998 baseline",
    subtitle = "Direct comparison of North Delta and South/Upper Egypt",
    x = NULL,
    y = "Post-1998 minus pre-1998"
  ) +
  theme(legend.position = "none")

ggsave(
  file.path(out_dir, "Figure6_Post1998_Change_North_South.png"),
  p_summary, width = 13, height = 10, dpi = 600
)

# ============================================================
# 20. Save session info
# ============================================================

sink(file.path(out_dir, "Session_Info.txt"))
sessionInfo()
sink()

message("DONE: Strong North-South analysis saved in: ", out_dir)