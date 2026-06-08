# =============================================================================
# Visualize CIMMYT climate metrics from DSSAT WTH files
# =============================================================================
# Input outputs from previous script:
#   CIMMYT_Annual_ClimateMetrics_Last90Days.csv
#   CIMMYT_YearLocation_ClimateMetrics_Last90Days.csv
#   CIMMYT_RecordLevel_ClimateMetrics_Last90Days.csv
#   CIMMYTLocs_Code_Check.csv
#
# Main figures produced:
#   Figure_01_Annual_Yield_Climate_Drivers.png/pdf
#   Figure_02_Yield_vs_Climate_Drivers.png/pdf
#   Figure_03_Location_Year_Heatmaps.png/pdf
#   Figure_04_Spatial_Climate_Yield_Summary.png/pdf
#   Figure_05_RecordLevel_CIMMYT_Climate_Relationships.png/pdf
#
# Notes:
#   - Climate metrics are calculated for the last 90 days before maturity
#     or estimated maturity.
#   - Yield is t/ha.
#   - SRAD is MJ m-2 day-1.
#   - VPD is kPa.
# =============================================================================

# install.packages(c(
#   "tidyverse", "readxl", "patchwork", "ggrepel",
#   "scales", "viridis", "broom"
# ))

library(tidyverse)
library(readxl)
library(patchwork)
library(ggrepel)
library(scales)
library(viridis)
library(broom)

# -----------------------------------------------------------------------------
# 1) USER SETTINGS
# -----------------------------------------------------------------------------

base_dir <- "D:/Asseng_paper2024/Updated/Asseng_updated/40YRSSIMULATIONS/ForShawn/21072025/21072025/Update27032026"

sim_dir <- file.path(base_dir, "CIMMYTSimulations")

input_dir <- file.path(sim_dir, "CIMMYT_ClimateMetrics_From_WTH_LocCodes")

# If your files are in a different folder, modify input_dir above.
annual_file <- file.path(input_dir, "CIMMYT_Annual_ClimateMetrics_Last90Days.csv")
yearloc_file <- file.path(input_dir, "CIMMYT_YearLocation_ClimateMetrics_Last90Days.csv")
record_file <- file.path(input_dir, "CIMMYT_RecordLevel_ClimateMetrics_Last90Days.csv")
locs_file <- file.path(input_dir, "CIMMYTLocs_Code_Check.csv")

output_dir <- file.path(sim_dir, "CIMMYT_ClimateMetrics_Figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Breakpoint used in the manuscript
break_year <- 1998

# Colors
col_obs  <- "black"
col_sim  <- "darkgreen"
col_tmax <- "firebrick"
col_tmin <- "steelblue"
col_heat <- "darkorange3"
col_srad <- "forestgreen"
col_vpd  <- "purple4"
col_hdw  <- "brown4"

# -----------------------------------------------------------------------------
# 2) READ DATA
# -----------------------------------------------------------------------------

annual <- read_csv(annual_file, show_col_types = FALSE)
yearloc <- read_csv(yearloc_file, show_col_types = FALSE)
record <- read_csv(record_file, show_col_types = FALSE)
locs <- read_csv(locs_file, show_col_types = FALSE)

annual <- annual %>%
  mutate(Year = as.integer(Year)) %>%
  arrange(Year)

yearloc <- yearloc %>%
  mutate(
    Year = as.integer(Year),
    MatchedLocName = as.factor(MatchedLocName)
  ) %>%
  arrange(Year, MatchedLocName)

record <- record %>%
  mutate(
    Year = as.integer(Year),
    MatchedLocName = as.factor(MatchedLocName)
  ) %>%
  arrange(Year, MatchedLocName)

# -----------------------------------------------------------------------------
# 3) HELPER FUNCTIONS
# -----------------------------------------------------------------------------

theme_pub <- function(base_size = 14) {
  theme_bw(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 4),
      plot.subtitle = element_text(size = base_size),
      axis.title = element_text(size = base_size + 1),
      axis.text = element_text(size = base_size),
      legend.position = "top",
      legend.title = element_blank(),
      panel.grid.minor = element_line(color = "grey92", linewidth = 0.25),
      panel.grid.major = element_line(color = "grey85", linewidth = 0.35),
      strip.text = element_text(face = "bold", size = base_size)
    )
}

add_break_line <- function() {
  geom_vline(xintercept = break_year, linetype = "dashed", color = "grey35", linewidth = 0.6)
}

save_plot <- function(p, filename, width = 14, height = 9, dpi = 320) {
  ggsave(file.path(output_dir, paste0(filename, ".png")),
         p, width = width, height = height, dpi = dpi, limitsize = FALSE)
  ggsave(file.path(output_dir, paste0(filename, ".pdf")),
         p, width = width, height = height, limitsize = FALSE)
}

lm_label <- function(df, x, y) {
  d <- df %>%
    select(x = all_of(x), y = all_of(y)) %>%
    filter(!is.na(x), !is.na(y))

  if (nrow(d) < 4) {
    return("Insufficient data")
  }

  fit <- lm(y ~ x, data = d)
  sm <- summary(fit)

  paste0(
    "R² = ", round(sm$r.squared, 2),
    ", P = ", ifelse(coef(sm)[2, 4] < 0.001, "<0.001", signif(coef(sm)[2, 4], 2))
  )
}

# -----------------------------------------------------------------------------
# 4) FIGURE 1: ANNUAL YIELD AND CLIMATE DRIVERS
# -----------------------------------------------------------------------------

p_yield <- ggplot(annual, aes(x = Year)) +
  geom_ribbon(
    aes(
      ymin = annual_mean_YieldCIMMYT - annual_sd_YieldCIMMYT_locations,
      ymax = annual_mean_YieldCIMMYT + annual_sd_YieldCIMMYT_locations,
      fill = "Observed CIMMYT ± SD"
    ),
    alpha = 0.15,
    na.rm = TRUE
  ) +
  geom_ribbon(
    aes(
      ymin = annual_mean_SimYieldMME_t_ha - annual_sd_SimYieldMME_locations,
      ymax = annual_mean_SimYieldMME_t_ha + annual_sd_SimYieldMME_locations,
      fill = "Simulated MME ± SD"
    ),
    alpha = 0.18,
    na.rm = TRUE
  ) +
  geom_point(
    aes(y = annual_mean_YieldCIMMYT, color = "Observed CIMMYT"),
    size = 3.0,
    na.rm = TRUE
  ) +
  geom_line(
    aes(y = annual_mean_SimYieldMME_t_ha, color = "Simulated MME"),
    linewidth = 1.1,
    na.rm = TRUE
  ) +
  add_break_line() +
  scale_color_manual(values = c("Observed CIMMYT" = col_obs, "Simulated MME" = col_sim)) +
  scale_fill_manual(values = c("Observed CIMMYT ± SD" = "grey40", "Simulated MME ± SD" = col_sim)) +
  labs(
    title = "A. Annual observed and simulated wheat yield",
    y = "Grain yield (t/ha)",
    x = NULL
  ) +
  theme_pub(14) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

p_temp <- ggplot(annual, aes(x = Year)) +
  geom_line(aes(y = annual_mean_Tmax, color = "Tmax"), linewidth = 1.0, na.rm = TRUE) +
  geom_line(aes(y = annual_mean_Tmin, color = "Tmin"), linewidth = 1.0, na.rm = TRUE) +
  geom_ribbon(
    aes(
      ymin = annual_mean_Tmax - annual_sd_Tmax_locations,
      ymax = annual_mean_Tmax + annual_sd_Tmax_locations
    ),
    fill = col_tmax,
    alpha = 0.12,
    na.rm = TRUE
  ) +
  geom_ribbon(
    aes(
      ymin = annual_mean_Tmin - annual_sd_Tmin_locations,
      ymax = annual_mean_Tmin + annual_sd_Tmin_locations
    ),
    fill = col_tmin,
    alpha = 0.12,
    na.rm = TRUE
  ) +
  add_break_line() +
  scale_color_manual(values = c("Tmax" = col_tmax, "Tmin" = col_tmin)) +
  labs(
    title = "B. Mean temperature during last 90 days",
    y = "Temperature (°C)",
    x = NULL
  ) +
  theme_pub(14) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

p_heat <- ggplot(annual, aes(x = Year, y = annual_mean_HeatDays_Tmax_GT32)) +
  geom_col(fill = col_heat, alpha = 0.75, width = 0.75, na.rm = TRUE) +
  geom_line(color = "black", linewidth = 0.6, na.rm = TRUE) +
  geom_point(color = "black", size = 1.8, na.rm = TRUE) +
  add_break_line() +
  labs(
    title = "C. Heat stress days during last 90 days",
    y = "Days with Tmax > 32°C",
    x = NULL
  ) +
  theme_pub(14) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

p_srad <- ggplot(annual, aes(x = Year, y = annual_mean_SRAD)) +
  geom_ribbon(
    aes(
      ymin = annual_mean_SRAD - annual_sd_SRAD_locations,
      ymax = annual_mean_SRAD + annual_sd_SRAD_locations
    ),
    fill = col_srad,
    alpha = 0.18,
    na.rm = TRUE
  ) +
  geom_line(color = col_srad, linewidth = 1.1, na.rm = TRUE) +
  geom_point(color = col_srad, size = 2.1, na.rm = TRUE) +
  add_break_line() +
  labs(
    title = "D. Mean solar radiation during last 90 days",
    y = expression("SRAD (MJ " * m^{-2} * " day"^{-1} * ")"),
    x = NULL
  ) +
  theme_pub(14) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

p_vpd <- ggplot(annual, aes(x = Year, y = annual_mean_VPD)) +
  geom_line(color = col_vpd, linewidth = 1.05, na.rm = TRUE) +
  geom_point(color = col_vpd, size = 2.1, na.rm = TRUE) +
  add_break_line() +
  labs(
    title = "E. Mean vapor pressure deficit during last 90 days",
    y = "VPD (kPa)",
    x = "Year"
  ) +
  theme_pub(14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_hdw <- ggplot(annual, aes(x = Year, y = annual_mean_HDW_days)) +
  geom_col(fill = col_hdw, alpha = 0.75, width = 0.75, na.rm = TRUE) +
  add_break_line() +
  labs(
    title = "F. Hot-dry-windy days during last 90 days",
    y = "HDW-like days",
    x = "Year"
  ) +
  theme_pub(14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fig1 <- (p_yield / p_temp / p_heat / p_srad / (p_vpd | p_hdw)) +
  plot_layout(heights = c(1.35, 1, 1, 1, 1)) +
  plot_annotation(
    title = "CIMMYT wheat yield and climate drivers during the last 90 days of the season",
    subtitle = paste0("Dashed vertical line indicates the hypothesized breakpoint year: ", break_year),
    theme = theme(
      plot.title = element_text(face = "bold", size = 22),
      plot.subtitle = element_text(size = 15)
    )
  )

save_plot(fig1, "Figure_01_Annual_Yield_Climate_Drivers", width = 16, height = 18)

# -----------------------------------------------------------------------------
# 5) FIGURE 2: YIELD VS CLIMATE DRIVERS
# -----------------------------------------------------------------------------

scatter_data <- annual %>%
  select(
    Year,
    annual_mean_YieldCIMMYT,
    annual_mean_SimYieldMME_t_ha,
    annual_mean_Tmax,
    annual_mean_Tmin,
    annual_mean_HeatDays_Tmax_GT32,
    annual_mean_SRAD,
    annual_mean_VPD,
    annual_mean_HDW_days
  )

driver_long <- scatter_data %>%
  pivot_longer(
    cols = c(
      annual_mean_Tmax,
      annual_mean_Tmin,
      annual_mean_HeatDays_Tmax_GT32,
      annual_mean_SRAD,
      annual_mean_VPD,
      annual_mean_HDW_days
    ),
    names_to = "Driver",
    values_to = "DriverValue"
  ) %>%
  mutate(
    Driver = recode(
      Driver,
      annual_mean_Tmax = "Tmax",
      annual_mean_Tmin = "Tmin",
      annual_mean_HeatDays_Tmax_GT32 = "Days Tmax > 32°C",
      annual_mean_SRAD = "Solar radiation",
      annual_mean_VPD = "VPD",
      annual_mean_HDW_days = "HDW-like days"
    )
  )

yield_long <- driver_long %>%
  select(Year, Driver, DriverValue, annual_mean_YieldCIMMYT, annual_mean_SimYieldMME_t_ha) %>%
  pivot_longer(
    cols = c(annual_mean_YieldCIMMYT, annual_mean_SimYieldMME_t_ha),
    names_to = "YieldType",
    values_to = "Yield"
  ) %>%
  mutate(
    YieldType = recode(
      YieldType,
      annual_mean_YieldCIMMYT = "Observed CIMMYT",
      annual_mean_SimYieldMME_t_ha = "Simulated MME"
    )
  ) %>%
  filter(!is.na(DriverValue), !is.na(Yield))

stats_scatter <- yield_long %>%
  group_by(YieldType, Driver) %>%
  summarise(
    n = n(),
    r = cor(DriverValue, Yield, use = "complete.obs"),
    model = list(lm(Yield ~ DriverValue)),
    .groups = "drop"
  ) %>%
  mutate(
    glanced = map(model, broom::glance),
    p = map_dbl(model, ~ summary(.x)$coefficients[2, 4]),
    r2 = map_dbl(glanced, "r.squared"),
    label = paste0("R²=", round(r2, 2), ", P=", ifelse(p < 0.001, "<0.001", signif(p, 2)))
  ) %>%
  select(YieldType, Driver, n, r, r2, p, label)

write_csv(stats_scatter, file.path(output_dir, "Yield_vs_Climate_Driver_Regression_Stats.csv"))

label_positions <- yield_long %>%
  group_by(YieldType, Driver) %>%
  summarise(
    x = min(DriverValue, na.rm = TRUE),
    y = max(Yield, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(stats_scatter, by = c("YieldType", "Driver"))

fig2 <- ggplot(yield_long, aes(x = DriverValue, y = Yield, color = YieldType, shape = YieldType)) +
  geom_point(size = 2.4, alpha = 0.9, na.rm = TRUE) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.9, alpha = 0.15, na.rm = TRUE) +
  geom_text(
    data = label_positions,
    aes(x = x, y = y, label = label, color = YieldType),
    hjust = 0,
    vjust = 1.1,
    size = 3.5,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  facet_grid(YieldType ~ Driver, scales = "free_x") +
  scale_color_manual(values = c("Observed CIMMYT" = col_obs, "Simulated MME" = col_sim)) +
  scale_shape_manual(values = c("Observed CIMMYT" = 16, "Simulated MME" = 17)) +
  labs(
    title = "Relationships between wheat yield and last-90-day climate drivers",
    subtitle = "Points are annual means across matched CIMMYT locations; lines are linear regressions.",
    x = "Climate driver value",
    y = "Grain yield (t/ha)"
  ) +
  theme_pub(13) +
  theme(
    legend.position = "top",
    strip.text = element_text(size = 12, face = "bold")
  )

save_plot(fig2, "Figure_02_Yield_vs_Climate_Drivers", width = 20, height = 9)

# -----------------------------------------------------------------------------
# 6) FIGURE 3: LOCATION × YEAR HEATMAPS
# -----------------------------------------------------------------------------

heatmap_data <- yearloc %>%
  mutate(MatchedLocName = fct_reorder(MatchedLocName, MatchedLocLat, .fun = mean, na.rm = TRUE))

heatmap_long <- heatmap_data %>%
  select(
    Year,
    MatchedLocName,
    mean_Tmax,
    mean_HeatDays_Tmax_GT32,
    mean_SRAD,
    mean_VPD,
    mean_HDW_days,
    mean_YieldCIMMYT,
    mean_SimYieldMME_t_ha
  ) %>%
  pivot_longer(
    cols = c(
      mean_Tmax,
      mean_HeatDays_Tmax_GT32,
      mean_SRAD,
      mean_VPD,
      mean_HDW_days,
      mean_YieldCIMMYT,
      mean_SimYieldMME_t_ha
    ),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Variable = recode(
      Variable,
      mean_Tmax = "Tmax (°C)",
      mean_HeatDays_Tmax_GT32 = "Days Tmax > 32°C",
      mean_SRAD = "Solar radiation",
      mean_VPD = "VPD",
      mean_HDW_days = "HDW-like days",
      mean_YieldCIMMYT = "Observed yield",
      mean_SimYieldMME_t_ha = "Simulated yield"
    )
  )

fig3 <- ggplot(heatmap_long, aes(x = Year, y = MatchedLocName, fill = Value)) +
  geom_tile(color = "white", linewidth = 0.15) +
  facet_wrap(~ Variable, scales = "free", ncol = 2) +
  scale_fill_viridis_c(option = "C", na.value = "grey95") +
  labs(
    title = "Location-year patterns of CIMMYT yield and last-90-day climate drivers",
    x = "Year",
    y = "Matched CIMMYT location",
    fill = "Value"
  ) +
  theme_pub(12) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 13, face = "bold")
  )

save_plot(fig3, "Figure_03_Location_Year_Heatmaps", width = 16, height = 14)

# -----------------------------------------------------------------------------
# 7) FIGURE 4: SPATIAL SUMMARY BY CIMMYT LOCATION
# -----------------------------------------------------------------------------

spatial_summary <- yearloc %>%
  group_by(MatchedWeatherCode, MatchedLocName, MatchedLocLat, MatchedLocLong) %>%
  summarise(
    n_years = n_distinct(Year),
    mean_observed_yield = mean(mean_YieldCIMMYT, na.rm = TRUE),
    mean_simulated_yield = mean(mean_SimYieldMME_t_ha, na.rm = TRUE),
    mean_yield_gap = mean(mean_SimYieldMME_t_ha - mean_YieldCIMMYT, na.rm = TRUE),
    mean_Tmax = mean(mean_Tmax, na.rm = TRUE),
    mean_heat_days = mean(mean_HeatDays_Tmax_GT32, na.rm = TRUE),
    mean_SRAD = mean(mean_SRAD, na.rm = TRUE),
    mean_VPD = mean(mean_VPD, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(spatial_summary, file.path(output_dir, "Spatial_Summary_By_CIMMYT_Location.csv"))

p_spatial_heat <- ggplot(spatial_summary, aes(x = MatchedLocLong, y = MatchedLocLat)) +
  geom_point(aes(size = n_years, color = mean_heat_days), alpha = 0.9) +
  geom_text_repel(aes(label = MatchedLocName), size = 3.3, max.overlaps = 30) +
  scale_color_viridis_c(option = "A") +
  scale_size_continuous(range = c(3, 9)) +
  labs(
    title = "A. Spatial distribution of heat-stress exposure",
    x = "Longitude",
    y = "Latitude",
    color = "Mean days\nTmax >32°C",
    size = "Years"
  ) +
  theme_pub(13) +
  theme(legend.position = "right")

p_spatial_srad <- ggplot(spatial_summary, aes(x = MatchedLocLong, y = MatchedLocLat)) +
  geom_point(aes(size = n_years, color = mean_SRAD), alpha = 0.9) +
  geom_text_repel(aes(label = MatchedLocName), size = 3.3, max.overlaps = 30) +
  scale_color_viridis_c(option = "D") +
  scale_size_continuous(range = c(3, 9)) +
  labs(
    title = "B. Spatial distribution of solar radiation",
    x = "Longitude",
    y = "Latitude",
    color = "Mean SRAD",
    size = "Years"
  ) +
  theme_pub(13) +
  theme(legend.position = "right")

p_spatial_gap <- ggplot(spatial_summary, aes(x = MatchedLocLong, y = MatchedLocLat)) +
  geom_point(aes(size = n_years, color = mean_yield_gap), alpha = 0.9) +
  geom_text_repel(aes(label = MatchedLocName), size = 3.3, max.overlaps = 30) +
  scale_color_gradient2(low = "steelblue", mid = "grey90", high = "firebrick", midpoint = 0) +
  scale_size_continuous(range = c(3, 9)) +
  labs(
    title = "C. Spatial distribution of yield gap",
    subtitle = "Yield gap = simulated MME − observed CIMMYT",
    x = "Longitude",
    y = "Latitude",
    color = "Yield gap\n(t/ha)",
    size = "Years"
  ) +
  theme_pub(13) +
  theme(legend.position = "right")

fig4 <- (p_spatial_heat | p_spatial_srad | p_spatial_gap) +
  plot_annotation(
    title = "Spatial summary of climate exposure and yield gap across CIMMYT matched locations",
    theme = theme(plot.title = element_text(face = "bold", size = 20))
  )

save_plot(fig4, "Figure_04_Spatial_Climate_Yield_Summary", width = 21, height = 7)

# -----------------------------------------------------------------------------
# 8) FIGURE 5: RECORD-LEVEL OBSERVED YIELD RELATIONSHIPS
# -----------------------------------------------------------------------------

record_long <- record %>%
  select(
    Year,
    Loc_desc,
    MatchedLocName,
    YieldCIMMYT,
    Tmax_mean,
    Tmin_mean,
    HeatDays_Tmax_GT32,
    SRAD_mean,
    VPD_mean,
    HDW_days
  ) %>%
  pivot_longer(
    cols = c(Tmax_mean, Tmin_mean, HeatDays_Tmax_GT32, SRAD_mean, VPD_mean, HDW_days),
    names_to = "Driver",
    values_to = "DriverValue"
  ) %>%
  mutate(
    Driver = recode(
      Driver,
      Tmax_mean = "Tmax",
      Tmin_mean = "Tmin",
      HeatDays_Tmax_GT32 = "Days Tmax > 32°C",
      SRAD_mean = "Solar radiation",
      VPD_mean = "VPD",
      HDW_days = "HDW-like days"
    )
  ) %>%
  filter(!is.na(YieldCIMMYT), !is.na(DriverValue))

record_stats <- record_long %>%
  group_by(Driver) %>%
  summarise(
    n = n(),
    model = list(lm(YieldCIMMYT ~ DriverValue)),
    .groups = "drop"
  ) %>%
  mutate(
    r2 = map_dbl(model, ~ summary(.x)$r.squared),
    p = map_dbl(model, ~ summary(.x)$coefficients[2, 4]),
    label = paste0("R²=", round(r2, 2), ", P=", ifelse(p < 0.001, "<0.001", signif(p, 2)))
  ) %>%
  select(Driver, n, r2, p, label)

write_csv(record_stats, file.path(output_dir, "RecordLevel_Yield_vs_Climate_Stats.csv"))

record_label_positions <- record_long %>%
  group_by(Driver) %>%
  summarise(
    x = min(DriverValue, na.rm = TRUE),
    y = max(YieldCIMMYT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(record_stats, by = "Driver")

fig5 <- ggplot(record_long, aes(x = DriverValue, y = YieldCIMMYT)) +
  geom_point(aes(color = Year), alpha = 0.65, size = 2.0, na.rm = TRUE) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.9, alpha = 0.12, na.rm = TRUE) +
  geom_text(
    data = record_label_positions,
    aes(x = x, y = y, label = label),
    hjust = 0,
    vjust = 1.1,
    size = 3.8,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  facet_wrap(~ Driver, scales = "free_x", ncol = 3) +
  scale_color_viridis_c(option = "C") +
  labs(
    title = "Record-level CIMMYT observed yield relationships with climate drivers",
    subtitle = "Each point represents a CIMMYT yield record matched to the nearest WTH location.",
    x = "Climate driver value",
    y = "Observed CIMMYT yield (t/ha)",
    color = "Year"
  ) +
  theme_pub(13) +
  theme(legend.position = "right")

save_plot(fig5, "Figure_05_RecordLevel_CIMMYT_Climate_Relationships", width = 17, height = 10)

# -----------------------------------------------------------------------------
# 9) OPTIONAL: COMPACT MANUSCRIPT-STYLE 4-PANEL FIGURE
# -----------------------------------------------------------------------------

p_compact_yield <- p_yield +
  labs(title = "A. Yield", y = "Yield (t/ha)") +
  theme(legend.position = "top")

p_compact_heat <- p_heat +
  labs(title = "B. Days >32°C", y = "Days") +
  theme(legend.position = "none")

p_compact_srad <- p_srad +
  labs(title = "C. Solar radiation", y = expression("MJ " * m^{-2} * " day"^{-1})) +
  theme(legend.position = "none")

p_compact_vpd <- p_vpd +
  labs(title = "D. VPD", y = "kPa") +
  theme(legend.position = "none")

fig_compact <- (p_compact_yield / p_compact_heat / p_compact_srad / p_compact_vpd) +
  plot_layout(heights = c(1.3, 1, 1, 1)) +
  plot_annotation(
    title = "Yield and key climate drivers during the last 90 days of the CIMMYT wheat season",
    subtitle = paste0("Climate metrics were extracted from DSSAT WTH files for matched CIMMYT locations; breakpoint = ", break_year),
    theme = theme(
      plot.title = element_text(face = "bold", size = 21),
      plot.subtitle = element_text(size = 14)
    )
  )

save_plot(fig_compact, "Figure_06_Compact_Yield_Heat_SRAD_VPD", width = 15, height = 14)

# -----------------------------------------------------------------------------
# 10) SAVE PROCESSED TABLES
# -----------------------------------------------------------------------------

write_csv(annual, file.path(output_dir, "Annual_Used_For_Figures.csv"))
write_csv(yearloc, file.path(output_dir, "YearLocation_Used_For_Figures.csv"))
write_csv(record, file.path(output_dir, "RecordLevel_Used_For_Figures.csv"))

message("Done.")
message("Figures saved in: ", output_dir)
