# =============================================================================
# UPDATED Visualize CIMMYT climate metrics from DSSAT WTH files
# =============================================================================
# Improvements:
#   1) Figure 01 keeps clear x-axis year labels.
#   2) Figure 04 plots CIMMYT locations on Egypt cropland shapefile/map.
#   3) Larger black fonts for axis labels, axis text, legends, titles, and facets.
#   4) All six figures exported with larger dimensions and high resolution.
# =============================================================================

# install.packages(c(
#   "tidyverse", "readxl", "patchwork", "ggrepel", "scales",
#   "viridis", "broom", "sf", "rnaturalearth", "rnaturalearthdata"
# ))

library(tidyverse)
library(readxl)
library(patchwork)
library(ggrepel)
library(scales)
library(viridis)
library(broom)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# -----------------------------------------------------------------------------
# 1) USER SETTINGS
# -----------------------------------------------------------------------------

base_dir <- "D:/Asseng_paper2024/Updated/Asseng_updated/40YRSSIMULATIONS/ForShawn/21072025/21072025/Update27032026"
sim_dir <- file.path(base_dir, "CIMMYTSimulations")
input_dir <- file.path(sim_dir, "CIMMYT_ClimateMetrics_From_WTH_LocCodes")

annual_file  <- file.path(input_dir, "CIMMYT_Annual_ClimateMetrics_Last90Days.csv")
yearloc_file <- file.path(input_dir, "CIMMYT_YearLocation_ClimateMetrics_Last90Days.csv")
record_file  <- file.path(input_dir, "CIMMYT_RecordLevel_ClimateMetrics_Last90Days.csv")
locs_file    <- file.path(input_dir, "CIMMYTLocs_Code_Check.csv")

egypt_crop_shp <- file.path(
  base_dir,
  "EgyptShapefile/DrYasserMap/EgyptCropland2024/EgyptCropland2024.shp"
)

output_dir <- file.path(sim_dir, "CIMMYT_ClimateMetrics_Figures_Updated")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

break_year <- 1998

col_obs  <- "black"
col_sim  <- "darkgreen"
col_tmax <- "firebrick"
col_tmin <- "steelblue"
col_heat <- "darkorange3"
col_srad <- "forestgreen"
col_vpd  <- "purple4"
col_hdw  <- "brown4"

base_font <- 20
title_font <- 27
subtitle_font <- 21
axis_title_font <- 23
axis_text_font <- 20
legend_text_font <- 20
legend_title_font <- 21
strip_font <- 20

# -----------------------------------------------------------------------------
# 2) READ DATA
# -----------------------------------------------------------------------------

annual <- read_csv(annual_file, show_col_types = FALSE) %>%
  mutate(Year = as.integer(Year)) %>%
  arrange(Year)

yearloc <- read_csv(yearloc_file, show_col_types = FALSE) %>%
  mutate(
    Year = as.integer(Year),
    MatchedLocName = as.factor(MatchedLocName)
  ) %>%
  arrange(Year, MatchedLocName)

record <- read_csv(record_file, show_col_types = FALSE) %>%
  mutate(
    Year = as.integer(Year),
    MatchedLocName = as.factor(MatchedLocName)
  ) %>%
  arrange(Year, MatchedLocName)

locs <- read_csv(locs_file, show_col_types = FALSE)

# -----------------------------------------------------------------------------
# 3) HELPERS
# -----------------------------------------------------------------------------

theme_pub_big <- function(base_size = base_font) {
  theme_bw(base_size = base_size) +
    theme(
      text = element_text(color = "black"),
      plot.title = element_text(face = "bold", size = title_font, color = "black"),
      plot.subtitle = element_text(size = subtitle_font, color = "black"),
      axis.title = element_text(size = axis_title_font, color = "black"),
      axis.text = element_text(size = axis_text_font, color = "black"),
      legend.position = "top",
      legend.title = element_text(size = legend_title_font, face = "bold", color = "black"),
      legend.text = element_text(size = legend_text_font, color = "black"),
      legend.key.size = unit(1.25, "cm"),
      legend.spacing.x = unit(0.35, "cm"),
      legend.spacing.y = unit(0.30, "cm"),
      panel.grid.minor = element_line(color = "grey92", linewidth = 0.25),
      panel.grid.major = element_line(color = "grey85", linewidth = 0.35),
      strip.text = element_text(face = "bold", size = strip_font, color = "black"),
      strip.background = element_rect(fill = "grey92", color = "grey50")
    )
}

add_break_line <- function() {
  geom_vline(
    xintercept = break_year,
    linetype = "dashed",
    color = "grey25",
    linewidth = 0.8
  )
}

save_plot <- function(p, filename, width = 18, height = 11, dpi = 320) {
  ggsave(file.path(output_dir, paste0(filename, ".png")), p,
         width = width, height = height, dpi = dpi, limitsize = FALSE)
  ggsave(file.path(output_dir, paste0(filename, ".pdf")), p,
         width = width, height = height, limitsize = FALSE)
}

x_breaks_2yr <- seq(
  floor(min(annual$Year, na.rm = TRUE) / 2) * 2,
  ceiling(max(annual$Year, na.rm = TRUE) / 2) * 2,
  by = 2
)

# -----------------------------------------------------------------------------
# 4) FIGURE 1
# -----------------------------------------------------------------------------

p_yield <- ggplot(annual, aes(x = Year)) +
  geom_ribbon(aes(ymin = annual_mean_YieldCIMMYT - annual_sd_YieldCIMMYT_locations,
                  ymax = annual_mean_YieldCIMMYT + annual_sd_YieldCIMMYT_locations,
                  fill = "Observed CIMMYT ± SD"),
              alpha = 0.16, na.rm = TRUE) +
  geom_ribbon(aes(ymin = annual_mean_SimYieldMME_t_ha - annual_sd_SimYieldMME_locations,
                  ymax = annual_mean_SimYieldMME_t_ha + annual_sd_SimYieldMME_locations,
                  fill = "Simulated MME ± SD"),
              alpha = 0.20, na.rm = TRUE) +
  geom_point(aes(y = annual_mean_YieldCIMMYT, color = "Observed CIMMYT"),
             size = 4.0, na.rm = TRUE) +
  geom_line(aes(y = annual_mean_SimYieldMME_t_ha, color = "Simulated MME"),
            linewidth = 1.45, na.rm = TRUE) +
  add_break_line() +
  scale_x_continuous(breaks = x_breaks_2yr) +
  scale_color_manual(values = c("Observed CIMMYT" = col_obs, "Simulated MME" = col_sim)) +
  scale_fill_manual(values = c("Observed CIMMYT ± SD" = "grey45", "Simulated MME ± SD" = col_sim)) +
  labs(title = "A. Annual observed and simulated wheat yield",
       y = "Grain yield (t/ha)", x = "Year", color = "", fill = "") +
  theme_pub_big() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"))

p_temp <- ggplot(annual, aes(x = Year)) +
  geom_ribbon(aes(ymin = annual_mean_Tmax - annual_sd_Tmax_locations,
                  ymax = annual_mean_Tmax + annual_sd_Tmax_locations),
              fill = col_tmax, alpha = 0.13, na.rm = TRUE) +
  geom_ribbon(aes(ymin = annual_mean_Tmin - annual_sd_Tmin_locations,
                  ymax = annual_mean_Tmin + annual_sd_Tmin_locations),
              fill = col_tmin, alpha = 0.13, na.rm = TRUE) +
  geom_line(aes(y = annual_mean_Tmax, color = "Tmax"), linewidth = 1.35, na.rm = TRUE) +
  geom_line(aes(y = annual_mean_Tmin, color = "Tmin"), linewidth = 1.35, na.rm = TRUE) +
  add_break_line() +
  scale_x_continuous(breaks = x_breaks_2yr) +
  scale_color_manual(values = c("Tmax" = col_tmax, "Tmin" = col_tmin)) +
  labs(title = "B. Mean temperature during last 90 days",
       y = "Temperature (°C)", x = "Year", color = "") +
  theme_pub_big() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"))

p_heat <- ggplot(annual, aes(x = Year, y = annual_mean_HeatDays_Tmax_GT32)) +
  geom_col(fill = col_heat, alpha = 0.80, width = 0.75, na.rm = TRUE) +
  geom_line(color = "black", linewidth = 0.8, na.rm = TRUE) +
  geom_point(color = "black", size = 2.5, na.rm = TRUE) +
  add_break_line() +
  scale_x_continuous(breaks = x_breaks_2yr) +
  labs(title = "C. Heat stress days during last 90 days",
       y = "Days with Tmax > 32°C", x = "Year") +
  theme_pub_big() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"))

p_srad <- ggplot(annual, aes(x = Year, y = annual_mean_SRAD)) +
  geom_ribbon(aes(ymin = annual_mean_SRAD - annual_sd_SRAD_locations,
                  ymax = annual_mean_SRAD + annual_sd_SRAD_locations),
              fill = col_srad, alpha = 0.20, na.rm = TRUE) +
  geom_line(color = col_srad, linewidth = 1.45, na.rm = TRUE) +
  geom_point(color = col_srad, size = 3.0, na.rm = TRUE) +
  add_break_line() +
  scale_x_continuous(breaks = x_breaks_2yr) +
  labs(title = "D. Mean solar radiation during last 90 days",
       y = expression("SRAD (MJ " * m^{-2} * " day"^{-1} * ")"),
       x = "Year") +
  theme_pub_big() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"))

p_vpd <- ggplot(annual, aes(x = Year, y = annual_mean_VPD)) +
  geom_line(color = col_vpd, linewidth = 1.40, na.rm = TRUE) +
  geom_point(color = col_vpd, size = 3.0, na.rm = TRUE) +
  add_break_line() +
  scale_x_continuous(breaks = x_breaks_2yr) +
  labs(title = "E. Mean vapor pressure deficit during last 90 days",
       y = "VPD (kPa)", x = "Year") +
  theme_pub_big() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"))

p_hdw <- ggplot(annual, aes(x = Year, y = annual_mean_HDW_days)) +
  geom_col(fill = col_hdw, alpha = 0.82, width = 0.75, na.rm = TRUE) +
  add_break_line() +
  scale_x_continuous(breaks = x_breaks_2yr) +
  labs(title = "F. Hot-dry-windy days during last 90 days",
       y = "HDW-like days", x = "Year") +
  theme_pub_big() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"))

fig1 <- (p_yield / p_temp / p_heat / p_srad / (p_vpd | p_hdw)) +
  plot_layout(heights = c(1.45, 1.15, 1.15, 1.15, 1.15)) +
  plot_annotation(
    title = "CIMMYT wheat yield and climate drivers during the last 90 days of the season",
    subtitle = paste0("Dashed vertical line indicates the hypothesized breakpoint year: ", break_year),
    theme = theme(plot.title = element_text(face = "bold", size = 32, color = "black"),
                  plot.subtitle = element_text(size = 24, color = "black"))
  )

save_plot(fig1, "Figure_01_Annual_Yield_Climate_Drivers_UPDATED", width = 22, height = 28)

# -----------------------------------------------------------------------------
# 5) FIGURE 2
# -----------------------------------------------------------------------------

scatter_data <- annual %>%
  select(Year, annual_mean_YieldCIMMYT, annual_mean_SimYieldMME_t_ha,
         annual_mean_Tmax, annual_mean_Tmin, annual_mean_HeatDays_Tmax_GT32,
         annual_mean_SRAD, annual_mean_VPD, annual_mean_HDW_days)

driver_long <- scatter_data %>%
  pivot_longer(cols = c(annual_mean_Tmax, annual_mean_Tmin,
                        annual_mean_HeatDays_Tmax_GT32, annual_mean_SRAD,
                        annual_mean_VPD, annual_mean_HDW_days),
               names_to = "Driver", values_to = "DriverValue") %>%
  mutate(Driver = recode(
    Driver,
    annual_mean_Tmax = "Tmax",
    annual_mean_Tmin = "Tmin",
    annual_mean_HeatDays_Tmax_GT32 = "Days Tmax > 32°C",
    annual_mean_SRAD = "Solar radiation",
    annual_mean_VPD = "VPD",
    annual_mean_HDW_days = "HDW-like days"
  ))

yield_long <- driver_long %>%
  select(Year, Driver, DriverValue, annual_mean_YieldCIMMYT, annual_mean_SimYieldMME_t_ha) %>%
  pivot_longer(cols = c(annual_mean_YieldCIMMYT, annual_mean_SimYieldMME_t_ha),
               names_to = "YieldType", values_to = "Yield") %>%
  mutate(YieldType = recode(
    YieldType,
    annual_mean_YieldCIMMYT = "Observed CIMMYT",
    annual_mean_SimYieldMME_t_ha = "Simulated MME"
  )) %>%
  filter(!is.na(DriverValue), !is.na(Yield))

stats_scatter <- yield_long %>%
  group_by(YieldType, Driver) %>%
  summarise(n = n(), model = list(lm(Yield ~ DriverValue)), .groups = "drop") %>%
  mutate(
    p = map_dbl(model, ~ summary(.x)$coefficients[2, 4]),
    r2 = map_dbl(model, ~ summary(.x)$r.squared),
    label = paste0("R²=", round(r2, 2), ", P=", ifelse(p < 0.001, "<0.001", signif(p, 2)))
  ) %>%
  select(YieldType, Driver, n, r2, p, label)

write_csv(stats_scatter, file.path(output_dir, "Yield_vs_Climate_Driver_Regression_Stats.csv"))

label_positions <- yield_long %>%
  group_by(YieldType, Driver) %>%
  summarise(x = min(DriverValue, na.rm = TRUE), y = max(Yield, na.rm = TRUE), .groups = "drop") %>%
  left_join(stats_scatter, by = c("YieldType", "Driver"))

fig2 <- ggplot(yield_long, aes(x = DriverValue, y = Yield, color = YieldType, shape = YieldType)) +
  geom_point(size = 3.2, alpha = 0.90, na.rm = TRUE) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.05, alpha = 0.16, na.rm = TRUE) +
  geom_text(data = label_positions,
            aes(x = x, y = y, label = label, color = YieldType),
            hjust = 0, vjust = 1.1, size = 5.0, fontface = "bold",
            inherit.aes = FALSE) +
  facet_grid(YieldType ~ Driver, scales = "free_x") +
  scale_color_manual(values = c("Observed CIMMYT" = col_obs, "Simulated MME" = col_sim)) +
  scale_shape_manual(values = c("Observed CIMMYT" = 16, "Simulated MME" = 17)) +
  labs(title = "Relationships between wheat yield and last-90-day climate drivers",
       subtitle = "Points are annual means across matched CIMMYT locations; lines are linear regressions.",
       x = "Climate driver value", y = "Grain yield (t/ha)", color = "", shape = "") +
  theme_pub_big() +
  theme(legend.position = "top", strip.text = element_text(size = 18, face = "bold", color = "black"))

save_plot(fig2, "Figure_02_Yield_vs_Climate_Drivers_UPDATED", width = 24, height = 12)

# -----------------------------------------------------------------------------
# 6) FIGURE 3
# -----------------------------------------------------------------------------

heatmap_data <- yearloc %>%
  mutate(MatchedLocName = fct_reorder(MatchedLocName, MatchedLocLat, .fun = mean, na.rm = TRUE))

heatmap_long <- heatmap_data %>%
  select(Year, MatchedLocName, mean_Tmax, mean_HeatDays_Tmax_GT32,
         mean_SRAD, mean_VPD, mean_HDW_days, mean_YieldCIMMYT,
         mean_SimYieldMME_t_ha) %>%
  pivot_longer(cols = c(mean_Tmax, mean_HeatDays_Tmax_GT32,
                        mean_SRAD, mean_VPD, mean_HDW_days,
                        mean_YieldCIMMYT, mean_SimYieldMME_t_ha),
               names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = recode(
    Variable,
    mean_Tmax = "Tmax (°C)",
    mean_HeatDays_Tmax_GT32 = "Days Tmax > 32°C",
    mean_SRAD = "Solar radiation",
    mean_VPD = "VPD",
    mean_HDW_days = "HDW-like days",
    mean_YieldCIMMYT = "Observed yield",
    mean_SimYieldMME_t_ha = "Simulated yield"
  ))

fig3 <- ggplot(heatmap_long, aes(x = Year, y = MatchedLocName, fill = Value)) +
  geom_tile(color = "white", linewidth = 0.15) +
  facet_wrap(~ Variable, scales = "free", ncol = 2) +
  scale_x_continuous(breaks = x_breaks_2yr) +
  scale_fill_viridis_c(option = "C", na.value = "grey95") +
  labs(title = "Location-year patterns of CIMMYT yield and last-90-day climate drivers",
       x = "Year", y = "Matched CIMMYT location", fill = "Value") +
  theme_pub_big() +
  theme(legend.position = "right",
        legend.title = element_text(size = 21, face = "bold", color = "black"),
        legend.text = element_text(size = 19, color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
        strip.text = element_text(size = 20, face = "bold", color = "black"))

save_plot(fig3, "Figure_03_Location_Year_Heatmaps_UPDATED", width = 22, height = 18)

# -----------------------------------------------------------------------------
# 7) FIGURE 4 WITH EGYPT MAP
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

egypt_crop <- tryCatch(
  sf::st_read(egypt_crop_shp, quiet = TRUE) %>% sf::st_make_valid(),
  error = function(e) {
    message("Could not read Egypt cropland shapefile. Using Egypt country boundary instead.")
    rnaturalearth::ne_countries(country = "Egypt", returnclass = "sf")
  }
)

if (is.na(sf::st_crs(egypt_crop))) {
  sf::st_crs(egypt_crop) <- 4326
} else {
  egypt_crop <- sf::st_transform(egypt_crop, 4326)
}

egypt_boundary <- rnaturalearth::ne_countries(country = "Egypt", returnclass = "sf") %>%
  sf::st_transform(4326)

points_sf <- spatial_summary %>%
  sf::st_as_sf(coords = c("MatchedLocLong", "MatchedLocLat"), crs = 4326, remove = FALSE)

bbox_vals <- c(xmin = 24.5, xmax = 35.5, ymin = 21.5, ymax = 32.5)

map_theme <- theme_pub_big() +
  theme(legend.position = "right",
        legend.title = element_text(size = 22, face = "bold", color = "black"),
        legend.text = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 23, color = "black"),
        axis.text = element_text(size = 20, color = "black"),
        panel.grid.major = element_line(color = "grey88", linewidth = 0.35))

base_map_layers <- list(
  geom_sf(data = egypt_boundary, fill = "grey98", color = "grey35", linewidth = 0.65),
  geom_sf(data = egypt_crop, fill = "grey70", color = NA, alpha = 0.55)
)

p_spatial_heat <- ggplot() +
  base_map_layers +
  geom_sf(data = points_sf, aes(size = n_years, color = mean_heat_days), alpha = 0.95) +
  geom_text_repel(data = spatial_summary,
                  aes(x = MatchedLocLong, y = MatchedLocLat, label = MatchedLocName),
                  size = 6.0, fontface = "bold", color = "black",
                  max.overlaps = 50, min.segment.length = 0) +
  coord_sf(xlim = c(bbox_vals["xmin"], bbox_vals["xmax"]),
           ylim = c(bbox_vals["ymin"], bbox_vals["ymax"]), expand = FALSE) +
  scale_color_viridis_c(option = "A") +
  scale_size_continuous(range = c(4, 11)) +
  labs(title = "A. Heat-stress exposure", x = "Longitude", y = "Latitude",
       color = "Mean days\nTmax >32°C", size = "Years") +
  map_theme

p_spatial_srad <- ggplot() +
  base_map_layers +
  geom_sf(data = points_sf, aes(size = n_years, color = mean_SRAD), alpha = 0.95) +
  geom_text_repel(data = spatial_summary,
                  aes(x = MatchedLocLong, y = MatchedLocLat, label = MatchedLocName),
                  size = 6.0, fontface = "bold", color = "black",
                  max.overlaps = 50, min.segment.length = 0) +
  coord_sf(xlim = c(bbox_vals["xmin"], bbox_vals["xmax"]),
           ylim = c(bbox_vals["ymin"], bbox_vals["ymax"]), expand = FALSE) +
  scale_color_viridis_c(option = "D") +
  scale_size_continuous(range = c(4, 11)) +
  labs(title = "B. Solar radiation", x = "Longitude", y = "Latitude",
       color = "Mean SRAD", size = "Years") +
  map_theme

p_spatial_gap <- ggplot() +
  base_map_layers +
  geom_sf(data = points_sf, aes(size = n_years, color = mean_yield_gap), alpha = 0.95) +
  geom_text_repel(data = spatial_summary,
                  aes(x = MatchedLocLong, y = MatchedLocLat, label = MatchedLocName),
                  size = 6.0, fontface = "bold", color = "black",
                  max.overlaps = 50, min.segment.length = 0) +
  coord_sf(xlim = c(bbox_vals["xmin"], bbox_vals["xmax"]),
           ylim = c(bbox_vals["ymin"], bbox_vals["ymax"]), expand = FALSE) +
  scale_color_gradient2(low = "steelblue", mid = "grey92", high = "firebrick", midpoint = 0) +
  scale_size_continuous(range = c(4, 11)) +
  labs(title = "C. Yield gap",
       subtitle = "Yield gap = simulated MME − observed CIMMYT",
       x = "Longitude", y = "Latitude",
       color = "Yield gap\n(t/ha)", size = "Years") +
  map_theme

fig4 <- (p_spatial_heat | p_spatial_srad | p_spatial_gap) +
  plot_annotation(
    title = "Spatial summary of climate exposure and yield gap across CIMMYT matched locations",
    subtitle = "Grey areas show the Egypt cropland layer; points show CIMMYT matched locations.",
    theme = theme(plot.title = element_text(face = "bold", size = 32, color = "black"),
                  plot.subtitle = element_text(size = 23, color = "black"))
  )

save_plot(fig4, "Figure_04_Spatial_Climate_Yield_Summary_EgyptMap_UPDATED", width = 26, height = 10)

# -----------------------------------------------------------------------------
# 8) FIGURE 5
# -----------------------------------------------------------------------------

record_long <- record %>%
  select(Year, Loc_desc, MatchedLocName, YieldCIMMYT, Tmax_mean, Tmin_mean,
         HeatDays_Tmax_GT32, SRAD_mean, VPD_mean, HDW_days) %>%
  pivot_longer(cols = c(Tmax_mean, Tmin_mean, HeatDays_Tmax_GT32,
                        SRAD_mean, VPD_mean, HDW_days),
               names_to = "Driver", values_to = "DriverValue") %>%
  mutate(Driver = recode(
    Driver,
    Tmax_mean = "Tmax",
    Tmin_mean = "Tmin",
    HeatDays_Tmax_GT32 = "Days Tmax > 32°C",
    SRAD_mean = "Solar radiation",
    VPD_mean = "VPD",
    HDW_days = "HDW-like days"
  )) %>%
  filter(!is.na(YieldCIMMYT), !is.na(DriverValue))

record_stats <- record_long %>%
  group_by(Driver) %>%
  summarise(n = n(), model = list(lm(YieldCIMMYT ~ DriverValue)), .groups = "drop") %>%
  mutate(
    r2 = map_dbl(model, ~ summary(.x)$r.squared),
    p = map_dbl(model, ~ summary(.x)$coefficients[2, 4]),
    label = paste0("R²=", round(r2, 2), ", P=", ifelse(p < 0.001, "<0.001", signif(p, 2)))
  ) %>%
  select(Driver, n, r2, p, label)

write_csv(record_stats, file.path(output_dir, "RecordLevel_Yield_vs_Climate_Stats.csv"))

record_label_positions <- record_long %>%
  group_by(Driver) %>%
  summarise(x = min(DriverValue, na.rm = TRUE), y = max(YieldCIMMYT, na.rm = TRUE), .groups = "drop") %>%
  left_join(record_stats, by = "Driver")

fig5 <- ggplot(record_long, aes(x = DriverValue, y = YieldCIMMYT)) +
  geom_point(aes(color = Year), alpha = 0.70, size = 2.8, na.rm = TRUE) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 1.05, alpha = 0.13, na.rm = TRUE) +
  geom_text(data = record_label_positions,
            aes(x = x, y = y, label = label),
            hjust = 0, vjust = 1.1, size = 5.2,
            fontface = "bold", color = "black", inherit.aes = FALSE) +
  facet_wrap(~ Driver, scales = "free_x", ncol = 3) +
  scale_color_viridis_c(option = "C") +
  labs(title = "Record-level CIMMYT observed yield relationships with climate drivers",
       subtitle = "Each point represents a CIMMYT yield record matched to the nearest WTH location.",
       x = "Climate driver value", y = "Observed CIMMYT yield (t/ha)",
       color = "Year") +
  theme_pub_big() +
  theme(legend.position = "right",
        legend.title = element_text(size = 22, face = "bold", color = "black"),
        legend.text = element_text(size = 20, color = "black"))

save_plot(fig5, "Figure_05_RecordLevel_CIMMYT_Climate_Relationships_UPDATED", width = 22, height = 13)

# -----------------------------------------------------------------------------
# 9) FIGURE 6
# -----------------------------------------------------------------------------

p_compact_yield <- p_yield + labs(title = "A. Yield", y = "Yield (t/ha)", x = "Year") +
  theme(legend.position = "top")
p_compact_heat <- p_heat + labs(title = "B. Days >32°C", y = "Days", x = "Year") +
  theme(legend.position = "none")
p_compact_srad <- p_srad + labs(title = "C. Solar radiation", y = expression("MJ " * m^{-2} * " day"^{-1}), x = "Year") +
  theme(legend.position = "none")
p_compact_vpd <- p_vpd + labs(title = "D. VPD", y = "kPa", x = "Year") +
  theme(legend.position = "none")

fig_compact <- (p_compact_yield / p_compact_heat / p_compact_srad / p_compact_vpd) +
  plot_layout(heights = c(1.3, 1, 1, 1)) +
  plot_annotation(
    title = "Yield and key climate drivers during the last 90 days of the CIMMYT wheat season",
    subtitle = paste0("Climate metrics were extracted from DSSAT WTH files for matched CIMMYT locations; breakpoint = ", break_year),
    theme = theme(plot.title = element_text(face = "bold", size = 32, color = "black"),
                  plot.subtitle = element_text(size = 23, color = "black"))
  )

save_plot(fig_compact, "Figure_06_Compact_Yield_Heat_SRAD_VPD_UPDATED", width = 20, height = 18)

# -----------------------------------------------------------------------------
# 10) SAVE PROCESSED TABLES
# -----------------------------------------------------------------------------

write_csv(annual, file.path(output_dir, "Annual_Used_For_Figures.csv"))
write_csv(yearloc, file.path(output_dir, "YearLocation_Used_For_Figures.csv"))
write_csv(record, file.path(output_dir, "RecordLevel_Used_For_Figures.csv"))
write_csv(spatial_summary, file.path(output_dir, "Spatial_Summary_By_CIMMYT_Location.csv"))

message("Done.")
message("Updated figures saved in: ", output_dir)
message("Main updated files:")
message("Figure_01_Annual_Yield_Climate_Drivers_UPDATED.png/pdf")
message("Figure_04_Spatial_Climate_Yield_Summary_EgyptMap_UPDATED.png/pdf")
