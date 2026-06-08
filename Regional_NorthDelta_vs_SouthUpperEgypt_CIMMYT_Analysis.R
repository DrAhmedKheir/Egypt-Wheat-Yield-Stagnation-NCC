# =============================================================================
# Regional analysis: North/Delta vs South/Upper Egypt for CIMMYT wheat data
# =============================================================================
# Purpose:
#   Following Senthold's suggestion, this script separates CIMMYT matched locations
#   into agro-climatic regions and repeats the key analyses by region:
#   1) regional annual summaries
#   2) regional yield/climate figures
#   3) regional yield-gap figure
#   4) regional climate-yield regression figure
#   5) regional PCA figure
#   6) regional Taylor diagram
# =============================================================================

# install.packages(c(
#   "tidyverse", "patchwork", "ggrepel", "viridis", "plotrix",
#   "broom", "scales"
# ))

library(tidyverse)
library(patchwork)
library(ggrepel)
library(viridis)
library(plotrix)
library(broom)
library(scales)

# -----------------------------------------------------------------------------
# 1) USER SETTINGS
# -----------------------------------------------------------------------------

input_dir <- "D:/Asseng_paper2024/Updated/Asseng_updated/40YRSSIMULATIONS/ForShawn/21072025/21072025/Update27032026/CIMMYTSimulations/CIMMYT_ClimateMetrics_From_WTH_LocCodes"

output_dir <- file.path(input_dir, "Regional_NorthDelta_vs_SouthUpperEgypt")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

annual_file  <- file.path(input_dir, "CIMMYT_Annual_ClimateMetrics_Last90Days.csv")
yearloc_file <- file.path(input_dir, "CIMMYT_YearLocation_ClimateMetrics_Last90Days.csv")
record_file  <- file.path(input_dir, "CIMMYT_RecordLevel_ClimateMetrics_Last90Days.csv")

break_year <- 1998

if (!file.exists(yearloc_file)) {
  yearloc_file <- list.files(input_dir, pattern = "YearLocation.*ClimateMetrics.*\\.csv$", full.names = TRUE)[1]
}
if (!file.exists(record_file)) {
  record_file <- list.files(input_dir, pattern = "RecordLevel.*ClimateMetrics.*\\.csv$", full.names = TRUE)[1]
}
if (!file.exists(annual_file)) {
  annual_file <- list.files(input_dir, pattern = "Annual.*ClimateMetrics.*\\.csv$", full.names = TRUE)[1]
}

# -----------------------------------------------------------------------------
# 2) READ DATA
# -----------------------------------------------------------------------------

yearloc <- read_csv(yearloc_file, show_col_types = FALSE) %>%
  mutate(
    Year = as.integer(Year),
    MatchedLocName = str_squish(as.character(MatchedLocName)),
    MatchedWeatherCode = str_squish(as.character(MatchedWeatherCode))
  )

record <- read_csv(record_file, show_col_types = FALSE) %>%
  mutate(
    Year = as.integer(Year),
    MatchedLocName = str_squish(as.character(MatchedLocName)),
    MatchedWeatherCode = str_squish(as.character(MatchedWeatherCode))
  )

annual <- read_csv(annual_file, show_col_types = FALSE) %>%
  mutate(Year = as.integer(Year))

# -----------------------------------------------------------------------------
# 3) REGION CLASSIFICATION
# -----------------------------------------------------------------------------
# Adjust if Urs/Senthold prefer different grouping.
# Fallback rule uses latitude if spelling is not found.

region_lookup <- tribble(
  ~MatchedLocName_key, ~Region,
  "KFS",             "North_Delta",
  "Kafrelsheikh",    "North_Delta",
  "Kafr El Sheikh",  "North_Delta",
  "Sakha",           "North_Delta",
  "Gemiza",          "North_Delta",
  "Gemiza2",         "North_Delta",
  "Etaybaroud",      "North_Delta",
  "Etay Baroud",     "North_Delta",
  "Nobaria",         "North_Delta",
  "Sharkia",         "North_Delta",
  "Qaliobia",        "North_Delta",
  "Qalioybya",       "North_Delta",

  "Giza",            "Middle_Egypt",
  "Benisuef",        "Middle_Egypt",
  "Beni Suef",       "Middle_Egypt",
  "SIDS",            "Middle_Egypt",
  "Sids",            "Middle_Egypt",
  "Hammam",          "Middle_Egypt",

  "Assuit",          "South_UpperEgypt",
  "Assiut",          "South_UpperEgypt",
  "Mallawy",         "South_UpperEgypt",
  "Malawy",          "South_UpperEgypt",
  "Shandweel",       "South_UpperEgypt",
  "Newvalley",       "South_UpperEgypt",
  "New Valley",      "South_UpperEgypt",
  "Mataana",         "South_UpperEgypt",
  "Mataana2",        "South_UpperEgypt",
  "Komombo",         "South_UpperEgypt",
  "Komoshem",        "South_UpperEgypt"
)

add_region <- function(df) {
  df %>%
    mutate(MatchedLocName_clean = str_squish(as.character(MatchedLocName))) %>%
    left_join(region_lookup, by = c("MatchedLocName_clean" = "MatchedLocName_key")) %>%
    mutate(
      Region = case_when(
        !is.na(Region) ~ Region,
        !is.na(MatchedLocLat) & MatchedLocLat >= 30.0 ~ "North_Delta",
        !is.na(MatchedLocLat) & MatchedLocLat >= 28.5 & MatchedLocLat < 30.0 ~ "Middle_Egypt",
        !is.na(MatchedLocLat) & MatchedLocLat < 28.5 ~ "South_UpperEgypt",
        TRUE ~ "Unclassified"
      ),
      Region = factor(
        Region,
        levels = c("North_Delta", "Middle_Egypt", "South_UpperEgypt", "Unclassified")
      )
    )
}

yearloc_reg <- add_region(yearloc)
record_reg  <- add_region(record)

write_csv(yearloc_reg, file.path(output_dir, "CIMMYT_YearLocation_With_Region.csv"))
write_csv(record_reg,  file.path(output_dir, "CIMMYT_RecordLevel_With_Region.csv"))

region_check <- yearloc_reg %>%
  distinct(MatchedLocName, MatchedWeatherCode, MatchedLocLat, MatchedLocLong, Region) %>%
  arrange(Region, MatchedLocLat)

write_csv(region_check, file.path(output_dir, "Region_Assignment_Check.csv"))

# -----------------------------------------------------------------------------
# 4) REGIONAL ANNUAL SUMMARY
# -----------------------------------------------------------------------------

regional_annual <- yearloc_reg %>%
  filter(Region != "Unclassified") %>%
  group_by(Region, Year) %>%
  summarise(
    n_locations = n_distinct(MatchedLocName),
    n_records = n(),

    obs_mean = mean(mean_YieldCIMMYT, na.rm = TRUE),
    obs_sd_locations = sd(mean_YieldCIMMYT, na.rm = TRUE),

    sim_mean = mean(mean_SimYieldMME_t_ha, na.rm = TRUE),
    sim_sd_locations = sd(mean_SimYieldMME_t_ha, na.rm = TRUE),

    yield_gap = sim_mean - obs_mean,

    Tmax_mean = mean(mean_Tmax, na.rm = TRUE),
    Tmax_sd = sd(mean_Tmax, na.rm = TRUE),

    Tmin_mean = mean(mean_Tmin, na.rm = TRUE),
    Tmin_sd = sd(mean_Tmin, na.rm = TRUE),

    HeatDays_mean = mean(mean_HeatDays_Tmax_GT32, na.rm = TRUE),
    HeatDays_sd = sd(mean_HeatDays_Tmax_GT32, na.rm = TRUE),

    SRAD_mean = mean(mean_SRAD, na.rm = TRUE),
    SRAD_sd = sd(mean_SRAD, na.rm = TRUE),

    VPD_mean = mean(mean_VPD, na.rm = TRUE),
    VPD_sd = sd(mean_VPD, na.rm = TRUE),

    HDW_mean = mean(mean_HDW_days, na.rm = TRUE),
    HDW_sd = sd(mean_HDW_days, na.rm = TRUE),

    .groups = "drop"
  ) %>%
  mutate(
    across(
      c(obs_sd_locations, sim_sd_locations, Tmax_sd, Tmin_sd, HeatDays_sd, SRAD_sd, VPD_sd, HDW_sd),
      ~ ifelse(is.na(.x), 0, .x)
    )
  ) %>%
  arrange(Region, Year)

write_csv(regional_annual, file.path(output_dir, "Regional_Annual_Summary.csv"))

# -----------------------------------------------------------------------------
# 5) TREND / BREAKPOINT REGRESSION STATS BY REGION
# -----------------------------------------------------------------------------

fit_region_trends <- function(df, y_col) {
  df %>%
    filter(is.finite(.data[[y_col]])) %>%
    mutate(
      Period = case_when(
        Year <= break_year ~ paste0("Before_", break_year),
        Year > break_year ~ paste0("After_", break_year)
      )
    ) %>%
    group_by(Region, Period) %>%
    group_modify(~{
      if (nrow(.x) < 4) {
        return(tibble(
          slope = NA_real_, intercept = NA_real_, r2 = NA_real_,
          p_value = NA_real_, n = nrow(.x)
        ))
      }
      fit <- lm(.data[[y_col]] ~ Year, data = .x)
      tibble(
        slope = coef(fit)[["Year"]],
        intercept = coef(fit)[["(Intercept)"]],
        r2 = summary(fit)$r.squared,
        p_value = summary(fit)$coefficients["Year", "Pr(>|t|)"],
        n = nrow(.x)
      )
    }) %>%
    ungroup() %>%
    mutate(Variable = y_col)
}

trend_stats <- bind_rows(
  fit_region_trends(regional_annual, "obs_mean"),
  fit_region_trends(regional_annual, "sim_mean"),
  fit_region_trends(regional_annual, "Tmax_mean"),
  fit_region_trends(regional_annual, "HeatDays_mean"),
  fit_region_trends(regional_annual, "SRAD_mean"),
  fit_region_trends(regional_annual, "yield_gap")
)

write_csv(trend_stats, file.path(output_dir, "Regional_Trend_Stats_BeforeAfter1998.csv"))

# -----------------------------------------------------------------------------
# 6) THEMES AND SAVE HELPER
# -----------------------------------------------------------------------------

theme_big <- function(base_size = 15) {
  theme_bw(base_size = base_size) +
    theme(
      text = element_text(color = "black"),
      plot.title = element_text(face = "bold", size = 22, color = "black"),
      plot.subtitle = element_text(size = 16, color = "black"),
      axis.title = element_text(size = 17, color = "black"),
      axis.text = element_text(size = 14, color = "black"),
      legend.title = element_text(size = 14, face = "bold", color = "black"),
      legend.text = element_text(size = 13, color = "black"),
      strip.text = element_text(size = 15, face = "bold", color = "black"),
      legend.position = "top",
      panel.grid.major = element_line(color = "grey86", linewidth = 0.35),
      panel.grid.minor = element_line(color = "grey93", linewidth = 0.20)
    )
}

save_plot <- function(p, name, width = 15, height = 10, dpi = 350) {
  ggsave(file.path(output_dir, paste0(name, ".png")), p,
         width = width, height = height, dpi = dpi, limitsize = FALSE)
  ggsave(file.path(output_dir, paste0(name, ".pdf")), p,
         width = width, height = height, limitsize = FALSE)
}

x_breaks <- seq(
  floor(min(regional_annual$Year, na.rm = TRUE) / 2) * 2,
  ceiling(max(regional_annual$Year, na.rm = TRUE) / 2) * 2,
  by = 2
)

# -----------------------------------------------------------------------------
# 7) FIGURE R01: REGIONAL ANNUAL YIELD AND CLIMATE DRIVERS
# -----------------------------------------------------------------------------

p_yield_region <- ggplot(regional_annual, aes(x = Year)) +
  geom_ribbon(aes(ymin = obs_mean - obs_sd_locations, ymax = obs_mean + obs_sd_locations,
                  fill = "Observed ± SD locations"), alpha = 0.15, na.rm = TRUE) +
  geom_ribbon(aes(ymin = sim_mean - sim_sd_locations, ymax = sim_mean + sim_sd_locations,
                  fill = "Simulated ± SD locations"), alpha = 0.20, na.rm = TRUE) +
  geom_point(aes(y = obs_mean, color = "Observed CIMMYT"), size = 2.8, na.rm = TRUE) +
  geom_line(aes(y = sim_mean, color = "Simulated MME"), linewidth = 1.15, na.rm = TRUE) +
  geom_vline(xintercept = break_year, linetype = "dashed", color = "grey35") +
  geom_smooth(data = regional_annual %>% filter(Year <= break_year), aes(y = obs_mean, color = "Observed trend <=1998"), method = "lm", se = FALSE, linewidth = 0.8, na.rm = TRUE) +
  geom_smooth(data = regional_annual %>% filter(Year > break_year), aes(y = obs_mean, color = "Observed trend >1998"), method = "lm", se = FALSE, linewidth = 0.8, na.rm = TRUE) +
  geom_smooth(data = regional_annual %>% filter(Year <= break_year), aes(y = sim_mean, color = "Simulated trend <=1998"), method = "lm", se = FALSE, linewidth = 0.8, linetype = "dashed", na.rm = TRUE) +
  geom_smooth(data = regional_annual %>% filter(Year > break_year), aes(y = sim_mean, color = "Simulated trend >1998"), method = "lm", se = FALSE, linewidth = 0.8, linetype = "dashed", na.rm = TRUE) +
  facet_wrap(~ Region, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = x_breaks) +
  scale_color_manual(values = c(
    "Observed CIMMYT" = "black",
    "Simulated MME" = "darkgreen",
    "Observed trend <=1998" = "firebrick",
    "Observed trend >1998" = "orange3",
    "Simulated trend <=1998" = "blue",
    "Simulated trend >1998" = "darkcyan"
  )) +
  scale_fill_manual(values = c(
    "Observed ± SD locations" = "grey55",
    "Simulated ± SD locations" = "forestgreen"
  )) +
  labs(
    title = "Regional annual observed and simulated CIMMYT wheat yield",
    subtitle = "Regions separate northern Delta, middle Egypt, and hotter Upper Egypt environments; dashed vertical line = 1998 breakpoint.",
    x = "Year", y = "Grain yield (t/ha)", color = "", fill = ""
  ) +
  theme_big() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_climate_region <- regional_annual %>%
  select(Region, Year, Tmax_mean, Tmin_mean, HeatDays_mean, SRAD_mean, VPD_mean, HDW_mean) %>%
  pivot_longer(cols = c(Tmax_mean, Tmin_mean, HeatDays_mean, SRAD_mean, VPD_mean, HDW_mean),
               names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = recode(Variable,
    Tmax_mean = "Tmax (°C)",
    Tmin_mean = "Tmin (°C)",
    HeatDays_mean = "Days Tmax >32°C",
    SRAD_mean = "SRAD (MJ m-2 day-1)",
    VPD_mean = "VPD (kPa)",
    HDW_mean = "HDW-like days"
  )) %>%
  ggplot(aes(x = Year, y = Value, color = Region)) +
  geom_line(linewidth = 1.0, na.rm = TRUE) +
  geom_point(size = 2.1, na.rm = TRUE) +
  geom_vline(xintercept = break_year, linetype = "dashed", color = "grey35") +
  facet_wrap(~ Variable, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = x_breaks) +
  scale_color_manual(values = c(
    "North_Delta" = "darkgreen",
    "Middle_Egypt" = "orange3",
    "South_UpperEgypt" = "firebrick",
    "Unclassified" = "grey40"
  )) +
  labs(title = "Regional climate drivers during the last 90 days of the wheat season",
       x = "Year", y = "Climate driver value", color = "Region") +
  theme_big() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fig_r01 <- p_yield_region / p_climate_region + plot_layout(heights = c(1.1, 1.6))
save_plot(fig_r01, "Figure_R01_Regional_Annual_Yield_Climate", width = 18, height = 19)

# -----------------------------------------------------------------------------
# 8) FIGURE R02: REGIONAL YIELD GAP
# -----------------------------------------------------------------------------

p_gap_region <- regional_annual %>%
  mutate(GapClass = case_when(
    yield_gap > 0 ~ "Simulated > Observed",
    yield_gap < 0 ~ "Simulated < Observed",
    TRUE ~ "Equal"
  )) %>%
  ggplot(aes(x = Year, y = yield_gap, fill = GapClass)) +
  geom_col(width = 0.75, alpha = 0.82, na.rm = TRUE) +
  geom_line(aes(group = Region), color = "black", linewidth = 0.7, na.rm = TRUE) +
  geom_point(color = "black", size = 1.8, na.rm = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey35") +
  geom_vline(xintercept = break_year, linetype = "dashed", color = "grey35") +
  facet_wrap(~ Region, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = x_breaks) +
  scale_fill_manual(values = c(
    "Simulated > Observed" = "firebrick",
    "Simulated < Observed" = "steelblue",
    "Equal" = "grey60"
  )) +
  labs(title = "Regional annual yield gap between simulated MME and observed CIMMYT yield",
       subtitle = "Yield gap = simulated MME yield − observed CIMMYT yield.",
       x = "Year", y = "Yield gap (t/ha)", fill = "") +
  theme_big() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot(p_gap_region, "Figure_R02_Regional_YieldGap", width = 16, height = 12)

# -----------------------------------------------------------------------------
# 9) FIGURE R03: REGIONAL CLIMATE-YIELD RELATIONSHIPS
# -----------------------------------------------------------------------------

driver_long <- regional_annual %>%
  select(Region, Year, obs_mean, sim_mean, Tmax_mean, Tmin_mean, HeatDays_mean, SRAD_mean, VPD_mean, HDW_mean) %>%
  pivot_longer(cols = c(Tmax_mean, Tmin_mean, HeatDays_mean, SRAD_mean, VPD_mean, HDW_mean),
               names_to = "Driver", values_to = "DriverValue") %>%
  pivot_longer(cols = c(obs_mean, sim_mean), names_to = "YieldType", values_to = "Yield") %>%
  mutate(
    YieldType = recode(YieldType, obs_mean = "Observed CIMMYT", sim_mean = "Simulated MME"),
    Driver = recode(Driver,
      Tmax_mean = "Tmax", Tmin_mean = "Tmin", HeatDays_mean = "Days Tmax >32°C",
      SRAD_mean = "Solar radiation", VPD_mean = "VPD", HDW_mean = "HDW-like days"
    )
  ) %>%
  filter(is.finite(DriverValue), is.finite(Yield))

relationship_stats <- driver_long %>%
  group_by(Region, YieldType, Driver) %>%
  group_modify(~{
    if (nrow(.x) < 4) return(tibble(slope = NA_real_, r2 = NA_real_, p_value = NA_real_, n = nrow(.x)))
    fit <- lm(Yield ~ DriverValue, data = .x)
    tibble(
      slope = coef(fit)[["DriverValue"]],
      r2 = summary(fit)$r.squared,
      p_value = summary(fit)$coefficients["DriverValue", "Pr(>|t|)"],
      n = nrow(.x)
    )
  }) %>%
  ungroup() %>%
  mutate(label = paste0("R²=", round(r2, 2), ", P=", ifelse(p_value < 0.001, "<0.001", signif(p_value, 2))))

write_csv(relationship_stats, file.path(output_dir, "Regional_ClimateYield_Relationship_Stats.csv"))

label_df <- driver_long %>%
  group_by(Region, YieldType, Driver) %>%
  summarise(x = min(DriverValue, na.rm = TRUE), y = max(Yield, na.rm = TRUE), .groups = "drop") %>%
  left_join(relationship_stats, by = c("Region", "YieldType", "Driver"))

p_relationships <- ggplot(driver_long, aes(x = DriverValue, y = Yield, color = YieldType, shape = YieldType)) +
  geom_point(size = 2.5, alpha = 0.85, na.rm = TRUE) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.9, alpha = 0.15, na.rm = TRUE) +
  geom_text(data = label_df, aes(x = x, y = y, label = label, color = YieldType),
            inherit.aes = FALSE, hjust = 0, vjust = 1.1, size = 3.3, fontface = "bold") +
  facet_grid(Region ~ Driver, scales = "free_x") +
  scale_color_manual(values = c("Observed CIMMYT" = "black", "Simulated MME" = "darkgreen")) +
  scale_shape_manual(values = c("Observed CIMMYT" = 16, "Simulated MME" = 17)) +
  labs(title = "Regional relationships between wheat yield and last-90-day climate drivers",
       subtitle = "Points are annual regional means; lines are linear regressions.",
       x = "Climate driver value", y = "Grain yield (t/ha)", color = "", shape = "") +
  theme_big(base_size = 13) +
  theme(strip.text = element_text(size = 12, face = "bold"))

save_plot(p_relationships, "Figure_R03_Regional_ClimateYield_Relationships", width = 24, height = 12)

# -----------------------------------------------------------------------------
# 10) FIGURE R04: REGIONAL PCA
# -----------------------------------------------------------------------------

pca_data <- regional_annual %>%
  select(Region, Year, obs_mean, sim_mean, yield_gap, Tmax_mean, Tmin_mean, HeatDays_mean, SRAD_mean, VPD_mean, HDW_mean) %>%
  drop_na()

pca_vars <- c("obs_mean", "sim_mean", "yield_gap", "Tmax_mean", "Tmin_mean", "HeatDays_mean", "SRAD_mean", "VPD_mean", "HDW_mean")
pca_x <- pca_data %>% select(all_of(pca_vars))
sds <- map_dbl(pca_x, sd, na.rm = TRUE)
pca_vars_keep <- names(sds)[is.finite(sds) & sds > 0]

pca_fit <- prcomp(pca_data %>% select(all_of(pca_vars_keep)), center = TRUE, scale. = TRUE)

pca_scores <- as_tibble(pca_fit$x) %>% bind_cols(pca_data %>% select(Region, Year), .)
pca_loadings <- as_tibble(pca_fit$rotation, rownames = "Variable")
pca_variance <- tibble(
  PC = paste0("PC", seq_along(pca_fit$sdev)),
  Eigenvalue = pca_fit$sdev^2,
  VariancePercent = 100 * (pca_fit$sdev^2 / sum(pca_fit$sdev^2)),
  CumulativeVariancePercent = cumsum(100 * (pca_fit$sdev^2 / sum(pca_fit$sdev^2)))
)

write_csv(pca_scores, file.path(output_dir, "Regional_PCA_Scores.csv"))
write_csv(pca_loadings, file.path(output_dir, "Regional_PCA_Loadings.csv"))
write_csv(pca_variance, file.path(output_dir, "Regional_PCA_Variance.csv"))

var_percent <- round(pca_variance$VariancePercent, 1)
load_plot <- pca_loadings %>% mutate(PC1_arrow = PC1 * 4, PC2_arrow = PC2 * 4)

p_pca <- ggplot(pca_scores, aes(PC1, PC2, color = Region, shape = Region, label = Year)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(size = 3.6, alpha = 0.90) +
  geom_text_repel(size = 3.0, max.overlaps = Inf, show.legend = FALSE) +
  geom_segment(data = load_plot, aes(x = 0, y = 0, xend = PC1_arrow, yend = PC2_arrow),
               inherit.aes = FALSE, color = "firebrick", linewidth = 0.8,
               arrow = arrow(length = unit(0.25, "cm"))) +
  geom_text_repel(data = load_plot, aes(x = PC1_arrow, y = PC2_arrow, label = Variable),
                  inherit.aes = FALSE, color = "firebrick", fontface = "bold", size = 4.0, max.overlaps = Inf) +
  scale_color_manual(values = c("North_Delta" = "darkgreen", "Middle_Egypt" = "orange3", "South_UpperEgypt" = "firebrick", "Unclassified" = "grey40")) +
  labs(title = "Regional PCA of wheat yield and last-90-day climate drivers",
       subtitle = "PCA separates regional climate-yield environments and highlights dominant climatic gradients.",
       x = paste0("PC1 (", var_percent[1], "%)"), y = paste0("PC2 (", var_percent[2], "%)"),
       color = "Region", shape = "Region") +
  theme_big()

p_scree <- ggplot(pca_variance, aes(x = factor(PC, levels = PC), y = VariancePercent)) +
  geom_col(fill = "steelblue", alpha = 0.85) +
  geom_line(aes(y = CumulativeVariancePercent, group = 1), color = "firebrick", linewidth = 1.0) +
  geom_point(aes(y = CumulativeVariancePercent), color = "firebrick", size = 3.0) +
  labs(title = "PCA explained variance", x = "Principal component", y = "Explained variance (%)") +
  theme_big() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fig_pca <- p_pca | p_scree
save_plot(fig_pca, "Figure_R04_Regional_PCA", width = 18, height = 8.5)

# -----------------------------------------------------------------------------
# 11) FIGURE R05: REGIONAL TAYLOR DIAGRAM
# -----------------------------------------------------------------------------

calc_taylor_stats <- function(obs, sim, region_name) {
  ok <- is.finite(obs) & is.finite(sim)
  obs <- obs[ok]
  sim <- sim[ok]
  tibble(
    Region = as.character(region_name),
    N = length(obs),
    SD_Obs = sd(obs),
    SD_Sim = sd(sim),
    Correlation = cor(obs, sim),
    RMSE = sqrt(mean((sim - obs)^2)),
    Bias = mean(sim - obs),
    MAE = mean(abs(sim - obs)),
    R2 = Correlation^2
  )
}

taylor_stats <- regional_annual %>%
  group_by(Region) %>%
  group_modify(~ calc_taylor_stats(.x$obs_mean, .x$sim_mean, unique(.x$Region))) %>%
  ungroup()

write_csv(taylor_stats, file.path(output_dir, "Regional_Taylor_Stats.csv"))

taylor_df <- regional_annual %>%
  select(Region, Year, obs = obs_mean, sim = sim_mean) %>%
  filter(is.finite(obs), is.finite(sim), Region != "Unclassified")

regions <- levels(droplevels(taylor_df$Region))
regions <- regions[regions %in% unique(taylor_df$Region)]

cols <- c("North_Delta" = "darkgreen", "Middle_Egypt" = "orange3", "South_UpperEgypt" = "firebrick", "Unclassified" = "grey40")
pchs <- c("North_Delta" = 16, "Middle_Egypt" = 15, "South_UpperEgypt" = 17, "Unclassified" = 4)

png(file.path(output_dir, "Figure_R05_Regional_Taylor.png"), width = 3200, height = 2600, res = 300)
par(mar = c(5, 5, 5, 10), xpd = TRUE)
first <- TRUE
for (reg in regions) {
  d <- taylor_df %>% filter(Region == reg)
  if (nrow(d) >= 4) {
    if (first) {
      plotrix::taylor.diagram(ref = d$obs, model = d$sim,
                              main = "Taylor diagram: regional observed CIMMYT vs simulated MME yield",
                              col = cols[as.character(reg)], pch = pchs[as.character(reg)], pcex = 1.8,
                              normalize = FALSE, sd.arcs = TRUE, ref.sd = TRUE,
                              grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99),
                              pos.cor = TRUE)
      first <- FALSE
    } else {
      plotrix::taylor.diagram(ref = d$obs, model = d$sim, add = TRUE,
                              col = cols[as.character(reg)], pch = pchs[as.character(reg)],
                              pcex = 1.8, normalize = FALSE, pos.cor = TRUE)
    }
  }
}
legend("topright", inset = c(-0.28, 0.02), legend = regions,
       col = cols[as.character(regions)], pch = pchs[as.character(regions)],
       pt.cex = 1.8, bty = "n", cex = 1.1)
dev.off()

pdf(file.path(output_dir, "Figure_R05_Regional_Taylor.pdf"), width = 10.5, height = 8.5)
par(mar = c(5, 5, 5, 10), xpd = TRUE)
first <- TRUE
for (reg in regions) {
  d <- taylor_df %>% filter(Region == reg)
  if (nrow(d) >= 4) {
    if (first) {
      plotrix::taylor.diagram(ref = d$obs, model = d$sim,
                              main = "Taylor diagram: regional observed CIMMYT vs simulated MME yield",
                              col = cols[as.character(reg)], pch = pchs[as.character(reg)], pcex = 1.8,
                              normalize = FALSE, sd.arcs = TRUE, ref.sd = TRUE,
                              grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99),
                              pos.cor = TRUE)
      first <- FALSE
    } else {
      plotrix::taylor.diagram(ref = d$obs, model = d$sim, add = TRUE,
                              col = cols[as.character(reg)], pch = pchs[as.character(reg)],
                              pcex = 1.8, normalize = FALSE, pos.cor = TRUE)
    }
  }
}
legend("topright", inset = c(-0.28, 0.02), legend = regions,
       col = cols[as.character(regions)], pch = pchs[as.character(regions)],
       pt.cex = 1.8, bty = "n", cex = 1.1)
dev.off()

# -----------------------------------------------------------------------------
# 12) NORTH VS SOUTH SUMMARY TABLE
# -----------------------------------------------------------------------------

north_south_summary <- regional_annual %>%
  filter(Region %in% c("North_Delta", "South_UpperEgypt")) %>%
  group_by(Region) %>%
  summarise(
    n_years = n_distinct(Year),
    mean_obs_yield = mean(obs_mean, na.rm = TRUE),
    mean_sim_yield = mean(sim_mean, na.rm = TRUE),
    mean_yield_gap = mean(yield_gap, na.rm = TRUE),
    mean_Tmax = mean(Tmax_mean, na.rm = TRUE),
    mean_Tmin = mean(Tmin_mean, na.rm = TRUE),
    mean_heat_days = mean(HeatDays_mean, na.rm = TRUE),
    mean_SRAD = mean(SRAD_mean, na.rm = TRUE),
    mean_VPD = mean(VPD_mean, na.rm = TRUE),
    mean_HDW_days = mean(HDW_mean, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(north_south_summary, file.path(output_dir, "NorthDelta_vs_SouthUpperEgypt_Summary.csv"))

message("Done.")
message("Regional analysis outputs saved in: ", output_dir)
message("Key files:")
message(" - Region_Assignment_Check.csv")
message(" - Regional_Annual_Summary.csv")
message(" - Figure_R01_Regional_Annual_Yield_Climate.png/pdf")
message(" - Figure_R02_Regional_YieldGap.png/pdf")
message(" - Figure_R03_Regional_ClimateYield_Relationships.png/pdf")
message(" - Figure_R04_Regional_PCA.png/pdf")
message(" - Figure_R05_Regional_Taylor.png/pdf")
