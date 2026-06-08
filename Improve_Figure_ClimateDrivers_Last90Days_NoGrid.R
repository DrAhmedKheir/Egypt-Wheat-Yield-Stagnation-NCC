# =============================================================================
# Publication-quality Figure: CIMMYT wheat yield and climate drivers
# during the last 90 days of the season
# =============================================================================
# Requested improvements:
#   - Remove grid lines
#   - Maximize font sizes in black
#   - Observed yield = symbols only; never connect observed points
#   - Simulated yield = solid line
#   - Keep dashed vertical breakpoint line at 1998
#   - Improve Panel C: heat stress days with bars + dashed regression trend lines
#   - Cleaner layout and larger output size
#
# Based on your previous climate-metric workflow for the last 90 days
# before maturity/estimated maturity. 
# =============================================================================

# install.packages(c("tidyverse", "patchwork", "scales"))

library(tidyverse)
library(patchwork)
library(scales)

# -----------------------------------------------------------------------------
# 1) USER SETTINGS
# -----------------------------------------------------------------------------

input_dir <- "D:/Asseng_paper2024/Updated/Asseng_updated/40YRSSIMULATIONS/ForShawn/21072025/21072025/Update27032026/CIMMYTSimulations/CIMMYT_ClimateMetrics_From_WTH_LocCodes"

annual_file <- file.path(input_dir, "CIMMYT_Annual_ClimateMetrics_Last90Days.csv")

output_dir <- file.path(input_dir, "Publication_ClimateDrivers_NoGrid")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

break_year <- 1998

fig_width  <- 20
fig_height <- 18
fig_dpi    <- 350

base_font     <- 22
title_font    <- 32
subtitle_font <- 23
panel_font    <- 24
axis_font     <- 22
axis_text     <- 18
legend_font   <- 18

# -----------------------------------------------------------------------------
# 2) READ DATA
# -----------------------------------------------------------------------------

annual <- readr::read_csv(annual_file, show_col_types = FALSE) %>%
  mutate(Year = as.integer(Year)) %>%
  arrange(Year) %>%
  filter(!is.na(Year))

# -----------------------------------------------------------------------------
# 3) PREPARE VARIABLES
# -----------------------------------------------------------------------------

df <- annual %>%
  transmute(
    Year,

    obs_yield = annual_mean_YieldCIMMYT,
    obs_yield_sd = annual_sd_YieldCIMMYT_locations,

    sim_yield = annual_mean_SimYieldMME_t_ha,
    sim_yield_sd = annual_sd_SimYieldMME_locations,

    Tmax = annual_mean_Tmax,
    Tmax_sd = annual_sd_Tmax_locations,

    Tmin = annual_mean_Tmin,
    Tmin_sd = annual_sd_Tmin_locations,

    HeatDays = annual_mean_HeatDays_Tmax_GT32,
    HeatDays_sd = annual_sd_HeatDays_Tmax_GT32_locations,

    SRAD = annual_mean_SRAD,
    SRAD_sd = annual_sd_SRAD_locations,

    VPD = annual_mean_VPD,
    HDW = annual_mean_HDW_days
  ) %>%
  mutate(
    obs_yield_sd = ifelse(is.na(obs_yield_sd), 0, obs_yield_sd),
    sim_yield_sd = ifelse(is.na(sim_yield_sd), 0, sim_yield_sd),
    Tmax_sd = ifelse(is.na(Tmax_sd), 0, Tmax_sd),
    Tmin_sd = ifelse(is.na(Tmin_sd), 0, Tmin_sd),
    HeatDays_sd = ifelse(is.na(HeatDays_sd), 0, HeatDays_sd),
    SRAD_sd = ifelse(is.na(SRAD_sd), 0, SRAD_sd)
  )

write_csv(df, file.path(output_dir, "Figure_ClimateDrivers_Last90Days_Data.csv"))

# -----------------------------------------------------------------------------
# 4) FIXED-1998 SEGMENTED REGRESSION STATS
# -----------------------------------------------------------------------------

fit_segmented_fixed1998 <- function(data, y_col) {

  d <- data %>%
    filter(!is.na(.data[[y_col]])) %>%
    mutate(
      Period = case_when(
        Year <= break_year ~ "1980–1998",
        Year >  break_year ~ "1998–2019"
      )
    )

  d %>%
    group_by(Period) %>%
    group_modify(~{
      if (nrow(.x) < 3) {
        return(tibble(
          slope = NA_real_,
          r2 = NA_real_,
          p_value = NA_real_,
          n = nrow(.x)
        ))
      }

      tmp <- .x %>% mutate(y = .data[[y_col]])
      fit <- lm(y ~ Year, data = tmp)
      sm <- summary(fit)

      tibble(
        slope = coef(fit)[["Year"]],
        r2 = sm$r.squared,
        p_value = sm$coefficients["Year", "Pr(>|t|)"],
        n = nrow(tmp)
      )
    }) %>%
    ungroup() %>%
    mutate(Variable = y_col)
}

climate_reg_stats <- bind_rows(
  fit_segmented_fixed1998(df, "Tmax"),
  fit_segmented_fixed1998(df, "Tmin"),
  fit_segmented_fixed1998(df, "HeatDays"),
  fit_segmented_fixed1998(df, "SRAD"),
  fit_segmented_fixed1998(df, "VPD"),
  fit_segmented_fixed1998(df, "HDW")
)

write_csv(
  climate_reg_stats,
  file.path(output_dir, "ClimateDrivers_Fixed1998_Regression_Stats.csv")
)

# -----------------------------------------------------------------------------
# 5) PUBLICATION THEME
# -----------------------------------------------------------------------------

theme_pub <- function() {
  theme_classic(base_size = base_font) +
    theme(
      text = element_text(color = "black"),
      plot.title = element_text(face = "bold", size = panel_font, color = "black"),
      plot.subtitle = element_text(size = subtitle_font - 2, color = "black"),
      axis.title = element_text(size = axis_font, color = "black"),
      axis.text = element_text(size = axis_text, color = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
      axis.line = element_line(color = "black", linewidth = 0.75),
      axis.ticks = element_line(color = "black", linewidth = 0.70),
      axis.ticks.length = unit(0.20, "cm"),
      legend.position = "top",
      legend.title = element_text(size = legend_font, face = "bold", color = "black"),
      legend.text = element_text(size = legend_font, color = "black"),
      legend.key.size = unit(0.9, "cm"),
      plot.margin = margin(7, 8, 7, 8)
    )
}

x_breaks <- seq(
  floor(min(df$Year, na.rm = TRUE) / 2) * 2,
  ceiling(max(df$Year, na.rm = TRUE) / 2) * 2,
  by = 2
)

vline_1998 <- geom_vline(
  xintercept = break_year,
  linetype = "dashed",
  color = "black",
  linewidth = 0.8
)

# -----------------------------------------------------------------------------
# 6) PANEL A: YIELD
# -----------------------------------------------------------------------------

pA <- ggplot(df, aes(x = Year)) +
  geom_ribbon(
    aes(
      ymin = obs_yield - obs_yield_sd,
      ymax = obs_yield + obs_yield_sd,
      fill = "Observed CIMMYT ± SD"
    ),
    alpha = 0.16,
    na.rm = TRUE
  ) +
  geom_ribbon(
    aes(
      ymin = sim_yield - sim_yield_sd,
      ymax = sim_yield + sim_yield_sd,
      fill = "Simulated MME ± SD"
    ),
    alpha = 0.22,
    na.rm = TRUE
  ) +
  geom_line(
    aes(y = sim_yield, color = "Simulated MME"),
    linewidth = 1.45,
    na.rm = TRUE
  ) +
  geom_point(
    aes(y = obs_yield, color = "Observed CIMMYT"),
    size = 3.3,
    shape = 16,
    na.rm = TRUE
  ) +
  vline_1998 +
  scale_x_continuous(breaks = x_breaks) +
  scale_color_manual(
    name = "",
    values = c(
      "Observed CIMMYT" = "black",
      "Simulated MME" = "darkgreen"
    )
  ) +
  scale_fill_manual(
    name = "",
    values = c(
      "Observed CIMMYT ± SD" = "grey70",
      "Simulated MME ± SD" = "forestgreen"
    )
  ) +
  labs(
    title = "A. Annual observed and simulated wheat yield",
    x = NULL,
    y = "Grain yield (t/ha)"
  ) +
  theme_pub() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# -----------------------------------------------------------------------------
# 7) PANEL B: TMAX AND TMIN
# -----------------------------------------------------------------------------

temp_long <- df %>%
  select(Year, Tmax, Tmin, Tmax_sd, Tmin_sd) %>%
  pivot_longer(
    cols = c(Tmax, Tmin),
    names_to = "Variable",
    values_to = "Temperature"
  ) %>%
  mutate(
    SD = ifelse(Variable == "Tmax", Tmax_sd, Tmin_sd),
    Variable = recode(Variable, Tmax = "Tmax", Tmin = "Tmin")
  )

pB <- ggplot(temp_long, aes(x = Year, y = Temperature, color = Variable, fill = Variable)) +
  geom_ribbon(
    aes(ymin = Temperature - SD, ymax = Temperature + SD),
    alpha = 0.13,
    color = NA,
    na.rm = TRUE
  ) +
  geom_line(linewidth = 1.35, na.rm = TRUE) +
  vline_1998 +
  scale_x_continuous(breaks = x_breaks) +
  scale_color_manual(values = c("Tmax" = "firebrick", "Tmin" = "steelblue")) +
  scale_fill_manual(values = c("Tmax" = "firebrick", "Tmin" = "steelblue")) +
  labs(
    title = "B. Mean temperature during the last 90 days",
    x = NULL,
    y = "Temperature (°C)",
    color = "",
    fill = ""
  ) +
  theme_pub() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# -----------------------------------------------------------------------------
# 8) PANEL C: HEAT STRESS DAYS - IMPROVED
# -----------------------------------------------------------------------------

heat_reg <- climate_reg_stats %>%
  filter(Variable == "HeatDays") %>%
  mutate(
    label = paste0(
      Period,
      ": slope = ", round(slope, 2),
      " d yr⁻¹; R² = ", round(r2, 2),
      "; P = ", ifelse(p_value < 0.001, "<0.001", signif(p_value, 2))
    )
  )

heat_label <- paste(heat_reg$label, collapse = "\n")

pC <- ggplot(df, aes(x = Year, y = HeatDays)) +
  geom_col(
    fill = "#D9822B",
    color = "#9A5A17",
    width = 0.72,
    alpha = 0.88,
    na.rm = TRUE
  ) +
  geom_smooth(
    data = df %>% filter(Year <= break_year),
    method = "lm",
    se = FALSE,
    color = "black",
    linetype = "dashed",
    linewidth = 1.10,
    na.rm = TRUE
  ) +
  geom_smooth(
    data = df %>% filter(Year > break_year),
    method = "lm",
    se = FALSE,
    color = "black",
    linetype = "longdash",
    linewidth = 1.10,
    na.rm = TRUE
  ) +
  vline_1998 +
  annotate(
    "label",
    x = min(df$Year, na.rm = TRUE) + 1,
    y = max(df$HeatDays, na.rm = TRUE) * 0.96,
    label = heat_label,
    hjust = 0,
    vjust = 1,
    size = 5.0,
    color = "black",
    fill = "white",
    label.size = 0.25
  ) +
  scale_x_continuous(breaks = x_breaks) +
  labs(
    title = "C. Heat-stress days during the last 90 days",
    subtitle = "Bars show days with Tmax >32°C; dashed lines show fixed-1998 segmented regressions.",
    x = NULL,
    y = "Days with Tmax >32°C"
  ) +
  theme_pub() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# -----------------------------------------------------------------------------
# 9) PANEL D: SOLAR RADIATION
# -----------------------------------------------------------------------------

pD <- ggplot(df, aes(x = Year, y = SRAD)) +
  geom_ribbon(
    aes(ymin = SRAD - SRAD_sd, ymax = SRAD + SRAD_sd),
    fill = "forestgreen",
    alpha = 0.20,
    na.rm = TRUE
  ) +
  geom_line(color = "darkgreen", linewidth = 1.35, na.rm = TRUE) +
  geom_point(color = "darkgreen", size = 2.5, na.rm = TRUE) +
  geom_smooth(
    data = df %>% filter(Year <= break_year),
    method = "lm",
    se = FALSE,
    color = "darkgreen",
    linetype = "dashed",
    linewidth = 0.95,
    na.rm = TRUE
  ) +
  geom_smooth(
    data = df %>% filter(Year > break_year),
    method = "lm",
    se = FALSE,
    color = "darkgreen",
    linetype = "longdash",
    linewidth = 0.95,
    na.rm = TRUE
  ) +
  vline_1998 +
  scale_x_continuous(breaks = x_breaks) +
  labs(
    title = "D. Mean solar radiation during the last 90 days",
    x = NULL,
    y = expression("SRAD (MJ m"^{-2}~"day"^{-1}*")")
  ) +
  theme_pub() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# -----------------------------------------------------------------------------
# 10) PANEL E: VPD
# -----------------------------------------------------------------------------

pE <- ggplot(df, aes(x = Year, y = VPD)) +
  geom_line(color = "purple4", linewidth = 1.20, na.rm = TRUE) +
  geom_point(color = "purple4", size = 2.6, na.rm = TRUE) +
  geom_smooth(
    data = df %>% filter(Year <= break_year),
    method = "lm",
    se = FALSE,
    color = "purple4",
    linetype = "dashed",
    linewidth = 0.95,
    na.rm = TRUE
  ) +
  geom_smooth(
    data = df %>% filter(Year > break_year),
    method = "lm",
    se = FALSE,
    color = "purple4",
    linetype = "longdash",
    linewidth = 0.95,
    na.rm = TRUE
  ) +
  vline_1998 +
  scale_x_continuous(breaks = x_breaks) +
  labs(
    title = "E. Mean vapor pressure deficit during the last 90 days",
    x = "Year",
    y = "VPD (kPa)"
  ) +
  theme_pub()

# -----------------------------------------------------------------------------
# 11) PANEL F: HDW-LIKE DAYS
# -----------------------------------------------------------------------------

pF <- ggplot(df, aes(x = Year, y = HDW)) +
  geom_col(
    fill = "#9E3D3D",
    color = "#702525",
    width = 0.72,
    alpha = 0.86,
    na.rm = TRUE
  ) +
  geom_smooth(
    data = df %>% filter(Year <= break_year),
    method = "lm",
    se = FALSE,
    color = "black",
    linetype = "dashed",
    linewidth = 0.95,
    na.rm = TRUE
  ) +
  geom_smooth(
    data = df %>% filter(Year > break_year),
    method = "lm",
    se = FALSE,
    color = "black",
    linetype = "longdash",
    linewidth = 0.95,
    na.rm = TRUE
  ) +
  vline_1998 +
  scale_x_continuous(breaks = x_breaks) +
  labs(
    title = "F. Hot-dry-windy days during the last 90 days",
    x = "Year",
    y = "HDW-like days"
  ) +
  theme_pub()

# -----------------------------------------------------------------------------
# 12) COMBINE FIGURE
# -----------------------------------------------------------------------------

bottom <- pE | pF

fig <- pA / pB / pC / pD / bottom +
  plot_layout(heights = c(1.15, 1.05, 1.05, 1.05, 1.15)) +
  plot_annotation(
    title = "CIMMYT wheat yield and climate drivers during the last 90 days of the season",
    subtitle = "Dashed vertical lines indicate the fixed breakpoint year: 1998. Panel C is simplified to bars plus segmented regression lines.",
    theme = theme(
      plot.title = element_text(face = "bold", size = title_font, color = "black"),
      plot.subtitle = element_text(size = subtitle_font, color = "black"),
      plot.margin = margin(8, 8, 8, 8)
    )
  )

# -----------------------------------------------------------------------------
# 13) SAVE OUTPUTS
# -----------------------------------------------------------------------------

ggsave(
  filename = file.path(output_dir, "Figure_ClimateDrivers_Last90Days_Publication_NoGrid.png"),
  plot = fig,
  width = fig_width,
  height = fig_height,
  dpi = fig_dpi,
  limitsize = FALSE
)

ggsave(
  filename = file.path(output_dir, "Figure_ClimateDrivers_Last90Days_Publication_NoGrid.pdf"),
  plot = fig,
  width = fig_width,
  height = fig_height,
  limitsize = FALSE
)

ggsave(file.path(output_dir, "Panel_A_Yield_NoGrid.png"), pA, width = 18, height = 5.8, dpi = fig_dpi, limitsize = FALSE)
ggsave(file.path(output_dir, "Panel_B_Temperature_NoGrid.png"), pB, width = 18, height = 5.2, dpi = fig_dpi, limitsize = FALSE)
ggsave(file.path(output_dir, "Panel_C_HeatStress_Improved_NoGrid.png"), pC, width = 18, height = 5.2, dpi = fig_dpi, limitsize = FALSE)
ggsave(file.path(output_dir, "Panel_D_SRAD_NoGrid.png"), pD, width = 18, height = 5.2, dpi = fig_dpi, limitsize = FALSE)
ggsave(file.path(output_dir, "Panel_E_VPD_NoGrid.png"), pE, width = 9, height = 5.6, dpi = fig_dpi, limitsize = FALSE)
ggsave(file.path(output_dir, "Panel_F_HDW_NoGrid.png"), pF, width = 9, height = 5.6, dpi = fig_dpi, limitsize = FALSE)

message("Done.")
message("Output folder: ", output_dir)
message("Main figure: Figure_ClimateDrivers_Last90Days_Publication_NoGrid.png")
message("Regression stats: ClimateDrivers_Fixed1998_Regression_Stats.csv")
