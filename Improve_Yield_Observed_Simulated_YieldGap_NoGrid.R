# =============================================================================
# Publication-quality CIMMYT observed vs simulated yield + annual yield gap
# =============================================================================
# Improvements requested:
#   - No grid lines
#   - Large, readable black fonts
#   - Observations = symbols only, never connected by lines
#   - Simulations = solid lines
#   - Regressions = dashed lines
#   - Cleaner legends
#   - Larger figure size
#   - Regression statistics retained as a clear separate table panel
# =============================================================================

# install.packages(c(
#   "readxl", "dplyr", "ggplot2", "stringr",
#   "patchwork", "gridExtra", "grid", "scales"
# ))

library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(patchwork)
library(gridExtra)
library(grid)
library(scales)

# -----------------------------------------------------------------------------
# 1) USER SETTINGS
# -----------------------------------------------------------------------------

base_dir <- "D:/Asseng_paper2024/Updated/Asseng_updated/40YRSSIMULATIONS/ForShawn/21072025/21072025/Update27032026/CIMMYTSimulations"

observed_file  <- file.path(base_dir, "CIMMYTYieldDetails.xls")
simulated_file <- file.path(base_dir, "CIMMYTSimulatedMME.xlsx")

output_dir <- file.path(base_dir, "Yield_Observed_vs_Simulated_Figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

convert_sim_kg_ha_to_t_ha <- TRUE
convert_stdevmodels_kg_ha_to_t_ha <- TRUE

period1_start <- 1980
period1_end   <- 1998
period2_start <- 1998
period2_end   <- 2019

fig_width  <- 24
fig_height <- 18
fig_dpi    <- 350

alpha_sd_locations <- 0.18
alpha_sd_models    <- 0.24

base_font     <- 24
title_font    <- 34
subtitle_font <- 25
panel_font    <- 27
axis_font     <- 25
axis_text     <- 22
legend_font   <- 21
table_font    <- 21

# -----------------------------------------------------------------------------
# 2) READ DATA
# -----------------------------------------------------------------------------

obs_raw <- read_excel(observed_file, sheet = 1)
sim_raw <- read_excel(simulated_file, sheet = 1)

if (!"StdevModels" %in% names(sim_raw)) {
  warning("Column StdevModels not found. Model-SD ribbon will be omitted.")
  sim_raw$StdevModels <- NA_real_
}

obs <- obs_raw %>%
  mutate(
    Year = as.integer(Year),
    Yield_obs = as.numeric(YieldCIMMYT)
  ) %>%
  filter(!is.na(Year), !is.na(Yield_obs))

sim <- sim_raw %>%
  mutate(
    Year = as.integer(Year),
    Location = str_squish(as.character(Location)),
    SimYieldMME = as.numeric(SimYieldMME),
    StdevModels = as.numeric(StdevModels),
    Yield_sim = if (convert_sim_kg_ha_to_t_ha) SimYieldMME / 1000 else SimYieldMME,
    SD_model_each = if (convert_stdevmodels_kg_ha_to_t_ha) StdevModels / 1000 else StdevModels
  ) %>%
  filter(!is.na(Year), !is.na(Yield_sim)) %>%
  mutate(
    SD_model_each = ifelse(SD_model_each < 0, NA_real_, SD_model_each),
    SD_model_each = ifelse(SD_model_each > 20, NA_real_, SD_model_each)
  )

# -----------------------------------------------------------------------------
# 3) ANNUAL SUMMARIES
# -----------------------------------------------------------------------------

obs_year <- obs %>%
  group_by(Year) %>%
  summarise(
    obs_mean = mean(Yield_obs, na.rm = TRUE),
    obs_sd = ifelse(n() > 1, sd(Yield_obs, na.rm = TRUE), NA_real_),
    obs_n = n(),
    .groups = "drop"
  )

sim_year <- sim %>%
  group_by(Year) %>%
  summarise(
    sim_mean = mean(Yield_sim, na.rm = TRUE),
    sim_sd_locations = ifelse(n() > 1, sd(Yield_sim, na.rm = TRUE), NA_real_),
    sim_sd_models = ifelse(all(is.na(SD_model_each)), NA_real_, mean(SD_model_each, na.rm = TRUE)),
    sim_n_records = n(),
    sim_n_locations = n_distinct(Location),
    .groups = "drop"
  ) %>%
  mutate(
    sim_loc_lower = sim_mean - sim_sd_locations,
    sim_loc_upper = sim_mean + sim_sd_locations,
    sim_mod_lower = sim_mean - sim_sd_models,
    sim_mod_upper = sim_mean + sim_sd_models
  )

plot_df <- full_join(obs_year, sim_year, by = "Year") %>%
  arrange(Year) %>%
  mutate(
    yield_gap = sim_mean - obs_mean,
    gap_type = case_when(
      is.na(yield_gap) ~ "Missing observed data",
      yield_gap >= 0 ~ "Simulated > observed",
      TRUE ~ "Simulated < observed"
    )
  )

write.csv(
  plot_df,
  file.path(output_dir, "Yield_AnnualMean_Observed_Simulated_With_YieldGap_Publication.csv"),
  row.names = FALSE
)

# -----------------------------------------------------------------------------
# 4) REGRESSION FUNCTIONS
# -----------------------------------------------------------------------------

make_regression_segment <- function(df, y_col, series_name, period_name, start_year, end_year) {

  tmp <- df %>%
    filter(Year >= start_year, Year <= end_year, !is.na(.data[[y_col]])) %>%
    select(Year, y = all_of(y_col))

  if (nrow(tmp) < 2) {
    return(list(
      pred = tibble(),
      stats = tibble(
        Series = series_name,
        Period = period_name,
        StartYear = start_year,
        EndYear = end_year,
        Slope_t_ha_per_year = NA_real_,
        Intercept = NA_real_,
        R2 = NA_real_,
        P_value = NA_real_,
        N = nrow(tmp)
      )
    ))
  }

  fit <- lm(y ~ Year, data = tmp)
  sm <- summary(fit)

  pred <- tibble(Year = seq(start_year, end_year, by = 1)) %>%
    mutate(
      y = predict(fit, newdata = .),
      Series = series_name,
      Period = period_name
    )

  stats <- tibble(
    Series = series_name,
    Period = period_name,
    StartYear = start_year,
    EndYear = end_year,
    Slope_t_ha_per_year = coef(fit)[["Year"]],
    Intercept = coef(fit)[["(Intercept)"]],
    R2 = sm$r.squared,
    P_value = coef(sm)[2, 4],
    N = nrow(tmp)
  )

  list(pred = pred, stats = stats)
}

whole_start <- min(plot_df$Year, na.rm = TRUE)
whole_end   <- max(plot_df$Year, na.rm = TRUE)

reg_specs <- tibble::tribble(
  ~period_name,   ~start_year,   ~end_year,
  "1980–1998",    period1_start, period1_end,
  "1998–2019",    period2_start, period2_end,
  "Whole period", whole_start,   whole_end
)

reg_results <- list()
k <- 1

for (i in seq_len(nrow(reg_specs))) {
  sp <- reg_specs[i, ]

  reg_results[[k]] <- make_regression_segment(
    plot_df, "obs_mean", "Observed", sp$period_name, sp$start_year, sp$end_year
  )
  k <- k + 1

  reg_results[[k]] <- make_regression_segment(
    plot_df, "sim_mean", "Simulated", sp$period_name, sp$start_year, sp$end_year
  )
  k <- k + 1
}

reg_pred <- bind_rows(lapply(reg_results, function(x) x$pred)) %>%
  mutate(Regression = paste(Series, Period, sep = " | "))

reg_stats <- bind_rows(lapply(reg_results, function(x) x$stats)) %>%
  mutate(Regression = paste(Series, Period, sep = " | "))

write.csv(
  reg_stats,
  file.path(output_dir, "Yield_Regression_Slopes_Observed_Simulated_Publication.csv"),
  row.names = FALSE
)

# -----------------------------------------------------------------------------
# 5) REGRESSION TABLE
# -----------------------------------------------------------------------------

format_p <- function(p) {
  ifelse(is.na(p), "NA", ifelse(p < 0.001, "<0.001", as.character(signif(p, 2))))
}

reg_table <- reg_stats %>%
  mutate(
    `Slope (t ha⁻¹ yr⁻¹)` = round(Slope_t_ha_per_year, 3),
    `R²` = round(R2, 2),
    `P-value` = format_p(P_value)
  ) %>%
  select(
    Series,
    Period,
    `Slope (t ha⁻¹ yr⁻¹)`,
    `R²`,
    `P-value`,
    N
  )

write.csv(
  reg_table,
  file.path(output_dir, "Yield_Regression_Stats_Table_Publication.csv"),
  row.names = FALSE
)

table_theme <- ttheme_minimal(
  base_size = table_font,
  core = list(
    fg_params = list(fontsize = table_font, col = "black"),
    bg_params = list(fill = c("white", "grey96"))
  ),
  colhead = list(
    fg_params = list(fontsize = table_font + 1, fontface = "bold", col = "black"),
    bg_params = list(fill = "grey88")
  )
)

reg_table_grob <- tableGrob(reg_table, rows = NULL, theme = table_theme)
p_c <- wrap_elements(reg_table_grob)

# -----------------------------------------------------------------------------
# 6) THEME, BREAKS, COLORS
# -----------------------------------------------------------------------------

x_breaks <- seq(
  from = floor(min(plot_df$Year, na.rm = TRUE) / 2) * 2,
  to   = ceiling(max(plot_df$Year, na.rm = TRUE) / 2) * 2,
  by = 2
)

pub_theme <- function() {
  theme_classic(base_size = base_font) +
    theme(
      text = element_text(color = "black"),
      plot.title = element_text(face = "bold", size = panel_font, color = "black"),
      plot.subtitle = element_text(size = subtitle_font - 2, color = "black"),
      axis.title = element_text(size = axis_font, color = "black"),
      axis.text = element_text(size = axis_text, color = "black"),
      axis.line = element_line(color = "black", linewidth = 0.8),
      axis.ticks = element_line(color = "black", linewidth = 0.7),
      axis.ticks.length = unit(0.25, "cm"),
      legend.position = "top",
      legend.title = element_text(size = legend_font, face = "bold", color = "black"),
      legend.text = element_text(size = legend_font, color = "black"),
      legend.key.size = unit(1.0, "cm"),
      legend.box = "vertical",
      legend.spacing.y = unit(0.18, "cm"),
      legend.spacing.x = unit(0.25, "cm"),
      plot.margin = margin(8, 8, 8, 8)
    )
}

line_colors <- c(
  "Simulated MME mean" = "darkgreen",
  "Observed | 1980–1998" = "firebrick",
  "Observed | 1998–2019" = "darkorange3",
  "Observed | Whole period" = "purple4",
  "Simulated | 1980–1998" = "blue3",
  "Simulated | 1998–2019" = "deepskyblue4",
  "Simulated | Whole period" = "darkgreen"
)

ribbon_colors <- c(
  "Simulated ± SD across locations" = "firebrick",
  "Simulated ± SD across models" = "forestgreen"
)

# -----------------------------------------------------------------------------
# 7) PANEL A
# -----------------------------------------------------------------------------

p_a <- ggplot(plot_df, aes(x = Year)) +

  geom_ribbon(
    aes(
      ymin = sim_loc_lower,
      ymax = sim_loc_upper,
      fill = "Simulated ± SD across locations"
    ),
    alpha = alpha_sd_locations,
    na.rm = TRUE
  ) +
  geom_ribbon(
    aes(
      ymin = sim_mod_lower,
      ymax = sim_mod_upper,
      fill = "Simulated ± SD across models"
    ),
    alpha = alpha_sd_models,
    na.rm = TRUE
  ) +

  # Simulation = solid line
  geom_line(
    aes(y = sim_mean, color = "Simulated MME mean"),
    linewidth = 1.7,
    na.rm = TRUE
  ) +

  # Observation = symbols + error bars only
  geom_errorbar(
    aes(
      ymin = obs_mean - obs_sd,
      ymax = obs_mean + obs_sd
    ),
    color = "black",
    width = 0.18,
    linewidth = 0.65,
    na.rm = TRUE
  ) +
  geom_point(
    aes(y = obs_mean),
    color = "black",
    fill = "black",
    shape = 21,
    size = 3.4,
    stroke = 0.8,
    na.rm = TRUE
  ) +

  # Regression = dashed lines
  geom_line(
    data = reg_pred,
    aes(x = Year, y = y, color = Regression, linetype = Period),
    linewidth = 1.10,
    inherit.aes = FALSE,
    na.rm = TRUE
  ) +

  scale_x_continuous(
    breaks = x_breaks,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_color_manual(
    name = "",
    values = line_colors,
    breaks = names(line_colors)
  ) +
  scale_fill_manual(
    name = "",
    values = ribbon_colors
  ) +
  scale_linetype_manual(
    name = "Regression period",
    values = c(
      "1980–1998" = "dashed",
      "1998–2019" = "longdash",
      "Whole period" = "dotdash"
    )
  ) +
  labs(
    title = "A. Annual mean observed CIMMYT yield and simulated MME yield",
    x = NULL,
    y = "Grain yield (t/ha)"
  ) +
  coord_cartesian(ylim = c(4.8, 12.4)) +
  pub_theme() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  guides(
    fill = guide_legend(order = 1, override.aes = list(alpha = 0.35)),
    color = guide_legend(order = 2),
    linetype = guide_legend(order = 3)
  )

# -----------------------------------------------------------------------------
# 8) PANEL B
# -----------------------------------------------------------------------------

p_b <- ggplot(plot_df, aes(x = Year, y = yield_gap)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.75, linetype = "dashed") +
  geom_col(
    aes(fill = gap_type),
    width = 0.75,
    alpha = 0.85,
    na.rm = TRUE
  ) +
  geom_point(
    color = "black",
    fill = "black",
    shape = 21,
    size = 2.4,
    stroke = 0.6,
    na.rm = TRUE
  ) +
  scale_x_continuous(
    breaks = x_breaks,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_fill_manual(
    name = "",
    values = c(
      "Simulated > observed" = "firebrick",
      "Simulated < observed" = "steelblue",
      "Missing observed data" = "grey65"
    )
  ) +
  labs(
    title = "B. Annual yield gap",
    subtitle = "Yield gap = simulated MME mean − observed CIMMYT mean",
    x = "Year",
    y = "Yield gap (t/ha)"
  ) +
  pub_theme() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    legend.position = "top"
  )

# -----------------------------------------------------------------------------
# 9) COMBINE AND SAVE
# -----------------------------------------------------------------------------

combined_plot <- p_a / p_b / p_c +
  plot_layout(heights = c(2.65, 1.0, 0.62)) +
  plot_annotation(
    title = "Observed and simulated CIMMYT wheat yield and annual yield gap",
    subtitle = "Observed CIMMYT yield is shown as symbols with SD error bars; simulated MME yield is shown as a solid line with uncertainty ribbons; regressions are dashed.",
    theme = theme(
      plot.title = element_text(face = "bold", size = title_font, color = "black"),
      plot.subtitle = element_text(size = subtitle_font, color = "black"),
      plot.margin = margin(8, 8, 8, 8)
    )
  )

ggsave(
  filename = file.path(output_dir, "Yield_Observed_Simulated_YieldGap_Publication_NoGrid.png"),
  plot = combined_plot,
  width = fig_width,
  height = fig_height,
  dpi = fig_dpi,
  limitsize = FALSE
)

ggsave(
  filename = file.path(output_dir, "Yield_Observed_Simulated_YieldGap_Publication_NoGrid.pdf"),
  plot = combined_plot,
  width = fig_width,
  height = fig_height,
  limitsize = FALSE
)

ggsave(
  filename = file.path(output_dir, "PanelA_Yield_Observed_Simulated_Publication_NoGrid.png"),
  plot = p_a,
  width = 22,
  height = 11,
  dpi = fig_dpi,
  limitsize = FALSE
)

ggsave(
  filename = file.path(output_dir, "PanelB_YieldGap_Publication_NoGrid.png"),
  plot = p_b,
  width = 22,
  height = 6.5,
  dpi = fig_dpi,
  limitsize = FALSE
)

message("Done.")
message("Outputs are in: ", output_dir)
message("Main figure: Yield_Observed_Simulated_YieldGap_Publication_NoGrid.png")
message("Regression statistics saved as: Yield_Regression_Stats_Table_Publication.csv")
