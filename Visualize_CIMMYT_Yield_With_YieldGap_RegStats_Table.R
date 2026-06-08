# =============================================================================
# CIMMYT annual observed vs simulated yield + annual yield gap
# + regression statistics in a separate readable panel/table
# =============================================================================
# This version avoids overlapping tiny labels inside Panel A.
# It creates:
#   Panel A = yield time series + regressions
#   Panel B = annual yield gap
#   Panel C = large regression statistics table
# =============================================================================

# install.packages(c("readxl", "dplyr", "ggplot2", "stringr", "patchwork", "gridExtra", "grid"))

library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(patchwork)
library(gridExtra)
library(grid)

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

alpha_sd_locations <- 0.22
alpha_sd_models    <- 0.28

period1_start <- 1980
period1_end   <- 1998
period2_start <- 1998
period2_end   <- 2019

# Much larger output figure
fig_width  <- 34
fig_height <- 34

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
# 3) ANNUAL OBSERVED AND SIMULATED SUMMARIES
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
      is.na(yield_gap) ~ "NA",
      yield_gap >= 0 ~ "Simulated > Observed",
      TRUE ~ "Simulated < Observed"
    )
  )

write.csv(
  plot_df,
  file.path(output_dir, "Yield_AnnualMean_Observed_Simulated_With_YieldGap.csv"),
  row.names = FALSE
)

# -----------------------------------------------------------------------------
# 4) REGRESSION HELPERS
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
  "1980-1998",    period1_start, period1_end,
  "1998-2019",    period2_start, period2_end,
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

reg_pred <- bind_rows(lapply(reg_results, function(x) x$pred))
reg_stats <- bind_rows(lapply(reg_results, function(x) x$stats))

write.csv(
  reg_stats,
  file.path(output_dir, "Yield_Regression_Slopes_Observed_Simulated.csv"),
  row.names = FALSE
)

reg_pred <- reg_pred %>%
  mutate(Regression = paste(Series, Period, sep = " | "))

# -----------------------------------------------------------------------------
# 5) REGRESSION STATISTICS TABLE FOR PANEL C
# -----------------------------------------------------------------------------

format_p <- function(p) {
  ifelse(is.na(p), "NA", ifelse(p < 0.001, "<0.001", as.character(signif(p, 2))))
}

reg_table <- reg_stats %>%
  mutate(
    `Slope (t ha-1 yr-1)` = round(Slope_t_ha_per_year, 3),
    `R2` = round(R2, 2),
    `P-value` = format_p(P_value)
  ) %>%
  select(
    Series,
    Period,
    `Slope (t ha-1 yr-1)`,
    R2,
    `P-value`,
    N
  )

# Save readable stats table
write.csv(
  reg_table,
  file.path(output_dir, "Yield_Regression_Stats_Table.csv"),
  row.names = FALSE
)

table_theme <- ttheme_minimal(
  base_size = 32,
  core = list(
    fg_params = list(fontsize = 32),
    bg_params = list(fill = c("white", "grey95"))
  ),
  colhead = list(
    fg_params = list(fontsize = 33, fontface = "bold"),
    bg_params = list(fill = "grey85")
  )
)

reg_table_grob <- tableGrob(reg_table, rows = NULL, theme = table_theme)

p_c <- wrap_elements(reg_table_grob) +
  plot_annotation(title = "C. Regression statistics") &
  theme(
    plot.title = element_text(face = "bold", size = 36)
  )

# -----------------------------------------------------------------------------
# 6) AXIS BREAKS AND COLORS
# -----------------------------------------------------------------------------

x_breaks <- seq(
  from = floor(min(plot_df$Year, na.rm = TRUE) / 2) * 2,
  to   = ceiling(max(plot_df$Year, na.rm = TRUE) / 2) * 2,
  by = 2
)

color_values <- c(
  "Observed CIMMYT mean ± SD" = "black",
  "Simulated MME mean" = "darkgreen",
  "Observed | 1980-1998" = "red3",
  "Observed | 1998-2019" = "darkorange3",
  "Observed | Whole period" = "purple4",
  "Simulated | 1980-1998" = "blue3",
  "Simulated | 1998-2019" = "deepskyblue4",
  "Simulated | Whole period" = "darkgreen"
)

# -----------------------------------------------------------------------------
# 7) PANEL A: OBSERVED VS SIMULATED
# -----------------------------------------------------------------------------

p_a <- ggplot(plot_df, aes(x = Year)) +
  geom_ribbon(
    aes(ymin = sim_loc_lower, ymax = sim_loc_upper,
        fill = "Simulated ± SD across locations"),
    alpha = alpha_sd_locations,
    na.rm = TRUE
  ) +
  geom_ribbon(
    aes(ymin = sim_mod_lower, ymax = sim_mod_upper,
        fill = "Simulated ± SD across models"),
    alpha = alpha_sd_models,
    na.rm = TRUE
  ) +
  geom_line(
    aes(y = sim_mean, color = "Simulated MME mean"),
    linewidth = 1.7,
    na.rm = TRUE
  ) +
  geom_errorbar(
    aes(ymin = obs_mean - obs_sd, ymax = obs_mean + obs_sd,
        color = "Observed CIMMYT mean ± SD"),
    width = 0.25,
    linewidth = 0.75,
    na.rm = TRUE
  ) +
  geom_point(
    aes(y = obs_mean, color = "Observed CIMMYT mean ± SD"),
    size = 4.0,
    na.rm = TRUE
  ) +
  geom_line(
    data = reg_pred,
    aes(x = Year, y = y, color = Regression, linetype = Period),
    linewidth = 1.25,
    inherit.aes = FALSE,
    na.rm = TRUE
  ) +
  scale_x_continuous(
    breaks = x_breaks,
    minor_breaks = seq(min(x_breaks), max(x_breaks), by = 1)
  ) +
  scale_color_manual(name = "", values = color_values) +
  scale_fill_manual(
    name = "",
    values = c(
      "Simulated ± SD across locations" = "firebrick",
      "Simulated ± SD across models" = "forestgreen"
    )
  ) +
  scale_linetype_manual(
    name = "Regression period",
    values = c(
      "1980-1998" = "solid",
      "1998-2019" = "longdash",
      "Whole period" = "dotdash"
    )
  ) +
  labs(
    title = "A. Annual mean observed CIMMYT yield vs simulated MME yield",
    x = NULL,
    y = "Grain yield (t/ha)"
  ) +
  coord_cartesian(ylim = c(4.8, 12.3)) +
  theme_bw(base_size = 32) +
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size = 33, face = "bold"),
    legend.text = element_text(size = 31),
    legend.key.size = unit(1.8, "cm"),
    legend.spacing.y = unit(0.55, "cm"),
    legend.spacing.x = unit(0.45, "cm"),

    plot.title = element_text(face = "bold", size = 30),
    axis.title = element_text(size = 36),
    axis.text = element_text(size = 32),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),

    panel.grid.minor = element_line(linewidth = 0.28, color = "grey90"),
    panel.grid.major = element_line(linewidth = 0.50, color = "grey85")
  )

# -----------------------------------------------------------------------------
# 8) PANEL B: YIELD GAP
# -----------------------------------------------------------------------------

p_b <- ggplot(plot_df, aes(x = Year, y = yield_gap)) +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.8, linetype = "dashed") +
  geom_col(aes(fill = gap_type), width = 0.75, alpha = 0.75, na.rm = TRUE) +
  geom_line(color = "black", linewidth = 0.85, na.rm = TRUE) +
  geom_point(color = "black", size = 3.2, na.rm = TRUE) +
  scale_x_continuous(
    breaks = x_breaks,
    minor_breaks = seq(min(x_breaks), max(x_breaks), by = 1)
  ) +
  scale_fill_manual(
    name = "",
    values = c(
      "Simulated > Observed" = "firebrick",
      "Simulated < Observed" = "steelblue",
      "NA" = "grey60"
    )
  ) +
  labs(
    title = "B. Annual yield gap",
    subtitle = "Yield gap = simulated MME mean − observed CIMMYT mean",
    x = "Year",
    y = "Yield gap (t/ha)"
  ) +
  theme_bw(base_size = 32) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 33, face = "bold"),
    legend.text = element_text(size = 31),
    legend.key.size = unit(1.5, "cm"),
    legend.spacing.x = unit(0.45, "cm"),

    plot.title = element_text(face = "bold", size = 30),
    plot.subtitle = element_text(size = 32),
    axis.title = element_text(size = 36),
    axis.text = element_text(size = 32),
    axis.text.x = element_text(angle = 90, hjust = 1),

    panel.grid.minor = element_line(linewidth = 0.28, color = "grey90"),
    panel.grid.major = element_line(linewidth = 0.50, color = "grey85")
  )

# -----------------------------------------------------------------------------
# 9) COMBINE PANELS AND SAVE
# -----------------------------------------------------------------------------

combined_plot <- p_a / p_b / p_c +
  plot_layout(heights = c(2.3, 1.0, 0.85)) +
  plot_annotation(
    title = "Observed and simulated CIMMYT wheat yield and annual yield gap",
    subtitle = "Regression statistics are shown separately to avoid overlap with yield observations.",
    theme = theme(
      plot.title = element_text(face = "bold", size = 36),
      plot.subtitle = element_text(size = 34)
    )
  )

ggsave(
  filename = file.path(output_dir, "Yield_AnnualMean_Observed_Simulated_With_YieldGap_RegStats_Table.png"),
  plot = combined_plot,
  width = fig_width,
  height = fig_height,
  dpi = 300,
  limitsize = FALSE
)

ggsave(
  filename = file.path(output_dir, "Yield_AnnualMean_Observed_Simulated_With_YieldGap_RegStats_Table.pdf"),
  plot = combined_plot,
  width = fig_width,
  height = fig_height,
  limitsize = FALSE
)

message("Done.")
message("Outputs are in: ", output_dir)
message("Regression statistics saved as: ", file.path(output_dir, "Yield_Regression_Stats_Table.csv"))
