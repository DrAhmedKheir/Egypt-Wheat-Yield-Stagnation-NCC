# =============================================================================
# CIMMYT annual observed vs simulated yield + annual yield gap
# + slope, P-value, and R2 labels on regression lines
# =============================================================================

# install.packages(c("readxl", "dplyr", "ggplot2", "stringr", "patchwork"))

library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(patchwork)

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

fig_width  <- 20
fig_height <- 14

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

format_p <- function(p) {
  ifelse(is.na(p), "NA", ifelse(p < 0.001, "<0.001", as.character(signif(p, 2))))
}

reg_labels <- reg_stats %>%
  mutate(
    Regression = paste(Series, Period, sep = " | "),
    label = paste0(
      Series, " ", Period,
      "\nSlope = ", round(Slope_t_ha_per_year, 3), " t ha^-1 yr^-1",
      "\nR2 = ", round(R2, 2), ", P = ", format_p(P_value)
    ),
    x = case_when(
      Series == "Observed" ~ 1980.5,
      Series == "Simulated" ~ 2007.5,
      TRUE ~ 1980.5
    ),
    y = case_when(
      Period == "1980-1998" ~ 11.9,
      Period == "1998-2019" ~ 11.25,
      Period == "Whole period" ~ 10.60,
      TRUE ~ 11
    )
  )

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
    linewidth = 1.25,
    na.rm = TRUE
  ) +
  geom_errorbar(
    aes(ymin = obs_mean - obs_sd, ymax = obs_mean + obs_sd,
        color = "Observed CIMMYT mean ± SD"),
    width = 0.25,
    linewidth = 0.65,
    na.rm = TRUE
  ) +
  geom_point(
    aes(y = obs_mean, color = "Observed CIMMYT mean ± SD"),
    size = 3.3,
    na.rm = TRUE
  ) +
  geom_line(
    data = reg_pred,
    aes(x = Year, y = y, color = Regression, linetype = Period),
    linewidth = 0.95,
    inherit.aes = FALSE,
    na.rm = TRUE
  ) +
  geom_label(
    data = reg_labels,
    aes(x = x, y = y, label = label, color = Regression),
    hjust = 0,
    size = 3.2,
    fontface = "bold",
    fill = alpha("white", 0.75),
    label.size = 0.15,
    show.legend = FALSE,
    inherit.aes = FALSE
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
  coord_cartesian(ylim = c(4.8, 12.4), clip = "off") +
  theme_bw(base_size = 15) +
  theme(
    legend.position = "top",
    legend.box = "vertical",
    plot.title = element_text(face = "bold", size = 18),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.minor = element_line(linewidth = 0.25, color = "grey90"),
    panel.grid.major = element_line(linewidth = 0.45, color = "grey85"),
    plot.margin = margin(5.5, 25, 5.5, 5.5)
  )

p_b <- ggplot(plot_df, aes(x = Year, y = yield_gap)) +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.7, linetype = "dashed") +
  geom_col(aes(fill = gap_type), width = 0.75, alpha = 0.75, na.rm = TRUE) +
  geom_line(color = "black", linewidth = 0.7, na.rm = TRUE) +
  geom_point(color = "black", size = 2.2, na.rm = TRUE) +
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
  theme_bw(base_size = 15) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_line(linewidth = 0.25, color = "grey90"),
    panel.grid.major = element_line(linewidth = 0.45, color = "grey85")
  )

combined_plot <- p_a / p_b +
  plot_layout(heights = c(2.5, 1)) +
  plot_annotation(
    title = "Observed and simulated CIMMYT wheat yield and annual yield gap",
    subtitle = "Panel A shows annual yield means, uncertainty, segmented regressions, and regression statistics. Panel B shows yearly model bias as simulated minus observed yield.",
    theme = theme(
      plot.title = element_text(face = "bold", size = 22),
      plot.subtitle = element_text(size = 14)
    )
  )

ggsave(
  filename = file.path(output_dir, "Yield_AnnualMean_Observed_Simulated_With_YieldGap_RegStats.png"),
  plot = combined_plot,
  width = fig_width,
  height = fig_height,
  dpi = 300
)

ggsave(
  filename = file.path(output_dir, "Yield_AnnualMean_Observed_Simulated_With_YieldGap_RegStats.pdf"),
  plot = combined_plot,
  width = fig_width,
  height = fig_height
)

message("Done.")
message("Outputs are in: ", output_dir)
message("Regression statistics saved as: ", file.path(output_dir, "Yield_Regression_Slopes_Observed_Simulated.csv"))
