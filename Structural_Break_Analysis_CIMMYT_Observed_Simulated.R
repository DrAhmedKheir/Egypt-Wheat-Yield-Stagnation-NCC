# =============================================================================
# CIMMYT observed and simulated yield: segmented regression, breakpoints, Chow test
# =============================================================================
# Purpose:
#   Test whether observed CIMMYT yield and simulated CIMMYT MME yield show a
#   structural trend change around 1998.
#
# Input directory:
#   D:/Asseng_paper2024/Updated/Asseng_updated/40YRSSIMULATIONS/ForShawn/21072025/21072025/Update27032026/CIMMYTSimulations
#
# Input files:
#   CIMMYTYieldDetails.xls
#   CIMMYTSimulatedMME.xlsx
#
# Outputs:
#   StructuralBreak_AnnualYield_Data.csv
#   StructuralBreak_Segmented_Results.csv
#   StructuralBreak_ChowTest_Results.csv
#   StructuralBreak_Breakpoints_Results.csv
#   StructuralBreak_Observed_and_Simulated.png
#   StructuralBreak_Observed_and_Simulated.pdf
#   StructuralBreak_BIC_Observed.png
#   StructuralBreak_BIC_Simulated.png
#
# Notes:
#   Observed CIMMYT yield column expected: YieldCIMMYT
#   Simulated yield column expected: SimYieldMME
#   SimYieldMME is converted from kg/ha to t/ha by default.
# =============================================================================

# install.packages(c(
#   "readxl", "dplyr", "ggplot2", "stringr", "segmented",
#   "strucchange", "broom", "writexl", "patchwork"
# ))

library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(segmented)
library(strucchange)
library(broom)
library(writexl)
library(patchwork)

# -----------------------------------------------------------------------------
# 1) USER SETTINGS
# -----------------------------------------------------------------------------

base_dir <- "D:/Asseng_paper2024/Updated/Asseng_updated/40YRSSIMULATIONS/ForShawn/21072025/21072025/Update27032026/CIMMYTSimulations"

observed_file  <- file.path(base_dir, "CIMMYTYieldDetails.xls")
simulated_file <- file.path(base_dir, "CIMMYTSimulatedMME.xlsx")

output_dir <- file.path(base_dir, "Structural_Break_Analysis")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Main breakpoint hypothesis from Senthold/Urs discussion
break_year <- 1998

# Segmented regression initial breakpoint guess
initial_psi <- 1998

# Convert simulated yield from kg/ha to t/ha
convert_sim_kg_ha_to_t_ha <- TRUE

# -----------------------------------------------------------------------------
# 2) READ AND SUMMARISE DATA
# -----------------------------------------------------------------------------

obs_raw <- read_excel(observed_file, sheet = 1)
sim_raw <- read_excel(simulated_file, sheet = 1)

required_obs <- c("Year", "YieldCIMMYT")
required_sim <- c("Year", "SimYieldMME")

missing_obs <- setdiff(required_obs, names(obs_raw))
missing_sim <- setdiff(required_sim, names(sim_raw))

if (length(missing_obs) > 0) {
  stop("Observed file missing columns: ", paste(missing_obs, collapse = ", "))
}

if (length(missing_sim) > 0) {
  stop("Simulated file missing columns: ", paste(missing_sim, collapse = ", "))
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
    SimYieldMME = as.numeric(SimYieldMME),
    Yield_sim = if (convert_sim_kg_ha_to_t_ha) SimYieldMME / 1000 else SimYieldMME
  ) %>%
  filter(!is.na(Year), !is.na(Yield_sim))

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
    sim_sd = ifelse(n() > 1, sd(Yield_sim, na.rm = TRUE), NA_real_),
    sim_n = n(),
    .groups = "drop"
  )

annual <- full_join(obs_year, sim_year, by = "Year") %>%
  arrange(Year) %>%
  mutate(
    yield_gap = sim_mean - obs_mean
  )

write.csv(
  annual,
  file.path(output_dir, "StructuralBreak_AnnualYield_Data.csv"),
  row.names = FALSE
)

# -----------------------------------------------------------------------------
# 3) HELPER FUNCTIONS
# -----------------------------------------------------------------------------

format_p <- function(p) {
  ifelse(is.na(p), NA_character_, ifelse(p < 0.001, "<0.001", as.character(signif(p, 3))))
}

safe_segmented <- function(df, y_col, series_name, psi_guess = 1998) {

  dat <- df %>%
    filter(!is.na(.data[[y_col]])) %>%
    select(Year, y = all_of(y_col))

  out_empty <- list(
    model = NULL,
    result = tibble(
      Series = series_name,
      Method = "segmented",
      Breakpoint = NA_real_,
      Slope_before = NA_real_,
      Slope_after = NA_real_,
      Pseudo_R2 = NA_real_,
      N = nrow(dat),
      Note = "failed"
    ),
    pred = tibble()
  )

  if (nrow(dat) < 6) return(out_empty)

  lm0 <- lm(y ~ Year, data = dat)

  fit <- tryCatch(
    segmented(lm0, seg.Z = ~Year, psi = psi_guess),
    error = function(e) NULL
  )

  if (is.null(fit)) return(out_empty)

  bp <- as.numeric(fit$psi[, "Est."])

  sl <- slope(fit)$Year
  slope_before <- as.numeric(sl[1, "Est."])
  slope_after  <- as.numeric(sl[2, "Est."])

  # Pseudo R2 from fitted segmented model
  pseudo_r2 <- 1 - sum(residuals(fit)^2) / sum((dat$y - mean(dat$y))^2)

  pred <- tibble(Year = seq(min(dat$Year), max(dat$Year), by = 1)) %>%
    mutate(
      y = predict(fit, newdata = .),
      Series = series_name
    )

  result <- tibble(
    Series = series_name,
    Method = "segmented",
    Breakpoint = bp,
    Slope_before = slope_before,
    Slope_after = slope_after,
    Pseudo_R2 = pseudo_r2,
    N = nrow(dat),
    Note = "ok"
  )

  list(model = fit, result = result, pred = pred)
}

safe_breakpoints <- function(df, y_col, series_name) {

  dat <- df %>%
    filter(!is.na(.data[[y_col]])) %>%
    select(Year, y = all_of(y_col))

  if (nrow(dat) < 8) {
    return(tibble(
      Series = series_name,
      Method = "strucchange::breakpoints",
      Best_n_breaks_BIC = NA_integer_,
      Break_years = NA_character_,
      N = nrow(dat),
      Note = "too few observations"
    ))
  }

  bp <- tryCatch(
    breakpoints(y ~ Year, data = dat, h = 0.15),
    error = function(e) NULL
  )

  if (is.null(bp)) {
    return(tibble(
      Series = series_name,
      Method = "strucchange::breakpoints",
      Best_n_breaks_BIC = NA_integer_,
      Break_years = NA_character_,
      N = nrow(dat),
      Note = "failed"
    ))
  }

  bic_vals <- BIC(bp)
  best_k <- which.min(bic_vals) - 1

  bp_best <- breakpoints(y ~ Year, data = dat, breaks = best_k, h = 0.15)

  break_idx <- bp_best$breakpoints
  break_years <- if (all(is.na(break_idx))) {
    NA_character_
  } else {
    paste(dat$Year[break_idx], collapse = "; ")
  }

  tibble(
    Series = series_name,
    Method = "strucchange::breakpoints",
    Best_n_breaks_BIC = best_k,
    Break_years = break_years,
    N = nrow(dat),
    Note = "ok"
  )
}

manual_chow_test <- function(df, y_col, series_name, break_year = 1998) {

  dat <- df %>%
    filter(!is.na(.data[[y_col]])) %>%
    select(Year, y = all_of(y_col))

  before <- dat %>% filter(Year <= break_year)
  after  <- dat %>% filter(Year > break_year)

  if (nrow(before) < 3 || nrow(after) < 3) {
    return(tibble(
      Series = series_name,
      BreakYear = break_year,
      F_statistic = NA_real_,
      P_value = NA_real_,
      N_before = nrow(before),
      N_after = nrow(after),
      Note = "too few observations"
    ))
  }

  lm_full <- lm(y ~ Year, data = dat)
  lm_before <- lm(y ~ Year, data = before)
  lm_after <- lm(y ~ Year, data = after)

  RSS_full <- sum(residuals(lm_full)^2)
  RSS_1 <- sum(residuals(lm_before)^2)
  RSS_2 <- sum(residuals(lm_after)^2)

  k <- length(coef(lm_full))
  n1 <- nrow(before)
  n2 <- nrow(after)

  F_chow <- ((RSS_full - (RSS_1 + RSS_2)) / k) /
    ((RSS_1 + RSS_2) / (n1 + n2 - 2 * k))

  p_chow <- pf(
    F_chow,
    df1 = k,
    df2 = n1 + n2 - 2 * k,
    lower.tail = FALSE
  )

  tibble(
    Series = series_name,
    BreakYear = break_year,
    F_statistic = F_chow,
    P_value = p_chow,
    N_before = n1,
    N_after = n2,
    Note = ifelse(p_chow < 0.05, "significant break", "not significant")
  )
}

period_lm_stats <- function(df, y_col, series_name, start_year, end_year, period_name) {

  dat <- df %>%
    filter(
      Year >= start_year,
      Year <= end_year,
      !is.na(.data[[y_col]])
    ) %>%
    select(Year, y = all_of(y_col))

  if (nrow(dat) < 3) {
    return(tibble(
      Series = series_name,
      Period = period_name,
      StartYear = start_year,
      EndYear = end_year,
      Slope_t_ha_per_year = NA_real_,
      R2 = NA_real_,
      P_value = NA_real_,
      N = nrow(dat)
    ))
  }

  fit <- lm(y ~ Year, data = dat)
  sm <- summary(fit)

  tibble(
    Series = series_name,
    Period = period_name,
    StartYear = start_year,
    EndYear = end_year,
    Slope_t_ha_per_year = coef(fit)[["Year"]],
    R2 = sm$r.squared,
    P_value = coef(sm)[2, 4],
    N = nrow(dat)
  )
}

# -----------------------------------------------------------------------------
# 4) RUN ANALYSES
# -----------------------------------------------------------------------------

seg_obs <- safe_segmented(annual, "obs_mean", "Observed CIMMYT", initial_psi)
seg_sim <- safe_segmented(annual, "sim_mean", "Simulated MME", initial_psi)

segmented_results <- bind_rows(seg_obs$result, seg_sim$result) %>%
  mutate(
    Breakpoint = round(Breakpoint, 2),
    Slope_before = round(Slope_before, 4),
    Slope_after = round(Slope_after, 4),
    Pseudo_R2 = round(Pseudo_R2, 3)
  )

write.csv(
  segmented_results,
  file.path(output_dir, "StructuralBreak_Segmented_Results.csv"),
  row.names = FALSE
)

breakpoint_results <- bind_rows(
  safe_breakpoints(annual, "obs_mean", "Observed CIMMYT"),
  safe_breakpoints(annual, "sim_mean", "Simulated MME")
)

write.csv(
  breakpoint_results,
  file.path(output_dir, "StructuralBreak_Breakpoints_Results.csv"),
  row.names = FALSE
)

chow_results <- bind_rows(
  manual_chow_test(annual, "obs_mean", "Observed CIMMYT", break_year),
  manual_chow_test(annual, "sim_mean", "Simulated MME", break_year)
) %>%
  mutate(
    F_statistic = round(F_statistic, 3),
    P_value_formatted = format_p(P_value)
  )

write.csv(
  chow_results,
  file.path(output_dir, "StructuralBreak_ChowTest_Results.csv"),
  row.names = FALSE
)

period_results <- bind_rows(
  period_lm_stats(annual, "obs_mean", "Observed CIMMYT", 1980, 1998, "1980-1998"),
  period_lm_stats(annual, "obs_mean", "Observed CIMMYT", 1998, 2019, "1998-2019"),
  period_lm_stats(annual, "obs_mean", "Observed CIMMYT", min(annual$Year, na.rm = TRUE), max(annual$Year, na.rm = TRUE), "Whole period"),
  period_lm_stats(annual, "sim_mean", "Simulated MME", 1980, 1998, "1980-1998"),
  period_lm_stats(annual, "sim_mean", "Simulated MME", 1998, 2019, "1998-2019"),
  period_lm_stats(annual, "sim_mean", "Simulated MME", min(annual$Year, na.rm = TRUE), max(annual$Year, na.rm = TRUE), "Whole period")
) %>%
  mutate(
    Slope_t_ha_per_year = round(Slope_t_ha_per_year, 4),
    R2 = round(R2, 3),
    P_value_formatted = format_p(P_value)
  )

write.csv(
  period_results,
  file.path(output_dir, "StructuralBreak_Period_LM_Results.csv"),
  row.names = FALSE
)

write_xlsx(
  list(
    Annual_Data = annual,
    Segmented = segmented_results,
    Breakpoints = breakpoint_results,
    Chow_Test_1998 = chow_results,
    Period_LM = period_results
  ),
  file.path(output_dir, "StructuralBreak_All_Results.xlsx")
)

# -----------------------------------------------------------------------------
# 5) PLOTS
# -----------------------------------------------------------------------------

seg_pred <- bind_rows(seg_obs$pred, seg_sim$pred)

plot_long <- annual %>%
  select(Year, obs_mean, sim_mean) %>%
  tidyr::pivot_longer(
    cols = c(obs_mean, sim_mean),
    names_to = "Series",
    values_to = "Yield"
  ) %>%
  mutate(
    Series = recode(
      Series,
      obs_mean = "Observed CIMMYT",
      sim_mean = "Simulated MME"
    )
  )

p1 <- ggplot() +
  geom_point(
    data = plot_long,
    aes(x = Year, y = Yield, color = Series, shape = Series),
    size = 3.0,
    na.rm = TRUE
  ) +
  geom_line(
    data = plot_long %>% filter(Series == "Simulated MME"),
    aes(x = Year, y = Yield, color = Series),
    linewidth = 0.9,
    na.rm = TRUE
  ) +
  geom_line(
    data = seg_pred,
    aes(x = Year, y = y, color = Series),
    linewidth = 1.25,
    linetype = "solid",
    na.rm = TRUE
  ) +
  geom_vline(
    xintercept = break_year,
    linetype = "dashed",
    color = "grey35",
    linewidth = 0.8
  ) +
  scale_color_manual(
    values = c(
      "Observed CIMMYT" = "black",
      "Simulated MME" = "darkgreen"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Observed CIMMYT" = 16,
      "Simulated MME" = 17
    )
  ) +
  scale_x_continuous(
    breaks = seq(
      floor(min(annual$Year, na.rm = TRUE) / 2) * 2,
      ceiling(max(annual$Year, na.rm = TRUE) / 2) * 2,
      by = 2
    )
  ) +
  labs(
    title = "Segmented regression and structural-break analysis",
    subtitle = paste0("Dashed vertical line indicates hypothesized breakpoint year: ", break_year),
    x = "Year",
    y = "Grain yield (t/ha)",
    color = "",
    shape = ""
  ) +
  theme_bw(base_size = 15) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  file.path(output_dir, "StructuralBreak_Observed_and_Simulated.png"),
  p1,
  width = 13,
  height = 7,
  dpi = 300
)

ggsave(
  file.path(output_dir, "StructuralBreak_Observed_and_Simulated.pdf"),
  p1,
  width = 13,
  height = 7
)

# BIC plots from strucchange
save_bic_plot <- function(df, y_col, out_file, title_text) {

  dat <- df %>%
    filter(!is.na(.data[[y_col]])) %>%
    select(Year, y = all_of(y_col))

  if (nrow(dat) < 8) return(invisible(NULL))

  bp <- tryCatch(
    breakpoints(y ~ Year, data = dat, h = 0.15),
    error = function(e) NULL
  )

  if (is.null(bp)) return(invisible(NULL))

  png(out_file, width = 9, height = 6, units = "in", res = 300)
  plot(bp, main = title_text)
  dev.off()
}

save_bic_plot(
  annual,
  "obs_mean",
  file.path(output_dir, "StructuralBreak_BIC_Observed.png"),
  "BIC for breakpoints: observed CIMMYT yield"
)

save_bic_plot(
  annual,
  "sim_mean",
  file.path(output_dir, "StructuralBreak_BIC_Simulated.png"),
  "BIC for breakpoints: simulated MME yield"
)

# -----------------------------------------------------------------------------
# 6) PRINT SUMMARY
# -----------------------------------------------------------------------------

message("\n================ SEGMENTED RESULTS ================")
print(segmented_results)

message("\n================ BREAKPOINT RESULTS ================")
print(breakpoint_results)

message("\n================ CHOW TEST RESULTS ================")
print(chow_results)

message("\n================ PERIOD LINEAR MODELS ================")
print(period_results)

message("\nDone.")
message("All outputs saved to: ", output_dir)
