# =============================================================================
# Taylor diagram and PCA figures for CIMMYT climate/yield metrics
# =============================================================================
# Input directory:
# D:/Asseng_paper2024/Updated/Asseng_updated/40YRSSIMULATIONS/ForShawn/21072025/21072025/Update27032026/CIMMYTSimulations/CIMMYT_ClimateMetrics_From_WTH_LocCodes
#
# Main outputs:
#   1) Figure_Taylor_Annual_Observed_vs_Simulated.png/pdf
#   2) Figure_Taylor_LocationYear_Observed_vs_Simulated.png/pdf
#   3) Figure_PCA_ClimateYield_Annual.png/pdf
#   4) Figure_PCA_ClimateYield_RecordLevel.png/pdf
#   5) Figure_PCA_ClimateYield_LocationYear.png/pdf
#   6) CSV files with Taylor statistics and PCA scores/loadings
# =============================================================================

# install.packages(c(
#   "tidyverse", "readxl", "plotrix", "ggrepel", "factoextra",
#   "FactoMineR", "patchwork", "scales", "viridis"
# ))

library(tidyverse)
library(readxl)
library(plotrix)
library(ggrepel)
library(factoextra)
library(FactoMineR)
library(patchwork)
library(scales)
library(viridis)

# -----------------------------------------------------------------------------
# 1) USER SETTINGS
# -----------------------------------------------------------------------------

input_dir <- "D:/Asseng_paper2024/Updated/Asseng_updated/40YRSSIMULATIONS/ForShawn/21072025/21072025/Update27032026/CIMMYTSimulations/CIMMYT_ClimateMetrics_From_WTH_LocCodes"

output_dir <- file.path(input_dir, "Taylor_PCA_Figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

annual_file  <- file.path(input_dir, "CIMMYT_Annual_ClimateMetrics_Last90Days.csv")
record_file  <- file.path(input_dir, "CIMMYT_RecordLevel_ClimateMetrics_Last90Days.csv")
yearloc_file <- file.path(input_dir, "CIMMYT_YearLocation_ClimateMetrics_Last90Days.csv")

# If your local files have different names, this fallback searches the folder.
if (!file.exists(annual_file)) {
  annual_file <- list.files(input_dir, pattern = "Annual.*ClimateMetrics.*\\.csv$", full.names = TRUE)[1]
}
if (!file.exists(record_file)) {
  record_file <- list.files(input_dir, pattern = "RecordLevel.*ClimateMetrics.*\\.csv$", full.names = TRUE)[1]
}
if (!file.exists(yearloc_file)) {
  yearloc_file <- list.files(input_dir, pattern = "YearLocation.*ClimateMetrics.*\\.csv$", full.names = TRUE)[1]
}

# -----------------------------------------------------------------------------
# 2) READ DATA
# -----------------------------------------------------------------------------

annual <- readr::read_csv(annual_file, show_col_types = FALSE) %>%
  mutate(Year = as.integer(Year))

record <- readr::read_csv(record_file, show_col_types = FALSE) %>%
  mutate(
    Year = as.integer(Year),
    MatchedLocName = as.character(MatchedLocName),
    MatchedWeatherCode = as.character(MatchedWeatherCode)
  )

yearloc <- readr::read_csv(yearloc_file, show_col_types = FALSE) %>%
  mutate(
    Year = as.integer(Year),
    MatchedLocName = as.character(MatchedLocName),
    MatchedWeatherCode = as.character(MatchedWeatherCode)
  )

# -----------------------------------------------------------------------------
# 3) HELPER FUNCTIONS
# -----------------------------------------------------------------------------

theme_pca <- function(base_size = 15) {
  theme_bw(base_size = base_size) +
    theme(
      text = element_text(color = "black"),
      plot.title = element_text(face = "bold", size = 20, color = "black"),
      plot.subtitle = element_text(size = 15, color = "black"),
      axis.title = element_text(size = 16, color = "black"),
      axis.text = element_text(size = 14, color = "black"),
      legend.title = element_text(size = 14, face = "bold", color = "black"),
      legend.text = element_text(size = 13, color = "black"),
      legend.position = "right",
      panel.grid.major = element_line(color = "grey86", linewidth = 0.35),
      panel.grid.minor = element_line(color = "grey93", linewidth = 0.20)
    )
}

save_figure <- function(p, name, width = 12, height = 8, dpi = 350) {
  ggsave(file.path(output_dir, paste0(name, ".png")), p,
         width = width, height = height, dpi = dpi, limitsize = FALSE)
  ggsave(file.path(output_dir, paste0(name, ".pdf")), p,
         width = width, height = height, limitsize = FALSE)
}

calc_taylor_stats <- function(obs, sim, label = "Simulated MME") {
  ok <- is.finite(obs) & is.finite(sim)
  obs <- obs[ok]
  sim <- sim[ok]

  tibble(
    Series = label,
    N = length(obs),
    Mean_Obs = mean(obs),
    Mean_Sim = mean(sim),
    SD_Obs = sd(obs),
    SD_Sim = sd(sim),
    Correlation = cor(obs, sim),
    RMSE = sqrt(mean((sim - obs)^2)),
    Bias = mean(sim - obs),
    MAE = mean(abs(sim - obs)),
    CRMSE = sqrt(mean(((sim - mean(sim)) - (obs - mean(obs)))^2)),
    R2 = Correlation^2
  )
}

run_pca <- function(df, id_cols, variable_cols, pca_name) {

  pca_df <- df %>%
    select(any_of(c(id_cols, variable_cols))) %>%
    mutate(across(any_of(variable_cols), as.numeric))

  # Keep rows with at least some complete variables
  pca_complete <- pca_df %>%
    drop_na(any_of(variable_cols))

  if (nrow(pca_complete) < 5) {
    stop("Too few complete rows for PCA in: ", pca_name)
  }

  x <- pca_complete %>%
    select(any_of(variable_cols))

  # Remove zero-variance columns
  sds <- map_dbl(x, sd, na.rm = TRUE)
  keep_vars <- names(sds)[is.finite(sds) & sds > 0]
  x <- x %>% select(all_of(keep_vars))

  pca <- prcomp(x, center = TRUE, scale. = TRUE)

  scores <- as_tibble(pca$x) %>%
    bind_cols(pca_complete %>% select(any_of(id_cols)), .)

  loadings <- as_tibble(pca$rotation, rownames = "Variable")

  variance <- tibble(
    PC = paste0("PC", seq_along(pca$sdev)),
    Eigenvalue = pca$sdev^2,
    VariancePercent = (pca$sdev^2 / sum(pca$sdev^2)) * 100,
    CumulativeVariancePercent = cumsum((pca$sdev^2 / sum(pca$sdev^2)) * 100)
  )

  write_csv(scores, file.path(output_dir, paste0(pca_name, "_PCA_scores.csv")))
  write_csv(loadings, file.path(output_dir, paste0(pca_name, "_PCA_loadings.csv")))
  write_csv(variance, file.path(output_dir, paste0(pca_name, "_PCA_variance.csv")))

  list(pca = pca, scores = scores, loadings = loadings, variance = variance, variables = keep_vars)
}

make_pca_biplot <- function(pca_obj, scores, color_col = NULL, title = "PCA biplot", subtitle = "") {

  var_percent <- round((pca_obj$sdev^2 / sum(pca_obj$sdev^2)) * 100, 1)

  load <- as_tibble(pca_obj$rotation, rownames = "Variable") %>%
    mutate(
      PC1_arrow = PC1 * 4,
      PC2_arrow = PC2 * 4
    )

  if (!is.null(color_col) && color_col %in% names(scores)) {
    p <- ggplot(scores, aes(x = PC1, y = PC2, color = .data[[color_col]]))
  } else {
    p <- ggplot(scores, aes(x = PC1, y = PC2))
  }

  p +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey55") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey55") +
    geom_point(size = 3.2, alpha = 0.85) +
    geom_segment(
      data = load,
      aes(x = 0, y = 0, xend = PC1_arrow, yend = PC2_arrow),
      inherit.aes = FALSE,
      arrow = arrow(length = unit(0.22, "cm")),
      color = "firebrick",
      linewidth = 0.75
    ) +
    geom_text_repel(
      data = load,
      aes(x = PC1_arrow, y = PC2_arrow, label = Variable),
      inherit.aes = FALSE,
      color = "firebrick",
      fontface = "bold",
      size = 4.2,
      max.overlaps = Inf
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = paste0("PC1 (", var_percent[1], "%)"),
      y = paste0("PC2 (", var_percent[2], "%)"),
      color = color_col
    ) +
    theme_pca()
}

make_pca_scree <- function(variance, title = "PCA explained variance") {
  ggplot(variance, aes(x = factor(PC, levels = PC), y = VariancePercent)) +
    geom_col(fill = "steelblue", alpha = 0.85) +
    geom_line(aes(y = CumulativeVariancePercent, group = 1),
              color = "firebrick", linewidth = 1.0) +
    geom_point(aes(y = CumulativeVariancePercent),
               color = "firebrick", size = 2.8) +
    labs(
      title = title,
      x = "Principal component",
      y = "Explained variance (%)"
    ) +
    theme_pca() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# -----------------------------------------------------------------------------
# 4) TAYLOR DIAGRAMS
# -----------------------------------------------------------------------------
# Taylor diagram shows correlation, standard deviation, and centered RMS difference.
# Reference = observed CIMMYT yield. Test = simulated MME yield.

# 4A) Annual Taylor diagram
taylor_annual <- annual %>%
  select(Year, obs = annual_mean_YieldCIMMYT, sim = annual_mean_SimYieldMME_t_ha) %>%
  filter(is.finite(obs), is.finite(sim))

stats_annual <- calc_taylor_stats(taylor_annual$obs, taylor_annual$sim, "Annual simulated MME")
write_csv(stats_annual, file.path(output_dir, "Taylor_Annual_Statistics.csv"))

png(file.path(output_dir, "Figure_Taylor_Annual_Observed_vs_Simulated.png"),
    width = 2800, height = 2400, res = 300)
par(mar = c(5, 5, 5, 8), xpd = TRUE)
plotrix::taylor.diagram(
  ref = taylor_annual$obs,
  model = taylor_annual$sim,
  main = "Taylor diagram: annual observed CIMMYT vs simulated MME yield",
  col = "darkgreen",
  pch = 17,
  pcex = 2.2,
  normalize = FALSE,
  sd.arcs = TRUE,
  ref.sd = TRUE,
  grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99),
  pos.cor = TRUE
)
legend(
  "topright",
  inset = c(-0.22, 0.02),
  legend = c(
    "Simulated MME",
    paste0("R = ", round(stats_annual$Correlation, 2)),
    paste0("RMSE = ", round(stats_annual$RMSE, 2), " t/ha"),
    paste0("Bias = ", round(stats_annual$Bias, 2), " t/ha")
  ),
  pch = c(17, NA, NA, NA),
  col = c("darkgreen", "black", "black", "black"),
  bty = "n",
  cex = 1.05
)
dev.off()

pdf(file.path(output_dir, "Figure_Taylor_Annual_Observed_vs_Simulated.pdf"),
    width = 9.5, height = 8)
par(mar = c(5, 5, 5, 8), xpd = TRUE)
plotrix::taylor.diagram(
  ref = taylor_annual$obs,
  model = taylor_annual$sim,
  main = "Taylor diagram: annual observed CIMMYT vs simulated MME yield",
  col = "darkgreen",
  pch = 17,
  pcex = 2.2,
  normalize = FALSE,
  sd.arcs = TRUE,
  ref.sd = TRUE,
  grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99),
  pos.cor = TRUE
)
legend(
  "topright",
  inset = c(-0.22, 0.02),
  legend = c(
    "Simulated MME",
    paste0("R = ", round(stats_annual$Correlation, 2)),
    paste0("RMSE = ", round(stats_annual$RMSE, 2), " t/ha"),
    paste0("Bias = ", round(stats_annual$Bias, 2), " t/ha")
  ),
  pch = c(17, NA, NA, NA),
  col = c("darkgreen", "black", "black", "black"),
  bty = "n",
  cex = 1.05
)
dev.off()

# 4B) Location-year Taylor diagram
taylor_locyr <- yearloc %>%
  select(
    Year,
    MatchedLocName,
    obs = mean_YieldCIMMYT,
    sim = mean_SimYieldMME_t_ha
  ) %>%
  filter(is.finite(obs), is.finite(sim))

stats_locyr <- calc_taylor_stats(taylor_locyr$obs, taylor_locyr$sim, "Location-year simulated MME")
write_csv(stats_locyr, file.path(output_dir, "Taylor_LocationYear_Statistics.csv"))

png(file.path(output_dir, "Figure_Taylor_LocationYear_Observed_vs_Simulated.png"),
    width = 2800, height = 2400, res = 300)
par(mar = c(5, 5, 5, 8), xpd = TRUE)
plotrix::taylor.diagram(
  ref = taylor_locyr$obs,
  model = taylor_locyr$sim,
  main = "Taylor diagram: location-year observed CIMMYT vs simulated MME yield",
  col = "darkgreen",
  pch = 17,
  pcex = 2.2,
  normalize = FALSE,
  sd.arcs = TRUE,
  ref.sd = TRUE,
  grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99),
  pos.cor = TRUE
)
legend(
  "topright",
  inset = c(-0.22, 0.02),
  legend = c(
    "Simulated MME",
    paste0("R = ", round(stats_locyr$Correlation, 2)),
    paste0("RMSE = ", round(stats_locyr$RMSE, 2), " t/ha"),
    paste0("Bias = ", round(stats_locyr$Bias, 2), " t/ha"),
    paste0("N = ", stats_locyr$N)
  ),
  pch = c(17, NA, NA, NA, NA),
  col = c("darkgreen", "black", "black", "black", "black"),
  bty = "n",
  cex = 1.05
)
dev.off()

pdf(file.path(output_dir, "Figure_Taylor_LocationYear_Observed_vs_Simulated.pdf"),
    width = 9.5, height = 8)
par(mar = c(5, 5, 5, 8), xpd = TRUE)
plotrix::taylor.diagram(
  ref = taylor_locyr$obs,
  model = taylor_locyr$sim,
  main = "Taylor diagram: location-year observed CIMMYT vs simulated MME yield",
  col = "darkgreen",
  pch = 17,
  pcex = 2.2,
  normalize = FALSE,
  sd.arcs = TRUE,
  ref.sd = TRUE,
  grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99),
  pos.cor = TRUE
)
legend(
  "topright",
  inset = c(-0.22, 0.02),
  legend = c(
    "Simulated MME",
    paste0("R = ", round(stats_locyr$Correlation, 2)),
    paste0("RMSE = ", round(stats_locyr$RMSE, 2), " t/ha"),
    paste0("Bias = ", round(stats_locyr$Bias, 2), " t/ha"),
    paste0("N = ", stats_locyr$N)
  ),
  pch = c(17, NA, NA, NA, NA),
  col = c("darkgreen", "black", "black", "black", "black"),
  bty = "n",
  cex = 1.05
)
dev.off()

# Combined statistics
bind_rows(stats_annual, stats_locyr) %>%
  write_csv(file.path(output_dir, "Taylor_All_Statistics.csv"))

# -----------------------------------------------------------------------------
# 5) PCA FIGURES
# -----------------------------------------------------------------------------

# 5A) Annual PCA
annual_vars <- c(
  "annual_mean_YieldCIMMYT",
  "annual_mean_SimYieldMME_t_ha",
  "annual_mean_Tmax",
  "annual_mean_Tmin",
  "annual_mean_Tmean",
  "annual_mean_HeatDays_Tmax_GT32",
  "annual_mean_SRAD",
  "annual_mean_cumulative_SRAD",
  "annual_mean_VPD",
  "annual_mean_HDW_days"
)

annual_vars <- annual_vars[annual_vars %in% names(annual)]

pca_annual <- run_pca(
  df = annual,
  id_cols = c("Year", "n_matched_locations", "n_CIMMYT_records"),
  variable_cols = annual_vars,
  pca_name = "Annual_ClimateYield"
)

p_annual_biplot <- make_pca_biplot(
  pca_obj = pca_annual$pca,
  scores = pca_annual$scores,
  color_col = "Year",
  title = "PCA of annual CIMMYT yield and last-90-day climate drivers",
  subtitle = "Annual means across matched CIMMYT locations"
) +
  scale_color_viridis_c(option = "C")

p_annual_scree <- make_pca_scree(
  pca_annual$variance,
  title = "Annual PCA explained variance"
)

fig_pca_annual <- p_annual_biplot | p_annual_scree
save_figure(fig_pca_annual, "Figure_PCA_ClimateYield_Annual", width = 18, height = 8.5)

# 5B) Record-level PCA
record_vars <- c(
  "YieldCIMMYT",
  "SimYieldMME_t_ha",
  "Tmax_mean",
  "Tmin_mean",
  "Tmean_mean",
  "HeatDays_Tmax_GT32",
  "SRAD_mean",
  "SRAD_sum",
  "VPD_mean",
  "HDW_days",
  "RAIN_sum",
  "WIND_mean",
  "RHUM_mean"
)

record_vars <- record_vars[record_vars %in% names(record)]

pca_record <- run_pca(
  df = record,
  id_cols = c("Year", "Loc_desc", "MatchedLocName", "MatchedWeatherCode"),
  variable_cols = record_vars,
  pca_name = "RecordLevel_ClimateYield"
)

p_record_biplot <- make_pca_biplot(
  pca_obj = pca_record$pca,
  scores = pca_record$scores,
  color_col = "Year",
  title = "PCA of record-level CIMMYT yield and last-90-day climate drivers",
  subtitle = "Each point represents one CIMMYT record matched to a WTH location"
) +
  scale_color_viridis_c(option = "C")

p_record_scree <- make_pca_scree(
  pca_record$variance,
  title = "Record-level PCA explained variance"
)

fig_pca_record <- p_record_biplot | p_record_scree
save_figure(fig_pca_record, "Figure_PCA_ClimateYield_RecordLevel", width = 18, height = 8.5)

# 5C) Location-year PCA
yearloc_vars <- c(
  "mean_YieldCIMMYT",
  "mean_SimYieldMME_t_ha",
  "mean_Tmax",
  "mean_Tmin",
  "mean_Tmean",
  "mean_HeatDays_Tmax_GT32",
  "mean_SRAD",
  "mean_cumulative_SRAD",
  "mean_VPD",
  "mean_HDW_days",
  "mean_RAIN_sum",
  "mean_WIND",
  "mean_RHUM"
)

yearloc_vars <- yearloc_vars[yearloc_vars %in% names(yearloc)]

pca_yearloc <- run_pca(
  df = yearloc,
  id_cols = c("Year", "MatchedLocName", "MatchedWeatherCode"),
  variable_cols = yearloc_vars,
  pca_name = "LocationYear_ClimateYield"
)

p_yearloc_biplot <- make_pca_biplot(
  pca_obj = pca_yearloc$pca,
  scores = pca_yearloc$scores,
  color_col = "Year",
  title = "PCA of location-year yield and last-90-day climate drivers",
  subtitle = "Each point represents one year × matched CIMMYT location"
) +
  scale_color_viridis_c(option = "C")

p_yearloc_scree <- make_pca_scree(
  pca_yearloc$variance,
  title = "Location-year PCA explained variance"
)

fig_pca_yearloc <- p_yearloc_biplot | p_yearloc_scree
save_figure(fig_pca_yearloc, "Figure_PCA_ClimateYield_LocationYear", width = 18, height = 8.5)

# -----------------------------------------------------------------------------
# 6) OPTIONAL: PCA BY PERIOD BEFORE/AFTER 1998
# -----------------------------------------------------------------------------

annual_period_scores <- pca_annual$scores %>%
  mutate(Period = ifelse(Year <= 1998, "1981-1998", "1999-2021"))

p_annual_period <- ggplot(annual_period_scores, aes(PC1, PC2, color = Period, label = Year)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(size = 3.6, alpha = 0.90) +
  geom_text_repel(size = 3.6, max.overlaps = Inf) +
  scale_color_manual(values = c("1981-1998" = "firebrick", "1999-2021" = "steelblue")) +
  labs(
    title = "Annual PCA scores separated by breakpoint period",
    subtitle = "Colors separate years before and after the hypothesized 1998 breakpoint",
    x = paste0("PC1 (", round(pca_annual$variance$VariancePercent[1], 1), "%)"),
    y = paste0("PC2 (", round(pca_annual$variance$VariancePercent[2], 1), "%)")
  ) +
  theme_pca()

save_figure(p_annual_period, "Figure_PCA_Annual_Scores_By_1998_Period", width = 11, height = 8)

# -----------------------------------------------------------------------------
# 7) SUMMARY MESSAGE
# -----------------------------------------------------------------------------

message("Done.")
message("Taylor and PCA outputs saved in: ", output_dir)
message("Main files:")
message(" - Figure_Taylor_Annual_Observed_vs_Simulated.png/pdf")
message(" - Figure_Taylor_LocationYear_Observed_vs_Simulated.png/pdf")
message(" - Figure_PCA_ClimateYield_Annual.png/pdf")
message(" - Figure_PCA_ClimateYield_RecordLevel.png/pdf")
message(" - Figure_PCA_ClimateYield_LocationYear.png/pdf")
message(" - Taylor_All_Statistics.csv")
message(" - PCA scores/loadings/variance CSV files")
