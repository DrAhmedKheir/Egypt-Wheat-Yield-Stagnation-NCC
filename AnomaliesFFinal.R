# =========================================================
# National wheat yield anomalies + relative yield gaps
# =========================================================

library(tidyverse)
library(gridExtra)

# ---------------------------
# Paths
# ---------------------------
infile  <- "D:/Asseng_paper2024/Updated/Asseng_updated/40YRSSIMULATIONS/ForShawn/21072025/21072025/Update27032026/CIMMYTSimulations/AnomaliesFromCimmytSimactualpoints.csv"
outfile <- "D:/Asseng_paper2024/Updated/Asseng_updated/40YRSSIMULATIONS/ForShawn/21072025/21072025/Update27032026/CIMMYTSimulations//National_YieldAnomaly_with_RYG.png"

# ---------------------------
# Load data
# ---------------------------
df_raw <- read_csv(infile, show_col_types = FALSE)

# ---------------------------
# Check required columns
# ---------------------------
required_cols <- c(
  "year", "FAOAnomaly", "Simanomaly", "CIMMYTAnomaly",
  "RYGSimCiMMYT", "RYGSimFAO"
)

missing_cols <- setdiff(required_cols, names(df_raw))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

# ---------------------------
# National summary
# ---------------------------
df_national <- df_raw %>%
  group_by(year) %>%
  summarise(
    FAO            = mean(FAOAnomaly, na.rm = TRUE),
    Sim            = mean(Simanomaly, na.rm = TRUE),
    CIMMYT         = mean(CIMMYTAnomaly, na.rm = TRUE),
    RYGSimCIMMYT   = mean(RYGSimCiMMYT, na.rm = TRUE),
    RYGSimFAO      = mean(RYGSimFAO, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(Year = year) %>%
  arrange(Year)

# ---------------------------
# Colors and styles
# ---------------------------
cols_main <- c(
  "FAO"    = "black",
  "Sim"    = "#1f77b4",
  "CIMMYT" = "#d62728"
)

cols_ryg <- c(
  "RYGSimCIMMYT" = "#d62728",
  "RYGSimFAO"    = "#1f77b4"
)

# ---------------------------
# Metrics for panel a
# ---------------------------
fit_sim <- lm(Sim ~ FAO, data = df_national)
fit_cim <- lm(CIMMYT ~ FAO, data = df_national)

r2_sim   <- summary(fit_sim)$r.squared
r2_cim   <- summary(fit_cim)$r.squared
rmse_sim <- sqrt(mean((df_national$Sim - df_national$FAO)^2, na.rm = TRUE))
rmse_cim <- sqrt(mean((df_national$CIMMYT - df_national$FAO)^2, na.rm = TRUE))

lims <- range(
  c(df_national$FAO, df_national$Sim, df_national$CIMMYT),
  na.rm = TRUE
)
lims <- c(floor(lims[1] - 5), ceiling(lims[2] + 5))

# ---------------------------
# Panel a: FAO vs Sim & CIMMYT
# ---------------------------
scatter_long <- df_national %>%
  select(FAO, Sim, CIMMYT) %>%
  pivot_longer(
    cols = c(Sim, CIMMYT),
    names_to = "Series",
    values_to = "Value"
  )

p1 <- ggplot(scatter_long, aes(x = FAO, y = Value)) +
  geom_abline(slope = 1, intercept = 0, color = "grey35", linewidth = 0.9) +
  geom_point(aes(color = Series, shape = Series), size = 3.2, alpha = 0.9) +
  geom_smooth(aes(color = Series, linetype = Series),
              method = "lm", se = FALSE, linewidth = 1.0) +
  annotate(
    "text",
    x = lims[1] + 1, y = lims[2] - 2,
    hjust = 0, size = 5, fontface = "bold",
    color = cols_main["Sim"],
    label = paste0("Simulated: R² = ", round(r2_sim, 2),
                   ", RMSE = ", round(rmse_sim, 1))
  ) +
  annotate(
    "text",
    x = lims[1] + 1, y = lims[2] - 7,
    hjust = 0, size = 5, fontface = "bold",
    color = cols_main["CIMMYT"],
    label = paste0("CIMMYT: R² = ", round(r2_cim, 2),
                   ", RMSE = ", round(rmse_cim, 1))
  ) +
  scale_color_manual(values = cols_main[c("Sim", "CIMMYT")]) +
  scale_shape_manual(values = c("Sim" = 17, "CIMMYT" = 16)) +
  scale_linetype_manual(values = c("Sim" = "dashed", "CIMMYT" = "solid")) +
  coord_equal(xlim = lims, ylim = lims, expand = FALSE) +
  labs(
    title = "a) National anomaly comparison",
    subtitle = "FAO anomaly versus simulated and CIMMYT anomalies",
    x = "FAO yield anomaly (%)",
    y = "Simulated / CIMMYT yield anomaly (%)",
    color = NULL, shape = NULL, linetype = NULL
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 13, color = "grey30"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.position = c(0.80, 0.18),
    legend.text = element_text(size = 12)
  )

# ---------------------------
# Panel b: Time series
# ---------------------------
ts_long <- df_national %>%
  select(Year, FAO, Sim, CIMMYT) %>%
  pivot_longer(
    cols = c(FAO, Sim, CIMMYT),
    names_to = "Series",
    values_to = "Anomaly"
  )

p2 <- ggplot(ts_long, aes(x = Year, y = Anomaly, color = Series, linetype = Series)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = cols_main) +
  scale_linetype_manual(values = c(
    "FAO" = "solid",
    "Sim" = "dashed",
    "CIMMYT" = "solid"
  )) +
  labs(
    title = "b) National end-of-season wheat yield anomalies",
    subtitle = "Annual mean anomalies based on matched records",
    x = "Year",
    y = "Yield anomaly (%)",
    color = NULL, linetype = NULL
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 13, color = "grey30"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.position = "top",
    legend.text = element_text(size = 12)
  )

# ---------------------------
# Panel c: Relative yield gaps
# ---------------------------
ryg_long <- df_national %>%
  select(Year, RYGSimCIMMYT, RYGSimFAO) %>%
  pivot_longer(
    cols = c(RYGSimCIMMYT, RYGSimFAO),
    names_to = "Series",
    values_to = "RYG"
  )

p3 <- ggplot(ryg_long, aes(x = Year, y = RYG, color = Series)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = cols_ryg,
    labels = c(
      "RYGSimCIMMYT" = "Simulated vs CIMMYT",
      "RYGSimFAO"    = "Simulated vs FAO"
    )
  ) +
  labs(
    title = "c) Relative yield gaps",
    subtitle = "Annual mean relative yield gaps based on matched records",
    x = "Year",
    y = "Relative yield gap (%)",
    color = NULL
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 13, color = "grey30"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.position = "top",
    legend.text = element_text(size = 12)
  )

# ---------------------------
# Save figure
# ---------------------------
png(outfile, width = 8.5, height = 13, units = "in", res = 400)

grid.arrange(
  p1, p2, p3,
  ncol = 1,
  heights = c(1.15, 1, 1)
)

dev.off()

cat("Figure saved to:\n", outfile, "\n")