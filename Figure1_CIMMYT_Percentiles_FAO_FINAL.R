# =============================================================================
# Figure 1. CIMMYT ESWYT percentiles + FAO national yield in Egypt
#
# Requested updates:
#   - Remove connecting lines between 50th and 90th percentile symbols
#   - Keep symbols for 50th and 90th percentiles
#   - Keep grey band between 50th and 90th percentiles
#   - Add FAO national yield line from FAOYears and FAOYield columns
#   - Percentile regression lines in two stages:
#       1981–1999 and 1999–2021
#   - 90th percentile symbols: dark green
#   - 50th percentile symbols: dark brown
#   - x-axis from 1979 to 2021 with 2-year interval
#   - high-resolution outputs
# =============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readr)
library(janitor)
library(grid)

# -----------------------------------------------------------------------------
# 1. PATHS
# -----------------------------------------------------------------------------

base_dir <- "D:/Asseng_paper2024/Updated/Asseng_updated/40YRSSIMULATIONS/ForShawn/21072025/21072025/Update27032026/UrslastFilesCVs"
yield_file <- file.path(base_dir, "ESWYT1-41_EGYPT_GID_Yield_Pheno_20260602.xlsx")

output_dir <- file.path(base_dir, "Figure1_ESWYT_Percentiles_FAO")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 2. SETTINGS
# -----------------------------------------------------------------------------

convert_yield_kg_ha_to_t_ha <- FALSE

x_min <- 1979
x_max <- 2021
x_breaks <- seq(x_min, x_max, by = 2)

period_1_start <- 1981
period_1_end   <- 1999
period_2_start <- 1999
period_2_end   <- 2021

fig_width  <- 14
fig_height <- 8.5
fig_dpi    <- 500

col_q50 <- "#5A2D0C"  # dark brown
col_q90 <- "#006400"  # dark green
col_fao <- "black"
band_col <- "grey65"

# -----------------------------------------------------------------------------
# 3. HELPER FUNCTIONS
# -----------------------------------------------------------------------------

find_col <- function(df, patterns) {
  hits <- names(df)[str_detect(names(df), regex(paste(patterns, collapse = "|"), ignore_case = TRUE))]
  if (length(hits) == 0) return(NA_character_)
  hits[1]
}

safe_lm_stats <- function(dat, y_col, period_name, start_year, end_year, year_col = "Year") {
  dat2 <- dat %>%
    filter(.data[[year_col]] >= start_year, .data[[year_col]] <= end_year) %>%
    filter(!is.na(.data[[y_col]]), !is.na(.data[[year_col]]))

  if (nrow(dat2) < 3) {
    return(tibble(
      Series = y_col,
      Period = period_name,
      Start_year = start_year,
      End_year = end_year,
      Slope_t_ha_per_year = NA_real_,
      Slope_t_ha_per_decade = NA_real_,
      R2 = NA_real_,
      P_value = NA_real_,
      N_years = nrow(dat2)
    ))
  }

  fit <- lm(reformulate(year_col, response = y_col), data = dat2)
  sm <- summary(fit)

  tibble(
    Series = y_col,
    Period = period_name,
    Start_year = min(dat2[[year_col]], na.rm = TRUE),
    End_year = max(dat2[[year_col]], na.rm = TRUE),
    Slope_t_ha_per_year = coef(fit)[[year_col]],
    Slope_t_ha_per_decade = coef(fit)[[year_col]] * 10,
    R2 = sm$r.squared,
    P_value = sm$coefficients[year_col, "Pr(>|t|)"],
    N_years = nrow(dat2)
  )
}

make_trend_df <- function(dat, y_col, period_name, start_year, end_year, year_col = "Year") {
  dat2 <- dat %>%
    filter(.data[[year_col]] >= start_year, .data[[year_col]] <= end_year) %>%
    filter(!is.na(.data[[y_col]]), !is.na(.data[[year_col]]))

  if (nrow(dat2) < 3) return(NULL)

  fit <- lm(reformulate(year_col, response = y_col), data = dat2)
  years <- seq(min(dat2[[year_col]]), max(dat2[[year_col]]), by = 1)

  tibble(
    Year = years,
    y_pred = predict(fit, newdata = tibble(!!year_col := years)),
    Series = y_col,
    Period = period_name
  )
}

# -----------------------------------------------------------------------------
# 4. READ DATA
# -----------------------------------------------------------------------------

raw <- read_excel(yield_file, sheet = 1) %>% clean_names()

message("Detected columns:")
print(names(raw))

year_col  <- find_col(raw, c("^harvest_yr$", "harvest", "^year$", "yr"))
yield_col <- find_col(raw, c("^yld_t_ha$", "blueorave_yld", "blue", "ave_yld", "yield", "yld"))
gid_col   <- find_col(raw, c("^gid$", "genotype", "line"))
loc_col   <- find_col(raw, c("loc_desc", "location", "site", "loc"))

fao_year_col  <- find_col(raw, c("^fao_years$", "faoyears", "fao_year", "faoyear"))
fao_yield_col <- find_col(raw, c("^fao_yield$", "faoyield", "fao_yld"))

if (any(is.na(c(year_col, yield_col)))) {
  stop("Could not detect harvest year or yield column. Please check printed column names.")
}

if (any(is.na(c(fao_year_col, fao_yield_col)))) {
  stop("Could not detect FAOYears or FAOYield columns. Please check printed column names.")
}

dat <- raw %>%
  transmute(
    HarvestYr = as.integer(.data[[year_col]]),
    Yield_raw = as.numeric(.data[[yield_col]]),
    GID = if (!is.na(gid_col)) as.character(.data[[gid_col]]) else NA_character_,
    Location = if (!is.na(loc_col)) as.character(.data[[loc_col]]) else NA_character_
  ) %>%
  mutate(
    Yield = if (convert_yield_kg_ha_to_t_ha) Yield_raw / 1000 else Yield_raw
  ) %>%
  filter(!is.na(HarvestYr), !is.na(Yield)) %>%
  arrange(HarvestYr)

fao <- raw %>%
  transmute(
    Year = as.integer(.data[[fao_year_col]]),
    FAO_yield = as.numeric(.data[[fao_yield_col]])
  ) %>%
  filter(!is.na(Year), !is.na(FAO_yield)) %>%
  distinct(Year, .keep_all = TRUE) %>%
  arrange(Year)

# -----------------------------------------------------------------------------
# 5. ANNUAL CIMMYT PERCENTILES
# -----------------------------------------------------------------------------

annual <- dat %>%
  group_by(HarvestYr) %>%
  summarise(
    n_records = n(),
    n_locations = n_distinct(Location),
    n_gid = n_distinct(GID),
    yield_q50 = quantile(Yield, 0.50, na.rm = TRUE, names = FALSE),
    yield_q90 = quantile(Yield, 0.90, na.rm = TRUE, names = FALSE),
    yield_mean = mean(Yield, na.rm = TRUE),
    yield_sd = sd(Yield, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(Year = HarvestYr) %>%
  arrange(Year)

write_csv(dat, file.path(output_dir, "Figure1_CIMMYT_LineLevel_Cleaned.csv"))
write_csv(annual, file.path(output_dir, "Figure1_CIMMYT_Annual_50_90_Percentiles.csv"))
write_csv(fao, file.path(output_dir, "Figure1_FAO_Yield.csv"))

# -----------------------------------------------------------------------------
# 6. TWO-STAGE REGRESSION LINES FOR CIMMYT PERCENTILES
# -----------------------------------------------------------------------------

periods <- tibble::tribble(
  ~Series,      ~Period,      ~start_year,    ~end_year,
  "yield_q50", "1981-1999",   period_1_start, period_1_end,
  "yield_q50", "1999-2021",   period_2_start, period_2_end,
  "yield_q90", "1981-1999",   period_1_start, period_1_end,
  "yield_q90", "1999-2021",   period_2_start, period_2_end
)

trend_df <- bind_rows(lapply(seq_len(nrow(periods)), function(i) {
  make_trend_df(
    dat = annual,
    y_col = periods$Series[i],
    period_name = periods$Period[i],
    start_year = periods$start_year[i],
    end_year = periods$end_year[i],
    year_col = "Year"
  )
}))

reg_stats <- bind_rows(lapply(seq_len(nrow(periods)), function(i) {
  safe_lm_stats(
    dat = annual,
    y_col = periods$Series[i],
    period_name = periods$Period[i],
    start_year = periods$start_year[i],
    end_year = periods$end_year[i],
    year_col = "Year"
  )
})) %>%
  mutate(
    Series = recode(
      Series,
      yield_q50 = "50th percentile (median)",
      yield_q90 = "90th percentile"
    ),
    P_value_label = ifelse(is.na(P_value), NA_character_,
                           ifelse(P_value < 0.001, "<0.001", sprintf("%.3f", P_value))),
    R2_label = ifelse(is.na(R2), NA_character_, sprintf("%.2f", R2))
  )

write_csv(reg_stats, file.path(output_dir, "Figure1_CIMMYT_Percentile_Regression_Stats.csv"))

# -----------------------------------------------------------------------------
# 7. PLOT
# -----------------------------------------------------------------------------

all_y <- c(annual$yield_q50, annual$yield_q90, fao$FAO_yield)
y_upper <- ceiling(max(all_y, na.rm = TRUE) + 1)

p <- ggplot() +
  geom_ribbon(
    data = annual,
    aes(x = Year, ymin = yield_q50, ymax = yield_q90, fill = "50th–90th percentile band"),
    alpha = 0.35,
    color = NA
  ) +
  geom_point(
    data = annual,
    aes(x = Year, y = yield_q50, color = "50th percentile (median)"),
    size = 4.1
  ) +
  geom_point(
    data = annual,
    aes(x = Year, y = yield_q90, color = "90th percentile"),
    size = 4.1
  ) +
  geom_line(
    data = trend_df %>% filter(Series == "yield_q50"),
    aes(x = Year, y = y_pred, group = Period),
    color = col_q50,
    linetype = "dashed",
    linewidth = 1.35
  ) +
  geom_line(
    data = trend_df %>% filter(Series == "yield_q90"),
    aes(x = Year, y = y_pred, group = Period),
    color = col_q90,
    linetype = "dashed",
    linewidth = 1.35
  ) +
  geom_line(
    data = fao %>% filter(Year >= x_min, Year <= x_max),
    aes(x = Year, y = FAO_yield, color = "FAO national yield"),
    linewidth = 1.55
  ) +
  scale_x_continuous(
    limits = c(x_min, x_max),
    breaks = x_breaks,
    expand = expansion(mult = c(0.005, 0.005))
  ) +
  scale_y_continuous(
    limits = c(0, y_upper),
    breaks = seq(0, y_upper, by = 2),
    expand = expansion(mult = c(0, 0.03))
  ) +
  scale_color_manual(
    name = "",
    values = c(
      "50th percentile (median)" = col_q50,
      "90th percentile" = col_q90,
      "FAO national yield" = col_fao
    )
  ) +
  scale_fill_manual(
    name = "",
    values = c("50th–90th percentile band" = band_col)
  ) +
  labs(
    title = "Figure 1. National wheat yield and genetic yield potential in Egypt",
    subtitle = "CIMMYT ESWYT 50th and 90th percentile yields are shown as symbols; dashed lines show two-stage regressions",
    x = "Harvest year",
    y = expression("Grain yield (t ha"^{-1}*")")
  ) +
  theme_classic(base_size = 18) +
  theme(
    text = element_text(color = "black"),
    plot.title = element_text(face = "bold", size = 25, color = "black"),
    plot.subtitle = element_text(size = 17, color = "black"),
    axis.title = element_text(face = "bold", size = 21, color = "black"),
    axis.text = element_text(face = "bold", size = 16, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.line = element_line(linewidth = 0.9, color = "black"),
    axis.ticks = element_line(linewidth = 0.8, color = "black"),
    legend.position = "top",
    legend.text = element_text(size = 16, color = "black"),
    legend.key.size = unit(0.75, "cm"),
    plot.margin = margin(12, 12, 12, 12)
  )

ggsave(
  filename = file.path(output_dir, "Figure1_CIMMYT_Percentiles_FAO_FINAL.png"),
  plot = p,
  width = fig_width,
  height = fig_height,
  dpi = fig_dpi,
  limitsize = FALSE
)

ggsave(
  filename = file.path(output_dir, "Figure1_CIMMYT_Percentiles_FAO_FINAL.pdf"),
  plot = p,
  width = fig_width,
  height = fig_height,
  limitsize = FALSE
)

message("Done.")
message("Outputs saved in: ", output_dir)
message("Main figure: Figure1_CIMMYT_Percentiles_FAO_FINAL.png")
message("Regression table: Figure1_CIMMYT_Percentile_Regression_Stats.csv")

print(reg_stats)
