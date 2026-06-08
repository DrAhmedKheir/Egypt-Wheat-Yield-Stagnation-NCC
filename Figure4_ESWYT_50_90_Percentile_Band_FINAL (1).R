# =============================================================================
# FINAL Nature Climate Change style Figure 4
# ESWYT line-level yield distribution in Egypt:
# 50th percentile (median) and 90th percentile yield potential
#
# Corrected according to Senthold + Ahmed:
#   - y-axis starts at 0
#   - no vertical breakpoint lines
#   - no error bars/lineranges
#   - grey band shows 50th–90th percentile range
#   - 90th percentile: TWO regressions
#       1981–2009 and 2010–2021
#   - 50th percentile: THREE regressions
#       1981–1999, 2000–2009, and 2010–2021
# =============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readr)
library(janitor)
library(grid)

base_dir <- "D:/Asseng_paper2024/Updated/Asseng_updated/40YRSSIMULATIONS/ForShawn/21072025/21072025/Update27032026/UrslastFilesCVs"
yield_file <- file.path(base_dir, "ESWYT1-41_EGYPT_GID_Yield_Pheno_20260602.xlsx")

output_dir <- file.path(base_dir, "Figure4_ESWYT_50_90_Percentiles_FINAL")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

convert_yield_kg_ha_to_t_ha <- FALSE

fig_width  <- 14
fig_height <- 8.5
fig_dpi    <- 500

col_q50 <- "#1F78B4"
col_q90 <- "#E31A1C"
band_col <- "grey55"

find_col <- function(df, patterns) {
  hits <- names(df)[str_detect(names(df), regex(paste(patterns, collapse = "|"), ignore_case = TRUE))]
  if (length(hits) == 0) return(NA_character_)
  hits[1]
}

clean_location <- function(x) {
  x %>%
    as.character() %>%
    str_to_lower() %>%
    str_replace_all("[-_\\.]", " ") %>%
    str_squish()
}

safe_lm_stats <- function(annual, y_col, period_name, start_year, end_year) {
  dat2 <- annual %>%
    filter(HarvestYr >= start_year, HarvestYr <= end_year) %>%
    filter(!is.na(.data[[y_col]]), !is.na(HarvestYr))

  if (nrow(dat2) < 3) {
    return(tibble(
      Metric = y_col,
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

  fit <- lm(reformulate("HarvestYr", response = y_col), data = dat2)
  sm <- summary(fit)

  tibble(
    Metric = y_col,
    Period = period_name,
    Start_year = min(dat2$HarvestYr, na.rm = TRUE),
    End_year = max(dat2$HarvestYr, na.rm = TRUE),
    Slope_t_ha_per_year = coef(fit)[["HarvestYr"]],
    Slope_t_ha_per_decade = coef(fit)[["HarvestYr"]] * 10,
    R2 = sm$r.squared,
    P_value = sm$coefficients["HarvestYr", "Pr(>|t|)"],
    N_years = nrow(dat2)
  )
}

make_trend_df <- function(annual, y_col, period_label, start_year, end_year) {
  dat2 <- annual %>%
    filter(HarvestYr >= start_year, HarvestYr <= end_year) %>%
    filter(!is.na(.data[[y_col]]))

  if (nrow(dat2) < 3) return(NULL)

  fit <- lm(reformulate("HarvestYr", response = y_col), data = dat2)
  years <- seq(min(dat2$HarvestYr), max(dat2$HarvestYr), by = 1)

  tibble(
    HarvestYr = years,
    y_pred = predict(fit, newdata = tibble(HarvestYr = years)),
    Metric = y_col,
    Period = period_label
  )
}

raw <- read_excel(yield_file, sheet = 1) %>% clean_names()

message("Detected columns:")
print(names(raw))

year_col  <- find_col(raw, c("harvest", "^year$", "yr"))
yield_col <- find_col(raw, c("blueorave_yld", "blue", "ave_yld", "yld", "yield"))
gid_col   <- find_col(raw, c("^gid$", "genotype", "line"))
loc_col   <- find_col(raw, c("loc_desc", "location", "site", "loc"))

if (any(is.na(c(year_col, yield_col)))) {
  stop("Could not detect year or yield column. Check printed column names.")
}

dat <- raw %>%
  transmute(
    HarvestYr = as.integer(.data[[year_col]]),
    Yield_raw = as.numeric(.data[[yield_col]]),
    GID = if (!is.na(gid_col)) as.character(.data[[gid_col]]) else NA_character_,
    Location = if (!is.na(loc_col)) as.character(.data[[loc_col]]) else NA_character_
  ) %>%
  mutate(
    Yield = if (convert_yield_kg_ha_to_t_ha) Yield_raw / 1000 else Yield_raw,
    Location_clean = clean_location(Location)
  ) %>%
  filter(!is.na(HarvestYr), !is.na(Yield)) %>%
  arrange(HarvestYr)

write_csv(dat, file.path(output_dir, "Figure4_ESWYT_LineLevel_Cleaned_FINAL.csv"))

annual <- dat %>%
  group_by(HarvestYr) %>%
  summarise(
    n_records = n(),
    n_locations = n_distinct(Location_clean),
    n_gid = n_distinct(GID),
    yield_min = min(Yield, na.rm = TRUE),
    yield_q10 = quantile(Yield, 0.10, na.rm = TRUE, names = FALSE),
    yield_q25 = quantile(Yield, 0.25, na.rm = TRUE, names = FALSE),
    yield_q50 = quantile(Yield, 0.50, na.rm = TRUE, names = FALSE),
    yield_q75 = quantile(Yield, 0.75, na.rm = TRUE, names = FALSE),
    yield_q90 = quantile(Yield, 0.90, na.rm = TRUE, names = FALSE),
    yield_max = max(Yield, na.rm = TRUE),
    yield_mean = mean(Yield, na.rm = TRUE),
    yield_sd = sd(Yield, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(HarvestYr)

write_csv(annual, file.path(output_dir, "Figure4_ESWYT_50_90_Percentile_Band_Data_FINAL.csv"))

periods <- tibble::tribble(
  ~Metric,      ~Period,       ~start_year, ~end_year,
  "yield_q50", "1981-1999",    1981,        1999,
  "yield_q50", "2000-2009",    2000,        2009,
  "yield_q50", "2010-2021",    2010,        2021,
  "yield_q90", "1981-2009",    1981,        2009,
  "yield_q90", "2010-2021",    2010,        2021
)

trend_list <- lapply(seq_len(nrow(periods)), function(i) {
  make_trend_df(
    annual = annual,
    y_col = periods$Metric[i],
    period_label = periods$Period[i],
    start_year = periods$start_year[i],
    end_year = periods$end_year[i]
  )
})

trend_df <- bind_rows(trend_list) %>%
  mutate(
    MetricLabel = recode(Metric,
                         yield_q50 = "50th percentile (median)",
                         yield_q90 = "90th percentile")
  )

reg_list <- lapply(seq_len(nrow(periods)), function(i) {
  safe_lm_stats(
    annual = annual,
    y_col = periods$Metric[i],
    period_name = periods$Period[i],
    start_year = periods$start_year[i],
    end_year = periods$end_year[i]
  )
})

reg_stats <- bind_rows(reg_list) %>%
  mutate(
    Metric = recode(Metric,
                    yield_q50 = "50th percentile (median)",
                    yield_q90 = "90th percentile"),
    P_value_label = ifelse(is.na(P_value), NA_character_,
                           ifelse(P_value < 0.001, "<0.001", sprintf("%.3f", P_value))),
    R2_label = ifelse(is.na(R2), NA_character_, sprintf("%.2f", R2))
  )

write_csv(reg_stats, file.path(output_dir, "Figure4_ESWYT_PeriodRegression_Stats_FINAL.csv"))

first_year <- min(annual$HarvestYr, na.rm = TRUE)
last_year  <- max(annual$HarvestYr, na.rm = TRUE)

x_breaks <- seq(floor(first_year / 2) * 2, ceiling(last_year / 2) * 2, by = 2)
y_upper <- ceiling(max(annual$yield_q90, annual$yield_max, na.rm = TRUE) + 1)

p_main <- ggplot(annual, aes(x = HarvestYr)) +
  geom_ribbon(
    aes(ymin = yield_q50, ymax = yield_q90, fill = "50th–90th percentile band"),
    alpha = 0.30,
    color = NA
  ) +
  geom_line(aes(y = yield_q50, color = "50th percentile (median)"),
            linewidth = 1.45) +
  geom_point(aes(y = yield_q50, color = "50th percentile (median)"),
             size = 3.5) +
  geom_line(aes(y = yield_q90, color = "90th percentile"),
            linewidth = 1.45) +
  geom_point(aes(y = yield_q90, color = "90th percentile"),
             size = 3.5) +
  geom_line(
    data = trend_df %>% filter(Metric == "yield_q50"),
    aes(x = HarvestYr, y = y_pred, group = Period),
    color = col_q50,
    linetype = "dashed",
    linewidth = 1.25,
    inherit.aes = FALSE
  ) +
  geom_line(
    data = trend_df %>% filter(Metric == "yield_q90"),
    aes(x = HarvestYr, y = y_pred, group = Period),
    color = col_q90,
    linetype = "dashed",
    linewidth = 1.25,
    inherit.aes = FALSE
  ) +
  scale_x_continuous(
    breaks = x_breaks,
    limits = c(first_year - 0.5, last_year + 0.5),
    expand = expansion(mult = c(0.005, 0.005))
  ) +
  scale_y_continuous(
    limits = c(0, y_upper),
    breaks = seq(0, y_upper, by = 2),
    expand = expansion(mult = c(0, 0.03))
  ) +
  scale_color_manual(
    name = "",
    values = c("50th percentile (median)" = col_q50,
               "90th percentile" = col_q90)
  ) +
  scale_fill_manual(
    name = "",
    values = c("50th–90th percentile band" = band_col)
  ) +
  labs(
    title = "Figure 4. Temporal evolution of wheat genetic yield potential in Egypt",
    subtitle = paste0(
      "Annual 50th and 90th percentile yields from CIMMYT ESWYT trials; ",
      "dashed lines show period-specific linear regressions"
    ),
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
    legend.text = element_text(size = 17, color = "black"),
    legend.key.size = unit(0.75, "cm"),
    plot.margin = margin(12, 12, 12, 12)
  )

ggsave(file.path(output_dir, "Figure4_ESWYT_50_90_Percentile_Band_FINAL.png"),
       p_main, width = fig_width, height = fig_height, dpi = fig_dpi, limitsize = FALSE)

ggsave(file.path(output_dir, "Figure4_ESWYT_50_90_Percentile_Band_FINAL.pdf"),
       p_main, width = fig_width, height = fig_height, limitsize = FALSE)

p_box <- ggplot(dat, aes(x = factor(HarvestYr), y = Yield)) +
  geom_boxplot(outlier.shape = NA, fill = "grey90", color = "black", linewidth = 0.55) +
  geom_point(
    aes(color = Location_clean),
    position = position_jitter(width = 0.18, height = 0),
    size = 0.75,
    alpha = 0.40,
    show.legend = FALSE
  ) +
  stat_summary(fun = median, geom = "point", shape = 18, size = 3.7, color = col_q50) +
  scale_y_continuous(
    limits = c(0, y_upper),
    breaks = seq(0, y_upper, by = 2),
    expand = expansion(mult = c(0, 0.03))
  ) +
  labs(
    title = "Supplementary figure. Line-level ESWYT yield distribution by harvest year",
    subtitle = "Boxplots show full breeder-trial distributions; blue diamonds indicate annual medians",
    x = "Harvest year",
    y = expression("Grain yield (t ha"^{-1}*")")
  ) +
  theme_classic(base_size = 18) +
  theme(
    text = element_text(color = "black"),
    plot.title = element_text(face = "bold", size = 23),
    plot.subtitle = element_text(size = 18),
    axis.title = element_text(face = "bold", size = 20),
    axis.text = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    axis.line = element_line(linewidth = 0.9, color = "black"),
    axis.ticks = element_line(linewidth = 0.8, color = "black")
  )

ggsave(file.path(output_dir, "Supplementary_ESWYT_LineLevel_Boxplots_FINAL.png"),
       p_box, width = 18, height = 9, dpi = fig_dpi, limitsize = FALSE)

ggsave(file.path(output_dir, "Supplementary_ESWYT_LineLevel_Boxplots_FINAL.pdf"),
       p_box, width = 18, height = 9, limitsize = FALSE)

message("Done.")
message("Outputs saved in: ", output_dir)
message("Main figure: Figure4_ESWYT_50_90_Percentile_Band_FINAL.png")
message("Regression table: Figure4_ESWYT_PeriodRegression_Stats_FINAL.csv")
message("Supplementary boxplot: Supplementary_ESWYT_LineLevel_Boxplots_FINAL.png")

print(reg_stats)
