###############################################################################
# Figure 3
# Simulated MME yield, national survey yield, and wheat phenology
#
# Final updates:
#   1. Exact typography match with Figure 2
#   2. Legends removed from both panels
#   3. X-axis extended to 2027
#   4. X-axis title standardized as "Harvest year (-)"
#   5. Panel-a y-axis title split over two lines so the unit remains visible
#   6. Panel-a symbols increased by 20%
###############################################################################

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# =============================================================================
# Paths
# =============================================================================

base_dir <- "D:/MMENCC"

mme_file <- file.path(
  base_dir,
  "Fig3ANCC.xlsx"
)

survey_file <- file.path(
  base_dir,
  "SurveyYield.xlsx"
)

out_dir <- file.path(
  base_dir,
  "Figure3_MME_Yield_Phenology_Survey"
)

dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# =============================================================================
# Read simulated MME yield and phenology data
# =============================================================================

df <- read_excel(
  mme_file
) %>%
  mutate(
    Years = as.integer(Years),
    
    MMESim = as.numeric(MMESim),
    Sdev = as.numeric(Sdev),
    
    DaysToAnthesis = as.numeric(DaysToAnthesis),
    DaysToMaturity = as.numeric(DaysToMaturity),
    
    SDDaysToAnthesis = as.numeric(SDDaysToAnthesis),
    SDDaysToMaturity = as.numeric(SDDaysToMaturity)
  ) %>%
  filter(
    Years >= 1980,
    Years <= 2019
  ) %>%
  arrange(Years)

# =============================================================================
# Read national survey-yield data
# =============================================================================

survey <- read_excel(
  survey_file
) %>%
  mutate(
    SurveyHarvestYears = as.integer(SurveyHarvestYears),
    ObsSurveyGY = as.numeric(ObsSurveyGY)
  ) %>%
  filter(
    SurveyHarvestYears >= 2015,
    SurveyHarvestYears <= 2019,
    !is.na(ObsSurveyGY)
  ) %>%
  group_by(
    Years = SurveyHarvestYears
  ) %>%
  summarise(
    SurveyGY_mean = mean(
      ObsSurveyGY,
      na.rm = TRUE
    ),
    
    SurveyGY_sd = sd(
      ObsSurveyGY,
      na.rm = TRUE
    ),
    
    n_sites = n(),
    
    .groups = "drop"
  )

write.csv(
  survey,
  file.path(
    out_dir,
    "Figure3_Survey_Annual_Mean_2015_2019.csv"
  ),
  row.names = FALSE
)

# =============================================================================
# Regression statistics
# =============================================================================

lm_stats <- function(data, x, y) {
  
  fit <- lm(
    as.formula(
      paste(y, "~", x)
    ),
    data = data
  )
  
  fit_summary <- summary(fit)
  
  data.frame(
    slope = coef(fit)[2],
    r2 = fit_summary$r.squared,
    pval = coef(fit_summary)[2, 4]
  )
}

fmt_p <- function(p) {
  
  ifelse(
    p < 0.001,
    "<0.001",
    sprintf("%.3f", p)
  )
}

reg_mme <- lm_stats(
  df,
  "Years",
  "MMESim"
)

reg_survey <- lm_stats(
  survey,
  "Years",
  "SurveyGY_mean"
)

reg_anth <- lm_stats(
  df,
  "Years",
  "DaysToAnthesis"
)

reg_mat <- lm_stats(
  df,
  "Years",
  "DaysToMaturity"
)

caption_stats <- paste0(
  "Simulated MME yield: slope = ",
  sprintf("%.3f", reg_mme$slope),
  " t ha-1 yr-1, R2 = ",
  sprintf("%.2f", reg_mme$r2),
  ", p = ",
  fmt_p(reg_mme$pval),
  ". ",
  
  "National survey yield: slope = ",
  sprintf("%.3f", reg_survey$slope),
  " t ha-1 yr-1, R2 = ",
  sprintf("%.2f", reg_survey$r2),
  ", p = ",
  fmt_p(reg_survey$pval),
  ". ",
  
  "Days to anthesis: slope = ",
  sprintf("%.3f", reg_anth$slope),
  " d yr-1, R2 = ",
  sprintf("%.2f", reg_anth$r2),
  ", p = ",
  fmt_p(reg_anth$pval),
  ". ",
  
  "Days to maturity: slope = ",
  sprintf("%.3f", reg_mat$slope),
  " d yr-1, R2 = ",
  sprintf("%.2f", reg_mat$r2),
  ", p = ",
  fmt_p(reg_mat$pval),
  "."
)

writeLines(
  caption_stats,
  file.path(
    out_dir,
    "Figure3_regression_statistics_caption.txt"
  )
)

# =============================================================================
# General figure settings
# =============================================================================

x_min <- 1979
x_max <- 2027

major_x_breaks <- seq(
  1979,
  2027,
  by = 2
)

minor_x_breaks <- seq(
  1979,
  2027,
  by = 1
)

survey_green <- "#006400"
anth_green   <- "#1B9E77"

# -----------------------------------------------------------------------------
# Exact typography copied from Figure 2
# -----------------------------------------------------------------------------

axis_title_size  <- 22
axis_text_size   <- 18
panel_label_size <- 6

# Figure 2 symbol sizes
circle_size   <- 4.2
triangle_size <- 4.5

# Panel-a symbols increased by exactly 20%
panel_a_mme_size    <- circle_size * 1.20
panel_a_survey_size <- 5.0 * 1.20

regression_width <- 1.25
errorbar_width   <- 0.45

# =============================================================================
# Common theme copied from Figure 2
# =============================================================================

common_axis_theme <- theme(
  axis.title = element_text(
    face = "bold",
    size = axis_title_size,
    color = "black"
  ),
  
  axis.title.y = element_text(
    face = "bold",
    size = axis_title_size,
    color = "black",
    lineheight = 0.95,
    margin = margin(r = 10)
  ),
  
  axis.title.x = element_text(
    face = "bold",
    size = axis_title_size,
    color = "black"
  ),
  
  axis.text = element_text(
    face = "bold",
    size = axis_text_size,
    color = "black"
  ),
  
  axis.text.x = element_text(
    face = "bold",
    size = axis_text_size,
    color = "black"
  ),
  
  axis.text.y = element_text(
    face = "bold",
    size = axis_text_size,
    color = "black"
  ),
  
  axis.line = element_line(
    linewidth = 1.0,
    color = "black"
  ),
  
  axis.ticks = element_line(
    linewidth = 0.8,
    color = "black"
  ),
  
  axis.ticks.length = unit(
    0.22,
    "cm"
  ),
  
  panel.border = element_rect(
    color = "black",
    fill = NA,
    linewidth = 0.8
  ),
  
  panel.grid = element_blank(),
  
  legend.position = "none"
)

# =============================================================================
# Panel a
# Simulated MME yield and national survey yield
# =============================================================================

p1 <- ggplot(
  df,
  aes(x = Years)
) +
  
  # Error bars for simulated MME yield
  geom_errorbar(
    aes(
      ymin = MMESim - Sdev,
      ymax = MMESim + Sdev
    ),
    width = 0.25,
    linewidth = errorbar_width,
    color = "black",
    alpha = 0.80
  ) +
  
  # Simulated MME yield: open circles enlarged by 20%
  geom_point(
    aes(
      y = MMESim
    ),
    shape = 21,
    size = panel_a_mme_size,
    stroke = 1.15,
    color = "black",
    fill = "white"
  ) +
  
  # Simulated-yield regression line
  geom_smooth(
    aes(
      y = MMESim
    ),
    method = "lm",
    se = FALSE,
    color = "black",
    linewidth = regression_width
  ) +
  
  # National survey yield: green diamonds enlarged by 20%
  geom_point(
    data = survey,
    aes(
      x = Years,
      y = SurveyGY_mean
    ),
    inherit.aes = FALSE,
    shape = 18,
    size = panel_a_survey_size,
    color = survey_green,
    fill = survey_green
  ) +
  
  annotate(
    "text",
    x = 1980,
    y = 14,
    label = "a)",
    hjust = 0,
    vjust = 1.35,
    fontface = "bold",
    size = panel_label_size
  ) +
  
  scale_x_continuous(
    limits = c(
      x_min,
      x_max
    ),
    breaks = major_x_breaks,
    minor_breaks = minor_x_breaks,
    expand = c(
      0,
      0
    )
  ) +
  
  scale_y_continuous(
    limits = c(
      0,
      14
    ),
    breaks = seq(
      0,
      14,
      by = 2
    ),
    labels = scales::label_number(
      accuracy = 1
    ),
    expand = c(
      0,
      0
    )
  ) +
  
  labs(
    x = NULL,
    
    # Two-line title prevents the unit from being clipped
    y = "Simulated and survey yield\n(t ha⁻¹)"
  ) +
  
  theme_classic(
    base_size = 16
  ) +
  
  common_axis_theme +
  
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    
    plot.margin = margin(
      t = 12,
      r = 10,
      b = 12,
      l = 24
    )
  )

# =============================================================================
# Prepare phenology data for panel b
# =============================================================================

phenology_long <- df %>%
  select(
    Years,
    DaysToAnthesis,
    SDDaysToAnthesis,
    DaysToMaturity,
    SDDaysToMaturity
  ) %>%
  pivot_longer(
    cols = c(
      DaysToAnthesis,
      DaysToMaturity
    ),
    names_to = "Phenology",
    values_to = "Days"
  ) %>%
  mutate(
    SD = case_when(
      Phenology == "DaysToAnthesis" ~ SDDaysToAnthesis,
      Phenology == "DaysToMaturity" ~ SDDaysToMaturity,
      TRUE ~ NA_real_
    ),
    
    Phenology = recode(
      Phenology,
      DaysToAnthesis = "Days to anthesis",
      DaysToMaturity = "Days to maturity"
    ),
    
    Phenology = factor(
      Phenology,
      levels = c(
        "Days to anthesis",
        "Days to maturity"
      )
    )
  )

# =============================================================================
# Panel b
# Days to anthesis and maturity
# =============================================================================

p2 <- ggplot(
  phenology_long,
  aes(
    x = Years,
    y = Days,
    color = Phenology,
    shape = Phenology
  )
) +
  
  geom_errorbar(
    aes(
      ymin = Days - SD,
      ymax = Days + SD
    ),
    width = 0.25,
    linewidth = errorbar_width,
    alpha = 0.80
  ) +
  
  # Days to anthesis
  geom_point(
    data = phenology_long %>%
      filter(
        Phenology == "Days to anthesis"
      ),
    size = circle_size
  ) +
  
  # Days to maturity
  geom_point(
    data = phenology_long %>%
      filter(
        Phenology == "Days to maturity"
      ),
    size = triangle_size
  ) +
  
  # Regression lines
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = regression_width
  ) +
  
  annotate(
    "text",
    x = 1980,
    y = 180,
    label = "b)",
    hjust = 0,
    vjust = 1.35,
    fontface = "bold",
    size = panel_label_size
  ) +
  
  scale_color_manual(
    values = c(
      "Days to anthesis" = anth_green,
      "Days to maturity" = "black"
    )
  ) +
  
  scale_shape_manual(
    values = c(
      "Days to anthesis" = 16,
      "Days to maturity" = 17
    )
  ) +
  
  scale_x_continuous(
    limits = c(
      x_min,
      x_max
    ),
    breaks = major_x_breaks,
    minor_breaks = minor_x_breaks,
    expand = c(
      0,
      0
    )
  ) +
  
  scale_y_continuous(
    limits = c(
      100,
      180
    ),
    breaks = seq(
      100,
      180,
      by = 20
    ),
    labels = scales::label_number(
      accuracy = 1
    ),
    expand = c(
      0,
      0
    )
  ) +
  
  labs(
    x = "Harvest year (-)",
    y = "Days"
  ) +
  
  theme_classic(
    base_size = 16
  ) +
  
  common_axis_theme +
  
  theme(
    axis.text.x = element_text(
      face = "bold",
      size = axis_text_size,
      color = "black",
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    
    plot.margin = margin(
      t = 12,
      r = 10,
      b = 12,
      l = 24
    )
  )

# =============================================================================
# Combine panels
# =============================================================================

fig <- p1 / p2 +
  plot_layout(
    heights = c(
      1,
      1
    )
  )

# =============================================================================
# Save outputs
# Same width as Figure 2 to preserve identical physical font sizes
# =============================================================================

png_file <- file.path(
  out_dir,
  "Figure3_FINAL_Figure2_fonts_complete_unit_symbols_plus20percent.png"
)

pdf_file <- file.path(
  out_dir,
  "Figure3_FINAL_Figure2_fonts_complete_unit_symbols_plus20percent.pdf"
)

ggsave(
  filename = png_file,
  plot = fig,
  width = 11.5,
  height = 10.0,
  dpi = 600,
  bg = "white",
  limitsize = FALSE
)

ggsave(
  filename = pdf_file,
  plot = fig,
  width = 11.5,
  height = 10.0,
  bg = "white",
  limitsize = FALSE
)

print(fig)

cat(
  "\nUpdated Figure 3 saved to:\n",
  png_file,
  "\n",
  pdf_file,
  "\n\n"
)

cat(caption_stats)