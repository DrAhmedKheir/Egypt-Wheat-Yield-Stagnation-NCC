library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(jsonlite)

# =============================================================================
# Paths
# =============================================================================

outdir <- "D:/Asseng_paper2024/Updated/Asseng_updated/40YRSSIMULATIONS/ForShawn/21072025/21072025/Update27032026/UrslastFilesCVs/MainFiguresUpdate19062026/Figure_1"

fao_yield_file <- file.path(
  outdir,
  "Figure1_FAO_yield_data.csv"
)

cimmyt_file <- file.path(
  outdir,
  "Figure1_CIMMYT_annual_50_90_percentiles.csv"
)

import_file <- file.path(
  outdir,
  "FAOSTAT_data_en_7-3-2026.csv"
)

# =============================================================================
# General settings
# =============================================================================

x_min <- 1979
x_max <- 2027

import_col <- "#e34a33"
fao_col    <- "#b2182b"
green_col  <- "darkgreen"

axis_title_size  <- 22
axis_text_size   <- 18
legend_text_size <- 13

common_axis_theme <- theme(
  axis.title = element_text(
    face = "bold",
    size = axis_title_size,
    color = "black"
  ),
  
  axis.title.y = element_text(
    face = "bold",
    size = axis_title_size,
    color = "black"
  ),
  
  axis.title.y.right = element_text(
    face = "bold",
    size = axis_title_size,
    color = "black",
    angle = 270,
    margin = margin(l = 10)
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
  
  axis.line = element_line(
    linewidth = 1.0,
    color = "black"
  ),
  
  axis.ticks = element_line(
    linewidth = 0.8,
    color = "black"
  )
)

# =============================================================================
# Panel a: Population and wheat imports
# =============================================================================

imports <- read_csv(
  import_file,
  show_col_types = FALSE
) %>%
  filter(
    Item == "Wheat",
    Element == "Import quantity"
  ) %>%
  transmute(
    Year = as.integer(Year),
    Wheat_imports_Mt = as.numeric(Value) / 1e6
  ) %>%
  filter(
    Year >= x_min,
    Year <= x_max
  )

pop_url <- paste0(
  "https://api.worldbank.org/v2/country/EGY/",
  "indicator/SP.POP.TOTL?format=json&per_page=20000"
)

pop_raw <- fromJSON(pop_url)[[2]]

population <- pop_raw %>%
  transmute(
    Year = as.integer(date),
    Population_million = as.numeric(value) / 1e6
  ) %>%
  filter(
    Year >= x_min,
    Year <= x_max
  ) %>%
  arrange(Year)

panel_a <- full_join(
  population,
  imports,
  by = "Year"
)

# -----------------------------------------------------------------------------
# Fixed primary and secondary axes
# Left axis:  0–125 million people
# Right axis: 0–16 Mt wheat imports
# -----------------------------------------------------------------------------

population_axis_max <- 125
imports_axis_max    <- 16

scale_factor <- population_axis_max / imports_axis_max

p_a <- ggplot(
  panel_a,
  aes(x = Year)
) +
  
  # Population observations
  geom_point(
    aes(
      y = Population_million
    ),
    color = "black",
    shape = 16,
    size = 4.2
  ) +
  
  # Population regression
  geom_smooth(
    aes(
      y = Population_million
    ),
    method = "lm",
    se = FALSE,
    color = "black",
    linewidth = 1.25
  ) +
  
  # Wheat-import observations
  geom_point(
    aes(
      y = Wheat_imports_Mt * scale_factor
    ),
    color = import_col,
    shape = 17,
    size = 4.5
  ) +
  
  # Wheat-import regression
  geom_smooth(
    aes(
      y = Wheat_imports_Mt * scale_factor
    ),
    method = "lm",
    se = FALSE,
    color = import_col,
    linewidth = 1.25
  ) +
  
  annotate(
    "text",
    x = 1980,
    y = Inf,
    label = "a)",
    hjust = 0,
    vjust = 1.35,
    fontface = "bold",
    size = 6
  ) +
  
  scale_x_continuous(
    limits = c(x_min, x_max),
    breaks = seq(1979, 2027, by = 2),
    expand = c(0, 0)
  ) +
  
  scale_y_continuous(
    name = "Population (million)",
    
    limits = c(
      0,
      population_axis_max
    ),
    
    breaks = seq(
      0,
      population_axis_max,
      by = 25
    ),
    
    labels = scales::label_number(
      accuracy = 1
    ),
    
    expand = c(0, 0),
    
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "Wheat imports (Mt)",
      breaks = seq(
        0,
        imports_axis_max,
        by = 2
      ),
      labels = scales::label_number(
        accuracy = 1
      )
    )
  ) +
  
  labs(
    x = NULL
  ) +
  
  theme_classic(
    base_size = 16
  ) +
  
  common_axis_theme +
  
  theme(
    # Remove panel-a legend
    legend.position = "none",
    
    axis.text.x = element_blank(),
    
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.8
    ),
    
    panel.grid = element_blank(),
    
    plot.margin = margin(
      6,
      10,
      18,
      10
    )
  )

# =============================================================================
# Panel b: CIMMYT genetic gain and FAO national yield
# =============================================================================

fao_yield <- read_csv(
  fao_yield_file,
  show_col_types = FALSE
) %>%
  transmute(
    Year = as.integer(Year),
    FAO_yield = as.numeric(FAO_yield)
  ) %>%
  filter(
    Year >= 1981,
    Year <= 2021
  )

cimmyt <- read_csv(
  cimmyt_file,
  show_col_types = FALSE
) %>%
  transmute(
    Year = as.integer(Year),
    P50 = as.numeric(yield_q50),
    P90 = as.numeric(yield_q90)
  ) %>%
  filter(
    Year >= 1981,
    Year <= 2021
  ) %>%
  filter(
    !is.na(P50),
    !is.na(P90)
  ) %>%
  arrange(Year)

panel_b <- full_join(
  cimmyt,
  fao_yield,
  by = "Year"
) %>%
  arrange(Year)

p_b <- ggplot() +
  
  # P50–P90 yield range
  geom_ribbon(
    data = cimmyt,
    aes(
      x = Year,
      ymin = P50,
      ymax = P90
    ),
    fill = "grey80",
    alpha = 0.55,
    color = NA
  ) +
  
  # P50 observations
  geom_point(
    data = panel_b,
    aes(
      x = Year,
      y = P50
    ),
    color = "black",
    shape = 16,
    size = 4.2
  ) +
  
  # P90 observations
  geom_point(
    data = panel_b,
    aes(
      x = Year,
      y = P90
    ),
    color = green_col,
    shape = 16,
    size = 4.2
  ) +
  
  # National FAO observations
  geom_point(
    data = panel_b,
    aes(
      x = Year,
      y = FAO_yield
    ),
    color = fao_col,
    shape = 17,
    size = 4.5
  ) +
  
  # ---------------------------------------------------------------------------
# Segmented regressions only
# ---------------------------------------------------------------------------

# P50: 1981–2009
geom_smooth(
  data = panel_b %>%
    filter(
      Year >= 1981,
      Year <= 2009
    ),
  aes(
    x = Year,
    y = P50
  ),
  method = "lm",
  se = FALSE,
  color = "black",
  linewidth = 1.25
) +
  
  # P50: 2009–2021
  geom_smooth(
    data = panel_b %>%
      filter(
        Year >= 2009,
        Year <= 2021
      ),
    aes(
      x = Year,
      y = P50
    ),
    method = "lm",
    se = FALSE,
    color = "black",
    linewidth = 1.25
  ) +
  
  # P90: 1981–2009
  geom_smooth(
    data = panel_b %>%
      filter(
        Year >= 1981,
        Year <= 2009
      ),
    aes(
      x = Year,
      y = P90
    ),
    method = "lm",
    se = FALSE,
    color = green_col,
    linewidth = 1.25
  ) +
  
  # P90: 2009–2021
  geom_smooth(
    data = panel_b %>%
      filter(
        Year >= 2009,
        Year <= 2021
      ),
    aes(
      x = Year,
      y = P90
    ),
    method = "lm",
    se = FALSE,
    color = green_col,
    linewidth = 1.25
  ) +
  
  # FAO yield: 1981–1998
  geom_smooth(
    data = panel_b %>%
      filter(
        Year >= 1981,
        Year <= 1998
      ),
    aes(
      x = Year,
      y = FAO_yield
    ),
    method = "lm",
    se = FALSE,
    color = fao_col,
    linewidth = 1.25
  ) +
  
  # FAO yield: 1998–2020
  geom_smooth(
    data = panel_b %>%
      filter(
        Year >= 1998,
        Year <= 2020
      ),
    aes(
      x = Year,
      y = FAO_yield
    ),
    method = "lm",
    se = FALSE,
    color = fao_col,
    linewidth = 1.25
  ) +
  
  annotate(
    "text",
    x = 1980,
    y = 15.45,
    label = "b)",
    hjust = 0,
    fontface = "bold",
    size = 6
  ) +
  
  scale_x_continuous(
    limits = c(x_min, x_max),
    breaks = seq(
      1979,
      2027,
      by = 2
    ),
    expand = c(0, 0)
  ) +
  
  scale_y_continuous(
    limits = c(0, 16),
    breaks = seq(
      0,
      16,
      by = 2
    ),
    labels = scales::label_number(
      accuracy = 1
    ),
    expand = c(0, 0)
  ) +
  
  labs(
    x = "Harvest year (-)",
    y = "Grain yield (t ha⁻¹)"
  ) +
  
  theme_classic(
    base_size = 16
  ) +
  
  common_axis_theme +
  
  theme(
    # Remove panel-b legend
    legend.position = "none",
    
    axis.text.x = element_text(
      face = "bold",
      size = axis_text_size,
      color = "black",
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.8
    ),
    
    panel.grid = element_blank(),
    
    plot.margin = margin(
      20,
      10,
      6,
      10
    )
  )

# =============================================================================
# Combine panels
# =============================================================================

fig1 <- p_a / p_b +
  plot_layout(
    heights = c(
      0.75,
      1.25
    )
  )

# =============================================================================
# Save outputs
# =============================================================================

png_file <- file.path(
  outdir,
  "Figure1_FINAL_no_legends_import_axis_0_16.png"
)

pdf_file <- file.path(
  outdir,
  "Figure1_FINAL_no_legends_import_axis_0_16.pdf"
)

ggsave(
  filename = png_file,
  plot = fig1,
  width = 11.5,
  height = 9.8,
  dpi = 600,
  bg = "white",
  limitsize = FALSE
)

ggsave(
  filename = pdf_file,
  plot = fig1,
  width = 11.5,
  height = 9.8,
  bg = "white",
  limitsize = FALSE
)

print(fig1)

cat(
  "\nFigure 1 saved to:\n",
  png_file,
  "\n",
  pdf_file,
  "\n"
)