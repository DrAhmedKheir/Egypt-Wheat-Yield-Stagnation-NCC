library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# =============================================================================
# Paths
# =============================================================================

base_dir <- paste0(
  "D:/Asseng_paper2024/Updated/Asseng_updated/40YRSSIMULATIONS/",
  "ForShawn/21072025/21072025/Update27032026/UrslastFilesCVs/",
  "MainFiguresUpdate19062026/Figure_2"
)

data_file <- file.path(
  base_dir,
  "DatsetFig2.csv"
)

outdir <- file.path(
  base_dir,
  "Figure2_updated"
)

dir.create(
  outdir,
  recursive = TRUE,
  showWarnings = FALSE
)

# =============================================================================
# Read data and resolve duplicated Year columns
# =============================================================================

df_raw <- read_csv(
  data_file,
  show_col_types = FALSE,
  name_repair = "unique"
)

names(df_raw) <- trimws(names(df_raw))

year_columns <- grep(
  "^Year",
  names(df_raw),
  value = TRUE
)

if (length(year_columns) == 0) {
  stop(
    "No Year column was found in DatsetFig2.csv.\n",
    "Available columns:\n",
    paste(names(df_raw), collapse = ", ")
  )
}

# Use the first Year column, which is the climate time-series year
climate_year_col <- year_columns[1]

cat(
  "Climate year column used:",
  climate_year_col,
  "\n"
)

df <- df_raw %>%
  mutate(
    Year = as.integer(.data[[climate_year_col]])
  ) %>%
  filter(
    Year >= 1980,
    Year <= 2019
  ) %>%
  arrange(Year)

cat(
  "Year range:",
  min(df$Year, na.rm = TRUE),
  "to",
  max(df$Year, na.rm = TRUE),
  "\n"
)

# =============================================================================
# General settings
# =============================================================================

x_min <- 1979
x_max <- 2027

green_col <- "darkgreen"
red_col   <- "#b2182b"

# -----------------------------------------------------------------------------
# Exact typography settings used in Figure 1
# -----------------------------------------------------------------------------

axis_title_size  <- 22
axis_text_size   <- 18
panel_label_size <- 6

# Match Figure 1 symbols and regression-line thickness
circle_size      <- 4.2
triangle_size    <- 4.5
regression_width <- 1.25
errorbar_width   <- 0.45

# =============================================================================
# Common theme: copied from Figure 1
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
    color = "black"
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
  
  panel.border = element_rect(
    color = "black",
    fill = NA,
    linewidth = 0.8
  ),
  
  panel.grid = element_blank(),
  
  legend.position = "none"
)

# =============================================================================
# Function for panels a, b and d
# =============================================================================

make_stage_panel <- function(
    data,
    y1,
    sd1,
    y2,
    sd2,
    ylab,
    panel_lab,
    ylim_vals,
    breaks_vals,
    show_x = FALSE
) {
  
  required_columns <- c(
    "Year",
    y1,
    sd1,
    y2,
    sd2
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(data)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      "Missing required columns:\n",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  long_df <- data %>%
    select(
      Year,
      all_of(y1),
      all_of(sd1),
      all_of(y2),
      all_of(sd2)
    ) %>%
    pivot_longer(
      cols = c(
        all_of(y1),
        all_of(y2)
      ),
      names_to = "Variable",
      values_to = "Value"
    ) %>%
    mutate(
      SD = case_when(
        Variable == y1 ~ .data[[sd1]],
        Variable == y2 ~ .data[[sd2]],
        TRUE ~ NA_real_
      ),
      
      Stage = case_when(
        Variable == y1 ~ "Sowing - anthesis",
        Variable == y2 ~ "Anthesis - maturity",
        TRUE ~ NA_character_
      ),
      
      Stage = factor(
        Stage,
        levels = c(
          "Sowing - anthesis",
          "Anthesis - maturity"
        )
      )
    )
  
  p <- ggplot(
    long_df,
    aes(
      x = Year,
      y = Value,
      color = Stage,
      shape = Stage
    )
  ) +
    
    geom_errorbar(
      aes(
        ymin = Value - SD,
        ymax = Value + SD
      ),
      width = 0.25,
      linewidth = errorbar_width,
      alpha = 0.65
    ) +
    
    # Separate symbol sizes to match Figure 1
    geom_point(
      data = long_df %>%
        filter(Stage == "Sowing - anthesis"),
      size = circle_size
    ) +
    
    geom_point(
      data = long_df %>%
        filter(Stage == "Anthesis - maturity"),
      size = triangle_size
    ) +
    
    geom_smooth(
      method = "lm",
      se = FALSE,
      linewidth = regression_width
    ) +
    
    annotate(
      "text",
      x = 1980,
      y = ylim_vals[2],
      label = panel_lab,
      hjust = 0,
      vjust = 1.35,
      fontface = "bold",
      size = panel_label_size
    ) +
    
    scale_color_manual(
      values = c(
        "Sowing - anthesis" = green_col,
        "Anthesis - maturity" = red_col
      )
    ) +
    
    scale_shape_manual(
      values = c(
        "Sowing - anthesis" = 16,
        "Anthesis - maturity" = 17
      )
    ) +
    
    scale_x_continuous(
      limits = c(
        x_min,
        x_max
      ),
      breaks = seq(
        1979,
        2027,
        by = 2
      ),
      expand = c(
        0,
        0
      )
    ) +
    
    scale_y_continuous(
      limits = ylim_vals,
      breaks = breaks_vals,
      labels = scales::label_number(
        accuracy = 1
      ),
      expand = c(
        0,
        0
      )
    ) +
    
    labs(
      x = if (show_x) "Harvest year (-)" else NULL,
      y = ylab
    ) +
    
    theme_classic(
      base_size = 16
    ) +
    
    common_axis_theme +
    
    theme(
      plot.margin = margin(
        12,
        10,
        12,
        10
      )
    )
  
  if (!show_x) {
    
    p <- p +
      theme(
        axis.text.x = element_blank(),
        axis.title.x = element_blank()
      )
    
  } else {
    
    p <- p +
      theme(
        axis.text.x = element_text(
          face = "bold",
          size = axis_text_size,
          color = "black",
          angle = 90,
          vjust = 0.5,
          hjust = 1
        )
      )
  }
  
  return(p)
}

# =============================================================================
# Panel a: Maximum temperature
# =============================================================================

p_a <- make_stage_panel(
  data = df,
  
  y1  = "Mean_Tmax_SowToAnth",
  sd1 = "SDMean_Tmax_SowToAnth",
  
  y2  = "Mean_Tmax_AnthToMat",
  sd2 = "SDMean_Tmax_AnthToMat",
  
  ylab = expression(
    bold(
      Tmax ~ "(" * degree * C * ")"
    )
  ),
  
  panel_lab = "a)",
  
  ylim_vals = c(
    18,
    34
  ),
  
  breaks_vals = seq(
    18,
    34,
    by = 2
  ),
  
  show_x = FALSE
)

# =============================================================================
# Panel b: Minimum temperature
# =============================================================================

p_b <- make_stage_panel(
  data = df,
  
  y1  = "TminStoA",
  sd1 = "SDTminStoA",
  
  y2  = "TminAtoM",
  sd2 = "SDTminAtoM",
  
  ylab = expression(
    bold(
      Tmin ~ "(" * degree * C * ")"
    )
  ),
  
  panel_lab = "b)",
  
  ylim_vals = c(
    6,
    16
  ),
  
  breaks_vals = seq(
    6,
    16,
    by = 2
  ),
  
  show_x = FALSE
)

# =============================================================================
# Panel c: Hot–dry–windy events
# Amplified index completely excluded
# =============================================================================

required_hdw_columns <- c(
  "Year",
  "HDW_SowToAnth",
  "SDHDW_SowToAnth",
  "HDW_AnthToMat",
  "SDHDW_AnthToMat"
)

missing_hdw_columns <- setdiff(
  required_hdw_columns,
  names(df)
)

if (length(missing_hdw_columns) > 0) {
  stop(
    "Missing HDW columns:\n",
    paste(missing_hdw_columns, collapse = ", ")
  )
}

hdw_long <- df %>%
  select(
    Year,
    HDW_SowToAnth,
    SDHDW_SowToAnth,
    HDW_AnthToMat,
    SDHDW_AnthToMat
  ) %>%
  pivot_longer(
    cols = c(
      HDW_SowToAnth,
      HDW_AnthToMat
    ),
    names_to = "Variable",
    values_to = "HDW"
  ) %>%
  mutate(
    HDW_SD = case_when(
      Variable == "HDW_SowToAnth" ~ SDHDW_SowToAnth,
      Variable == "HDW_AnthToMat" ~ SDHDW_AnthToMat,
      TRUE ~ NA_real_
    ),
    
    Stage = case_when(
      Variable == "HDW_SowToAnth" ~ "Sowing - anthesis",
      Variable == "HDW_AnthToMat" ~ "Anthesis - maturity",
      TRUE ~ NA_character_
    ),
    
    Stage = factor(
      Stage,
      levels = c(
        "Sowing - anthesis",
        "Anthesis - maturity"
      )
    )
  )

p_c <- ggplot(
  hdw_long,
  aes(
    x = Year,
    y = HDW,
    color = Stage,
    shape = Stage
  )
) +
  
  geom_errorbar(
    aes(
      ymin = HDW - HDW_SD,
      ymax = HDW + HDW_SD
    ),
    width = 0.25,
    linewidth = errorbar_width,
    alpha = 0.65
  ) +
  
  geom_point(
    data = hdw_long %>%
      filter(Stage == "Sowing - anthesis"),
    size = circle_size
  ) +
  
  geom_point(
    data = hdw_long %>%
      filter(Stage == "Anthesis - maturity"),
    size = triangle_size
  ) +
  
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = regression_width
  ) +
  
  annotate(
    "text",
    x = 1980,
    y = 80,
    label = "c)",
    hjust = 0,
    vjust = 1.35,
    fontface = "bold",
    size = panel_label_size
  ) +
  
  scale_color_manual(
    values = c(
      "Sowing - anthesis" = green_col,
      "Anthesis - maturity" = red_col
    )
  ) +
  
  scale_shape_manual(
    values = c(
      "Sowing - anthesis" = 16,
      "Anthesis - maturity" = 17
    )
  ) +
  
  scale_x_continuous(
    limits = c(
      x_min,
      x_max
    ),
    breaks = seq(
      1979,
      2027,
      by = 2
    ),
    expand = c(
      0,
      0
    )
  ) +
  
  scale_y_continuous(
    name = "HDW (hrs)",
    limits = c(
      -20,
      80
    ),
    breaks = seq(
      -20,
      80,
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
    x = NULL
  ) +
  
  theme_classic(
    base_size = 16
  ) +
  
  common_axis_theme +
  
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    
    plot.margin = margin(
      12,
      10,
      12,
      10
    )
  )

# =============================================================================
# Panel d: Solar radiation
# =============================================================================

p_d <- make_stage_panel(
  data = df,
  
  y1  = "SRADStoA",
  sd1 = "SDSRADStoA",
  
  y2  = "SRADAtoM",
  sd2 = "SDSRADAtoM",
  
  ylab = expression(
    bold(
      SRAD ~ "(MJ m"^{-2} * ")"
    )
  ),
  
  panel_lab = "d)",
  
  ylim_vals = c(
    12,
    28
  ),
  
  breaks_vals = seq(
    12,
    28,
    by = 2
  ),
  
  show_x = TRUE
)

# =============================================================================
# Combine panels
# =============================================================================

fig2 <- (
  p_a /
    p_b /
    p_c /
    p_d
) +
  plot_layout(
    heights = c(
      1,
      1,
      1,
      1
    )
  )

# =============================================================================
# Save outputs
# Important: same width as Figure 1
# =============================================================================

png_file <- file.path(
  outdir,
  "Figure2_FINAL_exact_font_match_Figure1.png"
)

pdf_file <- file.path(
  outdir,
  "Figure2_FINAL_exact_font_match_Figure1.pdf"
)

ggsave(
  filename = png_file,
  plot = fig2,
  width = 11.5,
  height = 17.25,
  dpi = 600,
  bg = "white",
  limitsize = FALSE
)

ggsave(
  filename = pdf_file,
  plot = fig2,
  width = 11.5,
  height = 17.25,
  bg = "white",
  limitsize = FALSE
)

print(fig2)

cat(
  "\nUpdated Figure 2 saved to:\n",
  png_file,
  "\n",
  pdf_file,
  "\n"
)