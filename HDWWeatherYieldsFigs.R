# ============================================================
# FINAL ENHANCED SCRIPT v2
#
# Creates:
#   1) Figure1_scatter_only.png
#   2) Figure1_distributions_only.png
#   3) Figure2_chord_sensitivity.png
#   4) Figure2_panelA_chord.png
#
# Uses:
#   D:/HourlyHDW/ForDensityEra5.csv
#   D:/HourlyHDW/ForDensitywithAllindicesEra5.csv
#
# Fixes:
#   - chord always includes FAO, CIMMYT, Survey, Simulated when present
#   - Tmin/Tmean distributions use correct stage colors
#   - larger fonts for both figures
#   - expanded x-axis for first row scatter panels
#
# Install once if needed:
# install.packages(c(
#   "tidyverse", "ggforce", "ggpmisc", "patchwork",
#   "broom", "circlize", "png", "cowplot"
# ))
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggforce)
  library(ggpmisc)
  library(patchwork)
  library(broom)
  library(circlize)
  library(png)
  library(cowplot)
  library(grid)
})

# ------------------------------------------------------------
# 1) Paths
# ------------------------------------------------------------
base_dir <- "D:/HourlyHDW"

file_density_basic <- file.path(base_dir, "ForDensityEra5.csv")
file_density_all   <- file.path(base_dir, "ForDensitywithAllindicesEra5.csv")

out_fig1_scatter <- file.path(base_dir, "Figure1_scatter_only.png")
out_fig1_dist    <- file.path(base_dir, "Figure1_distributions_only.png")
out_fig2         <- file.path(base_dir, "Figure2_chord_sensitivity.png")
out_chord_png    <- file.path(base_dir, "Figure2_panelA_chord.png")

# ------------------------------------------------------------
# 2) Styling
# ------------------------------------------------------------
base_font_size <- 28
title_size     <- 34
axis_title_sz  <- 26
axis_text_sz   <- 19
legend_text_sz <- 19
legend_title_sz<- 21
eq_text_size   <- 6.2

theme_big <- function() {
  theme_classic(base_size = base_font_size) +
    theme(
      plot.title   = element_text(size = title_size, face = "bold"),
      axis.title   = element_text(size = axis_title_sz, face = "bold"),
      axis.text    = element_text(size = axis_text_sz, color = "black"),
      legend.title = element_text(size = legend_title_sz, face = "bold"),
      legend.text  = element_text(size = legend_text_sz),
      legend.position = "bottom"
    )
}

# ------------------------------------------------------------
# 3) Helpers
# ------------------------------------------------------------
safe_read_csv <- function(path) {
  readr::read_csv(path, show_col_types = FALSE)
}

get_existing <- function(df, candidates) {
  candidates[candidates %in% names(df)]
}

get_group_vars <- function(df, group_name) {
  nm <- names(df)
  if (group_name == "HDW")   return(nm[str_detect(nm, "^HDW_")])
  if (group_name == "Tmax")  return(nm[str_detect(nm, "Tmax")])
  if (group_name == "Tmin")  return(nm[str_detect(nm, "Tmin")])
  if (group_name == "Tmean") return(nm[str_detect(nm, "Tmean")])
  character(0)
}

# robust stage extractor
extract_stage <- function(x) {
  case_when(
    str_detect(x, "AnthToMat|AtoM|Anthesis.?Maturity") ~ "Anthesis–Maturity",
    str_detect(x, "SowToAnth|StoA|Sow.?Anthesis") ~ "Sow–Anthesis",
    str_detect(x, "SowToMat|StoM|Sow.?Maturity") ~ "Sow–Maturity",
    TRUE ~ x
  )
}

stage_label_from_name <- function(x) {
  x %>%
    str_replace("^HDW_", "") %>%
    str_replace("^Mean_Tmax_", "") %>%
    str_replace("^Mean_Tmin_", "") %>%
    str_replace("^Mean_Tmean_", "") %>%
    str_replace("^Tmax", "") %>%
    str_replace("^Tmin", "") %>%
    str_replace("^Tmean", "") %>%
    extract_stage()
}

clean_index_label <- function(x) {
  x %>%
    str_replace("^Mean_", "") %>%
    str_replace("^HDW_", "HDW ") %>%
    str_replace_all("_", " ") %>%
    str_squish() %>%
    str_replace("StoA", " Sow–Anthesis") %>%
    str_replace("AtoM", " Anthesis–Maturity") %>%
    str_replace("StoM", " Sow–Maturity") %>%
    str_replace("SowToAnth", " Sow–Anthesis") %>%
    str_replace("SowToMat", " Sow–Maturity") %>%
    str_replace("AnthToMat", " Anthesis–Maturity") %>%
    str_squish()
}

save_plot <- function(p, filename, width = 16, height = 12, dpi = 320) {
  ggsave(
    filename = filename,
    plot = p,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white",
    limitsize = FALSE
  )
}

yield_name_map <- c(
  "FAOAvgYield" = "FAO",
  "CIMMYT_MedianBLUEYield" = "CIMMYT",
  "SurveyYiled" = "Survey",
  "SimYield" = "Simulated"
)

yield_colors <- c(
  "FAO" = "darkorange",
  "CIMMYT" = "steelblue",
  "Survey" = "forestgreen",
  "Simulated" = "purple4"
)

yield_shapes <- c(
  "FAO" = 16,
  "CIMMYT" = 17,
  "Survey" = 15,
  "Simulated" = 18
)

stage_levels <- c("Anthesis–Maturity", "Sow–Anthesis", "Sow–Maturity")

stage_colors <- c(
  "Anthesis–Maturity" = "#F8766D",
  "Sow–Anthesis"      = "#00BA38",
  "Sow–Maturity"      = "#619CFF"
)

prepare_stage_factor <- function(x) {
  factor(extract_stage(x), levels = stage_levels)
}

# ------------------------------------------------------------
# 4) Read data
# ------------------------------------------------------------
df1 <- safe_read_csv(file_density_basic)
colnames(df1) <- make.names(colnames(df1))

df2 <- safe_read_csv(file_density_all)
colnames(df2) <- make.names(colnames(df2))

# ------------------------------------------------------------
# 5) FIGURE 1A - scatter only
# ------------------------------------------------------------
yield_vars_fig1 <- get_existing(df1, c(
  "FAOAvgYield", "CIMMYT_MedianBLUEYield", "SurveyYiled", "SimYield"
))

df_yield_long <- df1 %>%
  pivot_longer(
    cols = all_of(yield_vars_fig1),
    names_to = "YieldSource",
    values_to = "Yield"
  ) %>%
  mutate(YieldSource = recode(YieldSource, !!!yield_name_map)) %>%
  filter(!is.na(Yield))

hdw_vars_fig1   <- get_group_vars(df1, "HDW")
tmax_vars_fig1  <- get_group_vars(df1, "Tmax")
tmin_vars_fig1  <- get_group_vars(df1, "Tmin")
tmean_vars_fig1 <- get_group_vars(df1, "Tmean")

tmin_vars_fig1  <- setdiff(tmin_vars_fig1, tmax_vars_fig1)
tmean_vars_fig1 <- setdiff(tmean_vars_fig1, c(tmax_vars_fig1, tmin_vars_fig1))

plot_scatter <- function(data, xvar, xlab, panel_letter,
                         x_expand = 0.06, y_expand = 0.08) {
  
  sub <- data %>% filter(!is.na(.data[[xvar]]), !is.na(Yield))
  
  if (nrow(sub) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = paste0(panel_letter, ". No data")) + theme_void())
  }
  
  x_range <- range(sub[[xvar]], na.rm = TRUE)
  y_range <- range(sub$Yield, na.rm = TRUE)
  
  x_diff <- diff(x_range)
  y_diff <- diff(y_range)
  
  if (x_diff == 0) x_diff <- 1
  if (y_diff == 0) y_diff <- 1
  
  x_limits <- c(x_range[1] - x_diff * x_expand, x_range[2] + x_diff * x_expand)
  y_limits <- c(y_range[1] - y_diff * y_expand, y_range[2] + y_diff * y_expand)
  
  ggplot(sub, aes(x = .data[[xvar]], y = Yield, color = YieldSource, shape = YieldSource)) +
    geom_point(size = 4.5, alpha = 0.75) +
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 1.35) +
    stat_poly_eq(
      aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~")),
      formula = y ~ x,
      parse = TRUE,
      size = eq_text_size,
      fontface = "bold"
    ) +
    geom_mark_ellipse(aes(fill = YieldSource), alpha = 0.08, show.legend = FALSE) +
    scale_color_manual(values = yield_colors, drop = FALSE) +
    scale_fill_manual(values = yield_colors, drop = FALSE) +
    scale_shape_manual(values = yield_shapes, drop = FALSE) +
    coord_cartesian(xlim = x_limits, ylim = y_limits) +
    labs(
      title = paste0(panel_letter, ". ", xlab, " vs Yield"),
      x = xlab,
      y = "Yield (t/ha)"
    ) +
    theme_big() +
    theme(
      legend.title = element_blank(),
      plot.title = element_text(size = 23, face = "bold"),
      axis.title = element_text(size = 20, face = "bold"),
      axis.text = element_text(size = 17)
    )
}

scatter_panels <- list()
panel_letters <- letters
idx <- 1

for (v in hdw_vars_fig1) {
  scatter_panels[[length(scatter_panels) + 1]] <-
    plot_scatter(
      df_yield_long, v,
      paste0("HDW Hours (", stage_label_from_name(v), ")"),
      panel_letters[idx],
      x_expand = 0.18, y_expand = 0.08
    )
  idx <- idx + 1
}

for (v in tmax_vars_fig1) {
  scatter_panels[[length(scatter_panels) + 1]] <-
    plot_scatter(
      df_yield_long, v,
      paste0("Tmax (°C, ", stage_label_from_name(v), ")"),
      panel_letters[idx], 0.08, 0.08
    )
  idx <- idx + 1
}

for (v in tmin_vars_fig1) {
  scatter_panels[[length(scatter_panels) + 1]] <-
    plot_scatter(
      df_yield_long, v,
      paste0("Tmin (°C, ", stage_label_from_name(v), ")"),
      panel_letters[idx], 0.08, 0.08
    )
  idx <- idx + 1
}

for (v in tmean_vars_fig1) {
  scatter_panels[[length(scatter_panels) + 1]] <-
    plot_scatter(
      df_yield_long, v,
      paste0("Tmean (°C, ", stage_label_from_name(v), ")"),
      panel_letters[idx], 0.08, 0.08
    )
  idx <- idx + 1
}

final_plot_fig1_scatter <- wrap_plots(scatter_panels, ncol = 3, guides = "collect") &
  theme(legend.position = "bottom", legend.title = element_blank())

save_plot(final_plot_fig1_scatter, out_fig1_scatter, width = 30, height = 23)

# ------------------------------------------------------------
# 6) FIGURE 1B - distributions only
# ------------------------------------------------------------
make_density_panel <- function(df, vars, value_name, group_label, letter_now) {
  if (length(vars) == 0) return(NULL)
  
  long_df <- df %>%
    pivot_longer(cols = all_of(vars), names_to = "StageRaw", values_to = value_name) %>%
    mutate(Stage = prepare_stage_factor(StageRaw))
  
  xlab <- case_when(
    group_label == "HDW"   ~ "HDW Hours",
    group_label == "Tmax"  ~ "Tmax (°C)",
    group_label == "Tmin"  ~ "Tmin (°C)",
    group_label == "Tmean" ~ "Tmean (°C)",
    TRUE ~ value_name
  )
  
  ggplot(long_df, aes(x = .data[[value_name]], fill = Stage, color = Stage)) +
    geom_density(alpha = 0.30, linewidth = 1.3, na.rm = TRUE) +
    scale_fill_manual(values = stage_colors, drop = FALSE, na.translate = FALSE) +
    scale_color_manual(values = stage_colors, drop = FALSE, na.translate = FALSE) +
    labs(
      title = paste0(letter_now, ". ", group_label, " Distribution"),
      x = xlab,
      y = "Density",
      fill = "Stage",
      color = "Stage"
    ) +
    theme_big() +
    theme(
      plot.title = element_text(size = 24, face = "bold"),
      axis.title = element_text(size = 21, face = "bold"),
      axis.text = element_text(size = 18)
    )
}

dist_panels <- list()
idx2 <- 1

p_hdw_density <- make_density_panel(df1, hdw_vars_fig1, "HDW", "HDW", letters[idx2])
if (!is.null(p_hdw_density)) { dist_panels[[length(dist_panels)+1]] <- p_hdw_density; idx2 <- idx2 + 1 }

p_tmax_density <- make_density_panel(df1, tmax_vars_fig1, "Tmax", "Tmax", letters[idx2])
if (!is.null(p_tmax_density)) { dist_panels[[length(dist_panels)+1]] <- p_tmax_density; idx2 <- idx2 + 1 }

p_tmin_density <- make_density_panel(df1, tmin_vars_fig1, "Tmin", "Tmin", letters[idx2])
if (!is.null(p_tmin_density)) { dist_panels[[length(dist_panels)+1]] <- p_tmin_density; idx2 <- idx2 + 1 }

p_tmean_density <- make_density_panel(df1, tmean_vars_fig1, "Tmean", "Tmean", letters[idx2])
if (!is.null(p_tmean_density)) { dist_panels[[length(dist_panels)+1]] <- p_tmean_density }

final_plot_fig1_dist <- wrap_plots(dist_panels, ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")

save_plot(final_plot_fig1_dist, out_fig1_dist, width = 22, height = 15)

# ------------------------------------------------------------
# 7) FIGURE 2 - chord + sensitivity / distribution
# ------------------------------------------------------------
yield_vars_fig2 <- get_existing(df2, c(
  "FAOAvgYield", "CIMMYT_MedianBLUEYield", "SurveyYiled", "SimYield"
))

climate_vars <- setdiff(names(df2), yield_vars_fig2)
climate_vars <- climate_vars[sapply(df2[climate_vars], is.numeric)]

df_std <- df2 %>%
  mutate(across(all_of(climate_vars), ~ as.numeric(scale(.x)), .names = "z_{.col}"))

get_effects <- function(yield_col) {
  purrr::map_dfr(climate_vars, function(var) {
    tmp <- df_std %>%
      select(all_of(yield_col), all_of(paste0("z_", var))) %>%
      filter(!is.na(.data[[yield_col]]), !is.na(.data[[paste0("z_", var)]]))
    
    if (nrow(tmp) < 3) {
      return(tibble(
        YieldSource = yield_col,
        Index = var,
        Effect = NA_real_,
        SE = NA_real_,
        pval = NA_real_
      ))
    }
    
    model <- lm(as.formula(paste(yield_col, "~ z_", var, sep = "")), data = df_std)
    tidy_mod <- broom::tidy(model)
    
    tibble(
      YieldSource = yield_col,
      Index = var,
      Effect = tidy_mod$estimate[2] / mean(df2[[yield_col]], na.rm = TRUE) * 100,
      SE = tidy_mod$std.error[2] / mean(df2[[yield_col]], na.rm = TRUE) * 100,
      pval = tidy_mod$p.value[2]
    )
  })
}

results <- purrr::map_dfr(yield_vars_fig2, get_effects) %>%
  mutate(
    YieldSourceClean = recode(YieldSource, !!!yield_name_map),
    Index_clean = clean_index_label(Index),
    abs_effect = abs(Effect)
  )

# ---- chord with forced inclusion of every source ----
make_chord_panel <- function(results_df, outfile, top_n_total = 20) {
  
  source_names <- intersect(names(yield_colors), unique(results_df$YieldSourceClean))
  
  source_best <- results_df %>%
    filter(!is.na(Effect), YieldSourceClean %in% source_names) %>%
    group_by(YieldSourceClean) %>%
    slice_max(order_by = abs_effect, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  extra_best <- results_df %>%
    filter(!is.na(Effect)) %>%
    arrange(desc(abs_effect)) %>%
    slice_head(n = top_n_total)
  
  chord_df <- bind_rows(source_best, extra_best) %>%
    distinct(YieldSourceClean, Index_clean, .keep_all = TRUE) %>%
    mutate(
      from = YieldSourceClean,
      to = Index_clean,
      value = abs_effect,
      link_color = ifelse(Effect >= 0, "#D73027AA", "#4575B4AA")
    ) %>%
    select(from, to, value, link_color)
  
  png(outfile, width = 3600, height = 3000, res = 340)
  par(mar = c(1, 1, 4, 1))
  circos.clear()
  
  if (nrow(chord_df) == 0) {
    plot.new()
    text(0.5, 0.5, "a. Chord diagram\nNo data", cex = 1.8, font = 2)
    dev.off()
    return(invisible(NULL))
  }
  
  source_sectors <- unique(chord_df$from)
  index_sectors  <- unique(chord_df$to)
  
  grid_colors <- c(
    setNames(yield_colors[source_sectors], source_sectors),
    setNames(rep("#BDBDBD", length(index_sectors)), index_sectors)
  )
  
  circos.par(
    start.degree = 90,
    gap.after = c(rep(10, length(source_sectors) - 1), 16,
                  rep(2, length(index_sectors) - 1), 16),
    track.margin = c(0.01, 0.01),
    canvas.xlim = c(-1.45, 1.45),
    canvas.ylim = c(-1.40, 1.40)
  )
  
  chordDiagram(
    x = chord_df[, c("from", "to", "value")],
    grid.col = grid_colors,
    transparency = 0.18,
    annotationTrack = "grid",
    col = chord_df$link_color,
    directional = 0,
    link.lwd = 1.5,
    link.border = NA,
    link.sort = TRUE,
    link.decreasing = FALSE
  )
  
  circos.trackPlotRegion(
    track.index = 1,
    bg.border = NA,
    panel.fun = function(x, y) {
      sector.name <- get.cell.meta.data("sector.index")
      xlim <- get.cell.meta.data("xlim")
      ylim <- get.cell.meta.data("ylim")
      theta <- circlize(mean(xlim), 1.3)[1, 1] %% 360
      facing_opt <- if (theta < 90 || theta > 270) "clockwise" else "reverse.clockwise"
      adj_opt <- c(ifelse(theta < 90 || theta > 270, 0, 1), 0.5)
      
      circos.text(
        x = mean(xlim),
        y = ylim[1] + .40,
        labels = sector.name,
        facing = facing_opt,
        niceFacing = TRUE,
        adj = adj_opt,
        cex = 1.05,
        font = 2
      )
    }
  )
  
  title("a. Chord diagram of strongest standardized yield effects",
        cex.main = 1.55, font.main = 2)
  
  legend(
    "topleft",
    legend = c("Positive effect", "Negative effect"),
    fill = c("#D73027AA", "#4575B4AA"),
    border = NA,
    bty = "n",
    cex = 1.25
  )
  
  legend(
    "bottomleft",
    legend = names(yield_colors),
    fill = yield_colors,
    border = NA,
    bty = "n",
    cex = 1.25,
    title = "Yield source"
  )
  
  dev.off()
  circos.clear()
}

make_chord_panel(results, out_chord_png, top_n_total = 20)
chord_img <- png::readPNG(out_chord_png)
panel_a <- ggdraw() + draw_image(chord_img)

make_density2 <- function(df, vars, title_text, xlab_text) {
  if (length(vars) == 0) return(ggplot() + theme_void())
  
  long_df <- df %>%
    pivot_longer(cols = all_of(vars), names_to = "StageRaw", values_to = "Value") %>%
    mutate(Stage = prepare_stage_factor(StageRaw))
  
  ggplot(long_df, aes(x = Value, fill = Stage, color = Stage)) +
    geom_density(alpha = 0.30, linewidth = 1.3, na.rm = TRUE) +
    scale_fill_manual(values = stage_colors, drop = FALSE, na.translate = FALSE) +
    scale_color_manual(values = stage_colors, drop = FALSE, na.translate = FALSE) +
    labs(title = title_text, x = xlab_text, y = "Density", fill = "Stage", color = "Stage") +
    theme_big() +
    theme(
      plot.title = element_text(size = 24, face = "bold"),
      axis.title = element_text(size = 20, face = "bold"),
      axis.text = element_text(size = 17)
    )
}

make_sensitivity <- function(results_df, pattern, title_text, panel_letter) {
  dat <- results_df %>%
    filter(str_detect(Index, pattern), !is.na(Effect)) %>%
    mutate(Index_clean = clean_index_label(Index))
  
  if (nrow(dat) == 0) return(ggplot() + theme_void())
  
  ggplot(dat, aes(x = Index_clean, y = Effect, fill = YieldSourceClean)) +
    geom_col(position = position_dodge(width = 0.8)) +
    geom_errorbar(
      aes(ymin = Effect - SE, ymax = Effect + SE),
      width = 0.18,
      position = position_dodge(width = 0.8)
    ) +
    geom_hline(yintercept = 0, linewidth = 0.8) +
    scale_fill_manual(values = yield_colors, drop = FALSE) +
    labs(
      title = paste0(panel_letter, ". ", title_text),
      x = "",
      y = "% Yield change",
      fill = "Yield source"
    ) +
    theme_big() +
    theme(
      plot.title = element_text(size = 24, face = "bold"),
      axis.title = element_text(size = 20, face = "bold"),
      axis.text.x = element_text(size = 16, angle = 18, hjust = 1),
      axis.text.y = element_text(size = 17)
    )
}

hdw_vars_fig2   <- get_group_vars(df2, "HDW")
tmax_vars_fig2  <- get_group_vars(df2, "Tmax")
tmin_vars_fig2  <- get_group_vars(df2, "Tmin")
tmean_vars_fig2 <- get_group_vars(df2, "Tmean")

tmin_vars_fig2  <- setdiff(tmin_vars_fig2, tmax_vars_fig2)
tmean_vars_fig2 <- setdiff(tmean_vars_fig2, c(tmax_vars_fig2, tmin_vars_fig2))

panel_b <- make_density2(df2, hdw_vars_fig2, "b. HDW Distributions", "HDW Hours")
panel_c <- make_sensitivity(results, "HDW", "HDW yield sensitivity", "c")
panel_d <- make_density2(df2, tmax_vars_fig2, "d. Tmax Distributions", "Tmax (°C)")
panel_e <- make_sensitivity(results, "Tmax", "Tmax yield sensitivity", "e")
panel_f <- make_density2(df2, tmin_vars_fig2, "f. Tmin Distributions", "Tmin (°C)")
panel_g <- make_sensitivity(results, "Tmin", "Tmin yield sensitivity", "g")

panel_h <- NULL
panel_i <- NULL

if (length(tmean_vars_fig2) > 0) {
  panel_h <- make_density2(df2, tmean_vars_fig2, "h. Tmean Distributions", "Tmean (°C)")
  panel_i <- make_sensitivity(results, "Tmean", "Tmean yield sensitivity", "i")
}

if (!is.null(panel_h) && !is.null(panel_i)) {
  final_plot_fig2 <- (panel_a) /
    (panel_b | panel_c) /
    (panel_d | panel_e) /
    (panel_f | panel_g) /
    (panel_h | panel_i) +
    plot_layout(heights = c(1.65, 1, 1, 1, 1), guides = "collect") &
    theme(legend.position = "bottom")
} else {
  final_plot_fig2 <- (panel_a) /
    (panel_b | panel_c) /
    (panel_d | panel_e) /
    (panel_f | panel_g) +
    plot_layout(heights = c(1.65, 1, 1, 1), guides = "collect") &
    theme(legend.position = "bottom")
}

save_plot(final_plot_fig2, out_fig2, width = 26, height = 30)

message("Done.")
message("Saved: ", out_fig1_scatter)
message("Saved: ", out_fig1_dist)
message("Saved: ", out_fig2)
message("Saved: ", out_chord_png)