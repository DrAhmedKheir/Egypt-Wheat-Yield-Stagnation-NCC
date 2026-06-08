# =============================================================================
# Advanced analysis and Nature-style visualization of CIMMYT ESWYT line-level data
# Dataset: ESWYT1-41_EGYPT_GID_Yield_Pheno_20260602.xlsx
#
# Aim:
# Explore relationships among yield, phenology, sowing dates, genotypes
# (GID/Pedigree), years, and locations.
#
# Main outputs:
#   Fig01_temporal_yield_phenology_sowing.png
#   Fig02_yield_phenology_relationships.png
#   Fig03_genotype_stability_heatmap.png
#   Fig04_alluvial_region_phenology_yield.png
#   Fig05_chord_region_elite_pedigree.png
#   Fig06_circular_sowing_phenology_yield.png
#   Fig07_pca_genotype_environment_traits.png
#   Fig08_top_genotypes_performance.png
#
# Notes:
#   - Figures are designed for a Nature-style manuscript/supplement:
#     clean white background, large black fonts, no unnecessary grids.
#   - The alluvial and chord diagrams are exploratory and probably better
#     suited to Supplementary unless Senthold/Urs like them.
# =============================================================================

# -----------------------------------------------------------------------------
# 0. INSTALL PACKAGES IF NEEDED
# -----------------------------------------------------------------------------

packages <- c(
  "readxl", "dplyr", "tidyr", "stringr", "lubridate", "ggplot2",
  "readr", "janitor", "forcats", "scales", "viridis", "ggrepel",
  "patchwork", "ggalluvial", "circlize", "FactoMineR", "factoextra",
  "mgcv", "broom"
)

new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) install.packages(new_packages, dependencies = TRUE)

invisible(lapply(packages, library, character.only = TRUE))

# -----------------------------------------------------------------------------
# 1. PATHS
# -----------------------------------------------------------------------------

base_dir <- "D:/Asseng_paper2024/Updated/Asseng_updated/40YRSSIMULATIONS/ForShawn/21072025/21072025/Update27032026/UrslastFilesCVs"

input_file <- file.path(base_dir, "ESWYT1-41_EGYPT_GID_Yield_Pheno_20260602.xlsx")

output_dir <- file.path(base_dir, "ESWYT_Advanced_Yield_Phenology_Figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 2. GENERAL SETTINGS
# -----------------------------------------------------------------------------

break_year <- 1998
later_break <- 2010

fig_dpi <- 400

theme_nature <- function(base_size = 18) {
  theme_classic(base_size = base_size) +
    theme(
      text = element_text(color = "black"),
      plot.title = element_text(face = "bold", size = base_size + 7, color = "black"),
      plot.subtitle = element_text(size = base_size + 1, color = "black"),
      axis.title = element_text(face = "bold", size = base_size + 2, color = "black"),
      axis.text = element_text(face = "bold", size = base_size - 1, color = "black"),
      axis.line = element_line(linewidth = 0.85, color = "black"),
      axis.ticks = element_line(linewidth = 0.75, color = "black"),
      legend.title = element_text(face = "bold", size = base_size, color = "black"),
      legend.text = element_text(size = base_size - 1, color = "black"),
      strip.text = element_text(face = "bold", size = base_size, color = "black"),
      strip.background = element_rect(fill = "grey95", color = "black", linewidth = 0.7),
      panel.grid = element_blank(),
      plot.margin = margin(12, 12, 12, 12)
    )
}

save_fig <- function(plot, filename, width = 14, height = 9, dpi = fig_dpi) {
  ggsave(file.path(output_dir, paste0(filename, ".png")),
         plot = plot, width = width, height = height, dpi = dpi, limitsize = FALSE)
  ggsave(file.path(output_dir, paste0(filename, ".pdf")),
         plot = plot, width = width, height = height, limitsize = FALSE)
}

# -----------------------------------------------------------------------------
# 3. READ AND CLEAN DATA
# -----------------------------------------------------------------------------

raw <- readxl::read_excel(input_file, sheet = 1) %>%
  janitor::clean_names()

message("Columns detected:")
print(names(raw))

dat <- raw %>%
  transmute(
    Year = as.integer(harvest_yr),
    Location = as.character(loc_desc),
    Lat = as.numeric(lat),
    Lon = as.numeric(long),
    GID = as.character(gid),
    Pedigree = as.character(pedigree),
    SowingDate = as.Date(sowing_date),
    Heading = as.numeric(days_to_heading),
    Maturity = as.numeric(days_to_maturity),
    TGW = as.numeric(x1000_grain_weight),
    PlantHeight = as.numeric(plant_height),
    Yield = as.numeric(yld_t_ha)
  ) %>%
  filter(!is.na(Year), !is.na(Location), !is.na(GID), !is.na(Yield)) %>%
  mutate(
    Location_clean = Location %>%
      stringr::str_to_lower() %>%
      stringr::str_replace_all("[-_\\.]", " ") %>%
      stringr::str_squish(),

    Region = case_when(
      str_detect(Location_clean, "sakha|gemmeiza|gemiza|etay|baroud|kafr|kfs|nobaria|qaliobia|qalyubia|qalyubiya|el gemmeiza") ~ "North Delta",
      str_detect(Location_clean, "sids|beni|benisuef|beni suef") ~ "Middle Egypt",
      str_detect(Location_clean, "minya|malawy|assuit|asyut|shandweel|shandaweel|mattana|mataana|kom ombo|koom ombo|komombo") ~ "Upper Egypt",
      TRUE ~ "Other"
    ),

    Period = case_when(
      Year <= break_year ~ "1981-1998",
      Year > break_year & Year <= later_break ~ "1999-2010",
      Year > later_break ~ "2011-2021",
      TRUE ~ NA_character_
    ),

    SowingMonth = lubridate::month(SowingDate, label = TRUE, abbr = TRUE),
    SowingDOY = lubridate::yday(SowingDate),

    GrainFillingDuration = Maturity - Heading,

    YieldClass = case_when(
      Yield >= quantile(Yield, 0.90, na.rm = TRUE) ~ "Elite top 10%",
      Yield >= quantile(Yield, 0.50, na.rm = TRUE) ~ "Above median",
      TRUE ~ "Below median"
    ),

    HeadingClass = case_when(
      Heading <= quantile(Heading, 0.33, na.rm = TRUE) ~ "Early heading",
      Heading <= quantile(Heading, 0.66, na.rm = TRUE) ~ "Intermediate heading",
      TRUE ~ "Late heading"
    ),

    MaturityClass = case_when(
      Maturity <= quantile(Maturity, 0.33, na.rm = TRUE) ~ "Early maturity",
      Maturity <= quantile(Maturity, 0.66, na.rm = TRUE) ~ "Intermediate maturity",
      TRUE ~ "Late maturity"
    )
  )

write_csv(dat, file.path(output_dir, "ESWYT_cleaned_line_level_data.csv"))

# Basic diagnostics
diagnostics <- dat %>%
  summarise(
    n_records = n(),
    n_years = n_distinct(Year),
    first_year = min(Year, na.rm = TRUE),
    last_year = max(Year, na.rm = TRUE),
    n_locations = n_distinct(Location),
    n_GID = n_distinct(GID),
    n_pedigree = n_distinct(Pedigree),
    yield_min = min(Yield, na.rm = TRUE),
    yield_median = median(Yield, na.rm = TRUE),
    yield_max = max(Yield, na.rm = TRUE)
  )

write_csv(diagnostics, file.path(output_dir, "ESWYT_dataset_diagnostics.csv"))

# -----------------------------------------------------------------------------
# 4. ANNUAL SUMMARY
# -----------------------------------------------------------------------------

annual <- dat %>%
  group_by(Year) %>%
  summarise(
    n = n(),
    n_locations = n_distinct(Location),
    n_gid = n_distinct(GID),
    Yield50 = quantile(Yield, 0.50, na.rm = TRUE),
    Yield75 = quantile(Yield, 0.75, na.rm = TRUE),
    Yield90 = quantile(Yield, 0.90, na.rm = TRUE),
    Heading50 = median(Heading, na.rm = TRUE),
    Maturity50 = median(Maturity, na.rm = TRUE),
    GFD50 = median(GrainFillingDuration, na.rm = TRUE),
    SowingDOY50 = median(SowingDOY, na.rm = TRUE),
    TGW50 = median(TGW, na.rm = TRUE),
    Height50 = median(PlantHeight, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(annual, file.path(output_dir, "ESWYT_annual_summary.csv"))

# -----------------------------------------------------------------------------
# FIGURE 1. TEMPORAL CO-EVOLUTION OF YIELD, PHENOLOGY, AND SOWING DATE
# -----------------------------------------------------------------------------

annual_long <- annual %>%
  select(Year, Yield50, Yield90, Heading50, Maturity50, GFD50, SowingDOY50) %>%
  pivot_longer(-Year, names_to = "Variable", values_to = "Value") %>%
  mutate(
    Variable = recode(
      Variable,
      Yield50 = "Median yield",
      Yield90 = "90th percentile yield",
      Heading50 = "Median days to heading",
      Maturity50 = "Median days to maturity",
      GFD50 = "Median grain-filling duration",
      SowingDOY50 = "Median sowing day of year"
    ),
    Variable = factor(
      Variable,
      levels = c(
        "Median yield", "90th percentile yield",
        "Median days to heading", "Median days to maturity",
        "Median grain-filling duration", "Median sowing day of year"
      )
    )
  )

p1 <- ggplot(annual_long, aes(x = Year, y = Value)) +
  geom_line(linewidth = 1.2, color = "black") +
  geom_point(size = 2.7, color = "black") +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.1, color = "#1B9E77") +
  geom_vline(xintercept = break_year, linetype = "dashed", linewidth = 0.8) +
  facet_wrap(~Variable, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = seq(min(dat$Year), max(dat$Year), by = 5)) +
  labs(
    title = "Temporal co-evolution of wheat yield, phenology, and sowing date",
    subtitle = "Annual summaries from line-level CIMMYT ESWYT data in Egypt",
    x = "Harvest year",
    y = "Annual summary value"
  ) +
  theme_nature(18) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_fig(p1, "Fig01_temporal_yield_phenology_sowing", width = 15, height = 11)

# -----------------------------------------------------------------------------
# FIGURE 2. YIELD-PHENOLOGY RELATIONSHIPS WITH GAM SMOOTHERS
# -----------------------------------------------------------------------------

rel_dat <- dat %>%
  select(Year, Region, Yield, Heading, Maturity, GrainFillingDuration, SowingDOY, TGW, PlantHeight) %>%
  pivot_longer(
    cols = c(Heading, Maturity, GrainFillingDuration, SowingDOY, TGW, PlantHeight),
    names_to = "Trait",
    values_to = "TraitValue"
  ) %>%
  filter(!is.na(TraitValue), !is.na(Yield)) %>%
  mutate(
    Trait = recode(
      Trait,
      Heading = "Days to heading",
      Maturity = "Days to maturity",
      GrainFillingDuration = "Grain-filling duration",
      SowingDOY = "Sowing day of year",
      TGW = "1000-grain weight",
      PlantHeight = "Plant height"
    )
  )

p2 <- ggplot(rel_dat, aes(x = TraitValue, y = Yield, color = Region)) +
  geom_point(alpha = 0.18, size = 1.4) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5), se = TRUE, linewidth = 1.25, alpha = 0.18) +
  facet_wrap(~Trait, scales = "free_x", ncol = 3) +
  scale_color_manual(values = c(
    "North Delta" = "#0072B2",
    "Middle Egypt" = "#E69F00",
    "Upper Egypt" = "#D55E00",
    "Other" = "grey45"
  )) +
  labs(
    title = "Yield responses to phenology, sowing date, and plant traits",
    subtitle = "Curves are generalized additive model smoothers fitted by region",
    x = "Trait value",
    y = expression("Grain yield (t ha"^{-1}*")"),
    color = "Region"
  ) +
  theme_nature(18) +
  theme(legend.position = "top")

save_fig(p2, "Fig02_yield_phenology_relationships", width = 17, height = 10)

# Regression/GAM simple linear stats for export
rel_stats <- rel_dat %>%
  group_by(Region, Trait) %>%
  group_modify(~{
    if (nrow(.x) < 10) {
      return(tibble(slope = NA_real_, r2 = NA_real_, p_value = NA_real_, n = nrow(.x)))
    }
    fit <- lm(Yield ~ TraitValue, data = .x)
    sm <- summary(fit)
    tibble(
      slope = coef(fit)[["TraitValue"]],
      r2 = sm$r.squared,
      p_value = sm$coefficients["TraitValue", "Pr(>|t|)"],
      n = nrow(.x)
    )
  }) %>%
  ungroup()

write_csv(rel_stats, file.path(output_dir, "ESWYT_yield_trait_linear_stats.csv"))

# -----------------------------------------------------------------------------
# FIGURE 3. GENOTYPE STABILITY HEATMAP
# -----------------------------------------------------------------------------
# Select genotypes tested in enough records and show genotype x environment stability.

gid_summary <- dat %>%
  group_by(GID, Pedigree) %>%
  summarise(
    n_records = n(),
    n_years = n_distinct(Year),
    n_locations = n_distinct(Location),
    mean_yield = mean(Yield, na.rm = TRUE),
    median_yield = median(Yield, na.rm = TRUE),
    sd_yield = sd(Yield, na.rm = TRUE),
    cv_yield = 100 * sd_yield / mean_yield,
    mean_heading = mean(Heading, na.rm = TRUE),
    mean_maturity = mean(Maturity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_records >= 8, n_years >= 2) %>%
  arrange(desc(mean_yield))

write_csv(gid_summary, file.path(output_dir, "ESWYT_genotype_summary.csv"))

top_gids <- gid_summary %>%
  slice_max(order_by = mean_yield, n = 35) %>%
  pull(GID)

heat_dat <- dat %>%
  filter(GID %in% top_gids) %>%
  group_by(GID, Pedigree, Year) %>%
  summarise(Yield = mean(Yield, na.rm = TRUE), .groups = "drop") %>%
  group_by(GID) %>%
  mutate(Yield_z = as.numeric(scale(Yield))) %>%
  ungroup() %>%
  mutate(
    GID_label = paste0(GID, " | ", str_trunc(Pedigree, 28)),
    GID_label = fct_reorder(GID_label, Yield, .fun = mean, .desc = TRUE)
  )

p3 <- ggplot(heat_dat, aes(x = Year, y = GID_label, fill = Yield_z)) +
  geom_tile(color = "white", linewidth = 0.25) +
  scale_fill_gradient2(
    low = "#2166AC", mid = "white", high = "#B2182B",
    midpoint = 0, name = "Within-GID\nstandardized\nyield"
  ) +
  geom_vline(xintercept = break_year, linetype = "dashed", linewidth = 0.7) +
  scale_x_continuous(breaks = seq(min(dat$Year), max(dat$Year), by = 5)) +
  labs(
    title = "Yield stability of high-performing ESWYT genotypes",
    subtitle = "Rows show top-yielding GIDs; colors show yield anomalies within each genotype",
    x = "Harvest year",
    y = "Genotype | pedigree"
  ) +
  theme_nature(15) +
  theme(
    axis.text.y = element_text(size = 8, face = "bold"),
    legend.position = "right"
  )

save_fig(p3, "Fig03_genotype_stability_heatmap", width = 15, height = 12)

# -----------------------------------------------------------------------------
# FIGURE 4. ALLUVIAL/SANKEY: PERIOD -> REGION -> PHENOLOGY -> YIELD CLASS
# -----------------------------------------------------------------------------
# This is useful to communicate how high-yielding observations are distributed
# across periods, regions, and phenological classes.

alluvial_dat <- dat %>%
  filter(Region != "Other", !is.na(Period), !is.na(HeadingClass), !is.na(YieldClass)) %>%
  count(Period, Region, HeadingClass, YieldClass, name = "n") %>%
  mutate(
    Period = factor(Period, levels = c("1981-1998", "1999-2010", "2011-2021")),
    YieldClass = factor(YieldClass, levels = c("Below median", "Above median", "Elite top 10%"))
  )

p4 <- ggplot(
  alluvial_dat,
  aes(axis1 = Period, axis2 = Region, axis3 = HeadingClass, axis4 = YieldClass, y = n)
) +
  geom_alluvium(aes(fill = YieldClass), width = 0.12, alpha = 0.75) +
  geom_stratum(width = 0.12, fill = "grey95", color = "black", linewidth = 0.5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4.8, fontface = "bold") +
  scale_x_discrete(
    limits = c("Period", "Region", "Heading class", "Yield class"),
    expand = c(0.08, 0.08)
  ) +
  scale_fill_manual(values = c(
    "Below median" = "#BDBDBD",
    "Above median" = "#74ADD1",
    "Elite top 10%" = "#D73027"
  )) +
  labs(
    title = "Pathways linking period, region, phenology, and yield class",
    subtitle = "Alluvial flows show how ESWYT observations move from time periods to high-yield classes",
    x = "",
    y = "Number of line-level observations",
    fill = "Yield class"
  ) +
  theme_nature(18) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "top"
  )

save_fig(p4, "Fig04_alluvial_region_phenology_yield", width = 16, height = 9)

# -----------------------------------------------------------------------------
# FIGURE 5. CHORD DIAGRAM: REGION -> ELITE PEDIGREES
# -----------------------------------------------------------------------------
# Chord diagrams can be beautiful but should be used carefully.
# This one shows where elite pedigrees occur regionally.

elite_pedigrees <- dat %>%
  group_by(Year, Location) %>%
  mutate(local_q90 = quantile(Yield, 0.90, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(Yield >= local_q90, Region != "Other") %>%
  mutate(Pedigree_short = str_trunc(Pedigree, 35)) %>%
  count(Region, Pedigree_short, name = "n") %>%
  group_by(Pedigree_short) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  slice_max(order_by = total, n = 20) %>%
  select(Region, Pedigree_short, n)

write_csv(elite_pedigrees, file.path(output_dir, "ESWYT_elite_pedigree_region_counts.csv"))

if (nrow(elite_pedigrees) > 0) {

  chord_file_png <- file.path(output_dir, "Fig05_chord_region_elite_pedigree.png")
  chord_file_pdf <- file.path(output_dir, "Fig05_chord_region_elite_pedigree.pdf")

  # PNG
  png(chord_file_png, width = 3800, height = 3200, res = 350)
  circos.clear()
  circos.par(start.degree = 90, gap.after = c(rep(4, length(unique(elite_pedigrees$Region))),
                                             rep(1, length(unique(elite_pedigrees$Pedigree_short)))))
  chordDiagram(
    elite_pedigrees,
    transparency = 0.25,
    annotationTrack = "grid",
    preAllocateTracks = list(track.height = 0.08)
  )
  circos.trackPlotRegion(
    track.index = 1,
    panel.fun = function(x, y) {
      sector.name <- get.cell.meta.data("sector.index")
      xlim <- get.cell.meta.data("xlim")
      ylim <- get.cell.meta.data("ylim")
      circos.text(
        mean(xlim), ylim[1],
        sector.name,
        facing = "clockwise",
        niceFacing = TRUE,
        adj = c(0, 0.5),
        cex = 0.75,
        font = 2
      )
    },
    bg.border = NA
  )
  title("Regional distribution of elite ESWYT pedigrees", cex.main = 1.6, font.main = 2)
  dev.off()

  # PDF
  pdf(chord_file_pdf, width = 12, height = 10)
  circos.clear()
  circos.par(start.degree = 90)
  chordDiagram(elite_pedigrees, transparency = 0.25, annotationTrack = "grid")
  title("Regional distribution of elite ESWYT pedigrees")
  dev.off()
  circos.clear()
}

# -----------------------------------------------------------------------------
# FIGURE 6. CIRCULAR/WIND-STYLE FIGURE:
# SOWING DATE, PHENOLOGY, AND YIELD
# -----------------------------------------------------------------------------
# This polar plot helps show whether high-yield observations cluster around
# particular sowing windows and phenology durations.

polar_dat <- dat %>%
  filter(!is.na(SowingDOY), !is.na(Heading), !is.na(Yield), Region != "Other") %>%
  mutate(
    SowingDOY2 = ifelse(SowingDOY < 150, SowingDOY, SowingDOY - 365),
    YieldGroup = case_when(
      Yield >= quantile(Yield, 0.90, na.rm = TRUE) ~ "Elite top 10%",
      Yield >= quantile(Yield, 0.50, na.rm = TRUE) ~ "Above median",
      TRUE ~ "Below median"
    )
  )

p6 <- ggplot(polar_dat, aes(x = SowingDOY2, y = Heading, color = YieldGroup)) +
  geom_point(alpha = 0.35, size = 1.7) +
  stat_summary_bin(
    aes(group = YieldGroup),
    fun = median,
    bins = 18,
    geom = "line",
    linewidth = 1.3
  ) +
  coord_polar(theta = "x") +
  scale_color_manual(values = c(
    "Below median" = "grey65",
    "Above median" = "#2C7BB6",
    "Elite top 10%" = "#D7191C"
  )) +
  labs(
    title = "Circular sowing-date and phenology space of ESWYT wheat lines",
    subtitle = "Radius shows days to heading; angle shows sowing timing; colors show yield class",
    x = "Sowing date",
    y = "Days to heading",
    color = "Yield class"
  ) +
  theme_nature(18) +
  theme(
    axis.text.x = element_text(size = 11),
    legend.position = "top"
  )

save_fig(p6, "Fig06_circular_sowing_phenology_yield", width = 11, height = 10)

# -----------------------------------------------------------------------------
# FIGURE 7. PCA OF GENOTYPE-ENVIRONMENT-TRAIT SPACE
# -----------------------------------------------------------------------------

pca_dat <- dat %>%
  select(Yield, Heading, Maturity, GrainFillingDuration, SowingDOY, TGW, PlantHeight, Region, Period) %>%
  filter(complete.cases(.)) %>%
  mutate(
    Region = factor(Region),
    Period = factor(Period)
  )

if (nrow(pca_dat) >= 20) {

  pca_vars <- pca_dat %>%
    select(Yield, Heading, Maturity, GrainFillingDuration, SowingDOY, TGW, PlantHeight)

  pca_res <- FactoMineR::PCA(pca_vars, graph = FALSE, scale.unit = TRUE)

  p7 <- factoextra::fviz_pca_biplot(
    pca_res,
    geom.ind = "point",
    habillage = pca_dat$Region,
    addEllipses = TRUE,
    ellipse.level = 0.68,
    label = "var",
    repel = TRUE,
    pointsize = 1.8,
    alpha.ind = 0.35,
    col.var = "black"
  ) +
    labs(
      title = "Multivariate trait space linking yield, phenology, and sowing timing",
      subtitle = "PCA of line-level ESWYT records grouped by region"
    ) +
    theme_nature(18) +
    theme(legend.position = "top")

  save_fig(p7, "Fig07_pca_genotype_environment_traits", width = 13, height = 10)

  eig <- factoextra::get_eigenvalue(pca_res)
  write_csv(as.data.frame(eig), file.path(output_dir, "ESWYT_PCA_eigenvalues.csv"))

  var_contrib <- factoextra::get_pca_var(pca_res)$contrib %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Variable")
  write_csv(var_contrib, file.path(output_dir, "ESWYT_PCA_variable_contributions.csv"))
}

# -----------------------------------------------------------------------------
# FIGURE 8. TOP GENOTYPES: PERFORMANCE VS STABILITY
# -----------------------------------------------------------------------------

gid_perf <- gid_summary %>%
  filter(n_records >= 10, !is.na(cv_yield)) %>%
  mutate(
    Pedigree_label = str_trunc(Pedigree, 28),
    Stability = -cv_yield
  ) %>%
  arrange(desc(mean_yield))

label_gids <- gid_perf %>%
  slice_max(order_by = mean_yield, n = 15)

p8 <- ggplot(gid_perf, aes(x = cv_yield, y = mean_yield)) +
  geom_point(aes(size = n_records, color = mean_heading), alpha = 0.75) +
  ggrepel::geom_text_repel(
    data = label_gids,
    aes(label = Pedigree_label),
    size = 4.0,
    max.overlaps = 20,
    fontface = "bold",
    box.padding = 0.35
  ) +
  scale_color_viridis_c(option = "C", name = "Mean days\nto heading") +
  scale_size_continuous(name = "Records", range = c(2, 9)) +
  labs(
    title = "Genotype performance-stability trade-off",
    subtitle = "High mean yield and low coefficient of variation indicate broadly stable elite germplasm",
    x = "Yield coefficient of variation (%)",
    y = expression("Mean grain yield (t ha"^{-1}*")")
  ) +
  theme_nature(18) +
  theme(legend.position = "right")

save_fig(p8, "Fig08_top_genotypes_performance", width = 13, height = 9)

# -----------------------------------------------------------------------------
# 5. OPTIONAL: MIXED-EFFECT STYLE SUMMARY USING FIXED EFFECTS ONLY
# -----------------------------------------------------------------------------
# This avoids requiring lme4. It estimates broad effects with year, region,
# sowing date, heading, maturity, and GID fixed effects for exploration.

model_dat <- dat %>%
  select(Yield, Year, Region, SowingDOY, Heading, Maturity, GrainFillingDuration, TGW, PlantHeight, GID) %>%
  filter(complete.cases(.), Region != "Other") %>%
  mutate(
    Year_c = Year - mean(Year),
    SowingDOY_c = SowingDOY - mean(SowingDOY),
    Heading_c = Heading - mean(Heading),
    Maturity_c = Maturity - mean(Maturity),
    GFD_c = GrainFillingDuration - mean(GrainFillingDuration),
    TGW_c = TGW - mean(TGW),
    Height_c = PlantHeight - mean(PlantHeight)
  )

if (nrow(model_dat) > 50) {
  lm1 <- lm(Yield ~ Year_c + Region + SowingDOY_c + Heading_c + Maturity_c +
              GFD_c + TGW_c + Height_c, data = model_dat)

  lm_summary <- broom::tidy(lm1)
  write_csv(lm_summary, file.path(output_dir, "ESWYT_exploratory_linear_model_traits.csv"))
}

# -----------------------------------------------------------------------------
# FINAL MESSAGE
# -----------------------------------------------------------------------------

message("Done.")
message("All figures and tables saved in:")
message(output_dir)
