# =============================================================================
# Figure 04 - Strong Egypt map with REAL wheat/cropland shapefile
# =============================================================================
# Main fixes compared with previous versions:
#   1) The wheat/cropland layer is drawn with strong visible fill and outline.
#   2) It is drawn ABOVE the Egypt base polygon, so it cannot be hidden.
#   3) s2 geometry is disabled to avoid duplicate-vertex / invalid-loop errors.
#   4) If geometry cleaning fails, the script still tries to plot the raw cropped
#      shapefile instead of dropping the crop layer.
#   5) A diagnostic CSV is saved so you can check whether the crop layer was read.
#
# Expected output:
#   Figure_04_Egypt_WheatArea_VISIBLE_Nile_Lakes.png
# =============================================================================

# install.packages(c(
#   "tidyverse", "sf", "ggrepel", "patchwork", "viridis",
#   "rnaturalearth", "rnaturalearthdata"
# ))

library(tidyverse)
library(sf)
library(ggrepel)
library(patchwork)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)

# Very important for your shapefile
sf::sf_use_s2(FALSE)

# -----------------------------------------------------------------------------
# 1) PATHS
# -----------------------------------------------------------------------------

base_dir <- "D:/Asseng_paper2024/Updated/Asseng_updated/40YRSSIMULATIONS/ForShawn/21072025/21072025"

update_dir <- file.path(base_dir, "Update27032026")
sim_dir    <- file.path(update_dir, "CIMMYTSimulations")

climate_dir <- file.path(sim_dir, "CIMMYT_ClimateMetrics_From_WTH_LocCodes")

yearloc_file <- file.path(
  climate_dir,
  "CIMMYT_YearLocation_ClimateMetrics_Last90Days.csv"
)

crop_shp <- file.path(
  base_dir,
  "EgyptShapefile/DrYasserMap/EgyptCropland2024/EgyptCropland2024.shp"
)

output_dir <- file.path(sim_dir, "CIMMYT_ClimateMetrics_Figures_Updated")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Egypt extent
egypt_xlim <- c(24.5, 35.2)
egypt_ylim <- c(21.7, 32.2)

# -----------------------------------------------------------------------------
# 2) READ CIMMYT LOCATION SUMMARY
# -----------------------------------------------------------------------------

yearloc <- readr::read_csv(yearloc_file, show_col_types = FALSE) %>%
  mutate(
    Year = as.integer(Year),
    MatchedLocName = as.character(MatchedLocName),
    MatchedLocLat = as.numeric(MatchedLocLat),
    MatchedLocLong = as.numeric(MatchedLocLong)
  )

spatial_summary <- yearloc %>%
  group_by(MatchedWeatherCode, MatchedLocName, MatchedLocLat, MatchedLocLong) %>%
  summarise(
    n_years = n_distinct(Year),
    mean_observed_yield = mean(mean_YieldCIMMYT, na.rm = TRUE),
    mean_simulated_yield = mean(mean_SimYieldMME_t_ha, na.rm = TRUE),
    mean_yield_gap = mean(mean_SimYieldMME_t_ha - mean_YieldCIMMYT, na.rm = TRUE),
    mean_heat_days = mean(mean_HeatDays_Tmax_GT32, na.rm = TRUE),
    mean_SRAD = mean(mean_SRAD, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(MatchedLocLat), !is.na(MatchedLocLong))

points_sf <- spatial_summary %>%
  st_as_sf(
    coords = c("MatchedLocLong", "MatchedLocLat"),
    crs = 4326,
    remove = FALSE
  )

# -----------------------------------------------------------------------------
# 3) READ EGYPT BOUNDARY
# -----------------------------------------------------------------------------

egypt_boundary <- rnaturalearth::ne_countries(
  country = "Egypt",
  returnclass = "sf"
) %>%
  st_transform(4326)

# -----------------------------------------------------------------------------
# 4) READ AND PREPARE REAL CROP/WHEAT SHAPEFILE
# -----------------------------------------------------------------------------

message("Reading crop/wheat shapefile: ", crop_shp)

crop_raw <- st_read(crop_shp, quiet = FALSE)

# Assign/transform CRS
if (is.na(st_crs(crop_raw))) {
  message("Crop shapefile has no CRS. Assuming EPSG:4326.")
  st_crs(crop_raw) <- 4326
} else {
  crop_raw <- st_transform(crop_raw, 4326)
}

message("Original crop layer rows: ", nrow(crop_raw))
message("Original crop geometry types:")
print(table(as.character(st_geometry_type(crop_raw))))

# Crop to map extent.
# With s2 disabled, this should avoid the duplicate-vertex error.
bbox_poly <- st_as_sfc(
  st_bbox(
    c(
      xmin = egypt_xlim[1],
      ymin = egypt_ylim[1],
      xmax = egypt_xlim[2],
      ymax = egypt_ylim[2]
    ),
    crs = st_crs(crop_raw)
  )
)

crop_area <- tryCatch(
  {
    message("Trying fast crop with st_crop()...")
    suppressWarnings(st_crop(crop_raw, st_bbox(bbox_poly)))
  },
  error = function(e) {
    message("st_crop failed. Keeping full crop layer; coord_sf will limit display. Error: ", conditionMessage(e))
    crop_raw
  }
)

# Remove empty geometries
crop_area <- crop_area[!st_is_empty(crop_area), ]

message("Crop rows after crop/filter: ", nrow(crop_area))

# Try cleaning, but do not force failure.
crop_clean <- tryCatch(
  {
    message("Trying GEOS validation and simplification...")
    tmp <- suppressWarnings(st_make_valid(crop_area))
    tmp <- suppressWarnings(st_collection_extract(tmp, "POLYGON"))
    tmp <- tmp[!st_is_empty(tmp), ]
    tmp <- suppressWarnings(st_simplify(tmp, dTolerance = 0.0008, preserveTopology = FALSE))
    tmp <- tmp[!st_is_empty(tmp), ]
    tmp
  },
  error = function(e) {
    message("Geometry cleaning failed. Using uncooked cropped shapefile for plotting. Error: ", conditionMessage(e))
    crop_area
  }
)

message("Crop rows used for plotting: ", nrow(crop_clean))
message("Plot crop geometry types:")
print(table(as.character(st_geometry_type(crop_clean))))

# Save diagnostics
crop_diag <- tibble(
  crop_file = crop_shp,
  original_rows = nrow(crop_raw),
  cropped_rows = nrow(crop_area),
  plot_rows = nrow(crop_clean),
  crs = as.character(st_crs(crop_clean)$input)
)

write_csv(
  crop_diag,
  file.path(output_dir, "Figure04_CropLayer_Diagnostics.csv")
)

# If the shapefile still has no rows, stop clearly.
if (nrow(crop_clean) == 0) {
  stop("Crop/wheat shapefile was read but no geometries remained for plotting. Please check Figure04_CropLayer_Diagnostics.csv")
}

# -----------------------------------------------------------------------------
# 5) ADD NILE AND WATER BODIES
# -----------------------------------------------------------------------------

# Manual Nile line fallback
manual_nile_df <- tibble(
  lon = c(31.20, 31.15, 31.05, 30.95, 30.85, 30.65, 30.45, 30.25, 30.10, 30.05),
  lat = c(22.00, 23.10, 24.20, 25.30, 26.40, 27.40, 28.40, 29.20, 30.00, 31.10)
)

manual_nile <- manual_nile_df %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

rivers_all <- tryCatch(
  {
    ne_download(
      scale = 10,
      type = "rivers_lake_centerlines",
      category = "physical",
      returnclass = "sf"
    ) %>%
      st_transform(4326)
  },
  error = function(e) NULL
)

if (!is.null(rivers_all) && "name" %in% names(rivers_all)) {
  nile <- rivers_all %>%
    filter(str_detect(str_to_lower(name), "nile"))
  if (nrow(nile) == 0) nile <- manual_nile
} else {
  nile <- manual_nile
}

nile <- tryCatch(
  suppressWarnings(st_crop(nile, xmin = egypt_xlim[1], xmax = egypt_xlim[2],
                           ymin = egypt_ylim[1], ymax = egypt_ylim[2])),
  error = function(e) manual_nile
)

# Manual important water bodies as points/approximate locations
manual_lakes_points <- tibble(
  WaterBody = c("Lake Nasser", "Lake Qarun", "Northern Delta lakes"),
  lon = c(32.8, 30.6, 31.2),
  lat = c(23.3, 29.5, 31.2)
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# -----------------------------------------------------------------------------
# 6) MAP THEME
# -----------------------------------------------------------------------------

theme_map <- function() {
  theme_bw(base_size = 15) +
    theme(
      text = element_text(color = "black"),
      plot.title = element_text(face = "bold", size = 17, color = "black"),
      plot.subtitle = element_text(size = 12, color = "black"),
      axis.title = element_text(size = 15, color = "black"),
      axis.text = element_text(size = 13, color = "black"),
      legend.title = element_text(size = 13, face = "bold", color = "black"),
      legend.text = element_text(size = 12, color = "black"),
      legend.position = "right",
      legend.key.height = unit(0.85, "cm"),
      legend.key.width = unit(0.45, "cm"),
      panel.grid.major = element_line(color = "grey88", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      plot.margin = margin(5, 5, 5, 5)
    )
}

coord_egypt <- coord_sf(
  xlim = egypt_xlim,
  ylim = egypt_ylim,
  expand = FALSE
)

# IMPORTANT:
# Crop layer is intentionally drawn AFTER Egypt boundary and BEFORE points.
# It uses strong beige/orange fill and dark outline so you can see it clearly.
base_layers <- list(
  geom_sf(
    data = egypt_boundary,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.55
  ),
  geom_sf(
    data = crop_clean,
    fill = "#C98A2B",
    color = "#7A4F12",
    linewidth = 0.15,
    alpha = 0.85
  ),
  geom_sf(
    data = nile,
    color = "#0072B2",
    linewidth = 1.05,
    alpha = 0.98
  ),
  geom_sf(
    data = manual_lakes_points,
    color = "#0072B2",
    size = 2.2,
    alpha = 0.75
  )
)

make_label_layer <- function() {
  geom_text_repel(
    data = spatial_summary,
    aes(x = MatchedLocLong, y = MatchedLocLat, label = MatchedLocName),
    size = 2.25,
    fontface = "bold",
    color = "black",
    box.padding = 0.10,
    point.padding = 0.20,
    segment.size = 0.18,
    segment.color = "grey35",
    min.segment.length = 0,
    max.overlaps = Inf,
    force = 16,
    force_pull = 0.25,
    seed = 123
  )
}

# -----------------------------------------------------------------------------
# 7) PANEL A: HEAT STRESS
# -----------------------------------------------------------------------------

p_heat <- ggplot() +
  base_layers +
  geom_sf(
    data = points_sf,
    aes(size = n_years, color = mean_heat_days),
    alpha = 0.98
  ) +
  make_label_layer() +
  scale_color_viridis_c(
    option = "A",
    name = "Mean days\nTmax > 32°C"
  ) +
  scale_size_continuous(
    name = "Years",
    range = c(3.0, 8.0),
    breaks = c(5, 10, 15, 20, 25)
  ) +
  coord_egypt +
  labs(
    title = "A. Heat-stress exposure",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_map()

# -----------------------------------------------------------------------------
# 8) PANEL B: SOLAR RADIATION
# -----------------------------------------------------------------------------

p_srad <- ggplot() +
  base_layers +
  geom_sf(
    data = points_sf,
    aes(size = n_years, color = mean_SRAD),
    alpha = 0.98
  ) +
  make_label_layer() +
  scale_color_viridis_c(
    option = "D",
    name = "Mean SRAD\n(MJ m-2 day-1)"
  ) +
  scale_size_continuous(
    name = "Years",
    range = c(3.0, 8.0),
    breaks = c(5, 10, 15, 20, 25)
  ) +
  coord_egypt +
  labs(
    title = "B. Solar radiation",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_map()

# -----------------------------------------------------------------------------
# 9) PANEL C: YIELD GAP
# -----------------------------------------------------------------------------

gap_lim <- max(abs(spatial_summary$mean_yield_gap), na.rm = TRUE)

p_gap <- ggplot() +
  base_layers +
  geom_sf(
    data = points_sf,
    aes(size = n_years, color = mean_yield_gap),
    alpha = 0.98
  ) +
  make_label_layer() +
  scale_color_gradient2(
    low = "steelblue",
    mid = "grey95",
    high = "firebrick",
    midpoint = 0,
    limits = c(-gap_lim, gap_lim),
    name = "Yield gap\n(t/ha)"
  ) +
  scale_size_continuous(
    name = "Years",
    range = c(3.0, 8.0),
    breaks = c(5, 10, 15, 20, 25)
  ) +
  coord_egypt +
  labs(
    title = "C. Yield gap",
    subtitle = "Simulated MME − observed CIMMYT",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_map()

# -----------------------------------------------------------------------------
# 10) COMBINE AND SAVE
# -----------------------------------------------------------------------------

fig_spatial <- (p_heat | p_srad | p_gap) +
  plot_annotation(
    title = "Spatial distribution of climate exposure and yield gap across CIMMYT wheat locations in Egypt",
    subtitle = "Visible wheat/cropland shapefile is shown in orange-beige, the River Nile/water bodies in blue, and points represent matched CIMMYT locations.",
    theme = theme(
      plot.title = element_text(face = "bold", size = 23, color = "black"),
      plot.subtitle = element_text(size = 16, color = "black")
    )
  )

ggsave(
  filename = file.path(output_dir, "Figure_04_Egypt_WheatArea_VISIBLE_Nile_Lakes.png"),
  plot = fig_spatial,
  width = 24,
  height = 9.5,
  dpi = 350,
  limitsize = FALSE
)

ggsave(
  filename = file.path(output_dir, "Figure_04_Egypt_WheatArea_VISIBLE_Nile_Lakes.pdf"),
  plot = fig_spatial,
  width = 24,
  height = 9.5,
  limitsize = FALSE
)

ggsave(
  filename = file.path(output_dir, "Figure_04A_HeatStress_Egypt_WheatArea_VISIBLE.png"),
  plot = p_heat,
  width = 8.5,
  height = 9.5,
  dpi = 350,
  limitsize = FALSE
)

ggsave(
  filename = file.path(output_dir, "Figure_04B_SRAD_Egypt_WheatArea_VISIBLE.png"),
  plot = p_srad,
  width = 8.5,
  height = 9.5,
  dpi = 350,
  limitsize = FALSE
)

ggsave(
  filename = file.path(output_dir, "Figure_04C_YieldGap_Egypt_WheatArea_VISIBLE.png"),
  plot = p_gap,
  width = 8.5,
  height = 9.5,
  dpi = 350,
  limitsize = FALSE
)

message("Done.")
message("Output folder: ", output_dir)
message("Main figure: Figure_04_Egypt_WheatArea_VISIBLE_Nile_Lakes.png")
message("Please check diagnostic file: Figure04_CropLayer_Diagnostics.csv")
