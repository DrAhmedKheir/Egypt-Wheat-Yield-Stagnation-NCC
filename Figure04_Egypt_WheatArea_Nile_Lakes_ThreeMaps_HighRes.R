# =============================================================================
# Figure 04 - High-resolution Egypt wheat/cropland map + Nile/lakes + CIMMYT sites
# =============================================================================
# Improvements:
#   - Three large maps in one page
#   - High resolution PNG + PDF
#   - Larger legend, axis, title and location-label fonts
#   - Actual wheat/crop-area shapefile shown clearly
#   - River Nile and major water bodies shown clearly
#   - Small but readable non-overlapping CIMMYT location labels
#   - CRS fix retained: if shapefile coordinates already look like lon/lat,
#     CRS is overridden to EPSG:4326 instead of wrongly transforming.
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

sf::sf_use_s2(FALSE)

# -----------------------------------------------------------------------------
# 1) PATHS
# -----------------------------------------------------------------------------

base_dir <- "D:/Asseng_paper2024/Updated/Asseng_updated/40YRSSIMULATIONS/ForShawn/21072025/21072025"

update_dir  <- file.path(base_dir, "Update27032026")
sim_dir     <- file.path(update_dir, "CIMMYTSimulations")
climate_dir <- file.path(sim_dir, "CIMMYT_ClimateMetrics_From_WTH_LocCodes")

yearloc_file <- file.path(climate_dir, "CIMMYT_YearLocation_ClimateMetrics_Last90Days.csv")

crop_shp <- file.path(
  base_dir,
  "EgyptShapefile/DrYasserMap/EgyptCropland2024/EgyptCropland2024.shp"
)

output_dir <- file.path(sim_dir, "CIMMYT_ClimateMetrics_Figures_Maps_HighRes")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

egypt_xlim <- c(24.5, 35.8)
egypt_ylim <- c(21.7, 32.3)

main_width  <- 30
main_height <- 13
main_dpi    <- 450

single_width  <- 11
single_height <- 12
single_dpi    <- 450

main_title_size     <- 34
main_subtitle_size  <- 23
panel_title_size    <- 25
panel_subtitle_size <- 19
axis_title_size     <- 21
axis_text_size      <- 18
legend_title_size   <- 19
legend_text_size    <- 17
location_label_size <- 3.0

point_size_range <- c(4.2, 10.5)
simplify_tolerance <- 0.0006

# -----------------------------------------------------------------------------
# 2) FUNCTIONS
# -----------------------------------------------------------------------------

looks_like_lonlat <- function(x) {
  bb <- sf::st_bbox(x)
  all(
    is.finite(bb),
    bb["xmin"] >= -180, bb["xmax"] <= 180,
    bb["ymin"] >= -90,  bb["ymax"] <= 90
  )
}

fix_crs_smart <- function(x, layer_name = "layer") {
  bb0 <- sf::st_bbox(x)
  message(layer_name, " original CRS: ", sf::st_crs(x)$input)
  message(layer_name, " original bbox: ",
          paste(names(bb0), round(as.numeric(bb0), 5), collapse = ", "))

  if (looks_like_lonlat(x)) {
    message(layer_name, ": coordinates look like lon/lat. Overriding CRS to EPSG:4326.")
    sf::st_crs(x) <- 4326
    return(x)
  }

  if (is.na(sf::st_crs(x))) {
    message(layer_name, ": no CRS found. Assuming EPSG:4326.")
    sf::st_crs(x) <- 4326
    return(x)
  }

  message(layer_name, ": transforming to EPSG:4326.")
  sf::st_transform(x, 4326)
}

safe_crop_bbox <- function(x, xlim, ylim) {
  bb <- sf::st_bbox(
    c(xmin = xlim[1], ymin = ylim[1], xmax = xlim[2], ymax = ylim[2]),
    crs = sf::st_crs(x)
  )

  out <- tryCatch(
    suppressWarnings(sf::st_crop(x, bb)),
    error = function(e) {
      message("st_crop failed; keeping original layer. Error: ", conditionMessage(e))
      x
    }
  )

  out <- out[!sf::st_is_empty(out), ]
  out
}

prepare_crop_geometry <- function(crop_area, simplify_tolerance = 0.0006) {
  message("Preparing crop/wheat geometry...")

  tmp <- crop_area
  tmp <- tmp[!sf::st_is_empty(tmp), ]

  tmp <- tryCatch(
    suppressWarnings(sf::st_make_valid(tmp)),
    error = function(e) {
      message("st_make_valid failed, keeping original geometry: ", conditionMessage(e))
      tmp
    }
  )

  geom_types <- unique(as.character(sf::st_geometry_type(tmp)))
  message("Geometry types after validation: ", paste(geom_types, collapse = ", "))

  if (any(grepl("GEOMETRYCOLLECTION", geom_types))) {
    tmp <- tryCatch(
      suppressWarnings(sf::st_collection_extract(tmp, "POLYGON")),
      error = function(e) {
        message("st_collection_extract failed, keeping original geometry: ", conditionMessage(e))
        tmp
      }
    )
  }

  tmp <- tmp[!sf::st_is_empty(tmp), ]

  if (nrow(tmp) > 0) {
    tmp <- tryCatch(
      suppressWarnings(sf::st_simplify(tmp, dTolerance = simplify_tolerance, preserveTopology = TRUE)),
      error = function(e) {
        message("st_simplify failed, using unsimplified geometry: ", conditionMessage(e))
        tmp
      }
    )
  }

  tmp <- tmp[!sf::st_is_empty(tmp), ]
  message("Final crop geometries for plotting: ", nrow(tmp))
  tmp
}

lon_lab <- function(x) paste0(x, "\u00B0E")
lat_lab <- function(x) paste0(x, "\u00B0N")

# -----------------------------------------------------------------------------
# 3) READ CIMMYT SPATIAL SUMMARY
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
  sf::st_as_sf(coords = c("MatchedLocLong", "MatchedLocLat"), crs = 4326, remove = FALSE)

write_csv(
  spatial_summary,
  file.path(output_dir, "Spatial_Summary_By_CIMMYT_Location_HighRes.csv")
)

# -----------------------------------------------------------------------------
# 4) READ EGYPT BOUNDARY AND WHEAT/CROP AREA
# -----------------------------------------------------------------------------

message("Reading Egypt boundary...")
egypt_boundary <- rnaturalearth::ne_countries(country = "Egypt", returnclass = "sf") %>%
  sf::st_transform(4326)

message("Reading wheat/crop shapefile...")
crop_raw <- sf::st_read(crop_shp, quiet = FALSE)

crop_raw <- fix_crs_smart(crop_raw, "Wheat/crop shapefile")

message("Crop rows after CRS correction: ", nrow(crop_raw))
message("Crop bbox after CRS correction:")
print(sf::st_bbox(crop_raw))

crop_area <- safe_crop_bbox(crop_raw, egypt_xlim, egypt_ylim)

if (nrow(crop_area) == 0) {
  message("Crop layer became zero after crop. Using full crop layer with coord_sf extent.")
  crop_area <- crop_raw
}

crop_clean <- prepare_crop_geometry(crop_area, simplify_tolerance = simplify_tolerance)

crop_diag <- tibble(
  crop_file = crop_shp,
  original_rows = nrow(crop_raw),
  cropped_rows = nrow(crop_area),
  final_plot_rows = nrow(crop_clean),
  crop_crs = as.character(sf::st_crs(crop_clean)$input),
  crop_bbox_xmin = as.numeric(sf::st_bbox(crop_clean)["xmin"]),
  crop_bbox_xmax = as.numeric(sf::st_bbox(crop_clean)["xmax"]),
  crop_bbox_ymin = as.numeric(sf::st_bbox(crop_clean)["ymin"]),
  crop_bbox_ymax = as.numeric(sf::st_bbox(crop_clean)["ymax"])
)

write_csv(crop_diag, file.path(output_dir, "Figure04_CropLayer_Diagnostics_HighRes.csv"))

if (nrow(crop_clean) == 0) {
  stop("Crop/wheat shapefile has no geometries for plotting. Check diagnostics CSV.")
}

# -----------------------------------------------------------------------------
# 5) RIVER NILE AND WATER BODIES
# -----------------------------------------------------------------------------

rivers_all <- tryCatch(
  rnaturalearth::ne_download(
    scale = 10,
    type = "rivers_lake_centerlines",
    category = "physical",
    returnclass = "sf"
  ) %>% sf::st_transform(4326),
  error = function(e) NULL
)

manual_nile_df <- tibble(
  lon = c(31.25, 31.18, 31.08, 30.98, 30.88, 30.73, 30.58, 30.42,
          30.27, 30.13, 30.05, 30.10, 30.45, 31.10),
  lat = c(21.85, 22.60, 23.40, 24.20, 25.00, 25.80, 26.60, 27.35,
          28.15, 29.00, 29.75, 30.45, 30.95, 31.35)
)

manual_nile <- manual_nile_df %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(do_union = FALSE) %>%
  sf::st_cast("LINESTRING")

if (!is.null(rivers_all) && "name" %in% names(rivers_all)) {
  nile <- rivers_all %>% filter(str_detect(str_to_lower(name), "nile"))
  if (nrow(nile) == 0) nile <- manual_nile
} else {
  nile <- manual_nile
}

nile <- safe_crop_bbox(nile, egypt_xlim, egypt_ylim)

lakes_all <- tryCatch(
  rnaturalearth::ne_download(
    scale = 10,
    type = "lakes",
    category = "physical",
    returnclass = "sf"
  ) %>% sf::st_transform(4326),
  error = function(e) NULL
)

if (!is.null(lakes_all)) {
  lakes_egypt <- safe_crop_bbox(lakes_all, egypt_xlim, egypt_ylim)
} else {
  lakes_egypt <- NULL
}

manual_lakes_points <- tibble(
  WaterBody = c("Lake Nasser", "Lake Qarun", "Northern Delta lakes"),
  lon = c(32.8, 30.6, 31.2),
  lat = c(23.3, 29.5, 31.2)
) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

# -----------------------------------------------------------------------------
# 6) THEME AND BASE LAYERS
# -----------------------------------------------------------------------------

theme_map <- function() {
  theme_classic(base_size = axis_text_size) +
    theme(
      text = element_text(color = "black"),
      plot.title = element_text(face = "bold", size = panel_title_size, color = "black"),
      plot.subtitle = element_text(size = panel_subtitle_size, color = "black"),
      axis.title = element_text(size = axis_title_size, color = "black"),
      axis.text = element_text(size = axis_text_size, color = "black"),
      axis.line = element_line(color = "black", linewidth = 0.7),
      axis.ticks = element_line(color = "black", linewidth = 0.7),
      legend.title = element_text(size = legend_title_size, face = "bold", color = "black"),
      legend.text = element_text(size = legend_text_size, color = "black"),
      legend.position = "right",
      legend.key.height = unit(1.05, "cm"),
      legend.key.width = unit(0.55, "cm"),
      legend.spacing.y = unit(0.20, "cm"),
      plot.margin = margin(8, 8, 8, 8)
    )
}

coord_egypt <- coord_sf(
  xlim = egypt_xlim,
  ylim = egypt_ylim,
  expand = FALSE
)

base_layers <- list(
  geom_sf(data = egypt_boundary, fill = "grey97", color = "grey25", linewidth = 0.75),
  geom_sf(data = crop_clean, fill = "#D9A441", color = "#7A4A08", linewidth = 0.18, alpha = 0.95),
  if (!is.null(lakes_egypt) && nrow(lakes_egypt) > 0) {
    geom_sf(data = lakes_egypt, fill = "#79BCE8", color = "#1976B2", linewidth = 0.35, alpha = 0.85)
  },
  geom_sf(data = nile, color = "#0072B2", linewidth = 1.35, alpha = 0.98),
  geom_sf(data = manual_lakes_points, color = "#0072B2", fill = "#79BCE8",
          shape = 21, size = 3.0, stroke = 0.7, alpha = 0.90)
)

label_layer <- geom_text_repel(
  data = spatial_summary,
  aes(x = MatchedLocLong, y = MatchedLocLat, label = MatchedLocName),
  size = location_label_size,
  fontface = "bold",
  color = "black",
  box.padding = 0.18,
  point.padding = 0.32,
  segment.size = 0.22,
  segment.color = "grey30",
  min.segment.length = 0,
  max.overlaps = Inf,
  force = 35,
  force_pull = 0.10,
  seed = 123
)

# -----------------------------------------------------------------------------
# 7) CREATE THREE MAP PANELS
# -----------------------------------------------------------------------------

p_heat <- ggplot() +
  base_layers +
  geom_sf(
    data = points_sf,
    aes(size = n_years, color = mean_heat_days),
    alpha = 0.98
  ) +
  label_layer +
  scale_color_viridis_c(
    option = "A",
    name = "Mean heat-stress\ndays",
    guide = guide_colorbar(
      title.position = "top",
      barheight = unit(4.2, "cm"),
      barwidth = unit(0.65, "cm")
    )
  ) +
  scale_size_continuous(
    name = "Years",
    range = point_size_range,
    breaks = c(5, 10, 15, 20, 25)
  ) +
  coord_egypt +
  scale_x_continuous(breaks = seq(26, 34, 2), labels = lon_lab) +
  scale_y_continuous(breaks = seq(22, 32, 2), labels = lat_lab) +
  labs(
    title = "A. Heat-stress exposure",
    subtitle = "Mean days with Tmax >32°C",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_map()

p_srad <- ggplot() +
  base_layers +
  geom_sf(
    data = points_sf,
    aes(size = n_years, color = mean_SRAD),
    alpha = 0.98
  ) +
  label_layer +
  scale_color_viridis_c(
    option = "D",
    name = "Mean SRAD\n(MJ m-2 d-1)",
    guide = guide_colorbar(
      title.position = "top",
      barheight = unit(4.2, "cm"),
      barwidth = unit(0.65, "cm")
    )
  ) +
  scale_size_continuous(
    name = "Years",
    range = point_size_range,
    breaks = c(5, 10, 15, 20, 25)
  ) +
  coord_egypt +
  scale_x_continuous(breaks = seq(26, 34, 2), labels = lon_lab) +
  scale_y_continuous(breaks = seq(22, 32, 2), labels = lat_lab) +
  labs(
    title = "B. Solar radiation",
    subtitle = "Mean incoming solar radiation",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_map()

gap_lim <- max(abs(spatial_summary$mean_yield_gap), na.rm = TRUE)

p_gap <- ggplot() +
  base_layers +
  geom_sf(
    data = points_sf,
    aes(size = n_years, color = mean_yield_gap),
    alpha = 0.98
  ) +
  label_layer +
  scale_color_gradient2(
    low = "steelblue",
    mid = "grey96",
    high = "firebrick",
    midpoint = 0,
    limits = c(-gap_lim, gap_lim),
    name = "Yield gap\n(t/ha)",
    guide = guide_colorbar(
      title.position = "top",
      barheight = unit(4.2, "cm"),
      barwidth = unit(0.65, "cm")
    )
  ) +
  scale_size_continuous(
    name = "Years",
    range = point_size_range,
    breaks = c(5, 10, 15, 20, 25)
  ) +
  coord_egypt +
  scale_x_continuous(breaks = seq(26, 34, 2), labels = lon_lab) +
  scale_y_continuous(breaks = seq(22, 32, 2), labels = lat_lab) +
  labs(
    title = "C. Yield gap",
    subtitle = "Simulated MME - observed CIMMYT",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_map()

# -----------------------------------------------------------------------------
# 8) COMBINE AND SAVE HIGH-RES MAP
# -----------------------------------------------------------------------------

fig_spatial <- (p_heat | p_srad | p_gap) +
  plot_layout(widths = c(1, 1, 1), guides = "keep") +
  plot_annotation(
    title = "Spatial distribution of climate exposure and yield gap across CIMMYT wheat locations in Egypt",
    subtitle = "Wheat/cropland area is shown in orange-brown, the River Nile and water bodies in blue, and CIMMYT matched locations as proportional symbols.",
    theme = theme(
      plot.title = element_text(face = "bold", size = main_title_size, color = "black"),
      plot.subtitle = element_text(size = main_subtitle_size, color = "black"),
      plot.margin = margin(10, 10, 10, 10)
    )
  )

ggsave(
  filename = file.path(output_dir, "Figure_04_ThreeMaps_Egypt_WheatArea_Nile_Lakes_HighRes.png"),
  plot = fig_spatial,
  width = main_width,
  height = main_height,
  dpi = main_dpi,
  limitsize = FALSE
)

ggsave(
  filename = file.path(output_dir, "Figure_04_ThreeMaps_Egypt_WheatArea_Nile_Lakes_HighRes.pdf"),
  plot = fig_spatial,
  width = main_width,
  height = main_height,
  limitsize = FALSE
)

ggsave(
  filename = file.path(output_dir, "Figure_04A_HeatStress_Egypt_WheatArea_Nile_Lakes_HighRes.png"),
  plot = p_heat,
  width = single_width,
  height = single_height,
  dpi = single_dpi,
  limitsize = FALSE
)

ggsave(
  filename = file.path(output_dir, "Figure_04B_SRAD_Egypt_WheatArea_Nile_Lakes_HighRes.png"),
  plot = p_srad,
  width = single_width,
  height = single_height,
  dpi = single_dpi,
  limitsize = FALSE
)

ggsave(
  filename = file.path(output_dir, "Figure_04C_YieldGap_Egypt_WheatArea_Nile_Lakes_HighRes.png"),
  plot = p_gap,
  width = single_width,
  height = single_height,
  dpi = single_dpi,
  limitsize = FALSE
)

message("Done.")
message("Output folder: ", output_dir)
message("Main figure: Figure_04_ThreeMaps_Egypt_WheatArea_Nile_Lakes_HighRes.png")
message("Diagnostic file: Figure04_CropLayer_Diagnostics_HighRes.csv")
