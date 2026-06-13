# =============================================================================
# Spatial interpolation of CIMMYT 90th percentile yield, farmer survey yield,
# and relative yield gap across Egypt at 10-km resolution
#
# Inputs:
#   D:/MMENCC/CIMMYTYield.xlsx
#   D:/MMENCC/SurveyYield.xlsx
#
# Outputs:
#   1) CIMMYT_90th_2015_2019_input_points.csv
#   2) Survey_mean_2015_2019_input_points.csv
#   3) CIMMYT_90th_10km_interpolated_grid.csv
#   4) Survey_10km_interpolated_grid.csv
#   5) Relative_Yield_Gap_10km_grid.csv
#
# RYG formula:
#   RYG (%) = ((CIMMYT_90th - Survey) / CIMMYT_90th) * 100
# =============================================================================

library(readxl)
library(dplyr)
library(readr)
library(sf)
library(gstat)
library(sp)
library(ggplot2)

# -----------------------------------------------------------------------------
# 1. Paths
# -----------------------------------------------------------------------------

base_dir <- "D:/MMENCC"

cimmyt_file <- file.path(base_dir, "CIMMYTYield.xlsx")
survey_file <- file.path(base_dir, "SurveyYield.xlsx")

out_dir <- file.path(base_dir, "Spatial_RYG_10km_2015_2019")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 2. Read data
# -----------------------------------------------------------------------------

cimmyt <- read_excel(cimmyt_file) %>%
  mutate(
    CIMMYTHarvestYr = as.integer(CIMMYTHarvestYr),
    Lat = as.numeric(Lat),
    Long = as.numeric(Long),
    CIMMYTYield = as.numeric(CIMMYTYield)
  ) %>%
  filter(
    CIMMYTHarvestYr %in% 2015:2019,
    !is.na(Lat),
    !is.na(Long),
    !is.na(CIMMYTYield)
  )

survey <- read_excel(survey_file) %>%
  mutate(
    SurveyHarvestYears = as.integer(SurveyHarvestYears),
    Lat = as.numeric(Lat),
    Long = as.numeric(Long),
    ObsSurveyGY = as.numeric(ObsSurveyGY)
  ) %>%
  filter(
    SurveyHarvestYears %in% 2015:2019,
    !is.na(Lat),
    !is.na(Long),
    !is.na(ObsSurveyGY)
  )

# -----------------------------------------------------------------------------
# 3. Aggregate CIMMYT and Survey data for 2015–2019
# -----------------------------------------------------------------------------
# CIMMYT:
# For each coordinate/location, calculate the 90th percentile yield across all
# genotypes and years during 2015–2019.
#
# Survey:
# For each coordinate, calculate mean observed farmer yield across 2015–2019.
# -----------------------------------------------------------------------------

cimmyt_90_points <- cimmyt %>%
  group_by(Long, Lat, Loc_desc) %>%
  summarise(
    CIMMYT_90th_yield = quantile(CIMMYTYield, 0.90, na.rm = TRUE),
    CIMMYT_mean_yield = mean(CIMMYTYield, na.rm = TRUE),
    n_records = n(),
    n_years = n_distinct(CIMMYTHarvestYr),
    n_genotypes = n_distinct(GID),
    .groups = "drop"
  )

survey_mean_points <- survey %>%
  group_by(Long, Lat) %>%
  summarise(
    Survey_mean_yield = mean(ObsSurveyGY, na.rm = TRUE),
    Survey_median_yield = median(ObsSurveyGY, na.rm = TRUE),
    n_records = n(),
    n_years = n_distinct(SurveyHarvestYears),
    .groups = "drop"
  )

write_csv(
  cimmyt_90_points,
  file.path(out_dir, "CIMMYT_90th_2015_2019_input_points.csv")
)

write_csv(
  survey_mean_points,
  file.path(out_dir, "Survey_mean_2015_2019_input_points.csv")
)

# -----------------------------------------------------------------------------
# 4. Convert to sf and project to Egypt metric CRS
# -----------------------------------------------------------------------------
# EPSG:32636 = WGS 84 / UTM zone 36N
# Suitable for Egypt and 10-km interpolation.
# -----------------------------------------------------------------------------

egypt_crs <- 32636

cimmyt_sf <- st_as_sf(
  cimmyt_90_points,
  coords = c("Long", "Lat"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(egypt_crs)

survey_sf <- st_as_sf(
  survey_mean_points,
  coords = c("Long", "Lat"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(egypt_crs)

# -----------------------------------------------------------------------------
# 5. Create common 10-km grid covering both datasets
# -----------------------------------------------------------------------------

all_points <- rbind(
  cimmyt_sf %>% select(geometry),
  survey_sf %>% select(geometry)
)

bbox <- st_bbox(all_points)

# Add buffer around points to cover Egypt wheat environments
buffer_m <- 50000

bbox_expanded <- bbox
bbox_expanded["xmin"] <- bbox_expanded["xmin"] - buffer_m
bbox_expanded["xmax"] <- bbox_expanded["xmax"] + buffer_m
bbox_expanded["ymin"] <- bbox_expanded["ymin"] - buffer_m
bbox_expanded["ymax"] <- bbox_expanded["ymax"] + buffer_m

grid_sf <- st_make_grid(
  st_as_sfc(bbox_expanded),
  cellsize = 10000,
  what = "centers",
  square = TRUE
) %>%
  st_sf() %>%
  mutate(grid_id = row_number())

grid_sp <- as(grid_sf, "Spatial")
cimmyt_sp <- as(cimmyt_sf, "Spatial")
survey_sp <- as(survey_sf, "Spatial")

# -----------------------------------------------------------------------------
# 6. IDW interpolation to common 10-km grid
# -----------------------------------------------------------------------------

idw_power <- 2
idw_nmax <- 12

cimmyt_idw <- gstat::idw(
  formula = CIMMYT_90th_yield ~ 1,
  locations = cimmyt_sp,
  newdata = grid_sp,
  idp = idw_power,
  nmax = idw_nmax
)

survey_idw <- gstat::idw(
  formula = Survey_mean_yield ~ 1,
  locations = survey_sp,
  newdata = grid_sp,
  idp = idw_power,
  nmax = idw_nmax
)

# -----------------------------------------------------------------------------
# 7. Convert interpolated grids back to sf and WGS84
# -----------------------------------------------------------------------------

cimmyt_grid <- st_as_sf(cimmyt_idw) %>%
  rename(CIMMYT_90th_yield_IDW = var1.pred) %>%
  mutate(grid_id = row_number()) %>%
  st_transform(4326)

survey_grid <- st_as_sf(survey_idw) %>%
  rename(Survey_yield_IDW = var1.pred) %>%
  mutate(grid_id = row_number()) %>%
  st_transform(4326)

# Extract lon/lat
cimmyt_grid_csv <- cimmyt_grid %>%
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(grid_id, lon, lat, CIMMYT_90th_yield_IDW)

survey_grid_csv <- survey_grid %>%
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(grid_id, lon, lat, Survey_yield_IDW)

# -----------------------------------------------------------------------------
# 8. Calculate Relative Yield Gap
# -----------------------------------------------------------------------------

ryg_grid <- cimmyt_grid_csv %>%
  left_join(survey_grid_csv, by = c("grid_id", "lon", "lat")) %>%
  mutate(
    Absolute_Yield_Gap_t_ha = CIMMYT_90th_yield_IDW - Survey_yield_IDW,
    Relative_Yield_Gap_percent = (
      (CIMMYT_90th_yield_IDW - Survey_yield_IDW) /
        CIMMYT_90th_yield_IDW
    ) * 100
  )

# -----------------------------------------------------------------------------
# 9. Save outputs
# -----------------------------------------------------------------------------

write_csv(
  cimmyt_grid_csv,
  file.path(out_dir, "CIMMYT_90th_10km_interpolated_grid.csv")
)

write_csv(
  survey_grid_csv,
  file.path(out_dir, "Survey_10km_interpolated_grid.csv")
)

write_csv(
  ryg_grid,
  file.path(out_dir, "Relative_Yield_Gap_10km_grid.csv")
)

write_csv(
  ryg_grid,
  file.path(out_dir, "RYG_CIMMYT90_minus_Survey_2015_2019_10km.csv")
)

# -----------------------------------------------------------------------------
# 10. Quick diagnostic maps
# -----------------------------------------------------------------------------

p1 <- ggplot(ryg_grid, aes(x = lon, y = lat, color = CIMMYT_90th_yield_IDW)) +
  geom_point(size = 1.2) +
  scale_color_viridis_c(option = "C") +
  theme_classic() +
  labs(
    title = "CIMMYT 90th percentile yield, 2015–2019",
    color = "t/ha",
    x = "Longitude",
    y = "Latitude"
  )

p2 <- ggplot(ryg_grid, aes(x = lon, y = lat, color = Survey_yield_IDW)) +
  geom_point(size = 1.2) +
  scale_color_viridis_c(option = "C") +
  theme_classic() +
  labs(
    title = "Survey observed yield, 2015–2019",
    color = "t/ha",
    x = "Longitude",
    y = "Latitude"
  )

p3 <- ggplot(ryg_grid, aes(x = lon, y = lat, color = Relative_Yield_Gap_percent)) +
  geom_point(size = 1.2) +
  scale_color_gradient2(
    low = "darkgreen",
    mid = "yellow",
    high = "red",
    midpoint = 0
  ) +
  theme_classic() +
  labs(
    title = "Relative yield gap: CIMMYT 90th percentile − Survey",
    color = "RYG (%)",
    x = "Longitude",
    y = "Latitude"
  )

ggsave(
  file.path(out_dir, "Map_CIMMYT_90th_10km.png"),
  p1,
  width = 7,
  height = 8,
  dpi = 500
)

ggsave(
  file.path(out_dir, "Map_Survey_10km.png"),
  p2,
  width = 7,
  height = 8,
  dpi = 500
)

ggsave(
  file.path(out_dir, "Map_RYG_10km.png"),
  p3,
  width = 7,
  height = 8,
  dpi = 500
)

message("Done.")
message("Outputs saved in: ", out_dir)