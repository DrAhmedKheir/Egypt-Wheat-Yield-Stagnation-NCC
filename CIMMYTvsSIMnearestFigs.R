suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(janitor)
  library(sf)
  library(patchwork)
  library(scales)
  library(viridis)
  library(rnaturalearth)
  library(purrr)
})

sf::sf_use_s2(FALSE)

base_dir   <- "D:/CIMMYTvsSim"
input_file <- file.path(base_dir, "observedCIMMYT_vs_simulated_wideMME.csv")
shape_file <- file.path(base_dir, "Shapefile", "EgyptCropland2024.shp")
out_dir    <- file.path(base_dir, "output_maps_gap_mme_safe")

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

save_plot <- function(p, filename, width = 10, height = 8, dpi = 320) {
  ggsave(
    filename = file.path(out_dir, filename),
    plot = p,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
}

find_first_existing <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)][1]
  if (length(hit) == 0 || is.na(hit)) return(NA_character_)
  hit
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

theme_map_pub <- function(base_size = 14) {
  theme_bw(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 4),
      plot.subtitle = element_text(size = base_size),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey88"),
      legend.title = element_text(face = "bold"),
      legend.position = "right"
    )
}

fit_segment_line <- function(df, series_name, start_year, end_year) {
  sub <- df %>%
    filter(series == series_name, year >= start_year, year <= end_year) %>%
    filter(!is.na(yield))
  
  if (nrow(sub) < 2) {
    return(tibble(
      year = numeric(0),
      fit = numeric(0),
      series = character(0),
      period = character(0),
      slope = numeric(0),
      intercept = numeric(0),
      r2 = numeric(0)
    ))
  }
  
  mod <- lm(yield ~ year, data = sub)
  newdat <- tibble(year = seq(min(sub$year), max(sub$year), by = 1))
  newdat$fit <- predict(mod, newdata = newdat)
  newdat$series <- series_name
  newdat$period <- paste0(start_year, "-", end_year)
  newdat$slope <- unname(coef(mod)[["year"]])
  newdat$intercept <- unname(coef(mod)[["(Intercept)"]])
  newdat$r2 <- summary(mod)$r.squared
  newdat
}

dat_raw <- read_csv(input_file, show_col_types = FALSE) %>% clean_names()

year_col    <- find_first_existing(dat_raw, c("year"))
obs_col     <- find_first_existing(dat_raw, c("observed_yield", "obs_yield"))
mme_col     <- find_first_existing(dat_raw, c("mme", "ensemble_mean", "simulated_yield"))
obs_lat_col <- find_first_existing(dat_raw, c("obs_lat", "lat_obs"))
obs_lon_col <- find_first_existing(dat_raw, c("obs_lon", "lon_obs"))
sim_lat_col <- find_first_existing(dat_raw, c("sim_lat", "lat_sim"))
sim_lon_col <- find_first_existing(dat_raw, c("sim_lon", "lon_sim"))
dist_col    <- find_first_existing(dat_raw, c("distance_km", "matched_distance_km"))
obs_loc_col <- find_first_existing(dat_raw, c("obs_location", "location", "loc_desc"))
sim_loc_col <- find_first_existing(dat_raw, c("sim_location", "matched_sim_location"))

required_detected <- c(year_col, obs_col, mme_col, obs_lat_col, obs_lon_col, sim_lat_col, sim_lon_col, dist_col)

if (any(is.na(required_detected))) {
  stop(
    paste0(
      "Could not detect all required columns.\n",
      "year_col    = ", year_col, "\n",
      "obs_col     = ", obs_col, "\n",
      "mme_col     = ", mme_col, "\n",
      "obs_lat_col = ", obs_lat_col, "\n",
      "obs_lon_col = ", obs_lon_col, "\n",
      "sim_lat_col = ", sim_lat_col, "\n",
      "sim_lon_col = ", sim_lon_col, "\n",
      "dist_col    = ", dist_col, "\n"
    )
  )
}

dat <- dat_raw %>%
  transmute(
    year = suppressWarnings(as.integer(.data[[year_col]])),
    observed_yield = as.numeric(.data[[obs_col]]),
    MME = as.numeric(.data[[mme_col]]),
    obs_lat = as.numeric(.data[[obs_lat_col]]),
    obs_lon = as.numeric(.data[[obs_lon_col]]),
    sim_lat = as.numeric(.data[[sim_lat_col]]),
    sim_lon = as.numeric(.data[[sim_lon_col]]),
    distance_km = as.numeric(.data[[dist_col]]),
    obs_location = if (!is.na(obs_loc_col)) as.character(.data[[obs_loc_col]]) else NA_character_,
    sim_location = if (!is.na(sim_loc_col)) as.character(.data[[sim_loc_col]]) else NA_character_
  ) %>%
  filter(
    !is.na(year),
    !is.na(observed_yield),
    !is.na(MME),
    !is.na(obs_lat), !is.na(obs_lon),
    !is.na(sim_lat), !is.na(sim_lon)
  ) %>%
  mutate(
    yield_gap = MME - observed_yield,
    abs_gap = abs(yield_gap)
  )

write_csv(dat, file.path(out_dir, "matched_observed_simulated_gap_table.csv"))

egypt_crop <- st_read(shape_file, quiet = TRUE)
egypt_crop <- st_make_valid(egypt_crop)
if (is.na(st_crs(egypt_crop))) st_crs(egypt_crop) <- 4326
egypt_crop <- st_transform(egypt_crop, 4326)

egypt_boundary <- ne_countries(scale = 50, country = "Egypt", returnclass = "sf") %>%
  st_transform(4326)

egypt_xlim <- c(24, 37)
egypt_ylim <- c(21.5, 32.5)

obs_sites_mean <- dat %>%
  group_by(obs_location, obs_lat, obs_lon) %>%
  summarise(
    observed_yield = mean(observed_yield, na.rm = TRUE),
    n_years = n(),
    .groups = "drop"
  )

sim_sites_mean <- dat %>%
  group_by(sim_location, sim_lat, sim_lon) %>%
  summarise(
    MME = mean(MME, na.rm = TRUE),
    n_years = n(),
    .groups = "drop"
  )

gap_sites_mean <- dat %>%
  group_by(obs_location, obs_lat, obs_lon) %>%
  summarise(
    yield_gap = mean(yield_gap, na.rm = TRUE),
    abs_gap = mean(abs_gap, na.rm = TRUE),
    mean_distance_km = mean(distance_km, na.rm = TRUE),
    n_years = n(),
    .groups = "drop"
  )

obs_sites_sf <- st_as_sf(obs_sites_mean, coords = c("obs_lon", "obs_lat"), crs = 4326, remove = FALSE)
sim_sites_sf <- st_as_sf(sim_sites_mean, coords = c("sim_lon", "sim_lat"), crs = 4326, remove = FALSE)
gap_sites_sf <- st_as_sf(gap_sites_mean, coords = c("obs_lon", "obs_lat"), crs = 4326, remove = FALSE)

yield_range <- range(c(obs_sites_sf$observed_yield, sim_sites_sf$MME), na.rm = TRUE)
gap_lim <- max(abs(gap_sites_sf$yield_gap), na.rm = TRUE)

add_base_layers <- function() {
  list(
    geom_sf(data = egypt_boundary, fill = "grey98", color = "black", linewidth = 0.6),
    geom_sf(data = egypt_crop, fill = "#E9F3E2", color = NA, alpha = 0.35)
  )
}

make_yield_map <- function(sf_df, value_col, title_txt, subtitle_txt, ylab_txt = NULL) {
  ggplot() +
    add_base_layers() +
    geom_sf(
      data = sf_df,
      aes(color = .data[[value_col]], size = .data[[value_col]]),
      alpha = 0.92
    ) +
    scale_color_viridis_c(option = "plasma", limits = yield_range, oob = scales::squish) +
    scale_size_continuous(range = c(2.5, 6), limits = yield_range) +
    coord_sf(xlim = egypt_xlim, ylim = egypt_ylim, expand = FALSE) +
    labs(
      title = title_txt,
      subtitle = subtitle_txt,
      x = "Longitude",
      y = "Latitude",
      color = ylab_txt %||% value_col,
      size = ylab_txt %||% value_col
    ) +
    theme_map_pub()
}

make_gap_map <- function(sf_df, value_col = "yield_gap", title_txt, subtitle_txt) {
  ggplot() +
    add_base_layers() +
    geom_sf(
      data = sf_df,
      aes(color = .data[[value_col]], size = abs(.data[[value_col]])),
      alpha = 0.94
    ) +
    scale_color_gradient2(
      low = "#2166AC",
      mid = "white",
      high = "#B2182B",
      midpoint = 0,
      limits = c(-gap_lim, gap_lim),
      oob = scales::squish
    ) +
    scale_size_continuous(range = c(2.5, 6)) +
    coord_sf(xlim = egypt_xlim, ylim = egypt_ylim, expand = FALSE) +
    labs(
      title = title_txt,
      subtitle = subtitle_txt,
      x = "Longitude",
      y = "Latitude",
      color = "Yield gap\n(MME - Obs)",
      size = "|Gap|"
    ) +
    theme_map_pub()
}

p_obs_mean <- make_yield_map(
  obs_sites_sf,
  "observed_yield",
  "Observed CIMMYT yield over Egypt",
  "Mean observed yield at CIMMYT coordinates across all available years",
  "Observed yield"
)

p_sim_mean <- make_yield_map(
  sim_sites_sf,
  "MME",
  "Simulated ensemble yield (MME) over Egypt",
  "Mean simulated yield at matched nearest simulated coordinates across all available years",
  "Simulated yield"
)

p_gap_mean <- make_gap_map(
  gap_sites_sf,
  "yield_gap",
  "Yield gap over Egypt",
  "Mean yield gap = MME - observed yield, shown at observed CIMMYT coordinates"
)

save_plot(p_obs_mean, "01_map_observed_mean.png", width = 10, height = 8.5)
save_plot(p_sim_mean, "02_map_simulated_mme_mean.png", width = 10, height = 8.5)
save_plot(p_gap_mean, "03_map_yield_gap_mean.png", width = 10, height = 8.5)

combined_main <- (p_obs_mean | p_sim_mean) / p_gap_mean +
  plot_annotation(title = "Observed yield, simulated yield, and yield gap over Egypt")

save_plot(combined_main, "04_combined_main_maps.png", width = 18, height = 14)

yearly_summary <- dat %>%
  group_by(year) %>%
  summarise(
    n_records = n(),
    observed_mean = mean(observed_yield, na.rm = TRUE),
    simulated_mean = mean(MME, na.rm = TRUE),
    gap_mean = mean(yield_gap, na.rm = TRUE),
    distance_km_mean = mean(distance_km, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(yearly_summary, file.path(out_dir, "yearly_summary_yield_gap.csv"))

yearly_long <- yearly_summary %>%
  select(year, observed_mean, simulated_mean) %>%
  pivot_longer(cols = c(observed_mean, simulated_mean), names_to = "series", values_to = "yield") %>%
  mutate(series = recode(
    series,
    observed_mean = "Observed CIMMYT",
    simulated_mean = "Simulated MME"
  ))

full_start <- min(yearly_long$year, na.rm = TRUE)
full_end   <- max(yearly_long$year, na.rm = TRUE)

trend_periods <- tibble(
  start_year = c(full_start, 1980, 2000),
  end_year   = c(full_end, 1995, 2020)
)

trend_lines <- pmap_dfr(trend_periods, function(start_year, end_year) {
  bind_rows(
    fit_segment_line(yearly_long, "Observed CIMMYT", start_year, end_year),
    fit_segment_line(yearly_long, "Simulated MME", start_year, end_year)
  )
})

trend_stats <- if (nrow(trend_lines) > 0) {
  trend_lines %>%
    distinct(series, period, slope, intercept, r2) %>%
    arrange(series, period)
} else {
  tibble(series = character(0), period = character(0), slope = numeric(0), intercept = numeric(0), r2 = numeric(0))
}

write_csv(trend_stats, file.path(out_dir, "trend_statistics_figure5.csv"))

period_lty <- c(
  setNames("solid", paste0(full_start, "-", full_end)),
  "1980-1995" = "dashed",
  "2000-2020" = "dotdash"
)

p_time <- ggplot() +
  geom_line(data = yearly_long, aes(x = year, y = yield, color = series), linewidth = 1.2) +
  geom_point(data = yearly_long, aes(x = year, y = yield, color = series), size = 2) +
  geom_line(data = trend_lines, aes(x = year, y = fit, color = series, linetype = period), linewidth = 1.1, alpha = 0.95) +
  scale_linetype_manual(values = period_lty) +
  labs(
    title = "Observed and simulated yield by year",
    subtitle = "Yearly mean yields from matched observed-simulated comparison records with regression lines",
    x = "Year",
    y = "Yield (t/ha)",
    color = NULL,
    linetype = "Regression period"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

save_plot(p_time, "05_yearly_mean_observed_vs_simulated.png", width = 13, height = 7.5)

p_time_facet <- ggplot() +
  geom_line(data = yearly_long, aes(x = year, y = yield, color = series), linewidth = 1.2, show.legend = FALSE) +
  geom_point(data = yearly_long, aes(x = year, y = yield, color = series), size = 2, show.legend = FALSE) +
  geom_line(data = trend_lines, aes(x = year, y = fit, linetype = period), color = "black", linewidth = 1.0) +
  facet_wrap(~ series, ncol = 1, scales = "free_y") +
  scale_linetype_manual(values = period_lty) +
  labs(
    title = "Trend lines for observed and simulated yield",
    subtitle = "Black lines represent whole period, 1980-1995, and 2000-2020 regressions",
    x = "Year",
    y = "Yield (t/ha)",
    linetype = "Regression period"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

save_plot(p_time_facet, "05b_yearly_mean_observed_vs_simulated_faceted_trends.png", width = 12, height = 9)

p_gap_time <- ggplot(yearly_summary, aes(x = year, y = gap_mean)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 1.1, color = "firebrick") +
  geom_point(size = 2, color = "firebrick") +
  labs(
    title = "Yearly mean yield gap",
    subtitle = "Yield gap = simulated MME - observed CIMMYT",
    x = "Year",
    y = "Yield gap (t/ha)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

save_plot(p_gap_time, "06_yearly_mean_yield_gap.png", width = 12, height = 7)

p_dist <- ggplot(dat, aes(x = factor(year), y = distance_km)) +
  geom_boxplot(fill = "grey75", color = "grey20", outlier.alpha = 0.3) +
  labs(
    title = "Distance to nearest simulated point by year",
    subtitle = "Nearest matched simulated point distance used in the observed-simulated comparison",
    x = "Year",
    y = "Distance (km)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

save_plot(p_dist, "07_distance_km_by_year.png", width = 14, height = 7)

summary_lines <- c(
  "Observed CIMMYT vs simulated MME mapping workflow",
  "=================================================",
  paste("Input CSV:", input_file),
  paste("Cropland shapefile:", shape_file),
  paste("Number of matched records:", nrow(dat)),
  paste("Observed locations:", n_distinct(dat$obs_location)),
  paste("Simulated locations:", n_distinct(dat$sim_location)),
  paste("Years:", paste(range(dat$year), collapse = " - ")),
  paste("Mean observed yield:", round(mean(dat$observed_yield, na.rm = TRUE), 3)),
  paste("Mean simulated MME yield:", round(mean(dat$MME, na.rm = TRUE), 3)),
  paste("Mean yield gap (MME - Obs):", round(mean(dat$yield_gap, na.rm = TRUE), 3)),
  paste("Mean nearest-point distance (km):", round(mean(dat$distance_km, na.rm = TRUE), 3))
)

writeLines(summary_lines, con = file.path(out_dir, "summary.txt"))

cat("\nAll outputs saved to:\n", out_dir, "\n")