# =============================================================================
# Visualize CIMMYT observed and simulated wheat yield with error bars
# =============================================================================
# Input files:
#   D:/Asseng_paper2024/Updated/Asseng_updated/40YRSSIMULATIONS/ForShawn/21072025/21072025/Update27032026/CIMMYTSimulations/CIMMYTYieldDetails.xls
#   D:/Asseng_paper2024/Updated/Asseng_updated/40YRSSIMULATIONS/ForShawn/21072025/21072025/Update27032026/CIMMYTSimulations/CIMMYTSimulatedMME.xlsx
#
# Output:
#   CIMMYT_Observed_vs_Simulated_Yield.png
#   CIMMYT_Observed_vs_Simulated_Yield.pdf
#   CIMMYT_Observed_vs_Simulated_Yield.csv
#
# Plot:
#   Simulated yield = line
#   Observed yield  = scatter points
#   Error bars      = observed mean +/- SE by Location-Year
#
# Notes:
#   Observed YieldCIMMYT appears to be in t/ha.
#   SimYieldMME appears to be in kg/ha, so it is converted to t/ha by /1000.
# =============================================================================

# install.packages(c("readxl", "dplyr", "ggplot2", "stringr", "janitor"))

library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)

# -----------------------------------------------------------------------------
# 1) USER SETTINGS
# -----------------------------------------------------------------------------

base_dir <- "D:/Asseng_paper2024/Updated/Asseng_updated/40YRSSIMULATIONS/ForShawn/21072025/21072025/Update27032026/CIMMYTSimulations"

observed_file  <- file.path(base_dir, "CIMMYTYieldDetails.xls")
simulated_file <- file.path(base_dir, "CIMMYTSimulatedMME.xlsx")

output_dir <- file.path(base_dir, "Yield_Observed_vs_Simulated_Figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Use TRUE because simulated values are around 7000 kg/ha while observed are around 5-8 t/ha.
convert_sim_kg_ha_to_t_ha <- TRUE

# -----------------------------------------------------------------------------
# 2) READ DATA
# -----------------------------------------------------------------------------

obs_raw <- read_excel(observed_file, sheet = 1)
sim_raw <- read_excel(simulated_file, sheet = 1)

# Expected observed columns:
# Nursery, Nursery_Yr, Year, Loc_desc, Lat, Long, SowingDate, YieldCIMMYT
# Expected simulated columns:
# Location, WeatherCode, Year, Lat, Long, SimYieldMME

obs <- obs_raw %>%
  mutate(
    Year = as.integer(Year),
    Lat = as.numeric(Lat),
    Long = as.numeric(Long),
    Yield_obs = as.numeric(YieldCIMMYT),
    Loc_desc = str_squish(as.character(Loc_desc))
  ) %>%
  filter(!is.na(Year), !is.na(Yield_obs), !is.na(Lat), !is.na(Long))

sim <- sim_raw %>%
  mutate(
    Year = as.integer(Year),
    Lat = as.numeric(Lat),
    Long = as.numeric(Long),
    SimYieldMME = as.numeric(SimYieldMME),
    Yield_sim = if (convert_sim_kg_ha_to_t_ha) SimYieldMME / 1000 else SimYieldMME,
    Location = str_squish(as.character(Location)),
    WeatherCode = str_squish(as.character(WeatherCode))
  ) %>%
  filter(!is.na(Year), !is.na(Yield_sim), !is.na(Lat), !is.na(Long))

# -----------------------------------------------------------------------------
# 3) MATCH OBSERVED LOCATIONS TO SIMULATED LOCATIONS BY NEAREST COORDINATE
# -----------------------------------------------------------------------------

sim_locs <- sim %>%
  distinct(Location, WeatherCode, Lat_sim = Lat, Long_sim = Long)

obs_locs <- obs %>%
  distinct(Loc_desc, Lat_obs = Lat, Long_obs = Long)

# Haversine distance in km
haversine_km <- function(lat1, lon1, lat2, lon2) {
  R <- 6371
  to_rad <- pi / 180
  phi1 <- lat1 * to_rad
  phi2 <- lat2 * to_rad
  dphi <- (lat2 - lat1) * to_rad
  dlambda <- (lon2 - lon1) * to_rad
  a <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlambda / 2)^2
  2 * R * atan2(sqrt(a), sqrt(1 - a))
}

loc_match <- lapply(seq_len(nrow(obs_locs)), function(i) {
  o <- obs_locs[i, ]

  tmp <- sim_locs %>%
    mutate(
      distance_km = haversine_km(o$Lat_obs, o$Long_obs, Lat_sim, Long_sim),
      Loc_desc = o$Loc_desc,
      Lat_obs = o$Lat_obs,
      Long_obs = o$Long_obs
    ) %>%
    arrange(distance_km) %>%
    slice(1)

  tmp
}) %>%
  bind_rows() %>%
  select(Loc_desc, Lat_obs, Long_obs, Location, WeatherCode, Lat_sim, Long_sim, distance_km)

write.csv(
  loc_match,
  file.path(output_dir, "Location_matching_observed_to_simulated.csv"),
  row.names = FALSE
)

# -----------------------------------------------------------------------------
# 4) SUMMARIZE OBSERVED YIELD AND PREPARE SIMULATED YIELD
# -----------------------------------------------------------------------------

obs_summary <- obs %>%
  left_join(loc_match %>% select(Loc_desc, Location, WeatherCode), by = "Loc_desc") %>%
  group_by(Location, WeatherCode, Year) %>%
  summarise(
    obs_mean = mean(Yield_obs, na.rm = TRUE),
    obs_sd = ifelse(n() > 1, sd(Yield_obs, na.rm = TRUE), NA_real_),
    obs_n = n(),
    obs_se = ifelse(obs_n > 1, obs_sd / sqrt(obs_n), NA_real_),
    .groups = "drop"
  )

sim_summary <- sim %>%
  group_by(Location, WeatherCode, Year) %>%
  summarise(
    sim_mean = mean(Yield_sim, na.rm = TRUE),
    sim_sd = ifelse(n() > 1, sd(Yield_sim, na.rm = TRUE), NA_real_),
    sim_n = n(),
    sim_se = ifelse(sim_n > 1, sim_sd / sqrt(sim_n), NA_real_),
    .groups = "drop"
  )

plot_df <- full_join(
  sim_summary,
  obs_summary,
  by = c("Location", "WeatherCode", "Year")
) %>%
  arrange(Location, Year)

write.csv(
  plot_df,
  file.path(output_dir, "CIMMYT_Observed_vs_Simulated_Yield.csv"),
  row.names = FALSE
)

# -----------------------------------------------------------------------------
# 5) PLOT ALL LOCATIONS IN FACETS
# -----------------------------------------------------------------------------

p <- ggplot(plot_df, aes(x = Year)) +
  geom_line(aes(y = sim_mean, group = WeatherCode, color = "Simulated MME"),
            linewidth = 0.7, na.rm = TRUE) +
  geom_point(aes(y = obs_mean, color = "Observed CIMMYT"),
             size = 2.2, na.rm = TRUE) +
  geom_errorbar(
    aes(ymin = obs_mean - obs_se, ymax = obs_mean + obs_se, color = "Observed CIMMYT"),
    width = 0.25,
    linewidth = 0.4,
    na.rm = TRUE
  ) +
  facet_wrap(~ Location, scales = "free_x", ncol = 4) +
  scale_color_manual(
    name = "",
    values = c("Simulated MME" = "steelblue", "Observed CIMMYT" = "black")
  ) +
  labs(
    title = "Observed CIMMYT yield vs simulated MME yield",
    subtitle = "Observed = points with SE error bars; simulated = line",
    x = "Year",
    y = "Grain yield (t/ha)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "top",
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  filename = file.path(output_dir, "CIMMYT_Observed_vs_Simulated_Yield.png"),
  plot = p,
  width = 16,
  height = 11,
  dpi = 300
)

ggsave(
  filename = file.path(output_dir, "CIMMYT_Observed_vs_Simulated_Yield.pdf"),
  plot = p,
  width = 16,
  height = 11
)

# -----------------------------------------------------------------------------
# 6) OPTIONAL: ONE PNG PER LOCATION
# -----------------------------------------------------------------------------

location_names <- sort(unique(na.omit(plot_df$Location)))

for (loc in location_names) {

  df_loc <- plot_df %>% filter(Location == loc)

  p_loc <- ggplot(df_loc, aes(x = Year)) +
    geom_line(aes(y = sim_mean, color = "Simulated MME"),
              linewidth = 0.9, na.rm = TRUE) +
    geom_point(aes(y = obs_mean, color = "Observed CIMMYT"),
               size = 2.8, na.rm = TRUE) +
    geom_errorbar(
      aes(ymin = obs_mean - obs_se, ymax = obs_mean + obs_se, color = "Observed CIMMYT"),
      width = 0.3,
      linewidth = 0.5,
      na.rm = TRUE
    ) +
    scale_color_manual(
      name = "",
      values = c("Simulated MME" = "steelblue", "Observed CIMMYT" = "black")
    ) +
    labs(
      title = paste("Observed vs simulated yield:", loc),
      x = "Year",
      y = "Grain yield (t/ha)"
    ) +
    theme_bw(base_size = 13) +
    theme(
      legend.position = "top",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  safe_name <- gsub("[^A-Za-z0-9_]+", "_", loc)

  ggsave(
    filename = file.path(output_dir, paste0("Yield_", safe_name, ".png")),
    plot = p_loc,
    width = 8,
    height = 5,
    dpi = 300
  )
}

message("Done.")
message("Outputs are in: ", output_dir)
