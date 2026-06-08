# =============================================================================
# CIMMYT matched-location climate metrics from DSSAT WTH files
# =============================================================================
# Purpose:
#   For each CIMMYT observed yield record and matched simulation location, extract
#   climate metrics from DSSAT WTH weather files.
#
# Metrics calculated:
#   - Mean daily Tmax
#   - Mean daily Tmin
#   - Mean daily Tmean
#   - Heat stress: number of days with Tmax > 32 °C
#   - Solar radiation: mean daily SRAD and cumulative SRAD
#   - VPD: vapor pressure deficit
#   - Optional HDW-like days: Tmax high + RH low + wind high
#
# Main input files:
#   Observed CIMMYT:
#     D:/Asseng_paper2024/Updated/Asseng_updated/40YRSSIMULATIONS/ForShawn/21072025/21072025/Update27032026/CIMMYTSimulations/CIMMYTYieldDetails.xls
#
#   Simulated CIMMYT:
#     D:/Asseng_paper2024/Updated/Asseng_updated/40YRSSIMULATIONS/ForShawn/21072025/21072025/Update27032026/CIMMYTSimulations/CIMMYTSimulatedMME.xlsx
#
#   WTH files:
#     D:/Asseng_paper2024/Updated/Asseng_updated/40YRSSIMULATIONS/ForShawn/21072025/21072025/Update27032026/CIMMYT_WTH_Output_RainFixed
#
# Output folder:
#     .../CIMMYTSimulations/CIMMYT_ClimateMetrics_From_WTH
#
# IMPORTANT:
#   Your CIMMYTYieldDetails file contains SowingDate but not anthesis/maturity.
#   Therefore, this script uses a practical "last 3 months" window:
#     window_start = SowingDate + 60 days
#     window_end   = SowingDate + 150 days
#   This approximates the last 90 days of the wheat season.
#
#   If you later add MaturityDate, HarvestDate, MDAT(DOY), or DaystoMaturity
#   columns, the script will automatically use them and define the last 90 days
#   as the final 90 days before maturity/harvest.
# =============================================================================

# install.packages(c("readxl", "dplyr", "readr", "stringr", "lubridate", "geosphere", "writexl"))

library(readxl)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(geosphere)
library(writexl)

# -----------------------------------------------------------------------------
# 1) USER SETTINGS
# -----------------------------------------------------------------------------

base_dir <- "D:/Asseng_paper2024/Updated/Asseng_updated/40YRSSIMULATIONS/ForShawn/21072025/21072025/Update27032026"

sim_dir <- file.path(base_dir, "CIMMYTSimulations")

observed_file  <- file.path(sim_dir, "CIMMYTYieldDetails.xls")
simulated_file <- file.path(sim_dir, "CIMMYTSimulatedMME.xlsx")

wth_dir <- file.path(base_dir, "CIMMYT_WTH_Output_RainFixed")

output_dir <- file.path(sim_dir, "CIMMYT_ClimateMetrics_From_WTH")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Matching threshold.
# Records farther than this from the nearest WeatherCode will be flagged.
max_match_distance_km <- 50

# Last 3 months settings.
# If maturity/harvest columns are absent, maturity is approximated as:
#   SowingDate + default_maturity_days_after_sowing
default_maturity_days_after_sowing <- 150
last_n_days <- 90

# Heat stress threshold.
heat_tmax_threshold <- 32

# Optional hot-dry-windy event definition.
hdw_tmax_threshold <- 32
hdw_rhum_threshold <- 30
hdw_wind_threshold <- 3

# SimYieldMME unit conversion.
convert_sim_kg_ha_to_t_ha <- TRUE

# -----------------------------------------------------------------------------
# 2) HELPER FUNCTIONS
# -----------------------------------------------------------------------------

clean_name <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    str_replace_all("\\s+", " ") %>%
    str_to_upper()
}

find_first_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

parse_wth_date <- function(date_code) {
  # DSSAT DATE usually YYDDD
  date_code <- str_pad(as.character(date_code), width = 5, side = "left", pad = "0")
  yy <- as.integer(substr(date_code, 1, 2))
  doy <- as.integer(substr(date_code, 3, 5))
  year <- ifelse(yy > 30, 1900 + yy, 2000 + yy)
  as.Date(doy - 1, origin = paste0(year, "-01-01"))
}

read_wth_file <- function(weather_code, wth_dir) {

  # Try common filename variants.
  candidates <- c(
    file.path(wth_dir, paste0(weather_code, ".WTH")),
    file.path(wth_dir, paste0(weather_code, ".wth")),
    file.path(wth_dir, weather_code)
  )

  wth_file <- candidates[file.exists(candidates)][1]

  if (is.na(wth_file)) {
    return(NULL)
  }

  lines <- readr::read_lines(wth_file, progress = FALSE)
  data_lines <- lines[grepl("^\\s*\\d{5}", lines)]

  if (length(data_lines) == 0) {
    return(NULL)
  }

  # Fixed-width DSSAT WTH format used in your previous scripts:
  # DATE SRAD TMAX TMIN RAIN DEWP WIND PAR RHUM VPRS
  w <- readr::read_fwf(
    I(data_lines),
    readr::fwf_positions(
      start = c(1, 6, 12, 18, 24, 30, 36, 42, 48, 54),
      end   = c(5,11,17,23,29,35,41,47,53,59),
      col_names = c("DATE", "SRAD", "TMAX", "TMIN", "RAIN", "DEWP", "WIND", "PAR", "RHUM", "VPRS")
    ),
    col_types = readr::cols(.default = readr::col_double(), DATE = readr::col_character()),
    progress = FALSE
  ) %>%
    mutate(
      WeatherCode = weather_code,
      date = parse_wth_date(DATE),
      YEAR = year(date),
      DOY = yday(date),
      # Missing codes
      across(c(SRAD, TMAX, TMIN, RAIN, DEWP, WIND, PAR, RHUM, VPRS),
             ~ ifelse(.x <= -90, NA_real_, .x)),
      TMEAN = (TMAX + TMIN) / 2,
      # Saturation vapor pressure from Tmax and Tmin
      es_tmax = 0.6108 * exp((17.27 * TMAX) / (TMAX + 237.3)),
      es_tmin = 0.6108 * exp((17.27 * TMIN) / (TMIN + 237.3)),
      es = (es_tmax + es_tmin) / 2,
      # Actual vapor pressure:
      # Prefer VPRS if present; otherwise derive from dew point temperature.
      ea_from_dewp = 0.6108 * exp((17.27 * DEWP) / (DEWP + 237.3)),
      ea = ifelse(!is.na(VPRS), VPRS, ea_from_dewp),
      VPD = pmax(es - ea, 0)
    )

  w
}

get_end_date <- function(row_df, sowing_date) {

  # If the observed file later includes one of these columns, use it.
  date_cols <- c("MaturityDate", "HarvestDate", "MATDate", "EndDate")
  doy_cols  <- c("MDAT(DOY)", "MDAT_DOY", "MaturityDOY", "HarvestDOY")
  dur_cols  <- c("DaystoMaturity", "DaysToMaturity", "Days_to_Maturity")

  dcol <- find_first_col(row_df, date_cols)
  if (!is.na(dcol)) {
    val <- suppressWarnings(as.Date(row_df[[dcol]][1]))
    if (!is.na(val)) return(val)
  }

  durcol <- find_first_col(row_df, dur_cols)
  if (!is.na(durcol)) {
    val <- suppressWarnings(as.numeric(row_df[[durcol]][1]))
    if (!is.na(val)) return(sowing_date + round(val))
  }

  doycol <- find_first_col(row_df, doy_cols)
  if (!is.na(doycol)) {
    doy <- suppressWarnings(as.integer(row_df[[doycol]][1]))
    if (!is.na(doy)) {
      # For Egyptian wheat, maturity usually occurs in the calendar year after sowing.
      maturity_year <- year(sowing_date) + ifelse(doy < yday(sowing_date), 1, 0)
      return(as.Date(doy - 1, origin = paste0(maturity_year, "-01-01")))
    }
  }

  # Fallback: approximate maturity as sowing + 150 days.
  sowing_date + default_maturity_days_after_sowing
}

summarise_window <- function(w, start_date, end_date) {

  ww <- w %>%
    filter(date >= start_date, date <= end_date)

  if (nrow(ww) == 0) {
    return(tibble(
      n_weather_days = 0,
      Tmax_mean = NA_real_,
      Tmin_mean = NA_real_,
      Tmean_mean = NA_real_,
      HeatDays_Tmax_GT32 = NA_real_,
      SRAD_mean = NA_real_,
      SRAD_sum = NA_real_,
      VPD_mean = NA_real_,
      HDW_days = NA_real_,
      RAIN_sum = NA_real_,
      WIND_mean = NA_real_,
      RHUM_mean = NA_real_
    ))
  }

  tibble(
    n_weather_days = nrow(ww),
    Tmax_mean = mean(ww$TMAX, na.rm = TRUE),
    Tmin_mean = mean(ww$TMIN, na.rm = TRUE),
    Tmean_mean = mean(ww$TMEAN, na.rm = TRUE),
    HeatDays_Tmax_GT32 = sum(ww$TMAX > heat_tmax_threshold, na.rm = TRUE),
    SRAD_mean = mean(ww$SRAD, na.rm = TRUE),
    SRAD_sum = sum(ww$SRAD, na.rm = TRUE),
    VPD_mean = mean(ww$VPD, na.rm = TRUE),
    HDW_days = sum(
      ww$TMAX > hdw_tmax_threshold &
        ww$RHUM < hdw_rhum_threshold &
        ww$WIND > hdw_wind_threshold,
      na.rm = TRUE
    ),
    RAIN_sum = sum(ww$RAIN, na.rm = TRUE),
    WIND_mean = mean(ww$WIND, na.rm = TRUE),
    RHUM_mean = mean(ww$RHUM, na.rm = TRUE)
  )
}

# -----------------------------------------------------------------------------
# 3) READ CIMMYT OBSERVED AND SIMULATED LOCATION DATA
# -----------------------------------------------------------------------------

obs_raw <- readxl::read_excel(observed_file, sheet = 1)
sim_raw <- readxl::read_excel(simulated_file, sheet = 1)

required_obs <- c("Year", "Lat", "Long", "SowingDate", "YieldCIMMYT")
required_sim <- c("WeatherCode", "Year", "Lat", "Long", "SimYieldMME")

missing_obs <- setdiff(required_obs, names(obs_raw))
missing_sim <- setdiff(required_sim, names(sim_raw))

if (length(missing_obs) > 0) {
  stop("Observed file is missing required columns: ", paste(missing_obs, collapse = ", "))
}

if (length(missing_sim) > 0) {
  stop("Simulated file is missing required columns: ", paste(missing_sim, collapse = ", "))
}

obs <- obs_raw %>%
  mutate(
    RecordID = row_number(),
    Year = as.integer(Year),
    Lat = as.numeric(Lat),
    Long = as.numeric(Long),
    SowingDate = as.Date(SowingDate),
    YieldCIMMYT = as.numeric(YieldCIMMYT),
    Loc_desc_clean = if ("Loc_desc" %in% names(.)) clean_name(Loc_desc) else NA_character_
  ) %>%
  filter(!is.na(Year), !is.na(Lat), !is.na(Long), !is.na(SowingDate))

sim_locations <- sim_raw %>%
  mutate(
    Year = as.integer(Year),
    Lat = as.numeric(Lat),
    Long = as.numeric(Long),
    SimYieldMME = as.numeric(SimYieldMME),
    SimYieldMME_t_ha = if (convert_sim_kg_ha_to_t_ha) SimYieldMME / 1000 else SimYieldMME,
    Location_clean = if ("Location" %in% names(.)) clean_name(Location) else NA_character_
  ) %>%
  filter(!is.na(WeatherCode), !is.na(Lat), !is.na(Long)) %>%
  group_by(WeatherCode) %>%
  summarise(
    SimLocation = first(Location),
    SimLocation_clean = first(Location_clean),
    SimLat = first(Lat),
    SimLong = first(Long),
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# 4) MATCH EACH CIMMYT OBSERVED RECORD TO NEAREST SIMULATED/WTH LOCATION
# -----------------------------------------------------------------------------

obs_coords <- as.matrix(obs[, c("Long", "Lat")])
sim_coords <- as.matrix(sim_locations[, c("SimLong", "SimLat")])

nearest_index <- geosphere::distm(obs_coords, sim_coords, fun = geosphere::distHaversine) %>%
  apply(1, which.min)

nearest_distance_km <- geosphere::distHaversine(
  obs_coords,
  sim_coords[nearest_index, , drop = FALSE]
) / 1000

obs_matched <- obs %>%
  mutate(
    MatchedWeatherCode = sim_locations$WeatherCode[nearest_index],
    MatchedSimLocation = sim_locations$SimLocation[nearest_index],
    MatchedSimLat = sim_locations$SimLat[nearest_index],
    MatchedSimLong = sim_locations$SimLong[nearest_index],
    MatchDistance_km = nearest_distance_km,
    MatchFlag = ifelse(MatchDistance_km <= max_match_distance_km, "OK", "CHECK_DISTANCE")
  )

# Add simulated yield for same year and WeatherCode.
sim_year_weather <- sim_raw %>%
  mutate(
    Year = as.integer(Year),
    SimYieldMME = as.numeric(SimYieldMME),
    SimYieldMME_t_ha = if (convert_sim_kg_ha_to_t_ha) SimYieldMME / 1000 else SimYieldMME
  ) %>%
  select(WeatherCode, Year, SimYieldMME, SimYieldMME_t_ha, StdevModels)

obs_matched <- obs_matched %>%
  left_join(
    sim_year_weather,
    by = c("MatchedWeatherCode" = "WeatherCode", "Year" = "Year")
  )

write.csv(
  obs_matched,
  file.path(output_dir, "CIMMYT_Observed_Matched_To_WTH_Locations.csv"),
  row.names = FALSE
)

# -----------------------------------------------------------------------------
# 5) READ ALL REQUIRED WTH FILES ONCE
# -----------------------------------------------------------------------------

weather_codes <- sort(unique(obs_matched$MatchedWeatherCode))

weather_list <- list()

for (code in weather_codes) {
  message("Reading WTH: ", code)
  w <- read_wth_file(code, wth_dir)
  if (is.null(w)) {
    warning("Could not read WTH file for WeatherCode: ", code)
  } else {
    weather_list[[code]] <- w
  }
}

# -----------------------------------------------------------------------------
# 6) CALCULATE CLIMATE METRICS FOR EACH CIMMYT RECORD
# -----------------------------------------------------------------------------

results <- vector("list", nrow(obs_matched))

for (i in seq_len(nrow(obs_matched))) {

  rec <- obs_matched[i, ]
  code <- rec$MatchedWeatherCode
  w <- weather_list[[code]]

  if (is.null(w)) {
    results[[i]] <- tibble(
      RecordID = rec$RecordID,
      WindowStart = as.Date(NA),
      WindowEnd = as.Date(NA),
      WindowType = "missing_wth",
      n_weather_days = 0,
      Tmax_mean = NA_real_,
      Tmin_mean = NA_real_,
      Tmean_mean = NA_real_,
      HeatDays_Tmax_GT32 = NA_real_,
      SRAD_mean = NA_real_,
      SRAD_sum = NA_real_,
      VPD_mean = NA_real_,
      HDW_days = NA_real_,
      RAIN_sum = NA_real_,
      WIND_mean = NA_real_,
      RHUM_mean = NA_real_
    )
    next
  }

  sowing_date <- rec$SowingDate
  end_date <- get_end_date(rec, sowing_date)
  start_date <- end_date - last_n_days + 1

  # Safety check: if the fallback maturity creates a window before sowing, reset.
  if (start_date < sowing_date) start_date <- sowing_date

  met <- summarise_window(w, start_date, end_date)

  results[[i]] <- met %>%
    mutate(
      RecordID = rec$RecordID,
      WindowStart = start_date,
      WindowEnd = end_date,
      WindowType = paste0("last_", last_n_days, "_days_before_maturity_or_fallback")
    ) %>%
    select(
      RecordID, WindowStart, WindowEnd, WindowType,
      everything()
    )
}

metrics_record <- bind_rows(results)

climate_record <- obs_matched %>%
  left_join(metrics_record, by = "RecordID") %>%
  arrange(Year, MatchedWeatherCode, RecordID)

write.csv(
  climate_record,
  file.path(output_dir, "CIMMYT_RecordLevel_ClimateMetrics_Last90Days.csv"),
  row.names = FALSE
)

# -----------------------------------------------------------------------------
# 7) SUMMARISE TO YEAR × MATCHED LOCATION
# -----------------------------------------------------------------------------

climate_year_location <- climate_record %>%
  group_by(
    Year,
    MatchedWeatherCode,
    MatchedSimLocation,
    MatchedSimLat,
    MatchedSimLong
  ) %>%
  summarise(
    n_CIMMYT_records = n(),
    n_unique_observed_locations = n_distinct(Loc_desc_clean),
    mean_match_distance_km = mean(MatchDistance_km, na.rm = TRUE),
    max_match_distance_km = max(MatchDistance_km, na.rm = TRUE),

    mean_YieldCIMMYT = mean(YieldCIMMYT, na.rm = TRUE),
    sd_YieldCIMMYT = ifelse(n() > 1, sd(YieldCIMMYT, na.rm = TRUE), NA_real_),

    mean_SimYieldMME_t_ha = mean(SimYieldMME_t_ha, na.rm = TRUE),

    mean_Tmax = mean(Tmax_mean, na.rm = TRUE),
    mean_Tmin = mean(Tmin_mean, na.rm = TRUE),
    mean_Tmean = mean(Tmean_mean, na.rm = TRUE),

    mean_HeatDays_Tmax_GT32 = mean(HeatDays_Tmax_GT32, na.rm = TRUE),
    sum_HeatDays_Tmax_GT32 = sum(HeatDays_Tmax_GT32, na.rm = TRUE),

    mean_SRAD = mean(SRAD_mean, na.rm = TRUE),
    mean_cumulative_SRAD = mean(SRAD_sum, na.rm = TRUE),

    mean_VPD = mean(VPD_mean, na.rm = TRUE),

    mean_HDW_days = mean(HDW_days, na.rm = TRUE),
    sum_HDW_days = sum(HDW_days, na.rm = TRUE),

    mean_RAIN_sum = mean(RAIN_sum, na.rm = TRUE),
    mean_WIND = mean(WIND_mean, na.rm = TRUE),
    mean_RHUM = mean(RHUM_mean, na.rm = TRUE),

    mean_n_weather_days = mean(n_weather_days, na.rm = TRUE),
    any_distance_flag = any(MatchFlag != "OK"),

    .groups = "drop"
  ) %>%
  arrange(Year, MatchedWeatherCode)

write.csv(
  climate_year_location,
  file.path(output_dir, "CIMMYT_YearLocation_ClimateMetrics_Last90Days.csv"),
  row.names = FALSE
)

# -----------------------------------------------------------------------------
# 8) ANNUAL SUMMARY ACROSS CIMMYT MATCHED LOCATIONS
# -----------------------------------------------------------------------------

climate_annual <- climate_year_location %>%
  group_by(Year) %>%
  summarise(
    n_matched_locations = n_distinct(MatchedWeatherCode),
    n_CIMMYT_records = sum(n_CIMMYT_records, na.rm = TRUE),

    annual_mean_YieldCIMMYT = mean(mean_YieldCIMMYT, na.rm = TRUE),
    annual_sd_YieldCIMMYT_locations = sd(mean_YieldCIMMYT, na.rm = TRUE),

    annual_mean_SimYieldMME_t_ha = mean(mean_SimYieldMME_t_ha, na.rm = TRUE),
    annual_sd_SimYieldMME_locations = sd(mean_SimYieldMME_t_ha, na.rm = TRUE),

    annual_mean_Tmax = mean(mean_Tmax, na.rm = TRUE),
    annual_sd_Tmax_locations = sd(mean_Tmax, na.rm = TRUE),

    annual_mean_Tmin = mean(mean_Tmin, na.rm = TRUE),
    annual_sd_Tmin_locations = sd(mean_Tmin, na.rm = TRUE),

    annual_mean_Tmean = mean(mean_Tmean, na.rm = TRUE),
    annual_mean_HeatDays_Tmax_GT32 = mean(mean_HeatDays_Tmax_GT32, na.rm = TRUE),

    annual_mean_SRAD = mean(mean_SRAD, na.rm = TRUE),
    annual_sd_SRAD_locations = sd(mean_SRAD, na.rm = TRUE),
    annual_mean_cumulative_SRAD = mean(mean_cumulative_SRAD, na.rm = TRUE),

    annual_mean_VPD = mean(mean_VPD, na.rm = TRUE),
    annual_mean_HDW_days = mean(mean_HDW_days, na.rm = TRUE),

    .groups = "drop"
  ) %>%
  arrange(Year)

write.csv(
  climate_annual,
  file.path(output_dir, "CIMMYT_Annual_ClimateMetrics_Last90Days.csv"),
  row.names = FALSE
)

# Excel workbook with all outputs.
writexl::write_xlsx(
  list(
    RecordLevel = climate_record,
    YearLocation = climate_year_location,
    Annual = climate_annual,
    MatchedLocations = obs_matched
  ),
  file.path(output_dir, "CIMMYT_ClimateMetrics_From_WTH_Last90Days.xlsx")
)

# -----------------------------------------------------------------------------
# 9) QUICK DIAGNOSTIC MESSAGES
# -----------------------------------------------------------------------------

message("\nDone.")
message("Outputs saved to: ", output_dir)
message("\nKey files:")
message("1) Record-level: CIMMYT_RecordLevel_ClimateMetrics_Last90Days.csv")
message("2) Year-location: CIMMYT_YearLocation_ClimateMetrics_Last90Days.csv")
message("3) Annual summary: CIMMYT_Annual_ClimateMetrics_Last90Days.csv")
message("4) Excel workbook: CIMMYT_ClimateMetrics_From_WTH_Last90Days.xlsx")

message("\nDistance match check:")
print(
  obs_matched %>%
    count(MatchFlag) %>%
    arrange(desc(n))
)

message("\nWeather files not read, if any:")
missing_weather_codes <- setdiff(weather_codes, names(weather_list))
print(missing_weather_codes)
