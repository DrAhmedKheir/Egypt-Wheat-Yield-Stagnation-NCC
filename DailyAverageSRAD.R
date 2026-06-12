library(tidyverse)

base_dir <- "D:/Asseng_paper2024/Updated/Asseng_updated/40YRSSIMULATIONS/ForShawn"

df <- read_csv(file.path(base_dir, "Tempmax_Enhanced.csv"))

# Storage for daily average radiation
srad_sow_to_ath <- rep(NA_real_, nrow(df))
srad_ath_to_mat <- rep(NA_real_, nrow(df))
srad_sow_to_mat <- rep(NA_real_, nrow(df))

for (i in seq_len(nrow(df))) {
  
  location_code <- df$Locationcode[i]
  year <- df$Years[i]
  adat <- df$`ADAT(DOY)`[i]
  mdat <- df$`MDAT(DOY)`[i]
  sowing_doy <- 313
  
  weather_file <- file.path(base_dir, paste0(location_code, ".WTH"))
  if (!file.exists(weather_file)) next
  
  lines <- read_lines(weather_file)
  data_lines <- lines[grepl("^\\d{5}", lines)]
  
  weather_df <- read_fwf(
    I(data_lines),
    fwf_positions(
      start = c(1, 6, 12, 18, 24, 30, 36, 42, 48, 54),
      end   = c(5,11,17,23,29,35,41,47,53,59),
      col_names = c("DATE", "SRAD", "TMAX", "TMIN", "RAIN", "DEWP",
                    "WIND", "PAR", "RHUM", "VPRS")
    ),
    col_types = cols(.default = col_double(), DATE = col_character())
  ) %>%
    mutate(
      YEAR = as.integer(if_else(
        substr(DATE, 1, 2) > "30",
        paste0("19", substr(DATE, 1, 2)),
        paste0("20", substr(DATE, 1, 2))
      )),
      DOY = as.integer(substr(DATE, 3, 5))
    )
  
  # Daily mean SRAD: sowing to anthesis
  srad_sow_to_ath[i] <- weather_df %>%
    filter(
      (YEAR == year - 1 & DOY >= sowing_doy) |
        (YEAR == year & DOY < adat)
    ) %>%
    summarise(mean_srad = mean(SRAD, na.rm = TRUE)) %>%
    pull(mean_srad)
  
  # Daily mean SRAD: anthesis to maturity
  srad_ath_to_mat[i] <- weather_df %>%
    filter(YEAR == year & DOY >= adat & DOY <= mdat) %>%
    summarise(mean_srad = mean(SRAD, na.rm = TRUE)) %>%
    pull(mean_srad)
  
  # Daily mean SRAD: sowing to maturity
  srad_sow_to_mat[i] <- weather_df %>%
    filter(
      (YEAR == year - 1 & DOY >= sowing_doy) |
        (YEAR == year & DOY <= mdat)
    ) %>%
    summarise(mean_srad = mean(SRAD, na.rm = TRUE)) %>%
    pull(mean_srad)
}

df$Mean_SRAD_SowToAnth <- srad_sow_to_ath
df$Mean_SRAD_AnthToMat <- srad_ath_to_mat
df$Mean_SRAD_SowToMat  <- srad_sow_to_mat

write_csv(
  df,
  file.path(base_dir, "SRAD_DailyMean_Updated.csv")
)

# Yearly average across locations
df_avg <- df %>%
  group_by(Years) %>%
  summarise(
    Mean_Days_to_Anthesis = mean(DaystoAnthesis, na.rm = TRUE),
    Mean_Days_to_Maturity = mean(DaystoMaturity, na.rm = TRUE),
    Mean_SRAD_SowToAnth = mean(Mean_SRAD_SowToAnth, na.rm = TRUE),
    Mean_SRAD_AnthToMat = mean(Mean_SRAD_AnthToMat, na.rm = TRUE),
    Mean_SRAD_SowToMat  = mean(Mean_SRAD_SowToMat, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  df_avg,
  file.path(base_dir, "SRAD_DailyMean_YearlyAvg.csv")
)