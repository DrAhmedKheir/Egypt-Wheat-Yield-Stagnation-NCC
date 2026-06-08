# =============================================================================
# Calculate annual average observed CIMMYT yield across locations
# =============================================================================

# install.packages(c("readxl", "dplyr", "writexl"))

library(readxl)
library(dplyr)
library(writexl)

base_dir <- "D:/Asseng_paper2024/Updated/Asseng_updated/40YRSSIMULATIONS/ForShawn/21072025/21072025/Update27032026/CIMMYTSimulations"

input_file <- file.path(base_dir, "CIMMYTYieldDetails.xls")

output_csv  <- file.path(base_dir, "Annual_Average_CIMMYT_Yield.csv")
output_xlsx <- file.path(base_dir, "Annual_Average_CIMMYT_Yield.xlsx")

# -----------------------------------------------------------------------------
# READ DATA
# -----------------------------------------------------------------------------

dat <- read_excel(input_file, sheet = 1)

required_cols <- c("Year", "YieldCIMMYT")

missing_cols <- setdiff(required_cols, names(dat))

if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

# -----------------------------------------------------------------------------
# CALCULATE ANNUAL AVERAGE ACROSS LOCATIONS
# -----------------------------------------------------------------------------

annual_mean <- dat %>%
  mutate(
    Year = as.integer(Year),
    YieldCIMMYT = as.numeric(YieldCIMMYT)
  ) %>%
  filter(
    !is.na(Year),
    !is.na(YieldCIMMYT)
  ) %>%
  group_by(Year) %>%
  summarise(
    n_locations_records = n(),
    mean_YieldCIMMYT = mean(YieldCIMMYT, na.rm = TRUE),
    sd_YieldCIMMYT = ifelse(n() > 1, sd(YieldCIMMYT, na.rm = TRUE), NA_real_),
    se_YieldCIMMYT = ifelse(n() > 1, sd_YieldCIMMYT / sqrt(n()), NA_real_),
    min_YieldCIMMYT = min(YieldCIMMYT, na.rm = TRUE),
    max_YieldCIMMYT = max(YieldCIMMYT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Year)

# -----------------------------------------------------------------------------
# SAVE OUTPUT
# -----------------------------------------------------------------------------

write.csv(annual_mean, output_csv, row.names = FALSE)

write_xlsx(
  list(Annual_Average_CIMMYT_Yield = annual_mean),
  output_xlsx
)

print(annual_mean)

message("Done.")
message("CSV saved to: ", output_csv)
message("Excel saved to: ", output_xlsx)
