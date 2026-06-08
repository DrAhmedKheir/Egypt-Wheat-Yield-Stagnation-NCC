# =============================================================================
# Calculate annual average simulated yield (SimYieldMME) across locations
# =============================================================================

# install.packages(c("readxl", "dplyr", "writexl"))

library(readxl)
library(dplyr)
library(writexl)

base_dir <- "D:/Asseng_paper2024/Updated/Asseng_updated/40YRSSIMULATIONS/ForShawn/21072025/21072025/Update27032026/CIMMYTSimulations"

input_file <- file.path(base_dir, "CIMMYTSimulatedMME.xlsx")

output_csv  <- file.path(base_dir, "Annual_Average_SimYieldMME.csv")
output_xlsx <- file.path(base_dir, "Annual_Average_SimYieldMME.xlsx")

# Set TRUE if SimYieldMME is in kg/ha and you also want t/ha columns.
convert_kg_ha_to_t_ha <- TRUE

dat <- read_excel(input_file, sheet = 1)

required_cols <- c("Year", "SimYieldMME")
missing_cols <- setdiff(required_cols, names(dat))

if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

annual_mean <- dat %>%
  mutate(
    Year = as.integer(Year),
    SimYieldMME = as.numeric(SimYieldMME)
  ) %>%
  filter(
    !is.na(Year),
    !is.na(SimYieldMME)
  ) %>%
  group_by(Year) %>%
  summarise(
    n_locations_records = n(),
    mean_SimYieldMME = mean(SimYieldMME, na.rm = TRUE),
    sd_SimYieldMME = ifelse(n() > 1, sd(SimYieldMME, na.rm = TRUE), NA_real_),
    se_SimYieldMME = ifelse(n() > 1, sd_SimYieldMME / sqrt(n()), NA_real_),
    min_SimYieldMME = min(SimYieldMME, na.rm = TRUE),
    max_SimYieldMME = max(SimYieldMME, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Year)

if (convert_kg_ha_to_t_ha) {
  annual_mean <- annual_mean %>%
    mutate(
      mean_SimYieldMME_t_ha = mean_SimYieldMME / 1000,
      sd_SimYieldMME_t_ha   = sd_SimYieldMME / 1000,
      se_SimYieldMME_t_ha   = se_SimYieldMME / 1000,
      min_SimYieldMME_t_ha  = min_SimYieldMME / 1000,
      max_SimYieldMME_t_ha  = max_SimYieldMME / 1000
    )
}

write.csv(annual_mean, output_csv, row.names = FALSE)

write_xlsx(
  list(Annual_Average_SimYieldMME = annual_mean),
  output_xlsx
)

print(annual_mean)

message("Done.")
message("CSV saved to: ", output_csv)
message("Excel saved to: ", output_xlsx)
