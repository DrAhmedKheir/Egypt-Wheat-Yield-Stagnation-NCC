library(readxl)
library(dplyr)
library(readr)

base_dir <- "D:/MMENCC"
infile <- file.path(base_dir, "Outs3Models.xlsx")

df <- read_excel(infile)

# Aggregate annual yields across all locations and three DSSAT models
annual_mme <- df %>%
  filter(HarvestYrSimSCER >= 1980, HarvestYrSimSCER <= 2019) %>%
  group_by(Year = HarvestYrSimSCER) %>%
  summarise(
    CSCER_mean = mean(CSCERYield, na.rm = TRUE),
    CSCRP_mean = mean(CSCRPYield, na.rm = TRUE),
    WHAPS_mean = mean(WHAPSYield, na.rm = TRUE),
    MME_mean = mean(MMEYield, na.rm = TRUE),
    MME_sd_locations = sd(MMEYield, na.rm = TRUE),
    n_locations = n_distinct(LocationSim),
    n_records = n(),
    .groups = "drop"
  )

write_csv(
  annual_mme,
  file.path(base_dir, "Annual_MME_Yield_1980_2019_AllLocations.csv")
)

print(annual_mme)