# ============================================================
# Causal inference (continuous treatment): Best sowing date (DOY) per Location
# Estimator: causaldrf::add_spl_est (semi-parametric ADRF using additive splines)
#
# Outputs:
#   1) BestSowingDate_byLocation_DOY.csv
#   2) BestSowingDate_byLocation_DOY_andDate.csv
#   3) DoseResponseCurves_byLocation.png  (optional)
#
# Notes:
# - This avoids the "drf" package confusion (many installs are NOT dose-response forests).
# - Works per-location; skips locations with too few rows.
# ============================================================

# ---- packages ----
req <- c("data.table", "dplyr", "lubridate", "ggplot2", "purrr", "tibble", "stringr", "causaldrf")
new <- req[!(req %in% installed.packages()[, "Package"])]
if (length(new) > 0) install.packages(new, dependencies = TRUE)

library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(tibble)
library(stringr)
library(causaldrf)

# ---- 0) file path ----
infile <- "D:/HourlyHDW/Calibrationwthfiles/AllObservations.csv"

# ---- 1) read data ----
df <- fread(infile)

# ---- 2) sanity checks ----
stopifnot(all(c("Location", "year", "Yield") %in% names(df)))

# ---- 3) parse sowing date (optional) and create/validate SowingDOY ----
if ("sowingdate" %in% names(df)) {
  df$sowingdate_parsed <- suppressWarnings(ymd(df$sowingdate))
  if (all(is.na(df$sowingdate_parsed))) df$sowingdate_parsed <- suppressWarnings(dmy(df$sowingdate))
  if (all(is.na(df$sowingdate_parsed))) df$sowingdate_parsed <- suppressWarnings(mdy(df$sowingdate))
}

if (!("SowingDOY" %in% names(df))) {
  if (!("sowingdate_parsed" %in% names(df))) stop("Need either SowingDOY or sowingdate.")
  df <- df %>% mutate(SowingDOY = yday(sowingdate_parsed))
}

# ---- 4) basic types / cleaning ----
df <- df %>%
  mutate(
    Yield = as.numeric(Yield),
    SowingDOY = as.numeric(SowingDOY),
    year = as.integer(year),
    Location = as.factor(Location)
  ) %>%
  filter(
    !is.na(Yield), is.finite(Yield),
    !is.na(SowingDOY), is.finite(SowingDOY),
    !is.na(Location),
    !is.na(year)
  )

# ---- 5) choose PRE-TREATMENT covariates (avoid mediators like heading/anthesis/maturity) ----
# Use only baseline/site/time covariates that confound sowing date choice and yield.
base_covars <- c("lat", "lon", "year")
base_covars <- base_covars[base_covars %in% names(df)]

# flexible time trend
df <- df %>%
  mutate(
    year_c  = year - mean(year, na.rm = TRUE),
    year_c2 = year_c^2
  )

time_covars <- c("year_c", "year_c2")

covars <- unique(c(base_covars, time_covars))
message("Using covariates: ", paste(covars, collapse = ", "))

# Build treatment model formula: SowingDOY ~ X
# If no covariates exist, use intercept-only.
treat_formula <- if (length(covars) > 0) {
  as.formula(paste("SowingDOY ~", paste(covars, collapse = " + ")))
} else {
  as.formula("SowingDOY ~ 1")
}

# ---- 6) per-location ADRF fit ----
fit_one_location <- function(dat_loc,
                             treat_formula,
                             n_grid = 51,
                             min_n = 30,
                             knot_num = 3,
                             treat_mod = "Normal",
                             seed = 123) {
  
  set.seed(seed)
  
  # keep only complete rows needed by the model
  need_cols <- unique(c("Yield", "SowingDOY", all.vars(treat_formula)))
  need_cols <- need_cols[need_cols %in% names(dat_loc)]
  dat_loc <- dat_loc %>% dplyr::select(dplyr::all_of(need_cols)) %>% as.data.frame()
  
  dat_loc <- dat_loc %>%
    filter(
      complete.cases(.),
      is.finite(Yield),
      is.finite(SowingDOY)
    )
  
  if (nrow(dat_loc) < min_n) {
    return(list(
      best_doy = NA_real_,
      best_yhat = NA_real_,
      curve = NULL,
      note = paste0("Too few rows after filtering (", nrow(dat_loc), ")")
    ))
  }
  
  tr <- dat_loc$SowingDOY
  
  # robust grid within observed range
  lo <- as.numeric(stats::quantile(tr, 0.05, na.rm = TRUE))
  hi <- as.numeric(stats::quantile(tr, 0.95, na.rm = TRUE))
  if (!is.finite(lo) || !is.finite(hi) || lo >= hi) {
    return(list(
      best_doy = NA_real_,
      best_yhat = NA_real_,
      curve = NULL,
      note = "Bad treatment range after quantiles"
    ))
  }
  
  grid <- seq(lo, hi, length.out = n_grid)
  
  # Fit ADRF with additive spline estimator
  # Returns $param = ADRF evaluated at grid values (in same order as grid_val)
  est <- tryCatch(
    causaldrf::add_spl_est(
      Y = Yield,
      treat = SowingDOY,
      treat_formula = treat_formula,
      data = dat_loc,
      grid_val = grid,
      knot_num = knot_num,
      treat_mod = treat_mod
    ),
    error = function(e) e
  )
  
  if (inherits(est, "error")) {
    return(list(
      best_doy = NA_real_,
      best_yhat = NA_real_,
      curve = NULL,
      note = paste0("Estimator failed: ", conditionMessage(est))
    ))
  }
  
  yhat <- as.numeric(est$param)
  if (length(yhat) != length(grid)) {
    return(list(
      best_doy = NA_real_,
      best_yhat = NA_real_,
      curve = NULL,
      note = paste0("Unexpected output length: yhat=", length(yhat), " grid=", length(grid))
    ))
  }
  
  best_i <- which.max(yhat)
  
  preds <- tibble::tibble(
    SowingDOY = grid,
    yhat = yhat
  )
  
  list(
    best_doy = grid[best_i],
    best_yhat = yhat[best_i],
    curve = preds,
    note = "OK"
  )
}

# ---- 7) run per location ----
loc_list <- df %>%
  group_by(Location) %>%
  group_split()

loc_names <- purrr::map_chr(loc_list, ~ as.character(unique(.x$Location)))

fits <- purrr::map(
  loc_list,
  ~ fit_one_location(
    dat_loc = .x,
    treat_formula = treat_formula,
    n_grid = 51,
    min_n = 30,
    knot_num = 3,
    treat_mod = "Normal",
    seed = 123
  )
)

# ---- 8) collect results ----
res_tbl <- tibble::tibble(
  Location = loc_names,
  BestSowingDOY = purrr::map_dbl(fits, "best_doy"),
  BestYield_hat = purrr::map_dbl(fits, "best_yhat"),
  Note = purrr::map_chr(fits, "note")
)

# ---- 9) add "date" representation (dummy reference year) ----
# DOY->Date needs a reference year; use 2001 (non-leap) for consistent mapping.
res_tbl2 <- res_tbl %>%
  mutate(
    BestSowingDate_2001 = ifelse(
      is.na(BestSowingDOY),
      NA_character_,
      as.character(as.Date(BestSowingDOY - 1, origin = "2001-01-01"))
    )
  )

# ---- 10) write outputs ----
out1 <- "D:/HourlyHDW/Calibrationwthfiles/BestSowingDate_byLocation_DOY.csv"
out2 <- "D:/HourlyHDW/Calibrationwthfiles/BestSowingDate_byLocation_DOY_andDate.csv"

data.table::fwrite(res_tbl, out1)
data.table::fwrite(res_tbl2, out2)

message("Wrote: ", out1)
message("Wrote: ", out2)

# ---- 11) optional plot: dose-response curves by location ----
curves <- purrr::imap_dfr(fits, function(fit, i) {
  if (is.null(fit$curve)) return(NULL)
  dplyr::mutate(fit$curve, Location = loc_names[[i]])
})

if (nrow(curves) > 0) {
  p <- ggplot(curves, aes(x = SowingDOY, y = yhat, group = Location)) +
    geom_line(alpha = 0.6) +
    facet_wrap(~ Location, scales = "free_y") +
    labs(
      title = "Estimated dose-response (ADRF) of Yield vs SowingDOY by Location",
      x = "Sowing day of year (DOY)",
      y = "Estimated mean potential Yield"
    ) +
    theme_classic()
  
  ggsave("D:/HourlyHDW/Calibrationwthfiles/DoseResponseCurves_byLocation.png", p, width = 14, height = 9, dpi = 300)
  message("Wrote: DoseResponseCurves_byLocation.png")
} else {
  message("No curves to plot (all locations failed or were too small).")
}

# ---- 12) print summary ----
print(res_tbl2 %>% arrange(desc(BestYield_hat)))




