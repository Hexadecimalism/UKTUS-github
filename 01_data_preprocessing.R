# 01_data_preprocessing.R
# -------------------------------------------------------------------
# Loads the prepared diary/person-day dataset, aligns types,
# adds a few derived fields (non-destructively), and saves:
#   • data_full   : the dataset "as is" (plus derived fields)
#   • data_nomiss : the same but with NAs dropped ONLY for model vars
#
# Notes:
# - Do NOT overwrite raw file; mutate() returns a new tibble.
# - Rely on the columns created earlier (e.g., *_01, WorkSta7, EducCur_group,
#   Educ_bin, LongIll_bin, game_minutes, game_any). If any are missing, the script
#   will gracefully add game_hours and otherwise leave things as-is.
# -------------------------------------------------------------------

library(dplyr)
library(arrow)   # for read_feather / write_feather
library(readr)   # for write_csv (optional)

# ---- 0) Locate and load the data we created earlier ----------------
# Prefer the RDS written earlier; fall back to Feather if using old python modified version.
if (file.exists("selected_columns_no_missing_time.RDS")) {
  message("Loading RDS: selected_columns_no_missing_time.RDS")
  data_raw <- readRDS("selected_columns_no_missing_time.RDS")
} else if (file.exists("selected_columns_no_missing_time.feather")) {
  message("Loading Feather: selected_columns_no_missing_time.feather")
  data_raw <- arrow::read_feather("selected_columns_no_missing_time.feather")
} else {
  stop("Could not find 'selected_columns_no_missing_time.RDS' or '.feather' in the working directory.")
}

# Quick peek (optional)
# str(data_raw); dplyr::glimpse(data_raw)

# ---- 1) Define the model variables to care about ----------------
outcomes_01 <- c(
  # 1–7 satisfactions (rescaled 0–1, higher = better)
  "SatInc_01","SatJob_01","SatBal_01","SatHouse_01","SatLeis_01","SatPart_01","SatisOv_01",
  # 0–10 wellbeing (rescaled 0–1)
  "Satis_01","Worth_01","Happy_01",
  # reversed & rescaled
  "Anxious_rev_01","GenHlth_rev_01",
  # 1–3 recodes (rescaled 0–1)
  "Rushed_01","Stressed_01"
)

controls <- c(
  "DVAge","DMSex",          # demographics
  "Educ_bin","LongIll_bin", # binaries we created
  "WorkSta7","EducCur_group" # factor recodes we created
)

predictor <- c("game_hours")  # hours of gaming (add if absent)

# ---- 2) Create a non-destructive “working copy” with safe types ----
# DO NOT mutate data_raw in-place; build data_full from it.
data_full <- data_raw %>%
  mutate(
    # IDs as character (safer for joins/prints)
    serial = as.character(serial),
    pnum   = as.character(pnum),
    
    # If game_hours isn’t present but game_minutes is, add it.
    game_hours = if (!"game_hours" %in% names(.)) {
      if ("game_minutes" %in% names(.)) as.numeric(game_minutes) / 60 else NA_real_
    } else {
      game_hours
    },
    
    # Ensure our factor recodes are factors if they exist
    WorkSta7       = if ("WorkSta7"       %in% names(.)) factor(WorkSta7)       else NULL,
    EducCur_group  = if ("EducCur_group"  %in% names(.)) factor(EducCur_group)  else NULL,
    
    # Ensure binaries are 0/1 numeric if they exist
    Educ_bin       = if ("Educ_bin"       %in% names(.)) as.integer(Educ_bin)   else NULL,
    LongIll_bin    = if ("LongIll_bin"    %in% names(.)) as.integer(LongIll_bin)else NULL
  )

# ---- 3) Build an NA-dropped analysis copy (targeted drop) ----------
# Only drop rows missing on *analysis* vars (predictor + outcomes + controls).
analysis_vars <- c(predictor, outcomes_01, controls)
vars_present  <- intersect(analysis_vars, names(data_full))

data_nomiss <- data_full %>%
  # Keep all the same columns, but only rows complete for the analysis set
  tidyr::drop_na(any_of(vars_present))

# ---- 4) Save both datasets to disk --------------------------------
# RDS (compact & preserves types)
saveRDS(data_full,   "cleaned_data_full.RDS",   compress = "xz")
saveRDS(data_nomiss, "cleaned_data_nomiss.RDS", compress = "xz")

# Feather (fast cross-language IO; optional)
arrow::write_feather(data_full,   "cleaned_data_full.feather")
arrow::write_feather(data_nomiss, "cleaned_data_nomiss.feather")

# CSV (human-readable; larger & loses factors’ levels; optional)
readr::write_csv(data_full,   "cleaned_data_full.csv")
readr::write_csv(data_nomiss, "cleaned_data_nomiss.csv")

message("Preprocessing complete: wrote cleaned_data_full/nomiss as RDS, Feather, and CSV.")
