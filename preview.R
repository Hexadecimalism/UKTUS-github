# -------------------------------------------------------------------
# Quick, clean scaffold for running many comparable regressions
# (a mini “specification curve” pass) on  prepared data.
#
#   1) Creates a treatment variable in HOURS (game_minutes / 60).
#   2) Picks outcomes that are already harmonized to 0–1 where higher = better.
#   3) Defines a sensible set of controls (including factors).
#   4) Splits the data into age bands (20–29, 30–39, 40–49, 50–59).
#   5) Runs the SAME linear model for each outcome within each age band.
#   6) Returns tidy estimates so I can preview the “game_hours” effect.
#
# Notes:
#   • Kept everything transparent and simple: lm + broom::tidy.
#   • Factors (e.g., WorkSta7, EducCur_group) are automatically dummy-coded by lm.
#   • Not yet weighted; can add weights = dia_wt_a (or _b) later if desired.
#   • This is just a scaffold—once
# -------------------------------------------------------------------

library(dplyr)
library(broom)

# 1) Create the treatment in hours (1 unit = 1 hour/day of gaming).
#    If game_hours already exists from a previous run, this overwrites it consistently.
selected_columns_no_missing_time <- selected_columns_no_missing_time %>%
  mutate(game_hours = game_minutes / 60)

# 2) Outcomes on a common 0–1 scale where higher = better.
#    (These names assume that ran the earlier harmonization step.)
outcomes_01 <- c(
  # 1–7 satisfactions (rescaled 0–1)
  "SatInc_01","SatJob_01","SatBal_01","SatHouse_01","SatLeis_01","SatPart_01","SatisOv_01",
  # 0–10 wellbeing (rescaled 0–1)
  "Satis_01","Worth_01","Happy_01",
  # reversed & rescaled (so higher = better)
  "Anxious_rev_01","GenHlth_rev_01",
  # 1–3 recodes (rescaled 0–1)
  "Rushed_01","Stressed_01"
)

# 3) Controls to adjust for key confounders.
#    • Educ_bin / LongIll_bin are 0/1; WorkSta7 / EducCur_group are factors.
#    • Feel free to add/remove here (e.g., weights, more demographics).
controls <- c("DVAge", "DMSex", "Educ_bin", "LongIll_bin", "WorkSta7", "EducCur_group")

# 4) Define age bands (inclusive of lower bound, exclusive of upper bound).
#    This reproduces 20–29, 30–39, 40–49, 50–59.
cut_ages <- function(x) {
  cut(x,
      breaks = c(20, 30, 40, 50, 60),
      right  = FALSE,
      labels = c("20-29", "30-39", "40-49", "50-59"))
}

# Build an analysis-ready dataset with age_band added.
dat <- selected_columns_no_missing_time %>%
  mutate(age_band = cut_ages(DVAge))

# 5) Function to run one model for a given outcome.
#    Model: outcome ~ game_hours + controls
#    • na.action = na.omit → drops rows with missing vars for that spec.
run_one <- function(df, outcome_name) {
  fml <- as.formula(paste(outcome_name, "~ game_hours +", paste(controls, collapse = " + ")))
  lm(fml, data = df, na.action = na.omit) %>%
    tidy() %>%
    mutate(outcome = outcome_name)
}

# 6) Run the grid of models: each outcome within each age band.
#    Result: a tidy table of all coefficients for all specs.
results <- dat %>%
  filter(!is.na(age_band)) %>%                 # drop rows without age band
  group_by(age_band) %>%                       # run per age band
  group_modify(~ bind_rows(                    # for each band, loop over outcomes
    lapply(outcomes_01, run_one, df = .x)
  )) %>%
  ungroup()

# 7) Preview the treatment effect only (the coefficient on game_hours),
#    along with SEs and p-values, per age band & outcome.
spec_preview <- results %>%
  filter(term == "game_hours") %>%
  select(age_band, outcome, estimate, std.error, p.value)

# Print a compact preview to console
spec_preview
