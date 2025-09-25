# 02_specification_curve_analysis.R
# ---------------------------------------------------------------
# Specification Curve Analysis (SCA) with 'specr' on the "as-is"
# on (cleaned_data_full.RDS).
#
# Key choices:
#   • Outcomes are on 0–1 scales (higher = better) for comparability.
#   • Treatment is game_hours (1 unit = 1 hour/day).
#   • Controls use the recodes created (factors/binaries).
#   • No mutation of the source data; this script is read-only.
#   • No data_nomiss here will fix that later.
# ---------------------------------------------------------------

library(specr)
library(dplyr)

# ---- 1) Load data (as-is) -------------------------------------
dat <- readRDS("cleaned_data_full.RDS")

# If game_hours isn’t present but game_minutes is, derive it (non-destructive)
if (!"game_hours" %in% names(dat) && "game_minutes" %in% names(dat)) {
  dat <- mutate(dat, game_hours = game_minutes / 60)
}

# ---- 2) Define outcomes, treatment, controls -------------------
# Outcomes: 0–1 scales (all higher = better) created in prep
wellbeing_vars_01 <- c(
  # 1–7 satisfactions → 0–1
  "SatInc_01","SatJob_01","SatBal_01","SatHouse_01","SatLeis_01","SatPart_01","SatisOv_01",
  # 0–10 wellbeing → 0–1
  "Satis_01","Worth_01","Happy_01",
  # reversed/rescaled
  "Anxious_rev_01","GenHlth_rev_01",
  # 1–3 recodes → 0–1
  "Rushed_01","Stressed_01"
)

# Treatment (1 unit = 1 hour/day of gaming)
x_var <- "game_hours"

# Controls: recodes that were built; will keep only those actually present
controls_all <- c("DVAge","DMSex","Educ_bin","LongIll_bin","WorkSta7","EducCur_group","HiQual")

# Keep only variables that exist to avoid errors if something’s missing
y_present <- intersect(wellbeing_vars_01, names(dat))
c_present <- intersect(controls_all,      names(dat))
stopifnot(x_var %in% names(dat))
message("SCA with ", length(y_present), " outcomes and ", length(c_present), " controls.")

# ---- 3) (Optional) subset variable (age bands) -----------------
# If added age_band in preprocessing, specr can subset by it.
subsets_spec <- NULL
if ("age_band" %in% names(dat)) {
  subsets_spec <- list(age_band = stats::na.omit(unique(dat$age_band)))
}

# ---- 4) Build the spec grid and run SCA ------------------------
specs <- setup(
  data     = dat,
  y        = y_present,
  x        = x_var,
  model    = "lm",
  controls = c_present,
  subsets  = subsets_spec
  # (can add robust/cluster/fixed args here later if needed)
)

results <- specr(specs)

# ---- 5) Plot + save -------------------------------------------
p <- plot(results)   # default plot (curve + choices overview)
print(p)

saveRDS(results, "sca_results_full.RDS")
ggplot2::ggsave("sca_plot_full.png", p, width = 11, height = 7, dpi = 300)

message("SCA complete (full dataset only). Saved sca_results_full.RDS and sca_plot_full.png.")
