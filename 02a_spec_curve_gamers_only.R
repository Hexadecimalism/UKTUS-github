# 02a_spec_curve_gamers_only.R
# ---------------------------------------------------------------
# Specification Curve Analysis on gamers only (game_hours > 0).
# Outcomes: 0–1 scales (higher = better).
# Treatment: game_hours (1 unit = 1 hour/day), restricted to > 0.
# Controls: recoded set used in the main SCA script.
# No subsets; per-spec NA handling via lm().
# ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(specr)
  library(dplyr)
})

# ---- Load data (as-is) ----------------------------------------
dat <- readRDS("cleaned_data_full.RDS")

# Ensure treatment exists (hours)
if (!"game_hours" %in% names(dat) && "game_minutes" %in% names(dat)) {
  dat <- mutate(dat, game_hours = game_minutes / 60)
}

# Restrict to gamers (non-zero time)
dat_gamers <- dat %>%
  filter(!is.na(game_hours) & game_hours > 0)

cat("Rows (all):   ", nrow(dat), "\n")
cat("Rows (gamers):", nrow(dat_gamers), "\n")

# ---- Define outcomes, treatment, controls ---------------------
wellbeing_vars_01 <- c(
  "SatInc_01","SatJob_01","SatBal_01","SatHouse_01","SatLeis_01","SatPart_01","SatisOv_01",
  "Satis_01","Worth_01","Happy_01",
  "Anxious_rev_01","GenHlth_rev_01",
  "Rushed_01","Stressed_01"
)
x_var <- "game_hours"
controls_all <- c("DVAge","DMSex","Educ_bin","LongIll_bin","WorkSta7","EducCur_group","HiQual")

# Keep only columns that exist; drop outcomes that are all-NA in the gamers sample
y_present <- intersect(wellbeing_vars_01, names(dat_gamers))
y_present <- y_present[vapply(y_present, function(v) sum(!is.na(dat_gamers[[v]])) > 0, logical(1))]
c_present <- intersect(controls_all, names(dat_gamers))
stopifnot(x_var %in% names(dat_gamers))
if (length(y_present) == 0) stop("No non-missing outcomes among gamers.")

cat("Outcomes used:", paste(y_present, collapse = ", "), "\n")
cat("Controls used:", paste(c_present, collapse = ", "), "\n")

# ---- Build the spec grid and run SCA (no subsets) --------------
specs <- setup(
  data     = dat_gamers,
  y        = y_present,
  x        = x_var,
  model    = "lm",
  controls = c_present
)

results <- specr(specs)

# ---- Plot + save -----------------------------------------------
p <- plot(results)
print(p)

saveRDS(results, "sca_results_gamers_only.RDS")
ggplot2::ggsave("sca_plot_gamers_only.png", p, width = 11, height = 7, dpi = 300)

cat("SCA (gamers only) complete. Saved sca_results_gamers_only.RDS and sca_plot_gamers_only.png.\n")
