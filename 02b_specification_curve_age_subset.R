# 02b_specification_curve_age_subset.R
# ---------------------------------------------------------------
# Specification Curve Analysis with age-band subsetting.
# - Implements methodology defined in "David_safe_linear_mode_test.R/"
# - Defines child/teen/uni + adult buckets.
# - Uses a character subset column (age_band_chr) to avoid factor/character compare errors.
# - Prunes subset levels with zero complete rows.
# - Wraps lm with error handling (lm_safe) to avoid aborting on empty cells.
# - Logs skipped age-band levels.
# - Outcomes: 0–1 scales (higher = better); Treatment: game_hours.
# ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(specr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

# ---------- parameters ----------
UNI_TOP <- 21                   # change to 22 if desired
log_path <- "sca_age_subset.log"

# ---------- logging helper ----------
logf <- function(fmt, ...) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] %s\n", ts, sprintf(fmt, ...)), file = log_path, append = TRUE)
}

logf("---- Starting 02b_specification_curve_age_subset ----")

# ---------- load data ----------
dat <- readRDS("cleaned_data_full.RDS")
logf("Loaded cleaned_data_full.RDS with %d rows and %d columns.", nrow(dat), ncol(dat))

# Ensure treatment exists (hours)
if (!"game_hours" %in% names(dat) && "game_minutes" %in% names(dat)) {
  dat <- mutate(dat, game_hours = game_minutes / 60)
  logf("Derived game_hours from game_minutes.")
}
if (!"game_hours" %in% names(dat)) stop("game_hours not found and cannot be derived.")
logf("Non-missing game_hours: %d", sum(!is.na(dat$game_hours)))

# ---------- variables ----------
wellbeing_vars_01 <- c(
  "SatInc_01","SatJob_01","SatBal_01","SatHouse_01","SatLeis_01","SatPart_01","SatisOv_01",
  "Satis_01","Worth_01","Happy_01",
  "Anxious_rev_01","GenHlth_rev_01",
  "Rushed_01","Stressed_01"
)
x_var <- "game_hours"
controls_all <- c("DVAge","DMSex","Educ_bin","LongIll_bin","WorkSta7","EducCur_group","HiQual")

# Keep only columns that exist; drop outcomes that are entirely NA
y_present <- intersect(wellbeing_vars_01, names(dat))
y_present <- y_present[vapply(y_present, function(v) sum(!is.na(dat[[v]])) > 0, logical(1))]
c_present <- intersect(controls_all, names(dat))
if (length(y_present) == 0) stop("No eligible outcomes (all-NA or missing).")

logf("Outcomes used (%d): %s", length(y_present), paste(y_present, collapse = ", "))
logf("Controls used (%d): %s", length(c_present), paste(c_present, collapse = ", "))

# ---------- age bands (factor for reporting + character for subsetting) ----------
to_num_age <- function(x) suppressWarnings(as.numeric(x))

if (!"age_band" %in% names(dat)) {
  dat <- dat %>%
    mutate(
      DVAge_num = to_num_age(DVAge),
      age_band = cut(
        DVAge_num,
        breaks = c(-Inf, 13, 18, UNI_TOP + 1, 30, 40, 50, 60, 70, Inf),
        right  = FALSE,
        labels = c(
          "Pre-teen",                # <13
          "Teen (13–17)",            # 13–17
          paste0("Uni (18–", UNI_TOP, ")"), # 18–UNI_TOP
          "22–29", "30–39", "40–49", "50–59", "60–69", "70+"
        )
      )
    ) %>%
    mutate(
      age_band = factor(
        age_band,
        levels = c(
          "Pre-teen","Teen (13–17)", paste0("Uni (18–", UNI_TOP, ")"),
          "22–29","30–39","40–49","50–59","60–69","70+"
        ),
        ordered = TRUE
      )
    ) %>%
    select(-DVAge_num)
  logf("Derived age_band with UNI_TOP=%d.", UNI_TOP)
}

# Character copy used for subsetting (prevents factor/character comparison errors)
dat <- dat %>%
  mutate(age_band_chr = if (!"age_band_chr" %in% names(.)) as.character(age_band) else age_band_chr)

# ---------- safe model wrapper ----------
lm_safe <- function(formula, data, ..., na.action = na.omit) {
  tryCatch(
    stats::lm(formula, data = data, na.action = na.action, ...),
    error = function(e) {
      message(sprintf("lm_safe: %s → returning placeholder lm(1 ~ 1)", conditionMessage(e)))
      stats::lm(1 ~ 1)
    }
  )
}

# ---------- subset pruning ----------
candidate_levels <- na.omit(unique(dat$age_band_chr))
levels_kept <- character(0)
levels_dropped <- character(0)

for (L in candidate_levels) {
  gdf <- dat[dat$age_band_chr %in% L, , drop = FALSE]
  ok_all <- all(vapply(y_present, function(y) {
    vars <- c(x_var, c_present, y)
    vars <- vars[vars %in% names(gdf)]
    sum(complete.cases(gdf[, vars, drop = FALSE])) >= 1
  }, logical(1)))
  if (ok_all) levels_kept <- c(levels_kept, L) else levels_dropped <- c(levels_dropped, L)
}

if (length(levels_dropped) > 0) {
  logf("Skipped age_band levels due to insufficient data: %s", paste(levels_dropped, collapse = ", "))
} else {
  logf("No age_band levels skipped.")
}

subsets_spec <- NULL
if (length(levels_kept) > 0) {
  subsets_spec <- list(age_band_chr = levels_kept)  # use the character column
  logf("Subsets used: %s", paste(levels_kept, collapse = ", "))
} else {
  logf("All age_band levels failed completeness check; running without subsets.")
}

# ---------- specr setup ----------
specs0 <- setup(
  data     = dat,
  y        = y_present,
  x        = x_var,
  model    = "lm_safe",   # safe wrapper
  controls = c_present,
  subsets  = subsets_spec,
  simplify = TRUE
)
logf("Initial spec grid size: %d rows.", nrow(specs0$specs))

# Optional pre-filter: drop spec rows that would have zero complete cases
if (nrow(specs0$specs) > 0) {
  grid <- specs0$specs
  keep <- vapply(seq_len(nrow(grid)), function(i) {
    g <- grid[i, , drop = FALSE]
    y  <- g$y
    ctrl <- g$controls[[1]]; if (is.null(ctrl)) ctrl <- character(0)
    rhs <- c(x_var, ctrl)
    fml_vars <- unique(c(y, rhs))
    dsub <- dat
    
    if (!is.null(g$subsets[[1]])) {
      sb <- g$subsets[[1]]
      for (nm in names(sb)) dsub <- dsub[dsub[[nm]] %in% sb[[nm]], , drop = FALSE]
    }
    
    fml_vars <- intersect(fml_vars, names(dsub))
    if (length(fml_vars) == 0) return(FALSE)
    sum(complete.cases(dsub[, fml_vars, drop = FALSE])) > 0
  }, logical(1))
  
  dropped_specs <- sum(!keep)
  if (dropped_specs > 0) logf("Pre-filter removed %d spec rows with zero complete cases.", dropped_specs)
  specs0$specs <- grid[keep, , drop = FALSE]
  logf("Final spec grid size after pre-filter: %d rows.", nrow(specs0$specs))
}

if (nrow(specs0$specs) == 0) stop("No viable specifications remain after pruning.")

# ---------- run SCA ----------
results <- specr(specs0)

# ---------- plot + save ----------
p <- plot(results)
print(p)

saveRDS(results, "sca_results_age_subset.RDS")
ggplot2::ggsave("sca_plot_age_subset.png", p, width = 11, height = 7, dpi = 300)

logf("Saved results to sca_results_age_subset.RDS and sca_plot_age_subset.png.")
logf("---- Finished 02b_specification_curve_age_subset ----")
