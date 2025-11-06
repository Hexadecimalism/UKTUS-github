# quick_non_na_counts_by_age_wide.R
# Count non-NA per age band for wellbeing ( *_01 ) and controls — no pivot.

suppressPackageStartupMessages({
  library(dplyr)
  library(haven)
})

# --- load data ---
dat <- readRDS("cleaned_data_full.RDS")

# --- variables (match age-subset SCA) ---
wellbeing_vars_01 <- c(
  "SatInc_01","SatJob_01","SatBal_01","SatHouse_01","SatLeis_01","SatPart_01","SatisOv_01",
  "Satis_01","Worth_01","Happy_01",
  "Anxious_rev_01","GenHlth_rev_01",
  "Rushed_01","Stressed_01"
)
controls_all <- c("DVAge","DMSex","Educ_bin","LongIll_bin","WorkSta7","EducCur_group","HiQual")

vars_requested <- c(wellbeing_vars_01, controls_all)
vars_present   <- intersect(vars_requested, names(dat))
if (length(vars_present) == 0) stop("None of the requested wellbeing/controls are present.")

# --- helper: robust numeric from possibly labelled SPSS imports ---
to_num_age <- function(x) {
  if (inherits(x, "haven_labelled")) x <- haven::zap_labels(x)
  suppressWarnings(as.numeric(x))
}

# --- create age bands if missing (pre-teen, teen, uni, adult buckets) ---
UNI_TOP <- 21  # set to 22 if the university band should cover 18–22

if (!"age_band" %in% names(dat)) {
  if (!"DVAge" %in% names(dat)) stop("DVAge is missing; cannot derive age bands.")
  dat <- dat %>%
    mutate(
      DVAge_num = to_num_age(DVAge),
      age_band = cut(
        DVAge_num,
        breaks = c(-Inf, 13, 18, UNI_TOP + 1, 30, 40, 50, 60, 70, Inf),
        right  = FALSE,
        labels = c(
          "Pre-teen",                       # <13
          "Teen (13–17)",                   # 13–17
          paste0("Uni (18–", UNI_TOP, ")"), # 18–UNI_TOP
          "22–29","30–39","40–49","50–59","60–69","70+"
        )
      )
    ) %>%
    select(-DVAge_num)
}

# --- compute non-NA counts per age band (wide table) ---
counts_wide <- dat %>%
  filter(!is.na(age_band)) %>%
  group_by(age_band) %>%
  summarise(
    across(
      .cols  = any_of(vars_present),
      .fns   = ~ sum(!is.na(.)),
      .names = "{.col}"
    ),
    .groups = "drop"
  ) %>%
  arrange(age_band)

# --- print lines like "Pre-teen SatJob_01: 0" (no pivot) ---
var_cols <- intersect(vars_present, names(counts_wide))
for (i in seq_len(nrow(counts_wide))) {
  band <- as.character(counts_wide$age_band[i])
  for (v in var_cols) {
    cat(sprintf("%s %s: %s\n", band, v, counts_wide[[v]][i]))
  }
}

# Optionally return the wide table for inspection
invisible(counts_wide)
