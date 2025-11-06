# quick_non_na_counts_teens.R
# Counts non-NA values for wellbeing ( *_01 ) and controls at ages 13–17, printed per age and variable.

suppressPackageStartupMessages({
  library(dplyr)
  library(haven)
})

# --- load data ---
dat <- readRDS("cleaned_data_full.RDS")

# --- variables (same set used in the age-subset SCA) ---
wellbeing_vars_01 <- c(
  "SatInc_01","SatJob_01","SatBal_01","SatHouse_01","SatLeis_01","SatPart_01","SatisOv_01",
  "Satis_01","Worth_01","Happy_01",
  "Anxious_rev_01","GenHlth_rev_01",
  "Rushed_01","Stressed_01"
)
controls_all <- c("DVAge","DMSex","Educ_bin","LongIll_bin","WorkSta7","EducCur_group","HiQual")

vars_requested <- c(wellbeing_vars_01, controls_all)
vars_present   <- intersect(vars_requested, names(dat))
stopifnot(length(vars_present) > 0)

# --- robust numeric age from possibly labelled SPSS imports ---
to_num_age <- function(x) {
  if (inherits(x, "haven_labelled")) x <- haven::zap_labels(x)
  suppressWarnings(as.numeric(x))
}

# --- restrict to ages 13–17 and compute counts (wide table) ---
dat_teens <- dat %>%
  mutate(DVAge_num = to_num_age(DVAge)) %>%
  filter(!is.na(DVAge_num), DVAge_num >= 13, DVAge_num <= 17)

counts_wide <- dat_teens %>%
  group_by(DVAge_num) %>%
  summarise(
    across(
      .cols  = any_of(vars_present),
      .fns   = ~ sum(!is.na(.)),
      .names = "{.col}"
    ),
    .groups = "drop"
  ) %>%
  arrange(DVAge_num)

# --- print lines like "13 SatJob_01: 64" ---
var_cols <- intersect(vars_present, names(counts_wide))
for (i in seq_len(nrow(counts_wide))) {
  age <- counts_wide$DVAge_num[i]
  for (v in var_cols) {
    cat(sprintf("%s %s: %s\n", age, v, counts_wide[[v]][i]))
  }
}

invisible(counts_wide)
