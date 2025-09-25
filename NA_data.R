#This checks the data to see what it would be like if we dropped all rows that have NA variables
#WARNING! THIS CREATES AND EMPTY DATAFRAME (0 OBSERVATIONS) WHICH IS WHY IT WAS ORIGINALLY UNUSED
#I should also note that lm()/specr should already be handling this
#regressions SHOULD be handling NA values

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# 0) Load
data_full <- readRDS("cleaned_data_full.RDS")

# 1) Ensure game_hours exists and inspect NA count (should be 0)
if (!"game_hours" %in% names(data_full) && "game_minutes" %in% names(data_full)) {
  data_full <- mutate(data_full, game_hours = game_minutes / 60)
}
cat("game_hours NA count:", sum(is.na(data_full$game_hours)), "\n")

# 2) Define sets (as in SCA script)
outcomes_01 <- c(
  "SatInc_01","SatJob_01","SatBal_01","SatHouse_01","SatLeis_01","SatPart_01","SatisOv_01",
  "Satis_01","Worth_01","Happy_01",
  "Anxious_rev_01","GenHlth_rev_01",
  "Rushed_01","Stressed_01"
)
controls <- c("DVAge","DMSex","Educ_bin","LongIll_bin","WorkSta7","EducCur_group","HiQual")

y_present <- intersect(outcomes_01, names(data_full))
# drop outcomes that are entirely NA
y_present <- y_present[vapply(y_present, function(v) sum(!is.na(data_full[[v]])) > 0, logical(1))]
c_present <- intersect(controls, names(data_full))

# 3) Missingness report (per variable)
analysis_vars <- c("game_hours", y_present, c_present)

miss_report <- data_full %>%
  select(any_of(analysis_vars)) %>%
  summarise(across(everything(), ~ sum(is.na(.)), .names = "{.col}")) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  arrange(desc(n_missing))

print(miss_report, n = Inf)

# 4) Build strict no-NA dataset across predictor + outcomes + controls
data_nomiss_strict <- data_full %>%
  drop_na(any_of(analysis_vars))

cat("\nRows before:", nrow(data_full), "\n")
cat("Rows after :", nrow(data_nomiss_strict), "\n")
cat("Dropped    :", nrow(data_full) - nrow(data_nomiss_strict), "\n")

# Optional save:
# saveRDS(data_nomiss_strict, "cleaned_data_nomiss_strict.RDS", compress = "xz")
