library(dplyr)
library(haven)
# Make new df with selected variables (incl. act1_1..act1_144), silently ignore missing columns, then remove merged_df to free memory
core_cols <- c(
  "serial","strata","psu","pnum","daynum","HhOut","IndOut","DMFlag","IMonth","IYear","DVAge","DPday",
  "DiaryDate_Act","DiaryDay_Act","DiaryDateDiff","dmonth","dyear","ddayw","DiaryType","WhenDiary",
  "AfterDiaryDay","WhereStart","WhereEnd","RushedD","Ordinary","KindOfDay","Trip","dia_wt_a","dia_wt_b",
  "ind_wt","DMSex","TVSet","TVSetNum","Cable","CableNum","Games","GamesNum","Comp","CompNum","WorkSta",
  "Educ","EducCur","HiQual","GenHlth","LongIll","Stressed","Rushed","SatisOv","Satis","Worth","Happy",
  "Anxious","SatHlth","SatInc","SatBal","SatSoc","SatHouse","SatLeis","SatPart","SatJob"
)

df <- merged_df %>%
  select(
    any_of(core_cols),
    tidyselect::num_range("act1_", 1:144)
  )

rm(merged_df); gc()

# Drop rows where ANY act1_* column contains 9960 or 9970 (missing time codes)
df2 <- df %>%
  filter(!if_any(starts_with("act1_"), ~ .x %in% c(9960, 9970)))

# Rename df2 and clean up, then save to RDS
selected_columns_no_missing_time <- df2

# Delete df
if (exists("df", inherits = FALSE)) rm(df)
if (exists("df2", inherits = FALSE)) rm(df2)

# Write to file
saveRDS(selected_columns_no_missing_time, "selected_columns_no_missing_time.rds", compress = "xz")


#This section is for removal or conversion of answers that were not given in the survey
# Replace negative sentinel codes with NA in selected columns,
# then also set SatJob/SatPart values of 8 (or "8.0") to NA.
replacement_values <- c(-8, -7, -2, -1, -9, -6)

columns_to_replace <- c(
  "SatInc","SatJob","SatBal","SatHouse","SatSoc","SatLeis","SatPart",
  "Worth","Happy","Anxious","Satis","Educ","GenHlth","LongIll","Rushed",
  "Stressed","WorkSta","SatisOv","EducCur"
)

selected_columns_no_missing_time <- selected_columns_no_missing_time %>%
  mutate(
    # negative codes → NA
    across(any_of(columns_to_replace), ~ {
      v <- .
      v[v %in% replacement_values] <- NA
      v
    }),
    # SatJob/SatPart: treat 8 or "8.0" as missing → NA
    across(any_of(c("SatJob","SatPart")), ~ {
      v <- .
      is_eight <- suppressWarnings(as.numeric(v) == 8)
      v[!is.na(is_eight) & is_eight] <- NA
      v
    })
  )


# SatisOv is also 1-7 range.
# 
# Satis, Worth, Happy are 0-10 range
# 
# Anxious are 0-10 range
# 
# Educ is binary 1 being yes and 2 being no (should that be changed to 0 and 1 with 0 being no and 1 being yes?)
# 
# LongIll is binary with 1 being yes and 2 being no.
# 
# GenHlth is 1-5 range with 1 being very good and 5 being very bad (should those values be reversed with negative outcomes at 1 and positive at 5?)
# 
# Rushed, Stressed are ranged 1-3 with 1 being always, 2 being sometimes and 3 being never.

library(dplyr)

# Harmonize scales: create 0–1 and z-score variants; reverse where needed so higher = better

# helpers
to_num <- function(x) suppressWarnings(as.numeric(x))
zscore <- function(x) {
  v <- to_num(x)
  mu <- mean(v, na.rm = TRUE); s <- sd(v, na.rm = TRUE)
  if (is.finite(s) && s > 0) (v - mu) / s else rep(NA_real_, length(v))
}

sat7 <- c("SatInc","SatJob","SatBal","SatHouse","SatLeis","SatPart","SatisOv")    # 1–7, higher=better
sat10 <- c("Satis","Worth","Happy")                                               # 0–10, higher=better
three <- c("Rushed","Stressed")                                                   # 1–3, higher=better
bin01 <- c("Educ","LongIll")                                                      # 1=yes, 2=no
# Anxious (0–10, higher=worse), GenHlth (1–5, 1=very good … 5=very bad) handled separately

selected_columns_no_missing_time <- selected_columns_no_missing_time %>%
  # 1) 1–7 satisfactions → _01 and _z
  mutate(
    across(any_of(sat7),
           .fns = list(`01` = ~ (to_num(.) - 1) / 6,
                       z = ~ zscore(.)),
           .names = "{.col}_{.fn}")
  ) %>%
  # 2) 0–10 scales → _01 and _z
  mutate(
    across(any_of(sat10),
           .fns = list(`01` = ~ to_num(.) / 10,
                       z = ~ zscore(.)),
           .names = "{.col}_{.fn}")
  ) %>%
  # 3) Anxious (reverse so higher=better) → _rev, _rev_01, _rev_z
  mutate(
    Anxious_rev    = 10 - to_num(Anxious),
    Anxious_rev_01 = (10 - to_num(Anxious)) / 10,
    Anxious_rev_z  = zscore(10 - to_num(Anxious))
  ) %>%
  # 4) GenHlth (reverse so higher=better) → _rev, _rev_01, _rev_z
  mutate(
    GenHlth_rev    = 6 - to_num(GenHlth),
    GenHlth_rev_01 = ( (6 - to_num(GenHlth)) - 1) / 4,
    GenHlth_rev_z  = zscore(6 - to_num(GenHlth))
  ) %>%
  # 5) 1–3 (Rushed, Stressed) → _01 and _z
  mutate(
    across(any_of(three),
           .fns = list(`01` = ~ (to_num(.) - 1) / 2,
                       z = ~ zscore(.)),
           .names = "{.col}_{.fn}")
  ) %>%
  # 6) Binary (Educ, LongIll): 1=yes → 1, 2=no → 0
  mutate(
    Educ_bin    = if_else(to_num(Educ) == 1, 1L, if_else(to_num(Educ) == 2, 0L, NA_integer_)),
    LongIll_bin = if_else(to_num(LongIll) == 1, 1L, if_else(to_num(LongIll) == 2, 0L, NA_integer_))
  )

# Optional quick sanity checks (uncomment to run):
# summary(select(selected_columns_no_missing_time, ends_with("_01")))
# sapply(select(selected_columns_no_missing_time, ends_with("_01")), range, na.rm = TRUE)







# Recode WorkSta into analysis-friendly variables while preserving the original:
# - Keep a full-detail labeled factor (WorkSta_factor) for descriptives.
# - Create a 7-way collapsed categorical (WorkSta7) with practical labour-market groupings.
# - Create a binary employed indicator (Employed_bin: 1 = employed, 0 = not).
# - Create a labour-force status factor (LF_status: Employed / Unemployed / Inactive).
# Notes:
# * First coerce WorkSta to numeric to strip any SPSS/haven labels cleanly.
# * Treat "On maternity leave" and "Unpaid worker in family business" as employed (standard in labour stats).
# * Group "Government training scheme" with unemployed (active labour-market participation).
# * "Doing something else" becomes "Other" or folds into "Inactive" for LF_status.

selected_columns_no_missing_time <- selected_columns_no_missing_time %>%
  mutate(
    # --- Helper: ensure WorkSta is numeric codes (handles haven_labelled etc.). ---
    #    - as.numeric() extracts the underlying code values (1, 2, 3, ..., 97).
    #    - suppressWarnings avoids label-related messages if present.
    WorkSta_num = suppressWarnings(as.numeric(WorkSta)),
    
    # --- A) Full-detail factor with human-readable labels (no collapsing). ---
    #    Use this for summary tables and to sanity-check the distribution of statuses.
    WorkSta_factor = factor(
      WorkSta_num,
      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 97),
      labels = c(
        "Self employed",
        "In paid employment",
        "Unemployed",
        "Retired",
        "On maternity leave",
        "Looking after family/home",
        "Full-time student",
        "Long-term sick/disabled",
        "Government training scheme",
        "Unpaid worker in family business",
        "Other"
      )
    ),
    
    # --- B) Collapsed 7-way status (categorical factor) for most analyses. ---
    #    Rationale for grouping:
    #      * Employed: 1,2,5,10 (self-employed, paid employment, on maternity leave, unpaid family worker).
    #      * Unemployed: 3,9 (includes govt training scheme as "active" unemployed).
    #      * Retired: 4
    #      * Home/Family care: 6
    #      * Student: 7
    #      * Sick/Disabled: 8
    #      * Other: 97
    WorkSta7 = case_when(
      WorkSta_num %in% c(1, 2, 5, 10) ~ "Employed",
      WorkSta_num %in% c(3, 9)        ~ "Unemployed",
      WorkSta_num == 4                ~ "Retired",
      WorkSta_num == 6                ~ "Home/Family care",
      WorkSta_num == 7                ~ "Student",
      WorkSta_num == 8                ~ "Sick/Disabled",
      WorkSta_num == 97               ~ "Other",
      TRUE                            ~ NA_character_
    ),
    # Set "Employed" as the reference by placing it first in levels.
    WorkSta7 = factor(
      WorkSta7,
      levels = c("Employed", "Unemployed", "Retired", "Home/Family care", "Student", "Sick/Disabled", "Other")
    ),
    
    # --- C) Binary employed indicator for quick controls / simple models. ---
    #    1 = employed (1,2,5,10). 0 = not employed (all other defined categories).
    #    Missing stays as NA.
    Employed_bin = case_when(
      WorkSta_num %in% c(1, 2, 5, 10)       ~ 1L,
      WorkSta_num %in% c(3, 4, 6, 7, 8, 9, 97) ~ 0L,
      is.na(WorkSta_num)                    ~ NA_integer_
    ),
    
    # --- D) Labour-force status (ternary factor): Employed / Unemployed / Inactive. ---
    #    Inactive includes: Retired (4), Home/Family (6), Student (7), Sick/Disabled (8), Other (97).
    LF_status = case_when(
      WorkSta_num %in% c(1, 2, 5, 10) ~ "Employed",
      WorkSta_num %in% c(3, 9)        ~ "Unemployed",
      WorkSta_num %in% c(4, 6, 7, 8, 97) ~ "Inactive",
      TRUE                             ~ NA_character_
    ),
    LF_status = factor(LF_status, levels = c("Employed", "Unemployed", "Inactive"))
  ) %>%
  # Drop the numeric helper to keep the dataset tidy (original WorkSta remains).
  select(-WorkSta_num)

# --- Optional QA (uncomment to inspect distributions) ---
# with(selected_columns_no_missing_time, table(WorkSta_factor, useNA = "ifany"))
# with(selected_columns_no_missing_time, table(WorkSta7, useNA = "ifany"))
# with(selected_columns_no_missing_time, table(LF_status, useNA = "ifany"))

# --- Optional: change reference level for modeling (example) ---
# selected_columns_no_missing_time$WorkSta7 <- relevel(selected_columns_no_missing_time$WorkSta7, ref = "Employed")
# selected_columns_no_missing_time$LF_status <- relevel(selected_columns_no_missing_time$LF_status, ref = "Employed")


# Recode EducCur using my preferred Option B (coarse 5-group scheme),
# AND convert codes 33 ("Don't know") and 34 ("None of the above") to NA.
#
# What this does:
# 1) Creates a helper numeric version of EducCur (EducCur_num) to work with raw codes.
# 2) Builds a "full-labels" factor (EducCur_factor) from the SPSS labels, but forces
#    entries with codes 33/34 to NA (so they are treated as missing).
# 3) Creates a collapsed factor EducCur_group with 4 *substantive* categories because
#    33/34 are set to NA and therefore drop out:
#      - HigherEducation: 1–3,7–9,19
#      - UpperSecondary: 10–13,16,17,24
#      - LowerSecondary/GCSE: 20–23
#      - Vocational/Other: 4–6,14,15,26,27,28,29,30,31,32
#    (None/Unknown would have been a 5th group, convert 33/34 to NA.)
#
# Notes:
# - Keep the original EducCur unchanged.
# - Using haven::as_factor preserves the original SPSS value labels for EducCur_factor.
# - Can use EducCur_group directly in models as a factor (unordered categories).

selected_columns_no_missing_time <- selected_columns_no_missing_time %>%
  mutate(
    # Helper: numeric codes for conditional logic (handles haven_labelled silently)
    EducCur_num = suppressWarnings(as.numeric(EducCur)),
    
    # Full-detail factor from SPSS labels; then set "Don't know"/"None of the above" to NA
    EducCur_factor = haven::as_factor(EducCur, levels = "labels"),
    EducCur_factor = replace(EducCur_factor, EducCur_num %in% c(33, 34), NA),
    
    # Collapsed Option B grouping (4 substantive categories; 33/34 -> NA)
    EducCur_group = case_when(
      EducCur_num %in% c(1, 2, 3, 7, 8, 9, 19)              ~ "HigherEducation",
      EducCur_num %in% c(10, 11, 12, 13, 16, 17, 24)        ~ "UpperSecondary",
      EducCur_num %in% c(20, 21, 22, 23)                    ~ "LowerSecondary/GCSE",
      EducCur_num %in% c(4, 5, 6, 14, 15, 26, 27, 28, 29, 30, 31, 32) ~ "Vocational/Other",
      EducCur_num %in% c(33, 34)                            ~ NA_character_,  # as necessary
      TRUE                                                  ~ NA_character_   # any unexpected/missing codes
    ),
    EducCur_group = factor(
      EducCur_group,
      levels = c("HigherEducation", "UpperSecondary", "LowerSecondary/GCSE", "Vocational/Other")
    )
  ) %>%
  # Drop the numeric helper to keep the data tidy
  select(-EducCur_num)

# --- Optional QA (uncomment to inspect) ---
# with(selected_columns_no_missing_time, table(EducCur_factor, useNA = "ifany"))
# with(selected_columns_no_missing_time, table(EducCur_group,  useNA = "ifany"))



# GAMING TIME SECTION
# 
# 
# GAMING TIME SECTION
# 
# 
# GAMING TIME SECTION
# 
# 
# 
# GAMING TIME SECTION



# Derive three gaming measures from the 144×10-minute activity slots:
# - game_slots   : count of 10-minute slots coded as 7330 (video/computer games)
# - game_minutes : total minutes spent gaming that diary day (= game_slots × 10)
# - game_any     : binary indicator (1 if any gaming that day, else 0)
#
# Implementation notes:
# * Select all columns whose names start with "act1_" (act1_1 .. act1_144).
# * Coerce them to numeric first (SPSS imports may be haven_labelled), then
#   build a numeric matrix for fast, vectorized comparison to the code 7330.
# * rowSums(... == 7330) counts TRUEs per row; na.rm=TRUE is defensive.
# * Minutes are stored as integers; the binary flag is 0/1 as integer.
selected_columns_no_missing_time <- selected_columns_no_missing_time %>%
  mutate(
    game_slots = {
      # pull all 144 activity columns and coerce to a plain numeric matrix
      act_mat <- as.matrix(suppressWarnings(
        sapply(pick(starts_with("act1_")), as.numeric)
      ))
      # count how many of the 144 slots equal 7330 on each row (person-day)
      rowSums(act_mat == 7330, na.rm = TRUE)
    },
    # convert slots to minutes (10 minutes per slot)
    game_minutes = as.integer(game_slots * 10L),
    # 1 if any gaming occurred (at least one slot), 0 otherwise
    game_any = as.integer(game_slots > 0L)
  )
saveRDS(selected_columns_no_missing_time, "selected_columns_no_missing_time.rds", compress = "xz")
# Optional quick checks (uncomment to use):
# summary(selected_columns_no_missing_time$game_slots)
# summary(selected_columns_no_missing_time$game_minutes)
# table(selected_columns_no_missing_time$game_any)
