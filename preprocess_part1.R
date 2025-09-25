library(haven)
library(dplyr)
library(digest)
#import data
data <- read_sav("UKTUS-data/uktus15_diary_wide.sav")
data2 <- read_sav("UKTUS-data/uktus15_individual.sav")


# Drop all columns whose names start with "othact", "wit", "wher", "dev", or "enj"
diary_wide <- data %>%
  select(
    -starts_with("othact"),
    -starts_with("wit"),
    -starts_with("wher"),
    -starts_with("dev"),
    -starts_with("enj")
  )
#view where bot columns interesect if merging by serial and pnum
setdiff(intersect(names(diary_wide), names(data2)), c("serial", "pnum"))

# Merge both datasets on serial and pnum
# By default, dplyr will keep all columns
# If there are overlapping column names, it will add suffixes "_x" and "_y"
merged_df <- inner_join(
  diary_wide,
  data2,
  by = c("serial", "pnum"),
  suffix = c("_x", "_y")
)

# Clean column names: if there's an _x but no _y version, drop the suffix
#for (col in names(merged_df)) {
#  if (endsWith(col, "_x")) {
#    base <- sub("_x$", "", col)
#    if (!(paste0(base, "_y") %in% names(merged_df))) {
#      names(merged_df)[names(merged_df) == col] <- base
#    }
#  }
#}
#deleting columns psu_y and strata_y
merged_df <- merged_df %>% select(-psu_y, -strata_y, -DVAge_y, -IndOut_y, -DMFlag_y, -IYear_y, -IMonth_y, -HhOut_y)


# Remove "_x" suffix from all column names
names(merged_df) <- sub("_x$", "", names(merged_df))

# Find pairs of columns that have the same values, ignoring class/labels/attributes
find_identical_columns_values_only <- function(df) {
  # normalize columns: drop haven/SPSS attributes, drop all attributes, align basic types
  normalize <- function(x) {
    if (inherits(x, "haven_labelled")) x <- haven::zap_labels(x)
    if (is.factor(x)) x <- as.character(x)
    attributes(x) <- NULL
    x
  }
  
  norm <- lapply(df, normalize)
  
  # fast candidate grouping by hashing normalized vectors, then verify with all.equal()
  if (!requireNamespace("digest", quietly = TRUE)) stop("Please install.packages('digest')")
  sig <- vapply(norm, function(x) digest::digest(x, algo = "xxhash64", serialize = TRUE), character(1))
  groups <- split(names(sig), sig)
  
  out <- list(); k <- 1
  for (g in groups) if (length(g) > 1) {
    for (i in seq_len(length(g) - 1)) for (j in (i + 1):length(g)) {
      a <- g[i]; b <- g[j]
      if (isTRUE(all.equal(norm[[a]], norm[[b]], check.attributes = FALSE))) {
        out[[k]] <- data.frame(col1 = a, col2 = b, row.names = NULL); k <- k + 1
      }
    }
  }
  if (length(out)) do.call(rbind, out) else data.frame(col1 = character(0), col2 = character(0))
}

# Run it
identical_columns_values <- find_identical_columns_values_only(merged_df)

# Print
if (nrow(identical_columns_values) == 0) {
  cat("No identical columns (values-only) found.\n")
} else {
  cat("Identical columns (values-only):\n")
  apply(identical_columns_values, 1, function(r) cat(r["col1"], "and", r["col2"], "\n"))
}

# Collapse true duplicate columns by value (ignoring labels/classes): keep the first name in each group, drop the rest, and print a summary.
#This should find 40 columns with duplicate values across 7 groups
dedupe_identical_columns <- function(df) {
  normalize <- function(x) {
    if (inherits(x, "haven_labelled")) x <- haven::zap_labels(x)
    if (is.factor(x)) x <- as.character(x)
    attributes(x) <- NULL
    x
  }
  norm <- lapply(df, normalize)
  
  if (!requireNamespace("digest", quietly = TRUE)) stop("Please install.packages('digest')")
  sig <- vapply(norm, function(x) digest::digest(x, algo = "xxhash64", serialize = TRUE), character(1))
  groups_all <- split(names(sig), sig)
  groups <- Filter(function(g) length(g) > 1, groups_all)
  
  # verify equality inside each hash group to avoid collisions
  groups <- lapply(groups, function(g) {
    ref <- g[1]
    g[sapply(g, function(nm) isTRUE(all.equal(norm[[ref]], norm[[nm]], check.attributes = FALSE)))]
  })
  groups <- Filter(function(g) length(g) > 1, groups)
  
  if (!length(groups)) {
    message("No duplicate columns by value found.")
    return(list(df = df, groups = list(), dropped = character(0)))
  }
  
  # keep the first (by original order), drop the rest
  ord <- setNames(seq_along(names(df)), names(df))
  reps <- vapply(groups, function(g) g[order(ord[g])][1], character(1))
  to_drop <- unlist(mapply(function(g, rep) setdiff(g, rep), groups, reps, SIMPLIFY = FALSE), use.names = FALSE)
  
  df2 <- df %>% select(-all_of(to_drop))
  
  cat("Dropped", length(to_drop), "duplicate columns across", length(groups), "groups.\n")
  for (i in seq_along(groups)) {
    cat(sprintf("[Group %d] kept: %s | dropped: %s\n",
                i, reps[i], paste(setdiff(groups[[i]], reps[i]), collapse = ", ")))
  }
  
  invisible(list(df = df2, groups = groups, kept = reps, dropped = to_drop))
}

res <- dedupe_identical_columns(merged_df)
merged_df <- res$df

# Keep only merged_df in the workspace; remove everything else and free memory
if (exists("merged_df", inherits = FALSE)) {
  rm(list = setdiff(ls(envir = .GlobalEnv), "merged_df"), envir = .GlobalEnv)
} else {
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
}
gc()

library(haven)
library(dplyr)
library(digest)
#Further tidying but will need to return here IF I need data that has been deleted
#
#
#
#
# BLOCK FOR NOTICE
#
#
#
#
# Drop all columns whose names start with "NPaid", "Vol", "Hlp", "CCare", or "Care"
merged_df <- merged_df %>%
  select(
    -starts_with("NPaid"),
    -starts_with("Vol"),
    -starts_with("Hlp"),
    -starts_with("CCare"),
    -starts_with("Care")
  )

# Save the merged_df object to an .rds file
out_path <- "UKTUS-data/merged_df.rds"
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
saveRDS(merged_df, out_path, compress = "xz")
