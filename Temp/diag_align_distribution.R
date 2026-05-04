## Confirm: did any Engineers say "misaligned" on s5_view_align?
## Look at raw values + count of aligndiff{role}_* dummies.
library(pacman)
p_load(haven, here, base, readr, data.table, stringr,
       tidyverse, dplyr, fixest, labelled)
setwd(here())
options(readr.show_types = FALSE)
date <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
source(here("1_Codes", "2_Import.R"))
source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R"))
source(here("1_Codes", "5_Cleaning.R"))

extra_perc <- c("s5_view_align", "s5_view_aligndiffeng", "s5_view_aligndiffhr",
                grep("^aligndiff(hr|eng)_", names(firm_dt), value = TRUE))
extra_perc <- intersect(extra_perc, names(firm_dt))

resp_dt <- unique(merge(
  firm_long_dt[, .(responseid, role)],
  firm_dt[, c("responseid", extra_perc), with = FALSE],
  by = "responseid"))

eng_items <- grep("^aligndiffeng_", extra_perc, value = TRUE)
hr_items  <- grep("^aligndiffhr_",  extra_perc, value = TRUE)

cat("\n=== s5_view_align distribution by role (raw values) ===\n")
cat("(Class of s5_view_align:", class(resp_dt$s5_view_align), ")\n")
print(resp_dt[, .N, by = .(role, s5_view_align)][order(role, s5_view_align)])

cat("\n=== Value labels on s5_view_align (if any) ===\n")
print(val_labels(resp_dt$s5_view_align))

cat("\n=== Class of s5_view_aligndiffeng / hr ===\n")
cat("aligndiffeng class:", class(resp_dt$s5_view_aligndiffeng), "\n")
cat("aligndiffhr  class:", class(resp_dt$s5_view_aligndiffhr),  "\n")

cat("\n=== Sample of raw s5_view_aligndiffeng (Eng) ===\n")
eng_raw <- resp_dt[role == "Eng", s5_view_aligndiffeng]
cat("  All NA:    ", sum(is.na(eng_raw)), "\n")
cat("  Empty str: ", sum(!is.na(eng_raw) & eng_raw == ""), "\n")
cat("  Non-empty: ", sum(!is.na(eng_raw) & eng_raw != ""), "\n")
cat("  Unique non-empty values (first 20):\n")
print(head(unique(eng_raw[!is.na(eng_raw) & eng_raw != ""]), 20))

cat("\n=== Sample of raw s5_view_aligndiffhr (HR) ===\n")
hr_raw <- resp_dt[role == "HR", s5_view_aligndiffhr]
cat("  All NA:    ", sum(is.na(hr_raw)), "\n")
cat("  Empty str: ", sum(!is.na(hr_raw) & hr_raw == ""), "\n")
cat("  Non-empty: ", sum(!is.na(hr_raw) & hr_raw != ""), "\n")
cat("  Unique non-empty values (first 20):\n")
print(head(unique(hr_raw[!is.na(hr_raw) & hr_raw != ""]), 20))

cat("\n=== count(aligndiff*_*) distribution by role ===\n")
resp_dt[, eng_count := rowSums(.SD, na.rm = FALSE), .SDcols = eng_items]
resp_dt[, hr_count  := rowSums(.SD, na.rm = FALSE), .SDcols = hr_items]
cat("Eng count distribution:\n")
print(resp_dt[role == "Eng", .N, by = eng_count][order(eng_count)])
cat("HR count distribution:\n")
print(resp_dt[role == "HR", .N, by = hr_count][order(hr_count)])

cat("\n=== count > 0 cross-tab with s5_view_align (Eng) ===\n")
print(resp_dt[role == "Eng", .N, by = .(s5_view_align, eng_count_pos = eng_count > 0)][order(s5_view_align)])
cat("\n=== count > 0 cross-tab with s5_view_align (HR) ===\n")
print(resp_dt[role == "HR", .N, by = .(s5_view_align, hr_count_pos = hr_count > 0)][order(s5_view_align)])

cat("\n--- Done ---\n")
