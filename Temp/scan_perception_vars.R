## Phase 1 diagnostic: inspect inter-departmental perception variables.
## Variables live in firm_dt (one row per respondent), not firm_long_dt.

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

cat("\n===========================================================\n")
cat("Phase 1: Perception variables -- distributions & cross-tabs\n")
cat("===========================================================\n")

target_vars <- c("s5_view_align", "s5_view_aligndiffeng", "s5_view_aligndiffhr",
                 "s5_hr_influence", "s5_engineer_care")

## firm_dt has one row per respondent. We also need role and dei_index_A_z,
## which live in firm_long_dt -- attach via responseid.
role_map <- unique(firm_long_dt[, .(responseid, role, dei_index_A_z)])
resp_dt  <- merge(firm_dt[, c("responseid", target_vars,
                               grep("^aligndiff(hr|eng)_", names(firm_dt), value = TRUE)),
                          with = FALSE],
                  role_map, by = "responseid", all.x = TRUE)

cat(sprintf("\nfirm_dt rows: %d  |  matched-to-role rows: %d  |  role missing: %d\n",
            nrow(firm_dt), sum(!is.na(resp_dt$role)), sum(is.na(resp_dt$role))))
cat("\n--- Respondent counts by role (firm_dt) ---\n")
print(resp_dt[, .N, by = role])

for (v in target_vars) {
  cat(sprintf("\n\n========== %s ==========\n", v))
  cat("Class:", class(resp_dt[[v]]), "\n")
  lbl <- attr(resp_dt[[v]], "label")
  if (!is.null(lbl)) cat("Label: ", lbl, "\n")
  vlabs <- attr(resp_dt[[v]], "labels")
  if (!is.null(vlabs)) { cat("Value labels:\n"); print(vlabs) }
  cat("\nFreq overall:\n")
  print(table(resp_dt[[v]], useNA = "always"))
  cat("\nFreq by role:\n")
  print(table(resp_dt[[v]], resp_dt$role, useNA = "always"))
}

cat("\n\n========== Multi-select sub-items (aligndiffhr_*, aligndiffeng_*) ==========\n")
for (v in grep("^aligndiff", names(resp_dt), value = TRUE)) {
  cat(sprintf("\n%s -- ", v))
  lbl <- attr(resp_dt[[v]], "label")
  if (!is.null(lbl)) cat(lbl)
  cat("\n"); print(table(resp_dt[[v]], useNA = "ifany"))
}

cat("\n\n========== Cross-tab with own DEI views (dei_index_A_z quartiles) ==========\n")
resp_dt[, dei_q := cut(dei_index_A_z,
                       breaks = quantile(dei_index_A_z, c(0, .25, .5, .75, 1), na.rm = TRUE),
                       include.lowest = TRUE, labels = c("Q1 (low)", "Q2", "Q3", "Q4 (high)"))]
for (v in target_vars) {
  cat(sprintf("\n--- %s by own DEI quartile ---\n", v))
  for (r in c("Eng", "HR")) {
    cat(sprintf("\nRole = %s:\n", r))
    sub <- resp_dt[role == r]
    if (nrow(sub) > 0) print(table(sub[[v]], sub$dei_q, useNA = "ifany"))
  }
}

cat("\n\n========== Putative bucket sizes ==========\n")
cat("\n[Eng backlash bucket] s5_view_aligndiffeng (Eng's view of misalignment with HR):\n")
print(table(resp_dt[role == "Eng", s5_view_aligndiffeng], useNA = "always"))
cat("\n[Eng backlash conditioning] s5_hr_influence (Eng's view of HR influence):\n")
print(table(resp_dt[role == "Eng", s5_hr_influence], useNA = "always"))
cat("\n[HR compensation bucket] s5_engineer_care (HR's view of Eng prioritizing DEI):\n")
print(table(resp_dt[role == "HR",  s5_engineer_care], useNA = "always"))

cat("\n--- Done ---\n")
