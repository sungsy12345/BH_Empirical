## Diagnose why s5_hr_influence (Eng) and s5_engineer_care (HR) have NAs.
## Three hypotheses:
##   (A) Survey skip-logic / branching (NA depends on prior answer).
##   (B) Stage-5 attrition (respondent didn't reach the s5 block).
##   (C) Plain non-response.

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

## firm_dt already has role-related vars; merge with suffix and pick the firm_long role.
role_map <- unique(firm_long_dt[, .(responseid, role_long = role)])
resp_dt  <- merge(firm_dt, role_map, by = "responseid", all.x = TRUE)
resp_dt[, role := role_long]

cat("\n========== Why is s5_hr_influence NA among Engineers? ==========\n")
e <- resp_dt[role == "Eng"]
cat(sprintf("Total Engineers: %d\n", nrow(e)))
cat(sprintf("NA on s5_hr_influence: %d (%.1f%%)\n",
            sum(is.na(e$s5_hr_influence)), 100 * mean(is.na(e$s5_hr_influence))))

cat("\n--- Are NA-engineers also NA on other s5 vars (= didn't reach stage 5)? ---\n")
other_s5 <- c("s5_view_align", "s5_view_dei_race", "s5_view_dei_gender",
              "s5_view_diverse", "s5_view_hiring_dei", "s5_policy_dei",
              "s5_engineer_care", "s5_ats", "s5_bh")
other_s5 <- intersect(other_s5, names(e))
cat("Mean missingness on other s5 vars by s5_hr_influence NA-status:\n")
e[, has_hri := !is.na(s5_hr_influence)]
miss_summary <- e[, lapply(.SD, function(x) mean(is.na(x))),
                  .SDcols = other_s5, by = has_hri]
print(miss_summary)

cat("\n--- Cross-tab: s5_hr_influence NA-status vs s5_view_align ---\n")
print(table(s5_view_align = e$s5_view_align,
            hri_NA = is.na(e$s5_hr_influence), useNA = "always"))

cat("\n========== Why is s5_engineer_care NA among HR? ==========\n")
h <- resp_dt[role == "HR"]
cat(sprintf("Total HR: %d\n", nrow(h)))
cat(sprintf("NA on s5_engineer_care: %d (%.1f%%)\n",
            sum(is.na(h$s5_engineer_care)), 100 * mean(is.na(h$s5_engineer_care))))

cat("\n--- Are NA-HRs also NA on other s5 vars? ---\n")
h[, has_ec := !is.na(s5_engineer_care)]
miss_summary_h <- h[, lapply(.SD, function(x) mean(is.na(x))),
                    .SDcols = other_s5, by = has_ec]
print(miss_summary_h)

cat("\n--- Cross-tab: s5_engineer_care NA-status vs s5_view_align ---\n")
print(table(s5_view_align = h$s5_view_align,
            ec_NA = is.na(h$s5_engineer_care), useNA = "always"))

cat("\n========== Bottom line ==========\n")
cat("If 'has_*' = FALSE rows show ~100% missingness on OTHER s5 vars,\n")
cat("  the NAs are stage-5 attrition (respondent dropped before reaching s5).\n")
cat("If they are populated on other s5 vars but NA only on s5_hr_influence /\n")
cat("  s5_engineer_care, the NAs likely reflect survey skip-logic or item refusal.\n")
cat("\n--- Done ---\n")
