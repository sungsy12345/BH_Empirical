## Check at what level `q_version` varies:
##  (a) does it vary WITHIN respondent (i.e., across the 18 resumes a single
##      respondent rates)?  -> resume-level
##  (b) does it vary WITHIN resume_index (i.e., across respondents who saw
##      the same resume)?    -> respondent-level
rm(list = ls())
date <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
library(pacman)
p_load(haven, here, base, readr, data.table, stringr, fixest)
options(tigris_use_cache = TRUE); setwd(here())
source(here("1_Codes", "2_Import.R")); source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R")); source(here("1_Codes", "5_Cleaning.R"))
source(here("1_Codes", "6_Hiring_Simulation.R"))

cat("\n--- # unique q_version values WITHIN each respondent ---\n")
within_resp <- firm_long_dt[, .(n_qv_within_resp = uniqueN(q_version)), by = responseid]
cat(sprintf("  min: %d   median: %d   max: %d   #respondents: %d\n",
            min(within_resp$n_qv_within_resp),
            median(within_resp$n_qv_within_resp),
            max(within_resp$n_qv_within_resp),
            nrow(within_resp)))
cat("  table of (n unique q_versions per respondent):\n")
print(within_resp[, .N, by = n_qv_within_resp])

cat("\n--- # unique q_version values WITHIN each resume_index ---\n")
within_ridx <- firm_long_dt[, .(n_qv_within_ridx = uniqueN(q_version)), by = resume_index]
print(within_ridx[order(resume_index)])

cat("\n--- crosstab: resume_index x q_version (count of obs) ---\n")
print(dcast(firm_long_dt, resume_index ~ q_version, value.var = "responseid",
            fun.aggregate = length))

cat("\n--- show actual q_version values per resume_index (any role) ---\n")
print(unique(firm_long_dt[, .(resume_index, q_version)])[order(resume_index, q_version)])
