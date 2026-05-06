## Univariate validity of each of the 4 lab components for predicting
## true_coding_z. Resume-level (N=18); HC1 robust SE. Companion to §6.
rm(list = ls())
date <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
library(pacman)
p_load(haven, here, base, readr, data.table, stringr, labelled, writexl,
       tidyverse, dplyr, ggplot2, fixest, modelsummary)
options(tigris_use_cache = TRUE); setwd(here())
source(here("1_Codes", "2_Import.R")); source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R")); source(here("1_Codes", "5_Cleaning.R"))
source(here("1_Codes", "6_Hiring_Simulation.R"))

## true_coding_z is created inside §4.1 of the Rmd; replicate it here.
firm_long_dt[, true_coding_z := (overall_score - mean(overall_score, na.rm = TRUE)) /
                                  sd(overall_score, na.rm = TRUE)]

resume_dt <- unique(firm_long_dt[, .(resume_index, true_coding_z,
                                      test_case, readability,
                                      time_efficiency, space_efficiency)])
cat("\nN unique resumes:", nrow(resume_dt), "\n\n")

## Z-score each component within the 18-resume sample so slopes are
## directly comparable across components.
for (cmp in c("test_case", "readability", "time_efficiency", "space_efficiency")) {
  resume_dt[, (paste0(cmp, "_zc")) := as.numeric(scale(get(cmp)))]
}

cat("--- LHS = true_coding_z, RHS = each component (resume-z-scored), univariate ---\n\n")
out <- rbindlist(lapply(c("test_case_zc", "readability_zc",
                          "time_efficiency_zc", "space_efficiency_zc"), function(cmp) {
  m <- feols(as.formula(paste0("true_coding_z ~ ", cmp)),
             data = resume_dt, vcov = "hetero")
  ct <- coeftable(m)
  data.table(
    component = sub("_zc$", "", cmp),
    estimate  = ct[cmp, "Estimate"],
    se        = ct[cmp, "Std. Error"],
    p_value   = ct[cmp, 4],
    r_squared = fitstat(m, "r2", verbose = FALSE)$r2,
    nobs      = nobs(m)
  )
}))
print(out)

cat("\n--- Same regressions but RHS = raw (unstandardized) component ---\n\n")
out_raw <- rbindlist(lapply(c("test_case", "readability",
                              "time_efficiency", "space_efficiency"), function(cmp) {
  m <- feols(as.formula(paste0("true_coding_z ~ ", cmp)),
             data = resume_dt, vcov = "hetero")
  ct <- coeftable(m)
  data.table(
    component = cmp,
    estimate  = ct[cmp, "Estimate"],
    se        = ct[cmp, "Std. Error"],
    p_value   = ct[cmp, 4],
    r_squared = fitstat(m, "r2", verbose = FALSE)$r2,
    nobs      = nobs(m)
  )
}))
print(out_raw)
