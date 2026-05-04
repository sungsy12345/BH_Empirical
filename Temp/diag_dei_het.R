library(pacman)
p_load(haven, here, base, readr, data.table, stringr,
       tidyverse, dplyr, fixest, labelled)
setwd(here())
date <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
source(here("1_Codes", "2_Import.R"))
source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R"))
source(here("1_Codes", "5_Cleaning.R"))
source(here("1_Codes", "6_Hiring_Simulation.R"))

cat("dei_index_z in firm_long_dt? ", "dei_index_z" %in% names(firm_long_dt), "\n")
cat("hr_pass5 in firm_long_dt?     ", "hr_pass5" %in% names(firm_long_dt), "\n")

hr_data <- firm_long_dt[role == "HR" & hr_pass5 == 1]
cat("\nHR k=5 sample size:", nrow(hr_data), "\n")
cat("dei_index_z range:", range(hr_data$dei_index_z, na.rm = TRUE), "\n")
cat("non-NA dei_index_z:", sum(!is.na(hr_data$dei_index_z)), "/", nrow(hr_data), "\n")

ctrl_mean <- hr_data[treat == "nonblind", mean(overall_score, na.rm = TRUE)]
ctrl_sd   <- hr_data[treat == "nonblind", sd(overall_score, na.rm = TRUE)]
hr_data[, overall_score_std := (overall_score - ctrl_mean) / ctrl_sd]

m <- tryCatch(feols(
  overall_score_std ~ i(treat, ref = "nonblind") * dei_index_z + resp_age |
    r_do + q_version + resp_gender + resp_race + s2_educ + s2_income,
  data = hr_data, vcov = ~ responseid
), error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL })

if (!is.null(m)) {
  cat("\nCoef names:\n")
  print(names(coef(m)))
  cat("\nCoefs:\n")
  print(coef(m))
}
