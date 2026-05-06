## Quick check: at k = 16, 17, 18, does the §1.2 regression succeed for
## both HR and Eng? If LHS is constant at high k, fixest fails silently
## inside tryCatch and the plot will lose those points.
rm(list = ls())
date <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
library(pacman)
p_load(haven, here, base, readr, data.table, stringr, fixest)
options(tigris_use_cache = TRUE); setwd(here())
source(here("1_Codes", "2_Import.R")); source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R")); source(here("1_Codes", "5_Cleaning.R"))
source(here("1_Codes", "6_Hiring_Simulation.R"))

cat("\n--- LHS variance at high k for HR (hr_pass{k}) ---\n")
for (k in c(15, 16, 17, 18)) {
  col <- paste0("hr_pass", k)
  v <- firm_long_dt[role == "HR", get(col)]
  cat(sprintf("  hr_pass%-2d : mean = %.3f, var = %.6f, n = %d, n_unique = %d\n",
              k, mean(v, na.rm=TRUE), var(v, na.rm=TRUE), length(v), length(unique(v))))
}
cat("\n--- LHS variance at high k for Eng (eng_top{k}) ---\n")
for (k in c(15, 16, 17, 18)) {
  col <- paste0("eng_top", k)
  v <- firm_long_dt[role == "Eng", get(col)]
  cat(sprintf("  eng_top%-2d : mean = %.3f, var = %.6f, n = %d, n_unique = %d\n",
              k, mean(v, na.rm=TRUE), var(v, na.rm=TRUE), length(v), length(unique(v))))
}

cat("\n--- Try the §1.2 regression at k=18 for both roles ---\n")
for (rl in c("HR", "Eng")) {
  col <- if (rl == "HR") "hr_pass18" else "eng_top18"
  fe  <- if (rl == "HR")
    "q_version + resp_gender + resp_race + s2_educ + s2_income"
  else
    "q_version + resp_gender + resp_race + s2_educ + s2_income + resp_python + resp_java"
  fml <- as.formula(paste0(col, " ~ i(race_gender, ref='Blind') | ", fe))
  res <- tryCatch(
    feols(fml, data = firm_long_dt[role == rl], vcov = ~ responseid),
    error = function(e) paste("ERROR:", conditionMessage(e)))
  if (is.character(res)) {
    cat(sprintf("  %s k=18: %s\n", rl, res))
  } else {
    cat(sprintf("  %s k=18: fitted OK; nobs = %d, n_coef = %d\n", rl, nobs(res), length(coef(res))))
    print(coeftable(res))
  }
}
