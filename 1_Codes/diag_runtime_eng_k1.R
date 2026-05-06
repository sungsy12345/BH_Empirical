date <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
library(pacman)
p_load(haven, here, data.table, fixest, readr, dplyr, tidyverse, stringr, labelled, splitstackshape, lubridate)
setwd(here())
source(here("1_Codes", "2_Import.R")); source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R")); source(here("1_Codes", "5_Cleaning.R"))
source(here("1_Codes", "6_Hiring_Simulation.R"))

d <- firm_long_dt[role == "Eng" & eng_top1 == 1 & !is.na(runtime)]
ctrl_mean <- d[treat == "nonblind", mean(runtime, na.rm = TRUE)]
ctrl_sd   <- d[treat == "nonblind", sd(runtime,   na.rm = TRUE)]
d[, std_outcome := (runtime - ctrl_mean) / ctrl_sd]
m <- feols(std_outcome ~ i(treat, ref = "nonblind") + resp_age |
             q_version + resp_gender + resp_race + s2_educ + s2_income +
             resp_python + resp_java,
           data = d, vcov = ~ responseid)
ct <- coeftable(m)
est <- ct["treat::blind","Estimate"]; se <- ct["treat::blind","Std. Error"]; p <- ct["treat::blind",4]
cat("\n>>> Eng k=1 RUNTIME blinding effect (in SD units, std relative to nonblind):\n")
cat(sprintf(">>>  est = %+.4f, se = %.4f, p = %.4f\n", est, se, p))
cat(sprintf(">>>  ci_low = %+.4f, ci_high = %+.4f\n", est-1.96*se, est+1.96*se))
cat(sprintf(">>>  current y-axis cap = +/- 0.6\n"))
cat(sprintf(">>>  est outside cap?     %s\n", ifelse(abs(est) > 0.6, "YES", "no")))
cat(sprintf(">>>  ci_high outside cap? %s\n", ifelse(est+1.96*se > 0.6, "YES", "no")))
cat(sprintf(">>>  ci_low  outside cap? %s\n", ifelse(est-1.96*se < -0.6, "YES", "no")))
