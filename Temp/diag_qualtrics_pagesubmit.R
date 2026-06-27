## Compare Qualtrics native page-submit time vs TaskMaster time-on-page,
## as the dependent variable in the §2.1 effort regression.
date <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
suppressPackageStartupMessages({
  library(pacman)
  p_load(haven, here, data.table, fixest, readr, dplyr, tidyverse, stringr,
         labelled, splitstackshape, lubridate)
})
setwd(here())
source(here("1_Codes", "2_Import.R"))
source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R"))
source(here("1_Codes", "5_Cleaning.R"))
source(here("1_Codes", "6_Hiring_Simulation.R"))

## Build a long version of the 18 res<i>_timer_pagesubmit columns and
## join to firm_long_dt on (responseid, r_do).
ps_cols <- paste0("res", 1:18, "_timer_pagesubmit")
ps_long <- melt(firm_dt[, c("responseid", ps_cols), with = FALSE],
                id.vars = "responseid",
                measure.vars = ps_cols,
                variable.name = "raw", value.name = "qualtrics_pagesubmit")
ps_long[, r_do := as.integer(sub("res([0-9]+)_timer_pagesubmit", "\\1", raw))]
ps_long[, raw := NULL]

dt <- merge(firm_long_dt, ps_long, by = c("responseid", "r_do"), all.x = TRUE)

cat(sprintf("\nN rows in firm_long_dt: %d\n", nrow(firm_long_dt)))
cat(sprintf("N rows w/ qualtrics_pagesubmit non-NA: %d\n",
            sum(!is.na(dt$qualtrics_pagesubmit))))
cat("\nDistribution of qualtrics_pagesubmit (sec) by treat x role:\n")
print(dt[, .(n   = .N,
             p10 = round(quantile(qualtrics_pagesubmit, 0.10, na.rm=TRUE), 1),
             med = round(median(qualtrics_pagesubmit, na.rm=TRUE), 1),
             mean= round(mean(qualtrics_pagesubmit, na.rm=TRUE), 1),
             p90 = round(quantile(qualtrics_pagesubmit, 0.90, na.rm=TRUE), 1),
             p99 = round(quantile(qualtrics_pagesubmit, 0.99, na.rm=TRUE), 1)),
         by = .(role, treat)][order(role, treat)])

## Specification mirror of §2.1: treat | resume_index + r_do +
## resp_gender + resp_race + s2_educ + s2_income (+ resp_python + resp_java for Eng).
cat("\n========== Effect of blinding on Qualtrics page-submit time (sec) ==========\n")
spec_hr <- function(d, dv) {
  feols(as.formula(sprintf("%s ~ i(treat, ref='nonblind') + resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income", dv)),
        data = d, vcov = ~ responseid)
}
spec_eng <- function(d, dv) {
  feols(as.formula(sprintf("%s ~ i(treat, ref='nonblind') + resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income + resp_python + resp_java", dv)),
        data = d, vcov = ~ responseid)
}

run_one <- function(label, m) {
  ct <- coeftable(m)["treat::blind", ]
  cat(sprintf("  %-50s  est = %+8.3f   se = %.3f   p = %.4f   N = %d\n",
              label, ct[1], ct[2], ct[4], m$nobs))
}

cat("\n[Qualtrics page-submit, raw]\n")
run_one("HR  - qualtrics_pagesubmit", spec_hr(dt[role == "HR"  & !is.na(qualtrics_pagesubmit)], "qualtrics_pagesubmit"))
run_one("Eng - qualtrics_pagesubmit", spec_eng(dt[role == "Eng" & !is.na(qualtrics_pagesubmit)], "qualtrics_pagesubmit"))

cat("\n[TaskMaster timeonpage, raw - for comparison]\n")
run_one("HR  - timeonpage  ", spec_hr(firm_long_dt[role == "HR"  & !is.na(timeonpage)],  "timeonpage"))
run_one("Eng - timeonpage  ", spec_eng(firm_long_dt[role == "Eng" & !is.na(timeonpage)], "timeonpage"))

cat("\n[TaskMaster timeoffpage, raw - for comparison]\n")
run_one("HR  - timeoffpage ", spec_hr(firm_long_dt[role == "HR"  & !is.na(timeoffpage)], "timeoffpage"))
run_one("Eng - timeoffpage ", spec_eng(firm_long_dt[role == "Eng" & !is.na(timeoffpage)], "timeoffpage"))

cat("\n[Qualtrics pagesubmit, log-transformed (winsor at 1st/99th pct)]\n")
qclip <- dt[!is.na(qualtrics_pagesubmit), quantile(qualtrics_pagesubmit, c(0.01, 0.99))]
dt[, qps_w := pmin(pmax(qualtrics_pagesubmit, qclip[1]), qclip[2])]
dt[, log_qps := log(pmax(qps_w, 0.5))]
run_one("HR  - log(qps)    ", spec_hr(dt[role == "HR"  & !is.na(log_qps)], "log_qps"))
run_one("Eng - log(qps)    ", spec_eng(dt[role == "Eng" & !is.na(log_qps)], "log_qps"))

cat("\n[Correlation of qualtrics_pagesubmit vs (timeonpage + timeoffpage)]\n")
print(dt[!is.na(qualtrics_pagesubmit) & !is.na(timeonpage) & !is.na(timeoffpage),
         .(cor_with_taskmaster_total = round(cor(qualtrics_pagesubmit, timeonpage + timeoffpage), 3),
           cor_with_timeonpage_only  = round(cor(qualtrics_pagesubmit, timeonpage), 3),
           cor_with_timeoffpage_only = round(cor(qualtrics_pagesubmit, timeoffpage), 3),
           N = .N), by = role])
