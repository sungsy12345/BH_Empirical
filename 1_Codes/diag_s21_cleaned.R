## §2.1 with outlier cleaning applied per the agreed rules.
##   timeonpage:   drop obs where timeonpage  > 600s   (10 min)
##   timeoffpage:  drop obs where timeoffpage > 3600s  (1 h)
##   qualtrics qps: drop obs where qps > 600s AND TaskMaster total < 60s
##                  (i.e., gross page-submit time looks long but TaskMaster
##                   recorded essentially no browser activity = walked away)
date <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
suppressPackageStartupMessages({
  library(pacman)
  p_load(haven, here, data.table, fixest, readr, dplyr, tidyverse, stringr,
         labelled, splitstackshape, lubridate)
})
setwd(here())
source(here("1_Codes","2_Import.R"))
source(here("1_Codes","3_Firm_Cleaning.R"))
source(here("1_Codes","4_Distribution_Cleaning.R"))
source(here("1_Codes","5_Cleaning.R"))
source(here("1_Codes","6_Hiring_Simulation.R"))

ps_cols <- paste0("res", 1:18, "_timer_pagesubmit")
ps_long <- melt(firm_dt[, c("responseid", ps_cols), with = FALSE],
                id.vars = "responseid",
                measure.vars = ps_cols,
                variable.name = "raw", value.name = "qps")
ps_long[, r_do := as.integer(sub("res([0-9]+)_timer_pagesubmit", "\\1", raw))]
ps_long[, raw := NULL]
dt <- merge(firm_long_dt, ps_long, by = c("responseid", "r_do"), all.x = TRUE)
dt[, taskmaster_total := timeonpage + timeoffpage]

## Cleaning flags (per-outcome).
dt[, ok_ton := !is.na(timeonpage)  & timeonpage  <= 600]
dt[, ok_toff:= !is.na(timeoffpage) & timeoffpage <= 3600]
dt[, ok_qps := !is.na(qps) & !(qps > 600 & taskmaster_total < 60)]

cat("\n##### Cleaning impact (per-outcome row drops) #####\n")
report <- function(label, ok) {
  total <- nrow(dt)
  k <- sum(!ok)
  cat(sprintf("  %-25s  drop %4d / %d  (%.2f%%)\n", label, k, total, 100*k/total))
}
report("timeonpage  > 600s",  dt$ok_ton)
report("timeoffpage > 3600s", dt$ok_toff)
report("qps > 600 & tm < 60", dt$ok_qps)

cat("\n##### Specification: i(treat, ref='nonblind') + resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income (+ resp_python+resp_java for Eng).  SE clustered by responseid. #####\n")

spec_hr  <- function(d, dv) feols(as.formula(sprintf("%s ~ i(treat, ref='nonblind') + resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income", dv)), data = d, vcov = ~ responseid)
spec_eng <- function(d, dv) feols(as.formula(sprintf("%s ~ i(treat, ref='nonblind') + resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income + resp_python + resp_java", dv)), data = d, vcov = ~ responseid)

run_one <- function(label, m) {
  ct <- coeftable(m)["treat::blind", ]
  cat(sprintf("  %-55s  est = %+9.3f   se = %.3f   p = %.4f   N = %d\n",
              label, ct[1], ct[2], ct[4], m$nobs))
}

cat("\n[BEFORE cleaning - reference]\n")
run_one("HR  - timeonpage           ", spec_hr (firm_long_dt[role == "HR" ], "timeonpage"))
run_one("Eng - timeonpage           ", spec_eng(firm_long_dt[role == "Eng"], "timeonpage"))
run_one("HR  - timeoffpage          ", spec_hr (firm_long_dt[role == "HR" ], "timeoffpage"))
run_one("Eng - timeoffpage          ", spec_eng(firm_long_dt[role == "Eng"], "timeoffpage"))
run_one("HR  - qualtrics_pagesubmit ", spec_hr (dt[role == "HR" ], "qps"))
run_one("Eng - qualtrics_pagesubmit ", spec_eng(dt[role == "Eng"], "qps"))

cat("\n[AFTER cleaning - per-outcome filter]\n")
run_one("HR  - timeonpage  (cleaned)", spec_hr (dt[role == "HR"  & ok_ton ], "timeonpage"))
run_one("Eng - timeonpage  (cleaned)", spec_eng(dt[role == "Eng" & ok_ton ], "timeonpage"))
run_one("HR  - timeoffpage (cleaned)", spec_hr (dt[role == "HR"  & ok_toff], "timeoffpage"))
run_one("Eng - timeoffpage (cleaned)", spec_eng(dt[role == "Eng" & ok_toff], "timeoffpage"))
run_one("HR  - qps         (cleaned)", spec_hr (dt[role == "HR"  & ok_qps], "qps"))
run_one("Eng - qps         (cleaned)", spec_eng(dt[role == "Eng" & ok_qps], "qps"))

cat("\n[AFTER cleaning - log transform on cleaned data]\n")
dt[, log_ton  := log(pmax(timeonpage,  1))]
dt[, log_toff := log(pmax(timeoffpage, 1))]
dt[, log_qps  := log(pmax(qps,         1))]
run_one("HR  - log(timeonpage)  ", spec_hr (dt[role == "HR"  & ok_ton ], "log_ton"))
run_one("Eng - log(timeonpage)  ", spec_eng(dt[role == "Eng" & ok_ton ], "log_ton"))
run_one("HR  - log(timeoffpage) ", spec_hr (dt[role == "HR"  & ok_toff], "log_toff"))
run_one("Eng - log(timeoffpage) ", spec_eng(dt[role == "Eng" & ok_toff], "log_toff"))
run_one("HR  - log(qps)         ", spec_hr (dt[role == "HR"  & ok_qps], "log_qps"))
run_one("Eng - log(qps)         ", spec_eng(dt[role == "Eng" & ok_qps], "log_qps"))

cat("\n##### Distribution check after cleaning #####\n")
cat("[timeonpage cleaned]\n")
print(dt[ok_ton, .(N=.N, mean=round(mean(timeonpage),1), p50=round(median(timeonpage),1), p90=round(quantile(timeonpage,0.90),1), max=round(max(timeonpage),1)), by=.(role, treat)][order(role,treat)])
cat("[timeoffpage cleaned]\n")
print(dt[ok_toff, .(N=.N, mean=round(mean(timeoffpage),1), p50=round(median(timeoffpage),1), p90=round(quantile(timeoffpage,0.90),1), max=round(max(timeoffpage),1)), by=.(role, treat)][order(role,treat)])
cat("[qps cleaned]\n")
print(dt[ok_qps, .(N=.N, mean=round(mean(qps),1), p50=round(median(qps),1), p90=round(quantile(qps,0.90),1), max=round(max(qps),1)), by=.(role, treat)][order(role,treat)])
