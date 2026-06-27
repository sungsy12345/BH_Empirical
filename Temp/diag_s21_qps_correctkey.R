## Re-run §2.1 with the CORRECT keying:  res<i>_timer_pagesubmit corresponds
## to resume_index = i (which content), not r_do (display position). The
## wide -> long melt at 5_Cleaning.R:425-468 confirms this: every res<i>_*
## column maps to resume_index = i in firm_long_dt.
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
ps_long[, resume_index := as.factor(as.integer(sub("res([0-9]+)_timer_pagesubmit", "\\1", raw)))]
ps_long[, raw := NULL]

dt <- merge(firm_long_dt, ps_long, by = c("responseid", "resume_index"), all.x = TRUE)
dt[, taskmaster_total := timeonpage + timeoffpage]

cat("\nN rows in firm_long_dt: ", nrow(firm_long_dt), "\n", sep="")
cat("N rows after merge:     ", nrow(dt), "\n", sep="")
cat("N rows w/ qps non-NA:   ", sum(!is.na(dt$qps)), "\n\n", sep="")

cat("##### Sanity check: cor(qps, taskmaster_total) by role -- under correct key, should be much higher than 0.025 ####\n")
print(dt[!is.na(qps) & !is.na(taskmaster_total),
         .(cor_total = round(cor(qps, taskmaster_total), 3),
           cor_ton   = round(cor(qps, timeonpage), 3),
           cor_toff  = round(cor(qps, timeoffpage), 3),
           N = .N), by = role])

cat("\n##### Distribution check by treat ####\n")
print(dt[!is.na(qps), .(N=.N, p10=round(quantile(qps,0.10),1), p50=round(median(qps),1), mean=round(mean(qps),1), p90=round(quantile(qps,0.90),1), p99=round(quantile(qps,0.99),1)), by=.(role, treat)][order(role,treat)])

dt[, ok_qps := !is.na(qps) & !(qps > 600 & taskmaster_total < 60)]

spec_hr  <- function(d, dv) feols(as.formula(sprintf("%s ~ i(treat, ref='nonblind') + resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income", dv)), data = d, vcov = ~ responseid)
spec_eng <- function(d, dv) feols(as.formula(sprintf("%s ~ i(treat, ref='nonblind') + resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income + resp_python + resp_java", dv)), data = d, vcov = ~ responseid)

run_one <- function(label, m) {
  ct <- coeftable(m)["treat::blind", ]
  cat(sprintf("  %-50s  est = %+9.3f   se = %.3f   p = %.4f   N = %d\n",
              label, ct[1], ct[2], ct[4], m$nobs))
}

cat("\n##### Main effect of blinding on Qualtrics PageSubmit (correct key) #####\n")
cat("[raw qps]\n")
run_one("HR  - qps  (cleaned)", spec_hr (dt[role == "HR"  & ok_qps], "qps"))
run_one("Eng - qps  (cleaned)", spec_eng(dt[role == "Eng" & ok_qps], "qps"))
cat("[log qps]\n")
dt[, log_qps := log(pmax(qps, 1))]
run_one("HR  - log(qps)  (cleaned)", spec_hr (dt[role == "HR"  & ok_qps], "log_qps"))
run_one("Eng - log(qps)  (cleaned)", spec_eng(dt[role == "Eng" & ok_qps], "log_qps"))

cat("\n##### Quality-heterogeneity (Blind x quality_z) - correct key #####\n")
dt[, gpa_z       := as.numeric(scale(gpa))]
dt[, test_case_z := as.numeric(scale(test_case))]

fit_int <- function(d, dv, qcol, role) {
  d <- copy(d)
  d[, blind := as.integer(treat == "blind")]
  d[, q_int := blind * d[[qcol]]]
  fe_extra <- if (role == "Eng") " + resp_python + resp_java" else ""
  fml <- as.formula(sprintf(
    "%s ~ blind + q_int + resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income%s",
    dv, fe_extra))
  feols(fml, data = d, vcov = ~ responseid)
}
extract_te <- function(m, qpts = c(-1, 0, 1)) {
  cf <- coef(m); V <- vcov(m); np <- length(cf)
  rows <- list()
  for (q in qpts) {
    v <- numeric(np)
    if ("blind" %in% names(cf)) v[match("blind", names(cf))] <- 1
    if ("q_int" %in% names(cf)) v[match("q_int", names(cf))] <- q
    est <- sum(v * cf); se <- sqrt(c(v %*% V %*% v))
    p   <- 2*pnorm(-abs(est/se))
    stars <- fcase(p < 0.01, "***", p < 0.05, "**", p < 0.10, "*", default = "")
    rows[[length(rows)+1]] <- data.table(q = q, est = round(est, 3), se = round(se, 3), p = round(p, 4), stars = stars)
  }
  rbindlist(rows)
}

run_block <- function(label, dv, qcol, ok_col) {
  cat(sprintf("\n----- %s  ~  Blind x %s -----\n", label, qcol))
  for (rl in c("HR", "Eng")) {
    d <- dt[role == rl & dt[[ok_col]] & !is.na(dt[[qcol]])]
    m <- fit_int(d, dv, qcol, rl)
    cat(sprintf("  [%s, N=%d]  Main(blind)=%.3f   Interaction(blind x %s)=%.3f\n",
                rl, m$nobs, coef(m)["blind"], qcol, coef(m)["q_int"]))
    out <- extract_te(m); out[, tier := c("Low (-1 SD)", "Mid (0)", "Top (+1 SD)")]
    print(out[, .(tier, est, se, p, stars)])
  }
}

run_block("qps", "qps", "gpa_z", "ok_qps")
cat("\n--- (Eng-relevant signal: test_case = coding ability) ---\n")
dt_eng <- dt[role == "Eng" & !is.na(test_case_z) & ok_qps]
m_eng_tc <- fit_int(dt_eng, "qps", "test_case_z", "Eng")
cat(sprintf("  [Eng, N=%d]  Main(blind)=%.3f   Interaction(blind x test_case_z)=%.3f\n",
            m_eng_tc$nobs, coef(m_eng_tc)["blind"], coef(m_eng_tc)["q_int"]))
out <- extract_te(m_eng_tc); out[, tier := c("Low (-1 SD)", "Mid (0)", "Top (+1 SD)")]
print(out[, .(tier, est, se, p, stars)])
