## §2.1 candidate-quality heterogeneity in the effect of blinding on time-spent.
## Hypothesis: low-ability resumes are dismissed quickly under either treatment;
## the blinding effect on time may bite only at higher ability tiers.
## Spec: DV ~ i(treat, ref='nonblind') * quality_z + resp_age | <standard FEs>
## Marginal effect of blinding at quality_z = -1, 0, +1 (Low / Mid / Top).
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

## Build qps and cleaning flags.
ps_cols <- paste0("res", 1:18, "_timer_pagesubmit")
ps_long <- melt(firm_dt[, c("responseid", ps_cols), with = FALSE],
                id.vars = "responseid",
                measure.vars = ps_cols,
                variable.name = "raw", value.name = "qps")
ps_long[, r_do := as.integer(sub("res([0-9]+)_timer_pagesubmit", "\\1", raw))]
ps_long[, raw := NULL]
dt <- merge(firm_long_dt, ps_long, by = c("responseid", "r_do"), all.x = TRUE)
dt[, taskmaster_total := timeonpage + timeoffpage]
dt[, ok_ton  := !is.na(timeonpage)  & timeonpage  <= 600]
dt[, ok_toff := !is.na(timeoffpage) & timeoffpage <= 3600]
dt[, ok_qps  := !is.na(qps) & !(qps > 600 & taskmaster_total < 60)]
dt[, gpa_z       := as.numeric(scale(gpa))]
dt[, test_case_z := as.numeric(scale(test_case))]

## Estimate marginal effect of treat at q = -1, 0, +1.  We construct the
## interaction term EXPLICITLY (blind_x_q = 1{blind} * q) because gpa_z /
## test_case_z are absorbed by resume_index FE, and the formula `treat * q`
## drops the entire treat:q term when fixest drops the main-effect q.
fit_int <- function(d, dv, qcol, role) {
  d <- copy(d)
  d[, blind   := as.integer(treat == "blind")]
  d[, q_int   := blind * d[[qcol]]]
  fe_extra <- if (role == "Eng") " + resp_python + resp_java" else ""
  fml <- as.formula(sprintf(
    "%s ~ blind + q_int + resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income%s",
    dv, fe_extra))
  feols(fml, data = d, vcov = ~ responseid)
}

extract_te <- function(m, qpts = c(-1, 0, 1)) {
  cf <- coef(m); V <- vcov(m); np <- length(cf)
  main_nm <- "blind"
  int_nm  <- "q_int"
  rows <- list()
  for (q in qpts) {
    v <- numeric(np)
    if (main_nm %in% names(cf)) v[match(main_nm, names(cf))] <- 1
    if (int_nm  %in% names(cf)) v[match(int_nm,  names(cf))] <- q
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
    out <- extract_te(m)
    out[, tier := c("Low (-1 SD)", "Mid (0)", "Top (+1 SD)")]
    print(out[, .(tier, est, se, p, stars)])
  }
}

cat("\n##### CANDIDATE-QUALITY HETEROGENEITY IN EFFORT EFFECT #####\n")
cat("Each block:  effect of blinding on the time outcome, at Low / Mid / Top quality\n")

run_block("timeonpage",  "timeonpage",  "gpa_z",       "ok_ton")
run_block("timeoffpage", "timeoffpage", "gpa_z",       "ok_toff")
run_block("qps (Qualtrics PageSubmit)", "qps", "gpa_z", "ok_qps")

cat("\n--- (Eng-relevant signal: test_case = coding ability) ---\n")
dt_eng <- dt[role == "Eng" & !is.na(test_case_z)]
for (dv_pair in list(c("timeonpage","ok_ton"), c("timeoffpage","ok_toff"), c("qps","ok_qps"))) {
  dv <- dv_pair[1]; ok <- dv_pair[2]
  cat(sprintf("\n----- Eng: %s  ~  Blind x test_case_z -----\n", dv))
  d  <- dt_eng[dt_eng[[ok]] == TRUE]
  m  <- fit_int(d, dv, "test_case_z", "Eng")
  cat(sprintf("  [N=%d]  Main(blind)=%.3f   Interaction(blind x test_case_z)=%.3f\n",
              m$nobs, coef(m)["blind"], coef(m)["q_int"]))
  out <- extract_te(m)
  out[, tier := c("Low (-1 SD)", "Mid (0)", "Top (+1 SD)")]
  print(out[, .(tier, est, se, p, stars)])
}
