## Layer 1 + Layer 2 anchoring diagnostic.
##  Q: do prior resumes (lag1, lag2) anchor evaluators' rating of the current
##     resume?  If yes, are §1.1 race-gender effects robust to lag controls?
##  Spec: build lag1/lag2 of objective ability (overall_score), prior own
##  rating (sc_overall_z), and prior race_gender. Test for anchoring; then
##  re-estimate §1.1 with lag controls.
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

setorder(firm_long_dt, responseid, r_do)
firm_long_dt[, lag1_score    := shift(overall_score,  1, type = "lag"), by = responseid]
firm_long_dt[, lag2_score    := shift(overall_score,  2, type = "lag"), by = responseid]
firm_long_dt[, lag1_own_rate := shift(sc_overall_z,   1, type = "lag"), by = responseid]
firm_long_dt[, lag2_own_rate := shift(sc_overall_z,   2, type = "lag"), by = responseid]
firm_long_dt[, lag1_rg       := shift(as.character(race_gender), 1, type = "lag"), by = responseid]
firm_long_dt[, lag2_rg       := shift(as.character(race_gender), 2, type = "lag"), by = responseid]
firm_long_dt[, lag1_score_z  := as.numeric(scale(lag1_score))]
firm_long_dt[, lag2_score_z  := as.numeric(scale(lag2_score))]

cat(sprintf("\nN total firm_long_dt rows: %d\n", nrow(firm_long_dt)))
cat(sprintf("N rows with lag1 non-NA:   %d\n", sum(!is.na(firm_long_dt$lag1_score))))
cat(sprintf("N rows with lag2 non-NA:   %d\n", sum(!is.na(firm_long_dt$lag2_score))))

cat("\n##############################################################\n")
cat("## LAYER 1A. Anchoring on PRIOR OBJECTIVE QUALITY (overall_score)\n")
cat("##############################################################\n")
cat("Spec: sc_overall_z ~ lag1_score_z + lag2_score_z + resp_age | resume_index + r_do + resp demographic FEs.\n")
cat("If lag terms are 0: no anchoring on objective prior quality.\n")
cat("Positive: assimilation (high prior → high current). Negative: contrast.\n\n")

spec_anchor <- function(role_lab, dv, rhs) {
  fe_extra <- if (role_lab == "Eng") " + resp_python + resp_java" else ""
  d <- firm_long_dt[role == role_lab & !is.na(lag1_score) & !is.na(lag2_score)]
  fml <- as.formula(sprintf("%s ~ %s + resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income%s",
                             dv, rhs, fe_extra))
  feols(fml, data = d, vcov = ~ responseid)
}
report_coefs <- function(label, m, terms) {
  ct <- coeftable(m)
  cat(sprintf("\n[%s, N=%d]\n", label, m$nobs))
  for (t in terms) {
    if (t %in% rownames(ct)) {
      r <- ct[t, ]
      stars <- fcase(r[4] < 0.01, "***", r[4] < 0.05, "**", r[4] < 0.10, "*", default = "")
      cat(sprintf("  %-20s = %+8.4f%s   (SE %.4f, p %.4f)\n", t, r[1], stars, r[2], r[4]))
    }
  }
}
for (rl in c("HR", "Eng")) {
  m <- spec_anchor(rl, "sc_overall_z", "lag1_score_z + lag2_score_z")
  report_coefs(sprintf("Anchor on prior objective quality, %s", rl), m,
               c("lag1_score_z", "lag2_score_z"))
}

cat("\n##############################################################\n")
cat("## LAYER 1B. Anchoring on EVALUATOR'S OWN PRIOR RATING\n")
cat("##############################################################\n")
cat("Spec: sc_overall_z ~ lag1_own_rate + lag2_own_rate + resp_age | <FEs>.\n\n")

for (rl in c("HR", "Eng")) {
  m <- spec_anchor(rl, "sc_overall_z", "lag1_own_rate + lag2_own_rate")
  report_coefs(sprintf("Anchor on own prior rating, %s", rl), m,
               c("lag1_own_rate", "lag2_own_rate"))
}

cat("\n##############################################################\n")
cat("## LAYER 1C. Anchoring on PRIOR DEMOGRAPHIC (lag1 race_gender)\n")
cat("##############################################################\n")
cat("Spec: sc_overall_z ~ i(lag1_rg, ref='Blind') + resp_age | <FEs>.\n")
cat("Restrict to nonblind respondents (under blind, lag1_rg = Blind always so no variation).\n\n")

spec_anchor_rg <- function(role_lab) {
  fe_extra <- if (role_lab == "Eng") " + resp_python + resp_java" else ""
  d <- firm_long_dt[role == role_lab & !is.na(lag1_rg) & treat == "nonblind"]
  d <- copy(d)
  d[, lag1_rg := factor(lag1_rg, levels = c("White_Male","White_Female","Asian_Male","Asian_Female","Hispanic_Male","Hispanic_Female"))]
  fml <- as.formula(sprintf("sc_overall_z ~ i(lag1_rg, ref='White_Male') + resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income%s",
                             fe_extra))
  feols(fml, data = d, vcov = ~ responseid)
}
for (rl in c("HR", "Eng")) {
  m <- spec_anchor_rg(rl)
  cat(sprintf("\n[%s, N=%d]  (joint test of lag1_rg levels follows)\n", rl, m$nobs))
  ct <- coeftable(m)
  rg_rows <- grep("^lag1_rg::", rownames(ct), value = TRUE)
  for (t in rg_rows) {
    r <- ct[t, ]; stars <- fcase(r[4] < 0.01, "***", r[4] < 0.05, "**", r[4] < 0.10, "*", default = "")
    cat(sprintf("  %-30s = %+7.4f%s   (SE %.4f, p %.4f)\n",
                sub("lag1_rg::", "", t), r[1], stars, r[2], r[4]))
  }
  ## Joint Wald
  if (length(rg_rows) > 0) {
    w <- tryCatch(wald(m, keep = "lag1_rg::"), error = function(e) NULL)
    if (!is.null(w)) cat(sprintf("  >>> Joint p (any lag1_rg ≠ 0): %.4f\n", w$p))
  }
}

cat("\n##############################################################\n")
cat("## LAYER 2. Robustness: §1.1 race_gender ATE with lag controls\n")
cat("##############################################################\n")
cat("Compare baseline §1.1 to a spec that adds lag1_score_z + lag2_score_z\n")
cat("as continuous controls. If race_gender coefficients are unchanged,\n")
cat("§1.1 is not driven by anchoring on prior objective quality.\n\n")

flip_signs <- function(m) { m$coefficients <- -1 * m$coefficients; m }
extract_rg_te <- function(m) {
  ct <- coeftable(m)
  rg_rows <- grep("^race_gender::", rownames(ct), value = TRUE)
  data.table(group = sub("race_gender::", "", rg_rows),
             est   = -ct[rg_rows, "Estimate"],   # sign-flip post-fit
             se    =  ct[rg_rows, "Std. Error"],
             p     =  ct[rg_rows, 4])
}

run_baseline <- function(role_lab, with_lag) {
  fe_extra <- if (role_lab == "Eng") " + resp_python + resp_java" else ""
  rhs_base <- "i(race_gender, ref='Blind') + resp_age"
  rhs_full <- if (with_lag) paste0(rhs_base, " + lag1_score_z + lag2_score_z") else rhs_base
  d <- firm_long_dt[role == role_lab]
  if (with_lag) d <- d[!is.na(lag1_score) & !is.na(lag2_score)]
  fml <- as.formula(sprintf("sc_overall_z ~ %s | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income%s",
                             rhs_full, fe_extra))
  feols(fml, data = d, vcov = ~ responseid)
}

for (rl in c("HR", "Eng")) {
  cat(sprintf("\n##### %s #####\n", rl))
  m_base <- run_baseline(rl, with_lag = FALSE)
  m_lag  <- run_baseline(rl, with_lag = TRUE)
  base_te <- extract_rg_te(m_base)
  lag_te  <- extract_rg_te(m_lag)
  out <- merge(base_te[, .(group, base_est = round(est, 4), base_p = round(p, 3))],
               lag_te[,  .(group, lag_est  = round(est, 4), lag_p  = round(p, 3))],
               by = "group")
  out[, delta := round(lag_est - base_est, 4)]
  out[, group := factor(group, levels = c("White_Male","White_Female","Asian_Male","Asian_Female","Hispanic_Male","Hispanic_Female"))]
  setorder(out, group)
  print(out)
  cat(sprintf("  N (baseline) = %d   |   N (with lag) = %d\n", m_base$nobs, m_lag$nobs))
}
