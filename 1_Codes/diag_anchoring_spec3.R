## Spec 3: §1.1 robustness audit. Do the demographic ATEs shrink when we
## add the within-session lag controls (lag_same_z, lag_diff_z) from
## Spec 1?
##
## If shrinkage is meaningful, part of the §1.1 "demographic premium /
## penalty" is actually a within-sequence stereotype-updating effect, not
## a stable demographic prior.
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

dt <- copy(firm_long_dt)
dt[, true_rg := factor(as.character(true_race_gender),
  levels = c("White_Male","White_Female","Asian_Male","Asian_Female",
             "Hispanic_Male","Hispanic_Female"))]

setorder(dt, responseid, r_do)

build_lags <- function(d) {
  d[, lag_same_demo_sum   := 0]
  d[, lag_same_demo_count := 0L]
  d[, lag_diff_demo_sum   := 0]
  d[, lag_diff_demo_count := 0L]
  for (rid in unique(d$responseid)) {
    idx <- which(d$responseid == rid)
    if (length(idx) < 2) next
    rd  <- d$r_do[idx]
    rg  <- as.character(d$true_rg[idx])
    sc  <- d$overall_score[idx]
    ord <- order(rd)
    idx_o <- idx[ord]; rd_o <- rd[ord]; rg_o <- rg[ord]; sc_o <- sc[ord]
    same_sum <- numeric(length(idx_o)); same_cnt <- integer(length(idx_o))
    diff_sum <- numeric(length(idx_o)); diff_cnt <- integer(length(idx_o))
    for (j in seq_along(idx_o)) {
      if (j == 1) next
      prev <- seq_len(j - 1)
      same <- which(rg_o[prev] == rg_o[j])
      diff <- which(rg_o[prev] != rg_o[j])
      same_sum[j] <- sum(sc_o[prev[same]], na.rm = TRUE)
      same_cnt[j] <- sum(!is.na(sc_o[prev[same]]))
      diff_sum[j] <- sum(sc_o[prev[diff]], na.rm = TRUE)
      diff_cnt[j] <- sum(!is.na(sc_o[prev[diff]]))
    }
    d$lag_same_demo_sum[idx_o]   <- same_sum
    d$lag_same_demo_count[idx_o] <- same_cnt
    d$lag_diff_demo_sum[idx_o]   <- diff_sum
    d$lag_diff_demo_count[idx_o] <- diff_cnt
  }
  d[, lag_same_demo_avg_q := fifelse(lag_same_demo_count > 0,
                                      lag_same_demo_sum / lag_same_demo_count, NA_real_)]
  d[, lag_diff_demo_avg_q := fifelse(lag_diff_demo_count > 0,
                                      lag_diff_demo_sum / lag_diff_demo_count, NA_real_)]
  d
}
dt <- build_lags(dt)
dt[, lag_same_z := as.numeric(scale(lag_same_demo_avg_q))]
dt[, lag_diff_z := as.numeric(scale(lag_diff_demo_avg_q))]

## Match §1.1's spec: i(race_gender, ref='Blind') + resp_age | resume_index +
## r_do + resp_gender + resp_race + s2_educ + s2_income (+ resp_python +
## resp_java for Eng).  Sign-flip post-fit so coef = effect of BLINDING
## for that demographic.
flip <- function(m) { m$coefficients <- -1 * m$coefficients; m }

fit_base <- function(role_lab, d) {
  fe_extra <- if (role_lab == "Eng") " + resp_python + resp_java" else ""
  feols(as.formula(sprintf(
    "sc_overall_z ~ i(race_gender, ref='Blind') + resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income%s",
    fe_extra)),
    data = d[role == role_lab], vcov = ~ responseid)
}
fit_lag <- function(role_lab, d) {
  fe_extra <- if (role_lab == "Eng") " + resp_python + resp_java" else ""
  feols(as.formula(sprintf(
    "sc_overall_z ~ i(race_gender, ref='Blind') + lag_same_z + lag_diff_z + resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income%s",
    fe_extra)),
    data = d[role == role_lab], vcov = ~ responseid)
}

extract_te <- function(m) {
  ct <- coeftable(m)
  rg_rows <- grep("^race_gender::", rownames(ct), value = TRUE)
  data.table(
    group = sub("race_gender::", "", rg_rows),
    est   = -ct[rg_rows, "Estimate"],
    se    =  ct[rg_rows, "Std. Error"],
    p     =  ct[rg_rows, 4]
  )
}

## Restrict to rows with non-NA lags so baseline and lag-controlled run on
## same sample; comparison is then about specification, not about sample.
dt_complete <- dt[!is.na(lag_same_z) & !is.na(lag_diff_z)]

cat(sprintf("\nN total: %d\n", nrow(dt)))
cat(sprintf("N with both lags non-NA: %d\n", nrow(dt_complete)))
cat(sprintf("  HR: %d, Eng: %d\n",
            nrow(dt_complete[role == "HR"]),  nrow(dt_complete[role == "Eng"])))

cat("\n##############################################################\n")
cat("## Demographic ATEs (sign-flipped: coef = effect of blinding\n")
cat("## for that demographic).  Same sample for base / lag specs.\n")
cat("##############################################################\n")

for (rl in c("HR", "Eng")) {
  m_base <- fit_base(rl, dt_complete)
  m_lag  <- fit_lag (rl, dt_complete)
  base_te <- extract_te(m_base)
  lag_te  <- extract_te(m_lag)
  out <- merge(base_te[, .(group, base_est = round(est, 4), base_p = round(p, 3))],
               lag_te[,  .(group, lag_est  = round(est, 4), lag_p  = round(p, 3))],
               by = "group")
  out[, delta := round(lag_est - base_est, 4)]
  out[, pct_change := sprintf("%+.0f%%", 100 * (lag_est - base_est) / base_est)]
  out[, group := factor(group, levels = c("White_Male","White_Female","Asian_Male","Asian_Female","Hispanic_Male","Hispanic_Female"))]
  setorder(out, group)
  cat(sprintf("\n##### %s   (N = %d) #####\n", rl, m_base$nobs))
  print(out)

  cat("\n  Lag-control coefficients in lag spec:\n")
  ct <- coeftable(m_lag)
  for (t in c("lag_same_z", "lag_diff_z")) {
    if (t %in% rownames(ct)) {
      r <- ct[t, ]
      stars <- fcase(r[4] < 0.01, "***", r[4] < 0.05, "**", r[4] < 0.10, "*", default = "")
      cat(sprintf("    %-12s = %+8.4f%s   (SE %.4f, p %.4f)\n", t, r[1], stars, r[2], r[4]))
    }
  }
}

cat("\n##############################################################\n")
cat("## Joint test: do all 6 race_gender ATEs jointly shrink?\n")
cat("##  Sum of squared coefficient changes (lag - base) across the 6 demographics.\n")
cat("##############################################################\n")
for (rl in c("HR", "Eng")) {
  m_base <- fit_base(rl, dt_complete)
  m_lag  <- fit_lag (rl, dt_complete)
  base_te <- extract_te(m_base)
  lag_te  <- extract_te(m_lag)
  cmb <- merge(base_te[, .(group, base = est)], lag_te[, .(group, lag = est)], by = "group")
  cmb[, sq_delta := (lag - base)^2]
  cat(sprintf("\n[%s]  Mean(|delta|) = %.4f   Sum_sq(delta) = %.4f\n",
              rl, mean(abs(cmb$lag - cmb$base)), sum(cmb$sq_delta)))
}
