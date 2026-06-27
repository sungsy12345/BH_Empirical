## Spec 1: within-demographic on-the-fly calibration.
## For each (responseid, r_do) row, compute the running mean of objective
## overall_score among prior candidates of the SAME demographic group
## (lag_same_demo_avg_q), and among DIFFERENT demographic groups
## (lag_diff_demo_avg_q, the placebo). Both based on TRUE assigned demo
## (true_race_gender), so the variable is well-defined for both arms.
##
## Headline test (nonblind only):
##   sc_overall_z ~ lag_same + lag_diff + i(true_rg) + ... | <FEs>
## Heterogeneity:
##   add i(true_rg, lag_same) interaction.
## Falsification 1 — placebo:
##   under nonblind, lag_diff coef should be << lag_same coef.
## Falsification 2 — blind sample:
##   under blind, lag_same coef should ≈ 0 (respondent doesn't see demo
##   so the running update can't go through demographic perception).
##
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

## Compute running same-demo and diff-demo averages of overall_score
## (objective coding score) for each (responseid, r_do).
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
    same_sum <- numeric(length(idx_o))
    same_cnt <- integer(length(idx_o))
    diff_sum <- numeric(length(idx_o))
    diff_cnt <- integer(length(idx_o))
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

cat(sprintf("\nN total: %d\n", nrow(dt)))
cat(sprintf("N with lag_same_demo_avg_q non-NA: %d (%.1f%%)\n",
            sum(!is.na(dt$lag_same_demo_avg_q)),
            100 * sum(!is.na(dt$lag_same_demo_avg_q)) / nrow(dt)))
cat(sprintf("N with lag_diff_demo_avg_q non-NA: %d (%.1f%%)\n",
            sum(!is.na(dt$lag_diff_demo_avg_q)),
            100 * sum(!is.na(dt$lag_diff_demo_avg_q)) / nrow(dt)))

## Standardize within sample so coefs are interpretable as "per +1 SD of
## the running same-demo mean".
dt[, lag_same_z := as.numeric(scale(lag_same_demo_avg_q))]
dt[, lag_diff_z := as.numeric(scale(lag_diff_demo_avg_q))]

##############################################################
## A. Nonblind sample, headline + placebo
##############################################################
cat("\n##############################################################\n")
cat("## A. Nonblind only: do prior same-demo candidates predict\n")
cat("##    current evaluation score?\n")
cat("##############################################################\n")
spec_main <- function(role_lab) {
  d <- dt[role == role_lab & treat == "nonblind" &
          !is.na(lag_same_z) & !is.na(lag_diff_z)]
  fe_extra <- if (role_lab == "Eng") " + resp_python + resp_java" else ""
  feols(as.formula(sprintf(
    "sc_overall_z ~ lag_same_z + lag_diff_z + i(true_rg, ref='White_Male') + resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income%s",
    fe_extra)),
    data = d, vcov = ~ responseid)
}

report <- function(label, m, terms) {
  ct <- coeftable(m)
  cat(sprintf("\n[%s, N=%d]\n", label, m$nobs))
  for (t in terms) {
    if (t %in% rownames(ct)) {
      r <- ct[t, ]
      stars <- fcase(r[4] < 0.01, "***", r[4] < 0.05, "**", r[4] < 0.10, "*", default = "")
      cat(sprintf("  %-35s = %+8.4f%s   (SE %.4f, p %.4f)\n",
                  t, r[1], stars, r[2], r[4]))
    }
  }
}

for (rl in c("HR", "Eng")) {
  m <- spec_main(rl)
  report(sprintf("Anchoring on prior demos, %s (nonblind)", rl), m,
         c("lag_same_z", "lag_diff_z"))
}

##############################################################
## B. Heterogeneity: race_gender x lag_same interaction
##############################################################
cat("\n##############################################################\n")
cat("## B. Within-group calibration heterogeneity:\n")
cat("##    does the calibration vary by which group is being scored?\n")
cat("##############################################################\n")
spec_het <- function(role_lab) {
  d <- dt[role == role_lab & treat == "nonblind" &
          !is.na(lag_same_z) & !is.na(lag_diff_z)]
  fe_extra <- if (role_lab == "Eng") " + resp_python + resp_java" else ""
  feols(as.formula(sprintf(
    "sc_overall_z ~ lag_same_z + lag_diff_z + i(true_rg, ref='White_Male') + i(true_rg, lag_same_z, ref='White_Male') + resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income%s",
    fe_extra)),
    data = d, vcov = ~ responseid)
}

for (rl in c("HR", "Eng")) {
  m <- spec_het(rl)
  ct <- coeftable(m)
  cat(sprintf("\n[%s, N=%d]\n  Marginal effect of +1 SD lag_same_demo, by demographic of CURRENT candidate:\n", rl, m$nobs))
  cf <- coef(m); V <- vcov(m); np <- length(cf)
  base_idx <- match("lag_same_z", names(cf))
  groups <- c("White_Male","White_Female","Asian_Male","Asian_Female","Hispanic_Male","Hispanic_Female")
  for (g in groups) {
    int_nm <- paste0("true_rg::", g, ":lag_same_z")
    v <- numeric(np); v[base_idx] <- 1
    if (int_nm %in% names(cf)) v[match(int_nm, names(cf))] <- 1
    est <- sum(v * cf); se <- sqrt(c(v %*% V %*% v))
    p   <- 2*pnorm(-abs(est/se))
    stars <- fcase(p < 0.01, "***", p < 0.05, "**", p < 0.10, "*", default = "")
    cat(sprintf("    %-18s = %+7.4f%s   (SE %.4f, p %.4f)\n",
                g, est, stars, se, p))
  }
}

##############################################################
## C. Falsification 2: blind sample - same spec
##############################################################
cat("\n##############################################################\n")
cat("## C. Falsification: under BLIND, respondent doesn't see demos\n")
cat("##    -> lag_same coef should be ~0.\n")
cat("##############################################################\n")
spec_blind <- function(role_lab) {
  d <- dt[role == role_lab & treat == "blind" &
          !is.na(lag_same_z) & !is.na(lag_diff_z)]
  fe_extra <- if (role_lab == "Eng") " + resp_python + resp_java" else ""
  feols(as.formula(sprintf(
    "sc_overall_z ~ lag_same_z + lag_diff_z + resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income%s",
    fe_extra)),
    data = d, vcov = ~ responseid)
}
for (rl in c("HR", "Eng")) {
  m <- spec_blind(rl)
  report(sprintf("Anchoring placebo (blind), %s", rl), m,
         c("lag_same_z", "lag_diff_z"))
}

##############################################################
## D. Pooled with Blind × lag_same interaction (test channel)
##############################################################
cat("\n##############################################################\n")
cat("## D. Pooled: does blinding turn off the anchoring channel?\n")
cat("##    coef on Blind × lag_same should be NEGATIVE if so.\n")
cat("##############################################################\n")
dt[, blind := as.integer(treat == "blind")]
spec_pooled <- function(role_lab) {
  d <- copy(dt[role == role_lab & !is.na(lag_same_z) & !is.na(lag_diff_z)])
  d[, lag_same_x_blind := lag_same_z * blind]
  d[, lag_diff_x_blind := lag_diff_z * blind]
  fe_extra <- if (role_lab == "Eng") " + resp_python + resp_java" else ""
  feols(as.formula(sprintf(
    "sc_overall_z ~ blind + lag_same_z + lag_diff_z + lag_same_x_blind + lag_diff_x_blind + resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income%s",
    fe_extra)),
    data = d, vcov = ~ responseid)
}
for (rl in c("HR", "Eng")) {
  m <- spec_pooled(rl)
  report(sprintf("Pooled with Blind interaction, %s", rl), m,
         c("blind", "lag_same_z", "lag_same_x_blind", "lag_diff_z", "lag_diff_x_blind"))
  cf <- coef(m); V <- vcov(m); np <- length(cf)
  i_main <- match("lag_same_z", names(cf))
  i_int  <- match("lag_same_x_blind", names(cf))
  if (!is.na(i_main) && !is.na(i_int)) {
    v <- numeric(np); v[i_main] <- 1; v[i_int] <- 1
    est <- sum(v * cf); se <- sqrt(c(v %*% V %*% v))
    p   <- 2*pnorm(-abs(est/se))
    stars <- fcase(p < 0.01, "***", p < 0.05, "**", p < 0.10, "*", default = "")
    cat(sprintf("  Implied lag_same effect under BLIND          = %+8.4f%s   (SE %.4f, p %.4f)\n",
                est, stars, se, p))
  }
}
