## §2.1 (Eng-only): heterogeneity in the effect of blinding on
##   open_resume / open_code1 / open_code2
## across (a) candidate quality and (b) candidate demographic cut.
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

eng <- copy(firm_long_dt[role == "Eng"])
eng[, gpa_z       := as.numeric(scale(gpa))]
eng[, test_case_z := as.numeric(scale(test_case))]
eng[, urm_tech_blind := fcase(
  treat == "blind",                    "Blind",
  treat == "nonblind" & urm_tech == 1, "Nonblind_URM",
  treat == "nonblind" & urm_tech == 0, "Nonblind_NonURM",
  default = NA_character_)]
eng[, urm_tech_blind := factor(urm_tech_blind,
  levels = c("Blind","Nonblind_NonURM","Nonblind_URM"))]
eng[, urm_axis_blind := fcase(
  treat == "blind", "Blind",
  treat == "nonblind" & race_gender %in% c("White_Male","Asian_Male"),     "Nonblind_MaleNonHisp",
  treat == "nonblind" & race_gender %in% c("White_Female","Asian_Female"), "Nonblind_FemaleNonHisp",
  treat == "nonblind" & race_gender %in% c("Hispanic_Male","Hispanic_Female"), "Nonblind_Hispanic",
  default = NA_character_)]
eng[, urm_axis_blind := factor(urm_axis_blind,
  levels = c("Blind","Nonblind_MaleNonHisp","Nonblind_FemaleNonHisp","Nonblind_Hispanic"))]

cat("\n##### Mean open rates by treat (Eng only) - reference #####\n")
print(eng[, .(N=.N,
              open_resume = round(mean(open_resume, na.rm=TRUE), 3),
              open_code1  = round(mean(open_code1,  na.rm=TRUE), 3),
              open_code2  = round(mean(open_code2,  na.rm=TRUE), 3)),
          by = treat])

## ---------------- (1) Main effect ------------------
spec_main <- function(d, dv) {
  feols(as.formula(sprintf("%s ~ i(treat, ref='nonblind') + resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income + resp_python + resp_java", dv)),
        data = d, vcov = ~ responseid)
}
report_main <- function(label, m) {
  ct <- coeftable(m)["treat::blind", ]
  cat(sprintf("  %-30s  est = %+7.4f   se = %.4f   p = %.4f   N = %d\n",
              label, ct[1], ct[2], ct[4], m$nobs))
}
cat("\n##### MAIN: Eng - effect of blinding on open rates #####\n")
report_main("open_resume", spec_main(eng, "open_resume"))
report_main("open_code1",  spec_main(eng, "open_code1"))
report_main("open_code2",  spec_main(eng, "open_code2"))

## ------------- (2) Quality interaction (continuous) ----------
fit_q <- function(d, dv, qcol) {
  d <- copy(d)
  d[, blind := as.integer(treat == "blind")]
  d[, q_int := blind * d[[qcol]]]
  feols(as.formula(sprintf("%s ~ blind + q_int + resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income + resp_python + resp_java", dv)),
        data = d, vcov = ~ responseid)
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
    rows[[length(rows)+1]] <- data.table(q = q,
                                          est = round(est, 4),
                                          se = round(se, 4),
                                          p = round(p, 4),
                                          stars = stars)
  }
  rbindlist(rows)
}

cat("\n##### Eng - effect of blinding on open rates, by candidate quality (continuous) #####\n")
for (dv in c("open_resume", "open_code1", "open_code2")) {
  for (qcol in c("gpa_z", "test_case_z")) {
    cat(sprintf("\n----- %s  ~  Blind x %s -----\n", dv, qcol))
    d <- eng[!is.na(eng[[qcol]])]
    m <- fit_q(d, dv, qcol)
    cat(sprintf("  Main(blind)=%.4f   Interaction(blind x %s)=%.4f   N=%d\n",
                coef(m)["blind"], qcol, coef(m)["q_int"], m$nobs))
    out <- extract_te(m); out[, tier := c("Low (-1 SD)", "Mid (0)", "Top (+1 SD)")]
    print(out[, .(tier, est, se, p, stars)])
  }
}

## --------- (3) Demographic interactions (categorical) ----------
## We use the §1.1.1 / §1.1.2 conventions:
##   race_gender (6-cell): WM, WF, AM, AF, HM, HF, with Blind reference
##   urm_axis_blind: Male NH / Female NH / Hispanic, with Blind reference
##   urm_tech_blind: NonURM / URM, with Blind reference
## For each, we run:  open_x ~ i(factor, ref='Blind') + resp_age | <FEs>
## Coefficient on each factor level = effect of that level RELATIVE TO
## blind (so a positive coef means that demo group is MORE LIKELY to have
## the file opened under nonblind). Sign-flip post-fit for clarity.

fit_cat <- function(d, dv, fcol) {
  feols(as.formula(sprintf("%s ~ i(%s, ref='Blind') + resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income + resp_python + resp_java", dv, fcol)),
        data = d, vcov = ~ responseid)
}
extract_cat <- function(m, fcol, levels_to_show) {
  ct <- coeftable(m)
  out <- list()
  for (lv in levels_to_show) {
    nm <- paste0(fcol, "::", lv)
    if (nm %in% rownames(ct)) {
      ## Sign flip so coef = effect of BLINDING vs being-this-level under nonblind.
      est <- -ct[nm, "Estimate"]
      se  <-  ct[nm, "Std. Error"]
      p   <-  ct[nm, 4]
      stars <- fcase(p < 0.01, "***", p < 0.05, "**", p < 0.10, "*", default = "")
      out[[length(out)+1]] <- data.table(level = lv,
                                          est = round(est, 4),
                                          se  = round(se, 4),
                                          p   = round(p, 4),
                                          stars = stars)
    }
  }
  rbindlist(out)
}

cat("\n##### Eng - effect of blinding on open rates, by candidate DEMOGRAPHIC cut #####\n")
cat("(coef = effect of BLINDING for that demographic, sign-flipped from nonblind dummy)\n")

for (dv in c("open_resume", "open_code1", "open_code2")) {
  cat(sprintf("\n----- %s -----\n", dv))

  ## 6-cell race_gender
  m <- fit_cat(eng, dv, "race_gender")
  print(extract_cat(m, "race_gender",
    c("White_Male","White_Female","Asian_Male","Asian_Female","Hispanic_Male","Hispanic_Female")))

  ## URM axis (Male NH, Female NH, Hispanic)
  m <- fit_cat(eng[!is.na(urm_axis_blind)], dv, "urm_axis_blind")
  cat("[urm_axis: Male NH / Female NH / Hispanic]\n")
  print(extract_cat(m, "urm_axis_blind",
    c("Nonblind_MaleNonHisp","Nonblind_FemaleNonHisp","Nonblind_Hispanic")))

  ## URM-tech (NonURM, URM)
  m <- fit_cat(eng[!is.na(urm_tech_blind)], dv, "urm_tech_blind")
  cat("[urm_tech: Majority / URM]\n")
  print(extract_cat(m, "urm_tech_blind",
    c("Nonblind_NonURM","Nonblind_URM")))
}
