## §2.1 (Eng-only): TREATMENT EFFECT of blinding on opening rates,
## differentiated by candidate quality and candidate true demographic.
##
## Spec (universal):
##   open_X ~ blind*Q + resp_age | resume_index + r_do + resp_gender +
##                                  resp_race + s2_educ + s2_income +
##                                  resp_python + resp_java
##   blind = 1{treat == "blind"}, vcov = ~ responseid.
##
## What we report = the BLINDING TREATMENT EFFECT, as a function of Q.
## For continuous quality (test_case_z, gpa_z): TE at Q = -1, 0, +1 SD.
## For categorical demographic (true_race_gender): TE at each level.
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
eng[, blind := as.integer(treat == "blind")]
eng[, true_rg := factor(as.character(true_race_gender),
  levels = c("White_Male","White_Female","Asian_Male","Asian_Female",
             "Hispanic_Male","Hispanic_Female"))]
## URM-axis from true demographic (visible regardless of treatment to the analyst).
eng[, true_axis := fcase(
  as.character(true_race_gender) %in% c("White_Male","Asian_Male"),     "Male_NonHisp",
  as.character(true_race_gender) %in% c("White_Female","Asian_Female"), "Female_NonHisp",
  as.character(true_race_gender) %in% c("Hispanic_Male","Hispanic_Female"), "Hispanic",
  default = NA_character_)]
eng[, true_axis := factor(true_axis,
  levels = c("Male_NonHisp","Female_NonHisp","Hispanic"))]
## URM-tech from true demographic (defined for blind too, unlike urm_tech which
## is NA when race_gender = "Blind").
eng[, true_urm := fcase(
  as.character(true_race_gender) %in% c("White_Male", "Asian_Male"), "Majority",
  as.character(true_race_gender) %in% c("White_Female", "Asian_Female",
                                          "Hispanic_Male", "Hispanic_Female"), "URM_tech",
  default = NA_character_)]
eng[, true_urm := factor(true_urm, levels = c("Majority","URM_tech"))]

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
                                          te_blind = round(est, 4),
                                          se = round(se, 4),
                                          p = round(p, 4),
                                          stars = stars)
  }
  rbindlist(rows)
}

cat("\n##############################################################\n")
cat("## TREATMENT EFFECT OF BLINDING -- by CANDIDATE QUALITY\n##\n")
cat("## Each row reports the blinding TE (= mean(open_X | blind) - mean(open_X | nonblind)),\n")
cat("## for a candidate at the quality tier indicated. p < 0.10/0.05/0.01.\n")
cat("##############################################################\n")

for (dv in c("open_resume", "open_code1", "open_code2")) {
  for (qcol in c("test_case_z", "gpa_z")) {
    d <- eng[!is.na(eng[[qcol]])]
    d <- copy(d); d[, q_int := blind * d[[qcol]]]
    fml <- as.formula(sprintf("%s ~ blind + q_int + resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income + resp_python + resp_java", dv))
    m <- feols(fml, data = d, vcov = ~ responseid)
    out <- extract_te(m); out[, tier := c("Low (-1 SD)", "Mid (0)", "Top (+1 SD)")]
    cat(sprintf("\n[%s, moderator = %s, N = %d]\n", dv, qcol, m$nobs))
    print(out[, .(tier, te_blind, se, p, stars)])
  }
}

cat("\n##############################################################\n")
cat("## TREATMENT EFFECT OF BLINDING -- by CANDIDATE TRUE DEMOGRAPHIC\n##\n")
cat("## Each row reports the blinding TE for candidates of that demographic.\n")
cat("## The TE for level k = coef(blind) + coef(blind:demo_k).\n")
cat("##############################################################\n")

extract_te_cat <- function(m, fcol, levels_to_show, ref_level) {
  cf <- coef(m); V <- vcov(m); np <- length(cf)
  rows <- list()
  for (lv in levels_to_show) {
    v <- numeric(np)
    main_nm <- "blind"
    if (main_nm %in% names(cf)) v[match(main_nm, names(cf))] <- 1
    if (lv != ref_level) {
      int_nm <- paste0("blind:", fcol, lv)
      if (int_nm %in% names(cf)) v[match(int_nm, names(cf))] <- 1
    }
    est <- sum(v * cf); se <- sqrt(c(v %*% V %*% v))
    p   <- 2*pnorm(-abs(est/se))
    stars <- fcase(p < 0.01, "***", p < 0.05, "**", p < 0.10, "*", default = "")
    rows[[length(rows)+1]] <- data.table(level = lv,
                                          te_blind = round(est, 4),
                                          se = round(se, 4),
                                          p = round(p, 4),
                                          stars = stars)
  }
  rbindlist(rows)
}

run_demo <- function(dv, fcol_name, levels_to_show, ref_level, label) {
  d <- eng[!is.na(eng[[fcol_name]])]
  d <- copy(d)
  d[[fcol_name]] <- factor(as.character(d[[fcol_name]]), levels = levels_to_show)
  d[[fcol_name]] <- relevel(d[[fcol_name]], ref = ref_level)
  fml <- as.formula(sprintf("%s ~ blind * %s + resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income + resp_python + resp_java",
                             dv, fcol_name))
  m <- feols(fml, data = d, vcov = ~ responseid)
  cat(sprintf("\n[%s, moderator = %s, N = %d]\n", dv, label, m$nobs))
  out <- extract_te_cat(m, fcol_name, levels_to_show, ref_level)
  print(out)
}

for (dv in c("open_resume", "open_code1", "open_code2")) {
  cat(sprintf("\n----- DV: %s -----\n", dv))
  run_demo(dv, "true_rg",
           levels_to_show = c("White_Male","White_Female","Asian_Male","Asian_Female","Hispanic_Male","Hispanic_Female"),
           ref_level = "White_Male",
           label = "true race_gender (6-cell, ref=White_Male)")
  run_demo(dv, "true_axis",
           levels_to_show = c("Male_NonHisp","Female_NonHisp","Hispanic"),
           ref_level = "Male_NonHisp",
           label = "true URM axis (3-cell, ref=Male_NonHisp)")
  run_demo(dv, "true_urm",
           levels_to_show = c("Majority","URM_tech"),
           ref_level = "Majority",
           label = "URM-tech (2-cell, ref=Majority)")
}
