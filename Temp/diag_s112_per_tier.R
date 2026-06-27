## Replicate §1.1.2 spec exactly (fit_pooled_cont + extract_pooled_te_cont)
## and pull per-tier blinding TE for each (demographic, role, quality_var) cell.
date <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
library(pacman)
p_load(haven, here, data.table, fixest, readr, dplyr, tidyverse, stringr,
       labelled, splitstackshape, lubridate)
setwd(here())
source(here("1_Codes", "2_Import.R")); source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R")); source(here("1_Codes", "5_Cleaning.R"))
source(here("1_Codes", "6_Hiring_Simulation.R"))

build_aux_cont <- function(d) {
  d <- copy(d)
  d[, urm_tech_blind := fcase(
    treat == "blind",                    "Blind",
    treat == "nonblind" & urm_tech == 1, "Nonblind_URM",
    treat == "nonblind" & urm_tech == 0, "Nonblind_NonURM",
    default = NA_character_)]
  d[, urm_tech_blind := factor(urm_tech_blind, levels = c("Blind","Nonblind_NonURM","Nonblind_URM"))]
  d[, urm_axis_blind := fcase(
    treat == "blind", "Blind",
    treat == "nonblind" & race_gender %in% c("White_Male","Asian_Male"), "Nonblind_MaleNonHisp",
    treat == "nonblind" & race_gender %in% c("White_Female","Asian_Female"), "Nonblind_FemaleNonHisp",
    treat == "nonblind" & race_gender %in% c("Hispanic_Male","Hispanic_Female"), "Nonblind_Hispanic",
    default = NA_character_)]
  d[, urm_axis_blind := factor(urm_axis_blind,
    levels = c("Blind","Nonblind_MaleNonHisp","Nonblind_FemaleNonHisp","Nonblind_Hispanic"))]
  d[, race_gender := factor(race_gender,
    levels = c("Blind","White_Male","White_Female","Asian_Male","Asian_Female","Hispanic_Male","Hispanic_Female"))]
  d[, gpa_z       := as.numeric(scale(gpa))]
  d[, test_case_z := as.numeric(scale(test_case))]
  d
}

fit_pooled_cont <- function(d, factor_col, quality_col) {
  extra_fe <- if (any(d$role == "Eng")) " + resp_python + resp_java" else ""
  fml <- as.formula(sprintf(
    "I(-sc_overall_z) ~ %s * %s + resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income%s",
    factor_col, quality_col, extra_fe))
  suppressMessages(suppressWarnings(feols(fml, data = d, vcov = ~ responseid + resume_index)))
}

## Note: no i() in formula -> coef names use bare concatenation
## (race_genderWhite_Male, race_genderWhite_Male:gpa_z, etc.)
extract_te <- function(m, factor_col, demo_levels, qcol, qpts = c(-1, 0, 1)) {
  cf <- coef(m); V <- vcov(m); np <- length(cf)
  rows <- list()
  for (d in demo_levels) {
    main_nm <- paste0(factor_col, d)
    int_nm  <- paste0(factor_col, d, ":", qcol)
    for (q in qpts) {
      v <- numeric(np)
      if (main_nm %in% names(cf)) v[match(main_nm, names(cf))] <- 1
      if (int_nm  %in% names(cf)) v[match(int_nm,  names(cf))] <- q
      est <- sum(v * cf); se <- sqrt(c(v %*% V %*% v))
      p   <- 2*pnorm(-abs(est/se))
      stars <- fcase(p < 0.01, "***", p < 0.05, "**", p < 0.10, "*", default = "")
      rows[[length(rows)+1]] <- data.table(demo = d, q = q, est = est, se = se, p = p, stars = stars)
    }
  }
  rbindlist(rows)
}

cat("\n========== §1.1.2 Per-tier blinding TE (sign-flipped via I(-sc_overall_z)) ==========\n")
for (rl in c("HR", "Eng")) {
  d_role <- build_aux_cont(firm_long_dt[role == rl])
  for (qcol in c("gpa_z", "test_case_z")) {
    cat(sprintf("\n##### Role: %s   Quality var: %s #####\n", rl, qcol))

    ## 6-cell race_gender
    m <- fit_pooled_cont(d_role, "race_gender", qcol)
    rg <- c("White_Male","White_Female","Asian_Male","Asian_Female","Hispanic_Male","Hispanic_Female")
    out <- extract_te(m, "race_gender", rg, qcol)
    cat("\n[race_gender 6-cell]\n")
    print(out[, .(demo, q, est = round(est, 4), p = round(p, 3), stars)])

    ## 3-axis (Male NH / Female NH / Hispanic)
    m <- fit_pooled_cont(d_role, "urm_axis_blind", qcol)
    ax <- c("Nonblind_MaleNonHisp","Nonblind_FemaleNonHisp","Nonblind_Hispanic")
    out <- extract_te(m, "urm_axis_blind", ax, qcol)
    cat("\n[urm_axis_blind: Male NH / Female NH / Hispanic]\n")
    print(out[, .(demo, q, est = round(est, 4), p = round(p, 3), stars)])

    ## 2-axis URM-tech
    m <- fit_pooled_cont(d_role, "urm_tech_blind", qcol)
    ut <- c("Nonblind_NonURM","Nonblind_URM")
    out <- extract_te(m, "urm_tech_blind", ut, qcol)
    cat("\n[urm_tech_blind: NonURM / URM]\n")
    print(out[, .(demo, q, est = round(est, 4), p = round(p, 3), stars)])
  }
}
