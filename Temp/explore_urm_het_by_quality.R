## Heterogeneity by candidate quality, URM cuts.
## Two cuts:
##   (1) urm_tech_blind: Blind / Nonblind_URM (WF+AF+HM+HF) / Nonblind_NonURM (WM+AM)
##   (2) urm_axis_blind: Blind / Nonblind_MaleNonHisp / Nonblind_FemaleNonHisp / Nonblind_Hispanic
## For each cut, split sample by GPA tier (Low/Mid/Top) and Coding tier (Bottom/Top half).
## LHS sign-flipped so positive => effect of blinding raises the score.

suppressPackageStartupMessages({
  library(pacman)
  p_load(haven, here, base, readr, data.table, stringr, labelled,
         tidyverse, dplyr, fixest, knitr)
})
options(readr.show_types = FALSE)
setwd(here())
date      <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
source(here("1_Codes", "2_Import.R"))
source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R"))
source(here("1_Codes", "5_Cleaning.R"))

build_aux <- function(d) {
  d[, urm_tech_blind := fcase(
    treat == "blind",                    "Blind",
    treat == "nonblind" & urm_tech == 1, "Nonblind_URM",
    treat == "nonblind" & urm_tech == 0, "Nonblind_NonURM",
    default = NA_character_)]
  d[, urm_tech_blind := factor(urm_tech_blind,
    levels = c("Blind", "Nonblind_URM", "Nonblind_NonURM"))]
  d[, urm_axis_blind := fcase(
    treat == "blind",                                                            "Blind",
    treat == "nonblind" & race_gender %in% c("White_Male", "Asian_Male"),         "Nonblind_MaleNonHisp",
    treat == "nonblind" & race_gender %in% c("White_Female", "Asian_Female"),     "Nonblind_FemaleNonHisp",
    treat == "nonblind" & race_gender %in% c("Hispanic_Male", "Hispanic_Female"), "Nonblind_Hispanic",
    default = NA_character_)]
  d[, urm_axis_blind := factor(urm_axis_blind,
    levels = c("Blind", "Nonblind_MaleNonHisp", "Nonblind_FemaleNonHisp", "Nonblind_Hispanic"))]
  d
}
dt_hr  <- build_aux(firm_long_dt[role == "HR"])
dt_eng <- build_aux(firm_long_dt[role == "Eng"])

fit_demo <- function(d, factor_var) {
  suppressMessages(suppressWarnings(feols(
    as.formula(sprintf(
      "I(-sc_overall_z) ~ i(%s, ref = 'Blind') + resp_age + I(resp_age^2) + I(resp_age^3) | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income",
      factor_var)),
    data = d, vcov = ~ responseid
  )))
}

stars <- function(p) ifelse(is.na(p), "",
                     ifelse(p < .01, "***",
                     ifelse(p < .05, "**",
                     ifelse(p < .10, "*", ""))))
fmt <- function(est, se, p) sprintf("%+7.3f (%5.3f)%-3s", est, se, stars(p))

extract_te <- function(fit, factor_var, levels_list) {
  ct <- coeftable(fit); rn <- rownames(ct)
  out <- data.table(group = levels_list, est = NA_real_, se = NA_real_, p = NA_real_)
  for (g in levels_list) {
    rname <- paste0(factor_var, "::", g)
    if (rname %in% rn) {
      out[group == g, est := ct[rname, "Estimate"]]
      out[group == g, se  := ct[rname, "Std. Error"]]
      out[group == g, p   := ct[rname, "Pr(>|t|)"]]
    }
  }
  out[, n := fit$nobs]
  out
}

print_block <- function(label, dt) {
  cat(sprintf("\n--- %s (N = %d) ---\n", label, dt$n[1]))
  for (i in seq_len(nrow(dt))) {
    r <- dt[i]
    cat(sprintf("  %-32s %s\n", gsub("_", " ", r$group), fmt(r$est, r$se, r$p)))
  }
}

run_cut <- function(cut_lbl, factor_var, levels_list) {
  cat(sprintf("\n##############################################\n"))
  cat(sprintf("%s\n", cut_lbl))
  cat(sprintf("##############################################\n"))

  cat("\n----- by GPA tier -----\n")
  for (role_name in c("HR", "Eng")) {
    d <- if (role_name == "HR") dt_hr else dt_eng
    cat(sprintf("\n==== ROLE: %s ====", role_name))
    for (lv in c("Low_GPA", "Mid_GPA", "Top_GPA")) {
      sub <- d[gpa_tier3 == lv]
      if (nrow(sub) == 0) next
      fit <- fit_demo(sub, factor_var)
      te  <- extract_te(fit, factor_var, levels_list)
      print_block(paste0("GPA = ", lv), te)
    }
  }

  cat("\n\n----- by Coding tier -----\n")
  for (role_name in c("HR", "Eng")) {
    d <- if (role_name == "HR") dt_hr else dt_eng
    cat(sprintf("\n==== ROLE: %s ====", role_name))
    for (lv in c(0, 1)) {
      sub <- d[top_half_coder == lv]
      if (nrow(sub) == 0) next
      fit <- fit_demo(sub, factor_var)
      te  <- extract_te(fit, factor_var, levels_list)
      lbl <- ifelse(lv == 1, "Coding = Top half", "Coding = Bottom half")
      print_block(lbl, te)
    }
  }
}

run_cut("URM-tech cut: URM = {WF, AF, HM, HF} / Non-URM = {WM, AM}",
        "urm_tech_blind",
        c("Nonblind_URM", "Nonblind_NonURM"))

run_cut("URM-axis cut: Male (NH) = {WM, AM} / Female (NH) = {WF, AF} / Hispanic = {HM, HF}",
        "urm_axis_blind",
        c("Nonblind_MaleNonHisp", "Nonblind_FemaleNonHisp", "Nonblind_Hispanic"))

cat("\nDone.\n")
