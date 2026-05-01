## Exploration: Option 2 (three-way URM decomposition) for §2.1 right-table.
## Cells: Blind / Nonblind_Hispanic (HM, HF) / Nonblind_FemaleNonHisp (WF, AF)
##        / Nonblind_MaleNonHisp (WM, AM, reference within nonblind).
## LHS sign-flipped so + => effect of blinding raises the score.

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

dt_hr  <- firm_long_dt[role == "HR"]
dt_eng <- firm_long_dt[role == "Eng"]

build_axis <- function(d) {
  d[, urm_axis_blind := fcase(
    treat == "blind", "Blind",
    treat == "nonblind" & race_gender %in% c("Hispanic_Male", "Hispanic_Female"), "Nonblind_Hispanic",
    treat == "nonblind" & race_gender %in% c("White_Female", "Asian_Female"),     "Nonblind_FemaleNonHisp",
    treat == "nonblind" & race_gender %in% c("White_Male",   "Asian_Male"),       "Nonblind_MaleNonHisp",
    default = NA_character_)]
  d[, urm_axis_blind := factor(urm_axis_blind,
    levels = c("Blind", "Nonblind_Hispanic", "Nonblind_FemaleNonHisp", "Nonblind_MaleNonHisp"))]
  d
}
dt_hr  <- build_axis(dt_hr)
dt_eng <- build_axis(dt_eng)

cat("\n--- Cell counts (unique respondents x resume) by urm_axis_blind ---\n")
print(dt_hr [, .N, by = urm_axis_blind][order(urm_axis_blind)])
print(dt_eng[, .N, by = urm_axis_blind][order(urm_axis_blind)])

fit_axis <- function(d, idx) {
  suppressMessages(suppressWarnings(feols(
    as.formula(sprintf(
      "I(-sc_overall_z) ~ i(urm_axis_blind, ref = 'Blind') + %s + i(urm_axis_blind, %s, ref = 'Blind') + resp_age + I(resp_age^2) + I(resp_age^3) | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income",
      idx, idx)),
    data = d, vcov = ~ responseid
  )))
}

print_axis <- function(fit, label) {
  ct <- coeftable(fit)
  rn <- rownames(ct)
  ## Drop main reference row "Blind" (none printed) and keep only urm_axis_blind rows.
  hits <- rn[grepl("^urm_axis_blind", rn)]
  cat(sprintf("\n--- %s (N = %d, R2 = %.3f) ---\n", label, fit$nobs, fitstat(fit, "r2")$r2))
  for (h in hits) {
    est <- ct[h, "Estimate"]; se <- ct[h, "Std. Error"]; p <- ct[h, "Pr(>|t|)"]
    star <- ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .1, "*", "")))
    cat(sprintf("  %-72s  %8.3f (%6.3f) %s\n", h, est, se, star))
  }
}

for (idx in c("dei_index_A_z", "dei_index_B_z", "dei_index_C_z")) {
  cat(sprintf("\n=========== %s ===========\n", idx))
  m_hr  <- fit_axis(dt_hr,  idx)
  m_eng <- fit_axis(dt_eng, idx)
  print_axis(m_hr,  paste0("HR  | ", idx))
  print_axis(m_eng, paste0("Eng | ", idx))
}

cat("\nDone.\n")
