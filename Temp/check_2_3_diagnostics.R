## Diagnostic A: per-race-gender triple (replace urm_tech_blind with race_gender
##                in §2.3 structure). Tests whether Engineer §2.3 null is driven
##                by URM aggregation hiding cell-level moderation.
## Diagnostic B: §2.2 + DEI Index triple (urm_tech_blind x firm_targets_fh x DEI Index).
##                A different triple using firm's concrete targeting instead of
##                firm's survey-perceived DEI attention.

library(pacman)
p_load(haven, here, base, readr, data.table, stringr, labelled,
       tidyverse, dplyr, fixest, knitr)
options(readr.show_types = FALSE)
setwd(here())
date      <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
source(here("1_Codes", "2_Import.R"))
source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R"))
source(here("1_Codes", "5_Cleaning.R"))

## Make a helper to grep + print a few coefs from a fit.
print_coefs <- function(fit, label, patterns) {
  ct <- coeftable(fit)
  rn <- rownames(ct)
  hits <- rn[Reduce(`|`, lapply(patterns, function(p) grepl(p, rn, fixed = FALSE)))]
  cat(sprintf("\n--- %s (N = %d) ---\n", label, fit$nobs))
  if (length(hits) == 0) { cat("  [no matching rows]\n"); return(invisible()) }
  for (h in hits) {
    est <- ct[h, "Estimate"]; se <- ct[h, "Std. Error"]; p <- ct[h, "Pr(>|t|)"]
    star <- ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .1, "*", "")))
    cat(sprintf("  %-72s  %8.3f (%6.3f) %s\n", h, est, se, star))
  }
}

## ================================================================
## DIAGNOSTIC A: per-race-gender triple (race_gender x DEI Index x Firm DEI Attn)
## ================================================================
cat("\n=========================================================\n")
cat("DIAGNOSTIC A: race_gender x DEI Index x Firm DEI Attn (triple)\n")
cat("LHS sign-flipped: positive => effect of blinding raises score.\n")
cat("=========================================================\n")

dt_hr  <- firm_long_dt[role == "HR"]
dt_eng <- firm_long_dt[role == "Eng"]

fit_triple_rg <- function(d, idx, proxy_b) {
  suppressMessages(suppressWarnings(feols(
    as.formula(sprintf(
      "I(-sc_overall_z) ~ %s + %s + I(%s * %s) + i(race_gender, ref = 'Blind') + i(race_gender, %s, ref = 'Blind') + i(race_gender, %s, ref = 'Blind') + i(race_gender, I(%s * %s), ref = 'Blind') + resp_age + I(resp_age^2) + I(resp_age^3) | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income",
      idx, proxy_b, idx, proxy_b, idx, proxy_b, idx, proxy_b)),
    data = d, vcov = ~ responseid
  )))
}

for (idx in c("dei_index_A_z", "dei_index_B_z", "dei_index_C_z")) {
  m_hr  <- fit_triple_rg(dt_hr,  idx, "firm_dei_attention_z")
  m_eng <- fit_triple_rg(dt_eng, idx, "firm_dei_attention_z")
  patt  <- c("race_gender::.*:I\\(", "race_gender::Hispanic", "race_gender::Asian", "race_gender::White")
  trip_only <- c("race_gender::.*:I\\(")
  cat(sprintf("\n========== %s : TRIPLE TERMS ONLY (race_gender x DEI x Firm Attn) ==========\n", idx))
  print_coefs(m_hr,  paste0("HR  | ", idx),  trip_only)
  print_coefs(m_eng, paste0("Eng | ", idx), trip_only)
}

## ================================================================
## DIAGNOSTIC B: §2.2 + DEI Index triple (urm_tech_blind x firm_targets_fh x DEI Index)
## ================================================================
cat("\n\n=========================================================\n")
cat("DIAGNOSTIC B: urm_tech_blind x firm_targets_fh x DEI Index (triple)\n")
cat("Compare to §2.3 which used firm_dei_attention_z instead of firm_targets_fh.\n")
cat("=========================================================\n")

dt_hr [, urm_tech_blind := fcase(
  treat == "blind",                    "Blind",
  treat == "nonblind" & urm_tech == 1, "Nonblind_URM",
  treat == "nonblind" & urm_tech == 0, "Nonblind_NonURM",
  default = NA_character_)]
dt_eng[, urm_tech_blind := fcase(
  treat == "blind",                    "Blind",
  treat == "nonblind" & urm_tech == 1, "Nonblind_URM",
  treat == "nonblind" & urm_tech == 0, "Nonblind_NonURM",
  default = NA_character_)]
dt_hr [, urm_tech_blind := factor(urm_tech_blind,
  levels = c("Blind", "Nonblind_URM", "Nonblind_NonURM"))]
dt_eng[, urm_tech_blind := factor(urm_tech_blind,
  levels = c("Blind", "Nonblind_URM", "Nonblind_NonURM"))]

dt_hr [, firm_targets_fh := as.integer(firm_targets_female == 1L | firm_targets_hispanic == 1L)]
dt_eng[, firm_targets_fh := as.integer(firm_targets_female == 1L | firm_targets_hispanic == 1L)]

fit_triple_22 <- function(d, idx) {
  suppressMessages(suppressWarnings(feols(
    as.formula(sprintf(
      "I(-sc_overall_z) ~ %s + firm_targets_fh + I(firm_targets_fh * %s) + i(urm_tech_blind, ref = 'Blind') + i(urm_tech_blind, %s, ref = 'Blind') + i(urm_tech_blind, firm_targets_fh, ref = 'Blind') + i(urm_tech_blind, I(firm_targets_fh * %s), ref = 'Blind') + resp_age + I(resp_age^2) + I(resp_age^3) | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income",
      idx, idx, idx, idx)),
    data = d, vcov = ~ responseid
  )))
}

for (idx in c("dei_index_A_z", "dei_index_B_z", "dei_index_C_z")) {
  m_hr  <- fit_triple_22(dt_hr,  idx)
  m_eng <- fit_triple_22(dt_eng, idx)
  show_pat <- c("urm_tech_blind::Nonblind_(URM|NonURM)$",                    # main
                paste0("urm_tech_blind::Nonblind_(URM|NonURM):", idx, "$"),  # x DEI
                "urm_tech_blind::Nonblind_(URM|NonURM):firm_targets_fh$",    # x firm targets
                "urm_tech_blind::Nonblind_(URM|NonURM):I\\(firm_targets_fh") # triple
  cat(sprintf("\n========== %s : urm_tech_blind x firm_targets_fh x DEI ==========\n", idx))
  print_coefs(m_hr,  paste0("HR  | ", idx), show_pat)
  print_coefs(m_eng, paste0("Eng | ", idx), show_pat)
}

cat("\n\nDone.\n")
