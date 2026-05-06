## Two diagnostics:
## (1) For every regression currently using `resp_age + resp_age^2 + resp_age^3`,
##     refit with `resp_age` linear only, and report any change in significance
##     for the focal coefficients.
## (2) Check whether `q_version` and `resume_index` are collinear when both
##     are absorbed as FE -- specifically, can they be jointly absorbed without
##     killing identification of `race_gender`?
##
## Tables affected by (1): §1.1.1 (race-gender), §1.1.2 (concordance), §4
## DEI/firm-targets tables (bh_dei_combined_*).

rm(list = ls())
date <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
library(pacman)
p_load(haven, here, base, readr, data.table, stringr, fixest)
options(tigris_use_cache = TRUE); setwd(here())
source(here("1_Codes", "2_Import.R")); source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R")); source(here("1_Codes", "5_Cleaning.R"))
source(here("1_Codes", "6_Hiring_Simulation.R"))

cat("\n==================== (1) AGE POLY: cubic vs linear ====================\n")

compare_age <- function(label, fml_cubic_str, fml_linear_str, data, vcov_arg = ~ responseid,
                         focal_pattern = "race_gender|treat::blind|dei_index_z|firm_targets|misalign_z") {
  cat(sprintf("\n--- %s ---\n", label))
  m_c <- suppressMessages(suppressWarnings(
    feols(as.formula(fml_cubic_str),  data = data, vcov = vcov_arg)))
  m_l <- suppressMessages(suppressWarnings(
    feols(as.formula(fml_linear_str), data = data, vcov = vcov_arg)))

  ct_c <- coeftable(m_c); ct_l <- coeftable(m_l)
  common <- intersect(rownames(ct_c), rownames(ct_l))
  common <- grep(focal_pattern, common, value = TRUE)
  if (length(common) == 0L) { cat("  (no focal coefs)\n"); return(invisible()) }

  out <- data.table(
    coef     = common,
    est_c    = ct_c[common, "Estimate"],
    p_c      = ct_c[common, 4],
    est_l    = ct_l[common, "Estimate"],
    p_l      = ct_l[common, 4]
  )
  out[, sig_c := fcase(p_c < 0.01, "***", p_c < 0.05, "**", p_c < 0.10, "*", default = "")]
  out[, sig_l := fcase(p_l < 0.01, "***", p_l < 0.05, "**", p_l < 0.10, "*", default = "")]
  out[, sig_changed := sig_c != sig_l]
  out[, est_pct_diff := round(100 * (est_l - est_c) / pmax(abs(est_c), 1e-6), 1)]
  print(out[, .(coef, est_c = round(est_c, 4), sig_c, est_l = round(est_l, 4),
                sig_l, sig_changed, est_pct_diff)])
  cat(sprintf("  >>> %d / %d focal coefs change significance class.\n",
              sum(out$sig_changed), nrow(out)))
}

base_fe <- "resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income + q_version"

## §1.1.1 race-gender (HR + Eng)
compare_age(
  "1.1.1 HR race-gender (sc_overall_z)",
  paste0("sc_overall_z ~ i(race_gender, ref='Blind') + resp_age + I(resp_age^2) + I(resp_age^3) | ", base_fe),
  paste0("sc_overall_z ~ i(race_gender, ref='Blind') + resp_age | ", base_fe),
  firm_long_dt[role == "HR"])

compare_age(
  "1.1.1 Eng race-gender (sc_overall_z)",
  paste0("sc_overall_z ~ i(race_gender, ref='Blind') + resp_age + I(resp_age^2) + I(resp_age^3) | ", base_fe),
  paste0("sc_overall_z ~ i(race_gender, ref='Blind') + resp_age | ", base_fe),
  firm_long_dt[role == "Eng"])

## §1.1.2 concordance: takes one of three concordance vars
firm_long_dt[, blind_concordance_race := fcase(
  treat == "blind",                              "Blind",
  treat == "nonblind" & concordance_race_detailed == 1, "Nonblind_Concordant",
  treat == "nonblind" & concordance_race_detailed == 0, "Nonblind_Nonconcordant",
  default = NA_character_)]
firm_long_dt[, blind_concordance_race := factor(blind_concordance_race,
  levels = c("Blind", "Nonblind_Concordant", "Nonblind_Nonconcordant"))]
compare_age(
  "1.1.2 HR concordance_race (sc_overall_z)",
  paste0("sc_overall_z ~ i(blind_concordance_race, ref='Blind') + resp_age + I(resp_age^2) + I(resp_age^3) | ", base_fe),
  paste0("sc_overall_z ~ i(blind_concordance_race, ref='Blind') + resp_age | ", base_fe),
  firm_long_dt[role == "HR"],
  focal_pattern = "blind_concordance")

compare_age(
  "1.1.2 Eng concordance_race",
  paste0("sc_overall_z ~ i(blind_concordance_race, ref='Blind') + resp_age + I(resp_age^2) + I(resp_age^3) | ", base_fe),
  paste0("sc_overall_z ~ i(blind_concordance_race, ref='Blind') + resp_age | ", base_fe),
  firm_long_dt[role == "Eng"],
  focal_pattern = "blind_concordance")

## §4 DEI heterogeneity (race-gender x DEI). Just the simple HR / Eng spec.
compare_age(
  "§4 DEI x race_gender HR (simple)",
  paste0("sc_overall_z ~ dei_index_z + i(race_gender, ref='Blind') + i(race_gender, dei_index_z, ref='Blind') + resp_age + I(resp_age^2) + I(resp_age^3) | ", base_fe),
  paste0("sc_overall_z ~ dei_index_z + i(race_gender, ref='Blind') + i(race_gender, dei_index_z, ref='Blind') + resp_age | ", base_fe),
  firm_long_dt[role == "HR"],
  focal_pattern = "race_gender|dei_index")

compare_age(
  "§4 DEI x race_gender Eng (simple)",
  paste0("sc_overall_z ~ dei_index_z + i(race_gender, ref='Blind') + i(race_gender, dei_index_z, ref='Blind') + resp_age + I(resp_age^2) + I(resp_age^3) | ", base_fe),
  paste0("sc_overall_z ~ dei_index_z + i(race_gender, ref='Blind') + i(race_gender, dei_index_z, ref='Blind') + resp_age | ", base_fe),
  firm_long_dt[role == "Eng"],
  focal_pattern = "race_gender|dei_index")

cat("\n==================== (2) Q-VERSION x RESUME-INDEX collinearity ====================\n\n")

cat("Crosstab of (resume_ver, q_version) -- nonblind only:\n")
print(unique(firm_long_dt[treat == "nonblind", .(resume_ver, q_version)])[order(q_version, resume_ver)])

cat("\nCrosstab of (resume_ver, q_version) -- blind only:\n")
print(unique(firm_long_dt[treat == "blind", .(resume_ver, q_version)])[order(q_version, resume_ver)])

cat("\nN respondents per (q_version, resume_ver) combo (HR):\n")
print(firm_long_dt[role == "HR",
                   .(n_resp = uniqueN(responseid)),
                   by = .(q_version, resume_ver)][order(q_version, resume_ver)])

cat("\nWithin a single q_version, how many distinct race_gender assignments\nappear at resume_index = 1 (across respondents)?  (HR)\n")
print(firm_long_dt[role == "HR" & resume_index == "1",
                   .(unique_race_gender = paste(sort(unique(as.character(race_gender))), collapse=",")),
                   by = q_version])

cat("\n--- Conclusive collinearity test: try fitting §1.1.1 spec and report whether race_gender coefs are dropped ---\n")
m_test <- suppressMessages(suppressWarnings(
  feols(sc_overall_z ~ i(race_gender, ref='Blind') | resume_index + q_version,
        data = firm_long_dt[role == "HR"], vcov = ~ responseid)))
cat("HR with ONLY resume_index + q_version FE (race_gender as RHS):\n")
print(coeftable(m_test))
cat(sprintf("\nCollin.var (variables dropped due to collinearity, if any):\n"))
print(m_test$collin.var)
cat(sprintf("nobs.origin: %d, nobs: %d\n", m_test$nobs_origin, m_test$nobs))
