## Print clean coefficient tables for §2.1, §2.2, §2.3 across all three indices,
## both roles, and both cuts (Per-RG and Per URM Axis). For storyline synthesis.

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
    treat == "blind",                                                            "Blind",
    treat == "nonblind" & race_gender %in% c("White_Male", "Asian_Male"),         "Nonblind_MaleNonHisp",
    treat == "nonblind" & race_gender %in% c("White_Female", "Asian_Female"),     "Nonblind_FemaleNonHisp",
    treat == "nonblind" & race_gender %in% c("Hispanic_Male", "Hispanic_Female"), "Nonblind_Hispanic",
    default = NA_character_)]
  d[, urm_axis_blind := factor(urm_axis_blind,
    levels = c("Blind", "Nonblind_MaleNonHisp", "Nonblind_FemaleNonHisp", "Nonblind_Hispanic"))]
  d[, urm_tech_blind := fcase(
    treat == "blind",                    "Blind",
    treat == "nonblind" & urm_tech == 1, "Nonblind_URM",
    treat == "nonblind" & urm_tech == 0, "Nonblind_NonURM",
    default = NA_character_)]
  d[, urm_tech_blind := factor(urm_tech_blind,
    levels = c("Blind", "Nonblind_URM", "Nonblind_NonURM"))]
  d[, firm_targets_fh := as.integer(firm_targets_female == 1L | firm_targets_hispanic == 1L)]
  d
}
dt_hr  <- build_axis(dt_hr)
dt_eng <- build_axis(dt_eng)

p <- function(fit, label, regex) {
  ct <- coeftable(fit); rn <- rownames(ct); hits <- rn[grepl(regex, rn)]
  cat(sprintf("\n--- %s ---\n", label))
  for (h in hits) {
    e <- ct[h, "Estimate"]; s <- ct[h, "Std. Error"]; pp <- ct[h, "Pr(>|t|)"]
    star <- ifelse(pp < .01, "***", ifelse(pp < .05, "**", ifelse(pp < .1, "*", "")))
    cat(sprintf("  %-78s %8.3f (%6.3f) %s\n", h, e, s, star))
  }
}

cat("\n############################################################\n")
cat("§2.1 — Per Race-Gender × DEI Index (interactions only)\n")
cat("############################################################\n")
fit_rg <- function(d, idx) feols(as.formula(sprintf(
  "I(-sc_overall_z) ~ %s + i(race_gender, ref='Blind') + i(race_gender, %s, ref='Blind') + resp_age + I(resp_age^2) + I(resp_age^3) | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income",
  idx, idx)), data = d, vcov = ~ responseid)
for (idx in c("dei_index_A_z","dei_index_B_z","dei_index_C_z")) {
  cat(sprintf("\n=== %s ===\n", idx))
  p(suppressMessages(suppressWarnings(fit_rg(dt_hr,  idx))), paste0("HR  | ", idx),
    "race_gender::.*:dei_index")
  p(suppressMessages(suppressWarnings(fit_rg(dt_eng, idx))), paste0("Eng | ", idx),
    "race_gender::.*:dei_index")
}

cat("\n\n############################################################\n")
cat("§2.2 — Triple terms (Firm Targets URM × DEI Index)\n")
cat("############################################################\n")

fit_22_rg <- function(d, idx) feols(as.formula(sprintf(
  "I(-sc_overall_z) ~ %s + firm_targets_fh + I(firm_targets_fh * %s) + i(race_gender, ref='Blind') + i(race_gender, %s, ref='Blind') + i(race_gender, firm_targets_fh, ref='Blind') + i(race_gender, I(firm_targets_fh * %s), ref='Blind') + resp_age + I(resp_age^2) + I(resp_age^3) | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income",
  idx, idx, idx, idx)), data = d, vcov = ~ responseid)
fit_22_urm <- function(d, idx) feols(as.formula(sprintf(
  "I(-sc_overall_z) ~ %s + firm_targets_fh + I(firm_targets_fh * %s) + i(urm_tech_blind, ref='Blind') + i(urm_tech_blind, %s, ref='Blind') + i(urm_tech_blind, firm_targets_fh, ref='Blind') + i(urm_tech_blind, I(firm_targets_fh * %s), ref='Blind') + resp_age + I(resp_age^2) + I(resp_age^3) | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income",
  idx, idx, idx, idx)), data = d, vcov = ~ responseid)
fit_22_axis <- function(d, idx) feols(as.formula(sprintf(
  "I(-sc_overall_z) ~ %s + firm_targets_fh + I(firm_targets_fh * %s) + i(urm_axis_blind, ref='Blind') + i(urm_axis_blind, %s, ref='Blind') + i(urm_axis_blind, firm_targets_fh, ref='Blind') + i(urm_axis_blind, I(firm_targets_fh * %s), ref='Blind') + resp_age + I(resp_age^2) + I(resp_age^3) | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income",
  idx, idx, idx, idx)), data = d, vcov = ~ responseid)

for (idx in c("dei_index_A_z","dei_index_B_z","dei_index_C_z")) {
  cat(sprintf("\n=== %s ===\n", idx))
  p(suppressMessages(suppressWarnings(fit_22_rg (dt_hr , idx))), paste0("HR  | per-RG | ", idx),  "race_gender::.*:I\\(firm_targets_fh")
  p(suppressMessages(suppressWarnings(fit_22_rg (dt_eng, idx))), paste0("Eng | per-RG | ", idx),  "race_gender::.*:I\\(firm_targets_fh")
  p(suppressMessages(suppressWarnings(fit_22_urm(dt_hr , idx))), paste0("HR  | URM cut | ", idx), "urm_tech_blind::.*:I\\(firm_targets_fh")
  p(suppressMessages(suppressWarnings(fit_22_urm(dt_eng, idx))), paste0("Eng | URM cut | ", idx), "urm_tech_blind::.*:I\\(firm_targets_fh")
  p(suppressMessages(suppressWarnings(fit_22_axis(dt_hr , idx))), paste0("HR  | URM axis| ", idx), "urm_axis_blind::.*:I\\(firm_targets_fh")
  p(suppressMessages(suppressWarnings(fit_22_axis(dt_eng, idx))), paste0("Eng | URM axis| ", idx), "urm_axis_blind::.*:I\\(firm_targets_fh")
}

cat("\n\n############################################################\n")
cat("§2.3 — Triple terms (DEI Index × Firm DEI Attention)\n")
cat("############################################################\n")

fit_23_rg <- function(d, idx) feols(as.formula(sprintf(
  "I(-sc_overall_z) ~ %s + firm_dei_attention_z + I(%s * firm_dei_attention_z) + i(race_gender, ref='Blind') + i(race_gender, %s, ref='Blind') + i(race_gender, firm_dei_attention_z, ref='Blind') + i(race_gender, I(%s * firm_dei_attention_z), ref='Blind') + resp_age + I(resp_age^2) + I(resp_age^3) | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income",
  idx, idx, idx, idx)), data = d, vcov = ~ responseid)
fit_23_urm <- function(d, idx) feols(as.formula(sprintf(
  "I(-sc_overall_z) ~ %s + firm_dei_attention_z + I(%s * firm_dei_attention_z) + i(urm_tech_blind, ref='Blind') + i(urm_tech_blind, %s, ref='Blind') + i(urm_tech_blind, firm_dei_attention_z, ref='Blind') + i(urm_tech_blind, I(%s * firm_dei_attention_z), ref='Blind') + resp_age + I(resp_age^2) + I(resp_age^3) | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income",
  idx, idx, idx, idx)), data = d, vcov = ~ responseid)
fit_23_axis <- function(d, idx) feols(as.formula(sprintf(
  "I(-sc_overall_z) ~ %s + firm_dei_attention_z + I(%s * firm_dei_attention_z) + i(urm_axis_blind, ref='Blind') + i(urm_axis_blind, %s, ref='Blind') + i(urm_axis_blind, firm_dei_attention_z, ref='Blind') + i(urm_axis_blind, I(%s * firm_dei_attention_z), ref='Blind') + resp_age + I(resp_age^2) + I(resp_age^3) | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income",
  idx, idx, idx, idx)), data = d, vcov = ~ responseid)

for (idx in c("dei_index_A_z","dei_index_B_z","dei_index_C_z")) {
  cat(sprintf("\n=== %s ===\n", idx))
  p(suppressMessages(suppressWarnings(fit_23_rg (dt_hr , idx))), paste0("HR  | per-RG | ", idx),  "race_gender::.*:I\\(.*firm_dei")
  p(suppressMessages(suppressWarnings(fit_23_rg (dt_eng, idx))), paste0("Eng | per-RG | ", idx),  "race_gender::.*:I\\(.*firm_dei")
  p(suppressMessages(suppressWarnings(fit_23_urm(dt_hr , idx))), paste0("HR  | URM cut | ", idx), "urm_tech_blind::.*:I\\(.*firm_dei")
  p(suppressMessages(suppressWarnings(fit_23_urm(dt_eng, idx))), paste0("Eng | URM cut | ", idx), "urm_tech_blind::.*:I\\(.*firm_dei")
  p(suppressMessages(suppressWarnings(fit_23_axis(dt_hr , idx))), paste0("HR  | URM axis| ", idx), "urm_axis_blind::.*:I\\(.*firm_dei")
  p(suppressMessages(suppressWarnings(fit_23_axis(dt_eng, idx))), paste0("Eng | URM axis| ", idx), "urm_axis_blind::.*:I\\(.*firm_dei")
}

cat("\nDone.\n")
