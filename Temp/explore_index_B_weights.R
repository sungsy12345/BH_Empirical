## Index B sensitivity to weighting between stated-views composite and revealed-priority.
## Index B (current): (1-w) * pro_dei_index_z + w * div_priority_z, z-scored within (role, treat).
## Try w in {0.20 (= 1/5, equal across 5 items), 0.33 (= 1/3), 0.50 (current), 0.67 (= 2/3)}.
## For each, run §2.1 axis (HR), §2.2 axis triple (HR), §2.3 axis triple (HR).
## The HR side is where the storyline lives; Eng is null and not shown.

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

dt_hr <- firm_long_dt[role == "HR"]

## Build axis + URM-cut + firm_targets_fh as before.
build_aux <- function(d) {
  d[, urm_axis_blind := fcase(
    treat == "blind",                                                            "Blind",
    treat == "nonblind" & race_gender %in% c("White_Male", "Asian_Male"),         "Nonblind_MaleNonHisp",
    treat == "nonblind" & race_gender %in% c("White_Female", "Asian_Female"),     "Nonblind_FemaleNonHisp",
    treat == "nonblind" & race_gender %in% c("Hispanic_Male", "Hispanic_Female"), "Nonblind_Hispanic",
    default = NA_character_)]
  d[, urm_axis_blind := factor(urm_axis_blind,
    levels = c("Blind", "Nonblind_MaleNonHisp", "Nonblind_FemaleNonHisp", "Nonblind_Hispanic"))]
  d[, firm_targets_fh := as.integer(firm_targets_female == 1L | firm_targets_hispanic == 1L)]
  d
}
dt_hr <- build_aux(dt_hr)

## Build alternative Index B with weight w on div_priority_z and (1-w) on pro_dei_index_z.
## Z-score within (role, treat) to match current convention.
build_index_B <- function(d, w) {
  d <- copy(d)
  d[, idx_B_w := (1 - w) * pro_dei_index_z + w * div_priority_z]
  d[, idx_B_w := (idx_B_w - mean(idx_B_w, na.rm = TRUE)) / sd(idx_B_w, na.rm = TRUE),
    by = .(role, treat)]
  d
}

## Helper to print one coefficient cleanly.
get_coef <- function(fit, pattern) {
  ct <- coeftable(fit); rn <- rownames(ct); h <- rn[grepl(pattern, rn)]
  if (length(h) == 0) return(c(est = NA, se = NA, p = NA))
  c(est = ct[h[1], "Estimate"], se = ct[h[1], "Std. Error"], p = ct[h[1], "Pr(>|t|)"])
}

fmt <- function(x) {
  star <- ifelse(is.na(x["p"]), "",
          ifelse(x["p"] < .01, "***",
          ifelse(x["p"] < .05, "**",
          ifelse(x["p"] < .1, "*", ""))))
  sprintf("%+7.3f (%5.3f)%-3s", x["est"], x["se"], star)
}

cat(sprintf("\n%-30s %-22s %-22s %-22s %-22s\n",
            "Coefficient", "w=0.20 (eq-5)", "w=0.33 (1/3)", "w=0.50 (current)", "w=0.67 (2/3)"))
cat(strrep("-", 120), "\n")

results <- list()
for (w in c(0.20, 0.33, 0.50, 0.67)) {
  dt <- build_index_B(dt_hr, w)
  idx <- "idx_B_w"

  ## §2.1 axis (Hispanic x DEI, no firm interaction):
  m_21 <- suppressMessages(suppressWarnings(feols(
    as.formula(sprintf(
      "I(-sc_overall_z) ~ i(urm_axis_blind, ref='Blind') + %s + i(urm_axis_blind, %s, ref='Blind') + resp_age + I(resp_age^2) + I(resp_age^3) | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income",
      idx, idx)),
    data = dt, vcov = ~ responseid)))

  ## §2.2 axis triple (Hispanic x DEI x firm_targets_fh):
  m_22 <- suppressMessages(suppressWarnings(feols(
    as.formula(sprintf(
      "I(-sc_overall_z) ~ %s + firm_targets_fh + I(firm_targets_fh * %s) + i(urm_axis_blind, ref='Blind') + i(urm_axis_blind, %s, ref='Blind') + i(urm_axis_blind, firm_targets_fh, ref='Blind') + i(urm_axis_blind, I(firm_targets_fh * %s), ref='Blind') + resp_age + I(resp_age^2) + I(resp_age^3) | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income",
      idx, idx, idx, idx)),
    data = dt, vcov = ~ responseid)))

  ## §2.3 axis triple (Hispanic x DEI x firm_dei_attention_z):
  m_23 <- suppressMessages(suppressWarnings(feols(
    as.formula(sprintf(
      "I(-sc_overall_z) ~ %s + firm_dei_attention_z + I(%s * firm_dei_attention_z) + i(urm_axis_blind, ref='Blind') + i(urm_axis_blind, %s, ref='Blind') + i(urm_axis_blind, firm_dei_attention_z, ref='Blind') + i(urm_axis_blind, I(%s * firm_dei_attention_z), ref='Blind') + resp_age + I(resp_age^2) + I(resp_age^3) | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income",
      idx, idx, idx, idx)),
    data = dt, vcov = ~ responseid)))

  results[[as.character(w)]] <- list(
    s21_hisp_x_dei  = get_coef(m_21, "urm_axis_blind::Nonblind_Hispanic:idx_B_w$"),
    s22_hisp_triple = get_coef(m_22, "urm_axis_blind::Nonblind_Hispanic:I\\(firm_targets_fh"),
    s22_male_triple = get_coef(m_22, "urm_axis_blind::Nonblind_MaleNonHisp:I\\(firm_targets_fh"),
    s22_hisp_x_dei  = get_coef(m_22, "urm_axis_blind::Nonblind_Hispanic:idx_B_w$"),
    s23_hisp_triple = get_coef(m_23, "urm_axis_blind::Nonblind_Hispanic:I\\(idx_B_w"),
    s23_male_triple = get_coef(m_23, "urm_axis_blind::Nonblind_MaleNonHisp:I\\(idx_B_w")
  )
}

print_row <- function(label, key) {
  cat(sprintf("%-30s %s %s %s %s\n", label,
              fmt(results[["0.2"]][[key]]),
              fmt(results[["0.33"]][[key]]),
              fmt(results[["0.5"]][[key]]),
              fmt(results[["0.67"]][[key]])))
}

cat("\n§2.1 axis HR  (Hispanic x DEI):\n")
print_row("  Hispanic x DEI",          "s21_hisp_x_dei")

cat("\n§2.2 axis HR triple (cell x FirmTargetsURM x DEI):\n")
print_row("  Hispanic x FT x DEI",     "s22_hisp_triple")
print_row("  Male(NH) x FT x DEI",     "s22_male_triple")
print_row("  Hispanic x DEI (at FT=0)","s22_hisp_x_dei")

cat("\n§2.3 axis HR triple (cell x DEI x FirmDEIAttn):\n")
print_row("  Hispanic x DEI x Attn",   "s23_hisp_triple")
print_row("  Male(NH) x DEI x Attn",   "s23_male_triple")

cat("\nDone.\n")
