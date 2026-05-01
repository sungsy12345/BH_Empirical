## §2.2 with firm_targets_female and firm_targets_hispanic SEPARATELY (not OR-combined).
## Currently: firm_targets_fh = (firm_targets_female == 1 OR firm_targets_hispanic == 1).
## Question: do these two firm characteristics moderate blinding differently?
## Compare three specifications: combined OR (current), female-only, hispanic-only.
## All use Index B (the headline index) and the URM-axis cut.

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
  d[, firm_targets_fh := as.integer(firm_targets_female == 1L | firm_targets_hispanic == 1L)]
  d
}
dt_hr  <- build_axis(dt_hr)
dt_eng <- build_axis(dt_eng)

cat("\n--- Firm-target indicator distribution (unique firms) ---\n")
firm_chars <- unique(dt_hr[, .(responseid, firm_targets_female, firm_targets_hispanic, firm_targets_fh)])
cat(sprintf("Total firms (HR sample): %d\n", nrow(firm_chars)))
cat(sprintf("  Targets Female only:           %d\n",
            firm_chars[firm_targets_female == 1L & firm_targets_hispanic == 0L, .N]))
cat(sprintf("  Targets Hispanic only:         %d\n",
            firm_chars[firm_targets_female == 0L & firm_targets_hispanic == 1L, .N]))
cat(sprintf("  Targets Both:                  %d\n",
            firm_chars[firm_targets_female == 1L & firm_targets_hispanic == 1L, .N]))
cat(sprintf("  Targets Neither:               %d\n",
            firm_chars[firm_targets_female == 0L & firm_targets_hispanic == 0L, .N]))
cat(sprintf("  Targets Female (any, OR):      %d\n",
            firm_chars[firm_targets_female == 1L, .N]))
cat(sprintf("  Targets Hispanic (any, OR):    %d\n",
            firm_chars[firm_targets_hispanic == 1L, .N]))
cat(sprintf("  Targets Female OR Hispanic:    %d\n",
            firm_chars[firm_targets_fh == 1L, .N]))

idx <- "dei_index_B_z"

## Helper: fit triple with given firm-target proxy.
fit_triple <- function(d, ftproxy) {
  suppressMessages(suppressWarnings(feols(
    as.formula(sprintf(
      "I(-sc_overall_z) ~ %s + %s + I(%s * %s) + i(urm_axis_blind, ref='Blind') + i(urm_axis_blind, %s, ref='Blind') + i(urm_axis_blind, %s, ref='Blind') + i(urm_axis_blind, I(%s * %s), ref='Blind') + resp_age + I(resp_age^2) + I(resp_age^3) | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income",
      idx, ftproxy, idx, ftproxy, idx, ftproxy, ftproxy, idx)),
    data = d, vcov = ~ responseid
  )))
}

p <- function(fit, label, regex) {
  ct <- coeftable(fit); rn <- rownames(ct); hits <- rn[grepl(regex, rn)]
  cat(sprintf("\n  --- %s (N = %d) ---\n", label, fit$nobs))
  for (h in hits) {
    e <- ct[h, "Estimate"]; s <- ct[h, "Std. Error"]; pp <- ct[h, "Pr(>|t|)"]
    star <- ifelse(pp < .01, "***", ifelse(pp < .05, "**", ifelse(pp < .1, "*", "")))
    short <- gsub("urm_axis_blind::Nonblind_", "", h)
    short <- gsub("MaleNonHisp", "Male(NH)", short)
    short <- gsub("FemaleNonHisp", "Female(NH)", short)
    cat(sprintf("    %-72s %8.3f (%6.3f) %s\n", short, e, s, star))
  }
}

cat("\n\n############################################################\n")
cat("§2.2 URM-axis triple — Index B — three firm-target proxies\n")
cat("############################################################\n")

for (ftproxy in c("firm_targets_fh", "firm_targets_female", "firm_targets_hispanic")) {
  cat(sprintf("\n========== ftproxy = %s ==========\n", ftproxy))
  m_hr  <- fit_triple(dt_hr,  ftproxy)
  m_eng <- fit_triple(dt_eng, ftproxy)
  ## Show all urm_axis_blind coefficients (levels, x DEI, x ftproxy, triple).
  p(m_hr,  paste0("HR  | ", ftproxy), "urm_axis_blind::")
  p(m_eng, paste0("Eng | ", ftproxy), "urm_axis_blind::")
}

cat("\n\nDone.\n")
