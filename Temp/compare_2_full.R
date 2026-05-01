## Full-consistency diagnostic: §2.1 - §2.4 results under
## "current" (with W/A/H or M/F filter) vs "extended" (all respondents,
## with non-applicant-pool race/gender coded as always-discordant in nonblind).
library(pacman)
p_load(haven, here, base, readr, data.table, stringr, labelled, writexl,
       tidyverse, dplyr, fixest, knitr)
options(readr.show_types = FALSE)
setwd(here())

date      <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"

source(here("1_Codes", "2_Import.R"))
source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R"))
source(here("1_Codes", "5_Cleaning.R"))

cat("\n========== Respondent race / gender breakdown (nonblind) ==========\n")
print(unique(firm_long_dt[treat == "nonblind",
                          .(responseid, role, resp_race, resp_gender)])[,
       .N, keyby = .(role, resp_race)])
cat("\n-- Genders --\n")
print(unique(firm_long_dt[treat == "nonblind",
                          .(responseid, role, resp_gender)])[,
       .N, keyby = .(role, resp_gender)])

## ========== Build extended factors ==========

## §2.1 (race only) — current and extended
firm_long_dt[, c21_cur := fcase(
  treat == "blind",                                     "Blind",
  treat == "nonblind" & concordance_race_detailed == 1, "Nonblind_Concordant",
  treat == "nonblind" & concordance_race_detailed == 0, "Nonblind_Nonconcordant",
  default = NA_character_)]
firm_long_dt[, c21_ext := fcase(
  treat == "blind",                                     "Blind",
  treat == "nonblind" & concordance_race_detailed == 1, "Nonblind_Concordant",
  treat == "nonblind" & concordance_race_detailed == 0, "Nonblind_Nonconcordant",
  is.na(concordance_race_detailed) & treat == "nonblind" &
    !is.na(resp_race) & !(resp_race %in% c("White","Asian","Hispanic")),
    "Nonblind_Nonconcordant",
  default = NA_character_)]
firm_long_dt[, c21_cur := factor(c21_cur, levels = c("Blind","Nonblind_Concordant","Nonblind_Nonconcordant"))]
firm_long_dt[, c21_ext := factor(c21_ext, levels = c("Blind","Nonblind_Concordant","Nonblind_Nonconcordant"))]

## §2.2 (gender only) — current and extended
## resp_gender values: Male / Female / Nonbinary (or others)
firm_long_dt[, c22_cur := fcase(
  treat == "blind",                              "Blind",
  treat == "nonblind" & concordance_gender == 1, "Nonblind_Concordant",
  treat == "nonblind" & concordance_gender == 0, "Nonblind_Nonconcordant",
  default = NA_character_)]
firm_long_dt[, c22_ext := fcase(
  treat == "blind",                              "Blind",
  treat == "nonblind" & concordance_gender == 1, "Nonblind_Concordant",
  treat == "nonblind" & concordance_gender == 0, "Nonblind_Nonconcordant",
  is.na(concordance_gender) & treat == "nonblind" &
    !is.na(resp_gender) & !(resp_gender %in% c("Male","Female")),
    "Nonblind_Nonconcordant",
  default = NA_character_)]
firm_long_dt[, c22_cur := factor(c22_cur, levels = c("Blind","Nonblind_Concordant","Nonblind_Nonconcordant"))]
firm_long_dt[, c22_ext := factor(c22_ext, levels = c("Blind","Nonblind_Concordant","Nonblind_Nonconcordant"))]

## §2.3 (broad race-gender) — current and extended
firm_long_dt[, c23_cur := fcase(
  treat == "blind",                                  "Blind",
  treat == "nonblind" & concordance_race_gender == 1, "Nonblind_Concordant",
  treat == "nonblind" & concordance_race_gender == 0, "Nonblind_Nonconcordant",
  default = NA_character_)]
firm_long_dt[, c23_ext := fcase(
  treat == "blind",                                  "Blind",
  treat == "nonblind" & concordance_race_gender == 1, "Nonblind_Concordant",
  treat == "nonblind" & concordance_race_gender == 0, "Nonblind_Nonconcordant",
  is.na(concordance_race_gender) & treat == "nonblind" &
    !is.na(resp_race) & !is.na(resp_gender) &
    (!(resp_race %in% c("White","Asian","Hispanic")) |
       !(resp_gender %in% c("Male","Female"))),
    "Nonblind_Nonconcordant",
  default = NA_character_)]
firm_long_dt[, c23_cur := factor(c23_cur, levels = c("Blind","Nonblind_Concordant","Nonblind_Nonconcordant"))]
firm_long_dt[, c23_ext := factor(c23_ext, levels = c("Blind","Nonblind_Concordant","Nonblind_Nonconcordant"))]

## §2.4 (detailed race-gender) — current and extended
## Current: concordance_race_gender_detailed_blind already keeps Korean/Japanese
##          as always-discordant (per our previous edit).
firm_long_dt[, c24_cur := concordance_race_gender_detailed_blind]
firm_long_dt[, c24_ext := fcase(
  c24_cur == "Concordant_Group",                      "Nonblind_Concordant",
  c24_cur == "Discordant_Group",                      "Nonblind_Nonconcordant",
  c24_cur == "Blind",                                 "Blind",
  is.na(c24_cur) & treat == "nonblind" &
    !is.na(resp_race) & !(resp_race %in% c("White","Asian","Hispanic")),
    "Nonblind_Nonconcordant",
  is.na(c24_cur) & treat == "nonblind" &
    !is.na(resp_gender) & !(resp_gender %in% c("Male","Female")),
    "Nonblind_Nonconcordant",
  is.na(c24_cur) & treat == "blind",                  "Blind",
  default = NA_character_)]
levels(firm_long_dt$c24_cur) <- c("Blind","Nonblind_Concordant","Nonblind_Nonconcordant")
firm_long_dt[, c24_ext := factor(c24_ext, levels = c("Blind","Nonblind_Concordant","Nonblind_Nonconcordant"))]

## ========== Helper ==========
fit_one <- function(d, lhs, conc) {
  feols(
    as.formula(sprintf(
      "%s ~ i(%s, ref = 'Blind') + resp_age + I(resp_age^2) + I(resp_age^3) | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income",
      lhs, conc)),
    data = d, vcov = ~ responseid)
}
flip <- function(m) { m$coefficients <- -1 * m$coefficients; m }
ext_coef <- function(m, conc) {
  cf <- coef(m); se <- sqrt(diag(vcov(m)))
  c_n <- paste0(conc, "::Nonblind_Concordant")
  d_n <- paste0(conc, "::Nonblind_Nonconcordant")
  data.frame(
    Conc = sprintf("%+.4f (%.4f)", cf[c_n], se[c_n]),
    Disc = sprintf("%+.4f (%.4f)", cf[d_n], se[d_n]),
    N    = nobs(m),
    N_resp = uniqueN(m$call$data$responseid)
  )
}

## ========== Run for each section ==========

run_section <- function(section, cur, ext, current_filter_fn) {
  cat(sprintf("\n========== %s ==========\n", section))
  for (rl in c("HR","Eng")) {
    d_cur <- current_filter_fn(rl)
    d_ext <- firm_long_dt[role == rl & !is.na(get(ext))]
    n_cur <- uniqueN(d_cur$responseid); n_ext <- uniqueN(d_ext$responseid)
    m_cur <- flip(fit_one(d_cur, "sc_overall_z", cur))
    m_ext <- flip(fit_one(d_ext, "sc_overall_z", ext))
    cat(sprintf("\n%s -- Current (N=%d resp): Conc %s | Disc %s | obs %d\n",
                rl, n_cur,
                sprintf("%+.4f (%.4f)", coef(m_cur)[paste0(cur, "::Nonblind_Concordant")],
                        sqrt(diag(vcov(m_cur)))[paste0(cur, "::Nonblind_Concordant")]),
                sprintf("%+.4f (%.4f)", coef(m_cur)[paste0(cur, "::Nonblind_Nonconcordant")],
                        sqrt(diag(vcov(m_cur)))[paste0(cur, "::Nonblind_Nonconcordant")]),
                nobs(m_cur)))
    cat(sprintf("%s -- Extended (N=%d resp): Conc %s | Disc %s | obs %d\n",
                rl, n_ext,
                sprintf("%+.4f (%.4f)", coef(m_ext)[paste0(ext, "::Nonblind_Concordant")],
                        sqrt(diag(vcov(m_ext)))[paste0(ext, "::Nonblind_Concordant")]),
                sprintf("%+.4f (%.4f)", coef(m_ext)[paste0(ext, "::Nonblind_Nonconcordant")],
                        sqrt(diag(vcov(m_ext)))[paste0(ext, "::Nonblind_Nonconcordant")]),
                nobs(m_ext)))
  }
}

run_section("§2.1 By Concordant Race (detailed)", "c21_cur", "c21_ext",
  function(r) firm_long_dt[role == r & resp_race %in% c("White","Asian","Hispanic")])

run_section("§2.2 By Concordant Gender", "c22_cur", "c22_ext",
  function(r) firm_long_dt[role == r & resp_gender %in% c("Male","Female")])

run_section("§2.3 By Concordant Race-Gender (broad)", "c23_cur", "c23_ext",
  function(r) firm_long_dt[role == r & resp_race %in% c("White","Asian","Hispanic")])

run_section("§2.4 By Concordant Race-Gender (detailed)", "c24_cur", "c24_ext",
  function(r) firm_long_dt[role == r & !is.na(c24_cur)])
