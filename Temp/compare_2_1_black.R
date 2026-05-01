## Diagnostic: §2.1 results — current spec (drops Black/other-race) vs
## extended spec (keeps Black/other-race as always-discordant in nonblind).
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

## ---------- Build current and extended factors ----------
## Current: uses concordance_race_detailed (Korean/Japanese already as
## always-discordant); restricts to W/A/H respondents in chunk.
firm_long_dt[, current_blind := fcase(
  treat == "blind",                                     "Blind",
  treat == "nonblind" & concordance_race_detailed == 1, "Nonblind_Concordant",
  treat == "nonblind" & concordance_race_detailed == 0, "Nonblind_Nonconcordant",
  default = NA_character_
)]
firm_long_dt[, current_blind := factor(current_blind,
  levels = c("Blind", "Nonblind_Concordant", "Nonblind_Nonconcordant"))]

## Extended: also code Black (and any other race not in W/A/H applicant pool)
## respondents as always-discordant in nonblind. (Asian sub-groups already
## handled by current concordance_race_detailed cleaning rule.)
firm_long_dt[, extended_blind := fcase(
  treat == "blind",                                     "Blind",
  treat == "nonblind" & concordance_race_detailed == 1, "Nonblind_Concordant",
  treat == "nonblind" & concordance_race_detailed == 0, "Nonblind_Nonconcordant",
  is.na(concordance_race_detailed) & treat == "nonblind" &
    !is.na(resp_race) & !(resp_race %in% c("White","Asian","Hispanic")),
    "Nonblind_Nonconcordant",
  default = NA_character_
)]
firm_long_dt[, extended_blind := factor(extended_blind,
  levels = c("Blind", "Nonblind_Concordant", "Nonblind_Nonconcordant"))]

## ---------- How many respondents would be added? ----------
cat("\n========== Respondent breakdown by resp_race (nonblind only) ==========\n")
print(unique(firm_long_dt[treat == "nonblind", .(responseid, role, resp_race)])[, .N,
       keyby = .(role, resp_race)])

cat("\n========== Cell counts: current vs extended ==========\n")
cat("\n-- Current (HR), W/A/H only --\n")
print(firm_long_dt[role == "HR" & resp_race %in% c("White","Asian","Hispanic"),
                   .N, keyby = current_blind])
cat("\n-- Extended (HR), all respondents --\n")
print(firm_long_dt[role == "HR", .N, keyby = extended_blind])
cat("\n-- Current (Eng), W/A/H only --\n")
print(firm_long_dt[role == "Eng" & resp_race %in% c("White","Asian","Hispanic"),
                   .N, keyby = current_blind])
cat("\n-- Extended (Eng), all respondents --\n")
print(firm_long_dt[role == "Eng", .N, keyby = extended_blind])

## ---------- Regressions: current vs extended ----------
fit_one <- function(d, lhs_name, conc_var) {
  feols(
    as.formula(sprintf(
      "%s ~ i(%s, ref = 'Blind') + resp_age + I(resp_age^2) + I(resp_age^3) | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income",
      lhs_name, conc_var)),
    data = d, vcov = ~ responseid)
}
flip <- function(m) { m$coefficients <- -1 * m$coefficients; m }
extract_coef <- function(m, conc_var) {
  cf <- coef(m); se <- sqrt(diag(vcov(m)))
  conc_name <- paste0(conc_var, "::Nonblind_Concordant")
  disc_name <- paste0(conc_var, "::Nonblind_Nonconcordant")
  data.frame(
    Concordant_est = cf[conc_name],
    Concordant_se  = se[conc_name],
    Discordant_est = cf[disc_name],
    Discordant_se  = se[disc_name],
    N = nobs(m)
  )
}

cat("\n========== HR: Overall (Z), signs flipped ==========\n")
m_cur_hr <- fit_one(firm_long_dt[role == "HR" & resp_race %in% c("White","Asian","Hispanic")],
                   "sc_overall_z", "current_blind") |> flip()
m_ext_hr <- fit_one(firm_long_dt[role == "HR" & !is.na(extended_blind)],
                   "sc_overall_z", "extended_blind") |> flip()
print(rbind(
  cbind(spec = "Current (W/A/H only)",                extract_coef(m_cur_hr, "current_blind")),
  cbind(spec = "Extended (incl. Black/other as disc.)", extract_coef(m_ext_hr, "extended_blind"))
), row.names = FALSE)

cat("\n========== Engineer: Overall (Z), signs flipped ==========\n")
m_cur_eng <- fit_one(firm_long_dt[role == "Eng" & resp_race %in% c("White","Asian","Hispanic")],
                    "sc_overall_z", "current_blind") |> flip()
m_ext_eng <- fit_one(firm_long_dt[role == "Eng" & !is.na(extended_blind)],
                    "sc_overall_z", "extended_blind") |> flip()
print(rbind(
  cbind(spec = "Current (W/A/H only)",                extract_coef(m_cur_eng, "current_blind")),
  cbind(spec = "Extended (incl. Black/other as disc.)", extract_coef(m_ext_eng, "extended_blind"))
), row.names = FALSE)

cat("\n========== Unique respondents added (extended - current) ==========\n")
n_cur_hr  <- uniqueN(firm_long_dt[role == "HR"  & resp_race %in% c("White","Asian","Hispanic"), responseid])
n_ext_hr  <- uniqueN(firm_long_dt[role == "HR"  & !is.na(extended_blind), responseid])
n_cur_eng <- uniqueN(firm_long_dt[role == "Eng" & resp_race %in% c("White","Asian","Hispanic"), responseid])
n_ext_eng <- uniqueN(firm_long_dt[role == "Eng" & !is.na(extended_blind), responseid])
cat(sprintf("HR  : current = %d, extended = %d, diff = %d\n", n_cur_hr,  n_ext_hr,  n_ext_hr  - n_cur_hr))
cat(sprintf("Eng : current = %d, extended = %d, diff = %d\n", n_cur_eng, n_ext_eng, n_ext_eng - n_cur_eng))
