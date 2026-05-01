## Diagnostic: §2.4 results with vs without Korean/Japanese/other-Asian respondents.
## Original spec drops them. Alternative spec keeps them as always-discordant.
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

resp_detailed_set <- c("White_Male", "White_Female",
                       "Hispanic_Male", "Hispanic_Female",
                       "Chinese_Male", "Chinese_Female",
                       "Indian_Male", "Indian_Female")

## ---------- Build the alternative concordance variable ----------
## Treats Korean / Japanese / Pakistani / etc. Asian respondents as always
## discordant in nonblind (since the applicant pool's Asian names are
## exclusively Chinese or Indian). Their blind rows remain "Blind".
firm_long_dt[, conc_alt := fcase(
  concordance_race_gender_detailed == 1, "Concordant_Group",
  concordance_race_gender_detailed == 0, "Discordant_Group",
  ## NEW: Korean / Japanese / etc. Asian respondents in nonblind -> Discordant
  is.na(concordance_race_gender_detailed) & treat == "nonblind" &
    resp_race == "Asian", "Discordant_Group",
  ## Blind rows -> "Blind" (covers respondents in the original detailed set
  ## AND respondents not in it)
  treat == "blind", "Blind"
)]
firm_long_dt[, conc_alt := factor(conc_alt,
  levels = c("Blind", "Concordant_Group", "Discordant_Group"))]

## ---------- Sanity check: row counts in each cell ----------
cat("\n========== Cell counts: original vs alternative spec ==========\n")
cat("\n-- Original (concordance_race_gender_detailed_blind), HR --\n")
print(firm_long_dt[role == "HR" & resp_race_gender_detailed %in% resp_detailed_set,
                   .N, by = concordance_race_gender_detailed_blind])
cat("\n-- Alternative (conc_alt), HR (all respondents) --\n")
print(firm_long_dt[role == "HR", .N, by = conc_alt])
cat("\n-- Original, Engineer --\n")
print(firm_long_dt[role == "Eng" & resp_race_gender_detailed %in% resp_detailed_set,
                   .N, by = concordance_race_gender_detailed_blind])
cat("\n-- Alternative, Engineer --\n")
print(firm_long_dt[role == "Eng", .N, by = conc_alt])

## ---------- Run regressions: original vs alternative ----------
fit_one <- function(d, lhs_name, conc_var) {
  feols(
    as.formula(sprintf(
      "%s ~ i(%s, ref = 'Blind') + resp_age + I(resp_age^2) + I(resp_age^3) | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income",
      lhs_name, conc_var)),
    data = d, vcov = ~ responseid)
}

flip <- function(m) { m$coefficients <- -1 * m$coefficients; m }

extract_coef <- function(m, conc_var) {
  cf <- coef(m)
  se <- sqrt(diag(vcov(m)))
  conc_name <- paste0(conc_var, "::Concordant_Group")
  disc_name <- paste0(conc_var, "::Discordant_Group")
  data.frame(
    Concordant_est = if (conc_name %in% names(cf)) cf[conc_name] else NA_real_,
    Concordant_se  = if (conc_name %in% names(cf)) se[conc_name] else NA_real_,
    Discordant_est = if (disc_name %in% names(cf)) cf[disc_name] else NA_real_,
    Discordant_se  = if (disc_name %in% names(cf)) se[disc_name] else NA_real_,
    N = nobs(m)
  )
}

cat("\n========== HR: Overall (Z) coefficients, signs flipped ==========\n")
m_orig_hr <- fit_one(firm_long_dt[role == "HR" & resp_race_gender_detailed %in% resp_detailed_set],
                    "sc_overall_z", "concordance_race_gender_detailed_blind") |> flip()
m_alt_hr  <- fit_one(firm_long_dt[role == "HR"],
                    "sc_overall_z", "conc_alt") |> flip()

res_hr <- rbind(
  cbind(spec = "Original (drop Korean/Japanese)", extract_coef(m_orig_hr, "concordance_race_gender_detailed_blind")),
  cbind(spec = "Alternative (keep, always discordant)", extract_coef(m_alt_hr, "conc_alt"))
)
print(res_hr, row.names = FALSE)

cat("\n========== Engineer: Overall (Z) coefficients, signs flipped ==========\n")
m_orig_eng <- fit_one(firm_long_dt[role == "Eng" & resp_race_gender_detailed %in% resp_detailed_set],
                     "sc_overall_z", "concordance_race_gender_detailed_blind") |> flip()
m_alt_eng  <- fit_one(firm_long_dt[role == "Eng"],
                     "sc_overall_z", "conc_alt") |> flip()

res_eng <- rbind(
  cbind(spec = "Original (drop Korean/Japanese)", extract_coef(m_orig_eng, "concordance_race_gender_detailed_blind")),
  cbind(spec = "Alternative (keep, always discordant)", extract_coef(m_alt_eng, "conc_alt"))
)
print(res_eng, row.names = FALSE)

cat("\n========== Unique respondents added (alt - orig) ==========\n")
n_orig_hr  <- uniqueN(firm_long_dt[role == "HR"  & resp_race_gender_detailed %in% resp_detailed_set, responseid])
n_alt_hr   <- uniqueN(firm_long_dt[role == "HR"  & !is.na(conc_alt), responseid])
n_orig_eng <- uniqueN(firm_long_dt[role == "Eng" & resp_race_gender_detailed %in% resp_detailed_set, responseid])
n_alt_eng  <- uniqueN(firm_long_dt[role == "Eng" & !is.na(conc_alt), responseid])
cat(sprintf("HR  : original = %d, alt = %d, diff = %d\n", n_orig_hr,  n_alt_hr,  n_alt_hr  - n_orig_hr))
cat(sprintf("Eng : original = %d, alt = %d, diff = %d\n", n_orig_eng, n_alt_eng, n_alt_eng - n_orig_eng))
