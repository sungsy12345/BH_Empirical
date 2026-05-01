## Diagnostic: how thin are the firm_targets_{white, male} cells?
## Specifically: how many firm-respondents (and resume rows) identify
## each match interaction in 8B §3.2?
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

cat("\n========== firm_long_dt sample sizes ==========\n")
print(firm_long_dt[, .(rows = .N,
                       respondents = uniqueN(responseid)),
                   by = .(role, treat)])

## ---------- firm-level: how many *respondents* select each target ----------
## Each respondent answers s5_policy_diversity once, but firm_long_dt is the
## resume-long version, so dedupe to 1 row per responseid for firm-level rates.
firm_resp <- unique(firm_long_dt[, .(responseid, role, treat,
                                     firm_targets_white,
                                     firm_targets_male,
                                     firm_targets_female,
                                     firm_targets_asian,
                                     firm_targets_hispanic,
                                     firm_targets_black)])

cat("\n========== Firm-side targets: % of respondents = 1 (by role x treat) ==========\n")
firm_rates <- firm_resp[, .(
  N            = .N,
  pct_white    = mean(firm_targets_white    == 1L, na.rm = TRUE) * 100,
  pct_male     = mean(firm_targets_male     == 1L, na.rm = TRUE) * 100,
  pct_female   = mean(firm_targets_female   == 1L, na.rm = TRUE) * 100,
  pct_asian    = mean(firm_targets_asian    == 1L, na.rm = TRUE) * 100,
  pct_hispanic = mean(firm_targets_hispanic == 1L, na.rm = TRUE) * 100,
  pct_black    = mean(firm_targets_black    == 1L, na.rm = TRUE) * 100
), by = .(role, treat)]
print(firm_rates)

cat("\n========== Counts: # of respondents (not %) selecting each target ==========\n")
firm_counts <- firm_resp[, .(
  N            = .N,
  n_white      = sum(firm_targets_white    == 1L, na.rm = TRUE),
  n_male       = sum(firm_targets_male     == 1L, na.rm = TRUE),
  n_female     = sum(firm_targets_female   == 1L, na.rm = TRUE),
  n_asian      = sum(firm_targets_asian    == 1L, na.rm = TRUE),
  n_hispanic   = sum(firm_targets_hispanic == 1L, na.rm = TRUE),
  n_black      = sum(firm_targets_black    == 1L, na.rm = TRUE)
), by = .(role, treat)]
print(firm_counts)

## ---------- match-interaction identification cells ----------
## How many resume rows fire each appl_X:firm_targets_X interaction?
firm_long_dt[, appl_white    := as.integer(treat == "nonblind" & race   == "White")]
firm_long_dt[, appl_male     := as.integer(treat == "nonblind" & gender == "Male")]
firm_long_dt[, appl_female   := as.integer(treat == "nonblind" & gender == "Female")]
firm_long_dt[, appl_asian    := as.integer(treat == "nonblind" & race   == "Asian")]
firm_long_dt[, appl_hispanic := as.integer(treat == "nonblind" & race   == "Hispanic")]

cell_id <- function(d, applvar, targetvar) {
  d[, .(rows_appl_eq_1   = sum(get(applvar) == 1L, na.rm = TRUE),
        rows_target_eq_1 = sum(get(targetvar) == 1L, na.rm = TRUE),
        rows_match_cell  = sum(get(applvar) == 1L & get(targetvar) == 1L, na.rm = TRUE),
        resp_match_cell  = uniqueN(responseid[get(applvar) == 1L & get(targetvar) == 1L])),
    by = role]
}

cat("\n========== Top-table match cells: appl_X = 1 & firm_targets_X = 1 ==========\n")
cat("\n-- White x White --\n");      print(cell_id(firm_long_dt, "appl_white",    "firm_targets_white"))
cat("\n-- Male x Male --\n");        print(cell_id(firm_long_dt, "appl_male",     "firm_targets_male"))
cat("\n-- Female x Female --\n");    print(cell_id(firm_long_dt, "appl_female",   "firm_targets_female"))
cat("\n-- Asian x Asian --\n");      print(cell_id(firm_long_dt, "appl_asian",    "firm_targets_asian"))
cat("\n-- Hispanic x Hispanic --\n");print(cell_id(firm_long_dt, "appl_hispanic", "firm_targets_hispanic"))

## ---------- White-targeting firms: are they unusual? ----------
cat("\n========== Co-targeting with White: what else do these firms target? ==========\n")
white_firms <- firm_resp[firm_targets_white == 1L]
cat("\nFirms with firm_targets_white = 1:\n")
print(white_firms[, .(role, treat,
                      male=firm_targets_male, female=firm_targets_female,
                      asian=firm_targets_asian, hispanic=firm_targets_hispanic,
                      black=firm_targets_black)])
cat("\nAmong White-targeting firms, average # of *other* target dimensions selected:\n")
print(white_firms[, .(N_white_targeting = .N,
                      avg_other_targets =
                        mean(firm_targets_male + firm_targets_female +
                             firm_targets_asian + firm_targets_hispanic +
                             firm_targets_black, na.rm = TRUE)),
                  by = role])

## ---------- Male-targeting firms: same diagnostic ----------
cat("\n========== Co-targeting with Male: what else do these firms target? ==========\n")
male_firms <- firm_resp[firm_targets_male == 1L]
cat("\nAmong Male-targeting firms, average # of *other* target dimensions selected:\n")
print(male_firms[, .(N_male_targeting = .N,
                     avg_other_targets =
                       mean(firm_targets_white + firm_targets_female +
                            firm_targets_asian + firm_targets_hispanic +
                            firm_targets_black, na.rm = TRUE),
                     pct_male_only =
                       mean(firm_targets_male == 1L &
                            firm_targets_white == 0L & firm_targets_female == 0L &
                            firm_targets_asian == 0L & firm_targets_hispanic == 0L &
                            firm_targets_black == 0L, na.rm = TRUE) * 100),
                  by = role])
