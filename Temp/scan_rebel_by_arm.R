## Confirm rebellion bucket size by treatment arm.
library(pacman)
p_load(haven, here, base, readr, data.table, stringr, dplyr)
setwd(here())
options(readr.show_types = FALSE)
date <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
source(here("1_Codes", "2_Import.R"))
source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R"))
source(here("1_Codes", "5_Cleaning.R"))

fld <- merge(firm_long_dt,
             firm_dt[, .(responseid, aligndiffeng_1, aligndiffeng_4, s5_engineer_care)],
             by = "responseid", all.x = TRUE)

fld[, eng_perceives_hr_overreach := as.integer(
       (!is.na(aligndiffeng_1) & aligndiffeng_1 == 1) |
       (!is.na(aligndiffeng_4) & aligndiffeng_4 == 1))]
fld[, hr_sees_eng_apathy := as.integer(
       !is.na(s5_engineer_care) & s5_engineer_care %in% c(0, 1))]

cat("\n=== Engineer rebellion bucket sizes by treatment arm ===\n")
e_resp <- unique(fld[role == "Eng", .(responseid, treat, eng_perceives_hr_overreach)])
print(e_resp[, .N, by = .(treat, eng_perceives_hr_overreach)])

cat("\n--- Total Eng respondents by perception ---\n")
print(e_resp[, .N, by = eng_perceives_hr_overreach])

cat("\n=== HR apathy bucket sizes by treatment arm ===\n")
h_resp <- unique(fld[role == "HR", .(responseid, treat, hr_sees_eng_apathy)])
print(h_resp[, .N, by = .(treat, hr_sees_eng_apathy)])

cat("\n--- Total HR respondents by perception ---\n")
print(h_resp[, .N, by = hr_sees_eng_apathy])

cat("\n--- Done ---\n")
