## Diagnose: why are Eng panels empty in §8.1-§8.3?
## Inspect coef names for the Engineer triple URM-axis x perc_role x DEI model.
library(pacman)
p_load(haven, here, base, readr, data.table, stringr,
       tidyverse, dplyr, fixest, labelled)
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
fld[, urm_axis_blind := fcase(
       treat == "blind",                                                            "Blind",
       treat == "nonblind" & race_gender %in% c("White_Male", "Asian_Male"),         "Nonblind_MaleNonHisp",
       treat == "nonblind" & race_gender %in% c("White_Female", "Asian_Female"),     "Nonblind_FemaleNonHisp",
       treat == "nonblind" & race_gender %in% c("Hispanic_Male", "Hispanic_Female"), "Nonblind_Hispanic",
       default = NA_character_)]
fld[, urm_axis_blind := factor(urm_axis_blind,
       levels = c("Blind", "Nonblind_MaleNonHisp", "Nonblind_FemaleNonHisp", "Nonblind_Hispanic"))]

dt_eng <- fld[role == "Eng"]
dt_eng[, perc_role := eng_perceives_hr_overreach]

m_eng <- feols(
  sc_overall_z ~ perc_role + dei_index_A_z + I(perc_role * dei_index_A_z) +
    i(urm_axis_blind, ref = "Blind") +
    i(urm_axis_blind, perc_role, ref = "Blind") +
    i(urm_axis_blind, dei_index_A_z, ref = "Blind") +
    i(urm_axis_blind, I(perc_role * dei_index_A_z), ref = "Blind") +
    resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income,
  data = dt_eng, vcov = ~ responseid + resume_index)

cat("\n========== ENGINEER TRIPLE MODEL: COEF NAMES ==========\n")
print(names(coef(m_eng)))
cat("\n========== Full coef summary ==========\n")
print(coef(m_eng))
cat("\n========== NA coefs in fixest object ==========\n")
print(m_eng$collin.var)
cat("\n========== Done ==========\n")
