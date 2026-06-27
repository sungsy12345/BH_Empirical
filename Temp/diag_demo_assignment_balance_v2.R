## Re-do the demographic-quality balance diagnostic with the right
## "unique resumes" definition: count the # of distinct resume_index
## values (1-18) that ever appear with each demo across the 7 nonblind
## resume_vers. Also report the resume_index x demo crosstab so the
## structure of the assignment is visible.

rm(list = ls())
date <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
library(pacman)
p_load(haven, here, base, readr, data.table, stringr, fixest)
options(tigris_use_cache = TRUE); setwd(here())
source(here("1_Codes", "2_Import.R")); source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R")); source(here("1_Codes", "5_Cleaning.R"))
source(here("1_Codes", "6_Hiring_Simulation.R"))

firm_long_dt[, true_coding_z := (overall_score - mean(overall_score, na.rm=TRUE)) /
                                  sd(overall_score, na.rm=TRUE)]

cat("\n--- Per assigned-demo (Eng nonblind): unique resume_index values + mean true_coding_z ---\n")
combo_eng <- unique(firm_long_dt[role=="Eng" & treat=="nonblind",
                                  .(resume_index, resume_ver, race_gender, true_coding_z)])
combo_eng[, race_gender := factor(race_gender,
            levels = c("White_Male","White_Female","Asian_Male","Asian_Female",
                       "Hispanic_Male","Hispanic_Female"))]

eng_summary <- combo_eng[, .(
  n_unique_students     = uniqueN(resume_index),
  n_stimulus_presentations = .N,
  n_versions_used       = uniqueN(resume_ver),
  mean_true_z_unweighted_by_student = mean(unique(.SD[, .(resume_index, true_coding_z)])$true_coding_z),
  mean_true_z_weighted_by_combo     = mean(true_coding_z, na.rm = TRUE)
), by = race_gender]
setorder(eng_summary, race_gender)
print(eng_summary)

cat("\n--- Per assigned-demo (HR nonblind): same ---\n")
combo_hr <- unique(firm_long_dt[role=="HR" & treat=="nonblind",
                                 .(resume_index, resume_ver, race_gender, true_coding_z)])
combo_hr[, race_gender := factor(race_gender,
           levels = c("White_Male","White_Female","Asian_Male","Asian_Female",
                      "Hispanic_Male","Hispanic_Female"))]

hr_summary <- combo_hr[, .(
  n_unique_students     = uniqueN(resume_index),
  n_stimulus_presentations = .N,
  n_versions_used       = uniqueN(resume_ver),
  mean_true_z_unweighted_by_student = mean(unique(.SD[, .(resume_index, true_coding_z)])$true_coding_z),
  mean_true_z_weighted_by_combo     = mean(true_coding_z, na.rm = TRUE)
), by = race_gender]
setorder(hr_summary, race_gender)
print(hr_summary)

cat("\n--- Crosstab (resume_index x assigned race_gender), nonblind, both roles ---\n")
ct <- unique(firm_long_dt[treat=="nonblind", .(resume_index, resume_ver, race_gender)])
ct_wide <- dcast(ct, resume_index ~ race_gender, value.var = "resume_ver",
                  fun.aggregate = function(x) paste(sort(unique(x)), collapse=","))
print(ct_wide)
