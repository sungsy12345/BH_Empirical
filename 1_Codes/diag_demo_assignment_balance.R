## diag_demo_assignment_balance.R
##
## Hypothesis: the §1.2 selection-effect result for HM (positive blinding)
## and HF (mildly positive, contradicting §1.1.x) is driven by *non-balance*
## of student-quality across demographic-assignment cells across the 7
## nonblind resume_vers. §1.1.x has resume_index FE so it's a clean
## within-student contrast; §1.2 has no resume_index FE so it pools across
## students and any quality imbalance shows up as a fake "demographic" effect.
##
## Two checks:
##   (A) Does mean true_coding_z differ across (race_gender) cells in
##       nonblind? Under perfect balance it should be equal across cells.
##   (B) Re-run §1.2 spec for Eng k=1 WITH resume_index FE added. If the
##       confounding hypothesis is right, the HM positive should shrink or
##       flip, and the HF coefficient should look more consistent with §1.1.

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

cat("\n========================================================\n")
cat(" CHECK A: STUDENT-QUALITY BALANCE ACROSS ASSIGNED DEMOGRAPHIC\n")
cat("========================================================\n")

cat("\n--- Mean true_coding_z by assigned race_gender (nonblind only), Engineer panel ---\n")
eng_nb <- firm_long_dt[role == "Eng" & treat == "nonblind"]
qual <- eng_nb[, .(mean_true_z = mean(true_coding_z, na.rm = TRUE),
                   sd_true_z   = sd(true_coding_z,   na.rm = TRUE),
                   n_obs       = .N,
                   n_unique_resumes = uniqueN(paste(resume_ver, resume_index))),
               by = race_gender]
qual[, race_gender := factor(race_gender, levels = c("White_Male", "White_Female",
                                                      "Asian_Male", "Asian_Female",
                                                      "Hispanic_Male", "Hispanic_Female"))]
setorder(qual, race_gender)
print(qual)

cat("\n  >> Same for HR panel:\n")
hr_nb <- firm_long_dt[role == "HR" & treat == "nonblind"]
qual_hr <- hr_nb[, .(mean_true_z = mean(true_coding_z, na.rm = TRUE),
                     sd_true_z   = sd(true_coding_z,   na.rm = TRUE),
                     n_obs       = .N,
                     n_unique_resumes = uniqueN(paste(resume_ver, resume_index))),
                 by = race_gender]
qual_hr[, race_gender := factor(race_gender, levels = c("White_Male", "White_Female",
                                                         "Asian_Male", "Asian_Female",
                                                         "Hispanic_Male", "Hispanic_Female"))]
setorder(qual_hr, race_gender)
print(qual_hr)

cat("\n--- Compare to blind arm mean true_coding_z (should be ~0 by construction) ---\n")
cat(sprintf("  Eng blind: mean true_coding_z = %.4f, n = %d unique resumes = %d\n",
            mean(firm_long_dt[role=="Eng" & treat=="blind", true_coding_z], na.rm=TRUE),
            nrow(firm_long_dt[role=="Eng" & treat=="blind"]),
            uniqueN(firm_long_dt[role=="Eng" & treat=="blind", paste(resume_ver, resume_index)])))

cat("\n--- Per-resume_index, list which (resume_ver, race_gender) assignments exist (nonblind) ---\n")
combo <- unique(firm_long_dt[treat == "nonblind",
                              .(resume_index, resume_ver, race_gender, true_coding_z)])
combo[, race_gender := factor(race_gender)]
setorder(combo, resume_index, resume_ver)
cat("  >> Mean true_coding_z weighted by # unique combos per cell\n  (i.e., the QUALITY of students who got assigned each demographic label, pooled across versions):\n")
combo_means <- combo[, .(mean_true_z = mean(true_coding_z, na.rm = TRUE),
                         n_unique    = .N), by = race_gender]
combo_means[, race_gender := factor(race_gender, levels = c("White_Male", "White_Female",
                                                             "Asian_Male", "Asian_Female",
                                                             "Hispanic_Male", "Hispanic_Female"))]
setorder(combo_means, race_gender)
print(combo_means)

cat("\n========================================================\n")
cat(" CHECK B: §1.2 ENG K=1 WITH vs WITHOUT resume_index FE\n")
cat("========================================================\n")

eng <- firm_long_dt[role == "Eng"]

cat("\n--- (B1) §1.2 spec EXACT (no resume_index FE) ---\n")
m_b1 <- feols(eng_top1 ~ i(race_gender, ref = 'Blind') + resp_age |
                q_version + resp_gender + resp_race + s2_educ + s2_income +
                resp_python + resp_java,
              data = eng, vcov = ~ responseid)
ct1 <- coeftable(m_b1)
cat("  Raw coefs (negate to get 'blinding effect'):\n")
for (rg in c("White_Male", "White_Female", "Asian_Male", "Asian_Female",
             "Hispanic_Male", "Hispanic_Female")) {
  k <- paste0("race_gender::", rg)
  if (k %in% rownames(ct1)) {
    cat(sprintf("    %-18s  raw = %+.4f (SE %.4f)  -->  flipped = %+.4f\n",
                rg, ct1[k,"Estimate"], ct1[k,"Std. Error"], -ct1[k,"Estimate"]))
  }
}

cat("\n--- (B2) §1.2 spec + resume_index FE (clean within-student contrast) ---\n")
m_b2 <- feols(eng_top1 ~ i(race_gender, ref = 'Blind') + resp_age |
                resume_index + q_version + resp_gender + resp_race + s2_educ + s2_income +
                resp_python + resp_java,
              data = eng, vcov = ~ responseid)
ct2 <- coeftable(m_b2)
cat("  Raw coefs (negate to get 'blinding effect'):\n")
for (rg in c("White_Male", "White_Female", "Asian_Male", "Asian_Female",
             "Hispanic_Male", "Hispanic_Female")) {
  k <- paste0("race_gender::", rg)
  if (k %in% rownames(ct2)) {
    cat(sprintf("    %-18s  raw = %+.4f (SE %.4f)  -->  flipped = %+.4f\n",
                rg, ct2[k,"Estimate"], ct2[k,"Std. Error"], -ct2[k,"Estimate"]))
  }
}

cat("\n--- (B3) Also +r_do FE for completeness (matches old chunk's HR spec) ---\n")
m_b3 <- feols(eng_top1 ~ i(race_gender, ref = 'Blind') + resp_age |
                resume_index + r_do + q_version + resp_gender + resp_race + s2_educ + s2_income +
                resp_python + resp_java,
              data = eng, vcov = ~ responseid)
ct3 <- coeftable(m_b3)
cat("  Raw coefs (negate to get 'blinding effect'):\n")
for (rg in c("White_Male", "White_Female", "Asian_Male", "Asian_Female",
             "Hispanic_Male", "Hispanic_Female")) {
  k <- paste0("race_gender::", rg)
  if (k %in% rownames(ct3)) {
    cat(sprintf("    %-18s  raw = %+.4f (SE %.4f)  -->  flipped = %+.4f\n",
                rg, ct3[k,"Estimate"], ct3[k,"Std. Error"], -ct3[k,"Estimate"]))
  }
}
