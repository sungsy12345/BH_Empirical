## Verify §1.2 Engineer-side Hispanic_Male coefficient at k=1.
## Replicate the exact spec from §1.2, print the raw coefficient,
## the sign-flipped "blinding effect" value, and cross-check against
## the empirical selection rates in each arm.

rm(list = ls())
date <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
library(pacman)
p_load(haven, here, base, readr, data.table, stringr, fixest)
options(tigris_use_cache = TRUE); setwd(here())
source(here("1_Codes", "2_Import.R")); source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R")); source(here("1_Codes", "5_Cleaning.R"))
source(here("1_Codes", "6_Hiring_Simulation.R"))

eng_dt <- firm_long_dt[role == "Eng"]

cat("\n========= EXACT §1.2 Engineer regression for k=1 =========\n")
cat("\nformula:\n")
cat("  eng_top1 ~ i(race_gender, ref = 'Blind') + resp_age |\n")
cat("            q_version + resp_gender + resp_race + s2_educ + s2_income +\n")
cat("            resp_python + resp_java\n")
cat("vcov: ~ responseid\n\n")

m_eng_k1 <- feols(eng_top1 ~ i(race_gender, ref = 'Blind') + resp_age |
                    q_version + resp_gender + resp_race + s2_educ + s2_income +
                    resp_python + resp_java,
                  data = eng_dt, vcov = ~ responseid)

ct <- coeftable(m_eng_k1)
cat("--- Raw coefficient table (no sign flip yet) ---\n")
print(ct)

cat("\n--- Hispanic_Male specifically ---\n")
hm_raw <- ct["race_gender::Hispanic_Male", "Estimate"]
hm_se  <- ct["race_gender::Hispanic_Male", "Std. Error"]
hm_p   <- ct["race_gender::Hispanic_Male", 4]
cat(sprintf("  RAW coef on race_gender::Hispanic_Male       = %+.4f (SE %.4f, p=%.3f)\n",
            hm_raw, hm_se, hm_p))
cat(sprintf("  Sign-flipped (= 'blinding effect' as plotted) = %+.4f\n", -hm_raw))
cat("  Interpretation:\n")
cat("    raw POSITIVE => HM nonblind resumes selected MORE often than blind resumes\n")
cat("                    (i.e., showing the HM identity HELPS selection)\n")
cat("                    => flipping gives NEGATIVE 'blinding effect' (blinding HURTS HM)\n")
cat("    raw NEGATIVE => HM nonblind resumes selected LESS often than blind\n")
cat("                    => flipping gives POSITIVE 'blinding effect' (blinding HELPS HM)\n")

cat("\n--- Empirical selection rates by arm (cross-check, no controls) ---\n")
emp <- eng_dt[, .(p_eng_top1 = mean(eng_top1, na.rm = TRUE),
                  n          = .N),
              by = .(race_gender)]
emp[, race_gender := factor(race_gender, levels = c("Blind", "White_Male", "White_Female",
                                                     "Asian_Male", "Asian_Female",
                                                     "Hispanic_Male", "Hispanic_Female"))]
setorder(emp, race_gender)
print(emp)

cat("\nDifferences vs blind baseline (raw, no controls):\n")
blind_mean <- emp[race_gender == "Blind", p_eng_top1]
emp_diff <- emp[race_gender != "Blind",
                .(race_gender, raw_diff_vs_blind = p_eng_top1 - blind_mean)]
print(emp_diff)

cat("\nIs the +0.0xx 'blinding effect' for HM in §1.2 plot consistent with the regression?\n")
cat(sprintf("  Plot value (sign-flipped from regression): %+.4f\n", -hm_raw))
cat(sprintf("  Plot value sign POSITIVE? : %s\n", ifelse(-hm_raw > 0, "YES", "NO")))
cat("\n--- DONE ---\n")
