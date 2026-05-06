## diag_stay_by_assigned_demo.R
## Question: Does sc_stay_z (predicted likelihood of staying 5 years) differ
## by ASSIGNED demographic (resume_ver) and by candidate qualification?
## Use the *assigned* race_gender (from resume_ver), NOT the actual student
## demographic (true_race_gender).
## Sample: nonblind arm only (race_gender = "Blind" otherwise).

rm(list = ls())
date <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
library(pacman)
p_load(haven, here, base, readr, data.table, stringr, fixest, modelsummary)
options(tigris_use_cache = TRUE); setwd(here())
source(here("1_Codes", "2_Import.R")); source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R")); source(here("1_Codes", "5_Cleaning.R"))
source(here("1_Codes", "6_Hiring_Simulation.R"))

## Construct true_coding_z (built in §4.1 of the Rmd; replicate here)
firm_long_dt[, true_coding_z := (overall_score - mean(overall_score, na.rm=TRUE)) /
                                  sd(overall_score, na.rm=TRUE)]

## Build assigned-demographic indicators from race_gender (resume_ver-assigned)
firm_long_dt[, female_assigned := as.integer(race_gender %in% c("White_Female", "Asian_Female", "Hispanic_Female"))]
firm_long_dt[, hispanic_assigned := as.integer(race_gender %in% c("Hispanic_Male", "Hispanic_Female"))]
firm_long_dt[, urm_tech_assigned := as.integer(race_gender %in% c("White_Female", "Asian_Female",
                                                                   "Hispanic_Male", "Hispanic_Female"))]

## resume_id for two-way clustering
firm_long_dt[, resume_id := paste(resume_ver, resume_index, sep = "_")]

## Restrict to nonblind (where assigned demographics are visible)
nb <- firm_long_dt[treat == "nonblind"]

cat("\n================================================================\n")
cat(" SC_STAY_Z BY ASSIGNED DEMOGRAPHIC + QUALIFICATION (NONBLIND ONLY)\n")
cat("================================================================\n")

cat("\nSample sizes (nonblind):\n")
print(nb[, .N, by = .(role, race_gender)][order(role, race_gender)])

for (rl in c("HR", "Eng")) {
  cat("\n\n=========== Role:", rl, "===========\n")
  d <- nb[role == rl]

  ## (1) Female effect on sc_stay_z, controlling for resume content via resume_index FE.
  ##     Cluster on responseid + resume_id.
  cat("\n--- (1) Female (assigned) effect on sc_stay_z ---\n")
  cat("    sc_stay_z ~ female_assigned | resume_index + r_do, vcov = ~ responseid + resume_id\n")
  m1 <- feols(sc_stay_z ~ female_assigned | resume_index + r_do,
              data = d, vcov = ~ responseid + resume_id)
  print(coeftable(m1))
  cat(sprintf("    R^2 = %.4f, N = %d\n", fitstat(m1, "r2", verbose=FALSE)$r2, nobs(m1)))

  ## (2) Quality effect on sc_stay_z. Cannot include resume_index FE
  ##     (true_coding_z is constant within resume_index), so include r_do FE only.
  cat("\n--- (2) Quality (true_coding_z) effect on sc_stay_z ---\n")
  cat("    sc_stay_z ~ true_coding_z | r_do, vcov = ~ responseid + resume_id\n")
  m2 <- feols(sc_stay_z ~ true_coding_z | r_do,
              data = d, vcov = ~ responseid + resume_id)
  print(coeftable(m2))
  cat(sprintf("    R^2 = %.4f, N = %d\n", fitstat(m2, "r2", verbose=FALSE)$r2, nobs(m2)))

  ## (3) Female + Female x Quality interaction.
  cat("\n--- (3) Female + Female x Quality interaction on sc_stay_z ---\n")
  cat("    sc_stay_z ~ female_assigned * true_coding_z | r_do, vcov = ~ responseid + resume_id\n")
  m3 <- feols(sc_stay_z ~ female_assigned * true_coding_z | r_do,
              data = d, vcov = ~ responseid + resume_id)
  print(coeftable(m3))
  cat(sprintf("    R^2 = %.4f, N = %d\n", fitstat(m3, "r2", verbose=FALSE)$r2, nobs(m3)))

  ## (4) Each race-gender cell vs White Male baseline (controlling for resume_index).
  cat("\n--- (4) Each assigned race-gender cell vs White_Male baseline ---\n")
  cat("    sc_stay_z ~ i(race_gender, ref='White_Male') | resume_index + r_do\n")
  d2 <- d[!is.na(race_gender) & race_gender != "Blind"]
  d2[, race_gender := factor(race_gender,
                              levels = c("White_Male", "White_Female",
                                         "Asian_Male", "Asian_Female",
                                         "Hispanic_Male", "Hispanic_Female"))]
  m4 <- feols(sc_stay_z ~ i(race_gender, ref = "White_Male") | resume_index + r_do,
              data = d2, vcov = ~ responseid + resume_id)
  print(coeftable(m4))
  cat(sprintf("    R^2 = %.4f, N = %d\n", fitstat(m4, "r2", verbose=FALSE)$r2, nobs(m4)))

  ## (5) Female effect at quality terciles (Low / Mid / High) -- check if
  ##     the alleged "too good for the firm" pattern holds for women.
  cat("\n--- (5) Female effect at quality terciles ---\n")
  qc <- quantile(d$true_coding_z, probs = c(1/3, 2/3), na.rm = TRUE)
  d[, qual_tier := cut(true_coding_z, breaks = c(-Inf, qc[1], qc[2], Inf),
                       labels = c("Low", "Mid", "High"), include.lowest = TRUE)]
  for (tier in c("Low", "Mid", "High")) {
    d_tier <- d[qual_tier == tier]
    if (nrow(d_tier) == 0) next
    m_t <- tryCatch(
      feols(sc_stay_z ~ female_assigned | resume_index + r_do,
            data = d_tier, vcov = ~ responseid + resume_id),
      error = function(e) NULL)
    if (is.null(m_t)) {
      cat(sprintf("    %s tier: regression failed\n", tier)); next
    }
    ct <- coeftable(m_t)
    if ("female_assigned" %in% rownames(ct)) {
      cat(sprintf("    %s-quality tier: Female coef = %+.4f (SE %.4f), p = %.3f, N = %d\n",
                  tier, ct["female_assigned","Estimate"], ct["female_assigned","Std. Error"],
                  ct["female_assigned",4], nobs(m_t)))
    }
  }
}

cat("\n================================================================\n")
cat(" END\n")
cat("================================================================\n")
