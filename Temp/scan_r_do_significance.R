## Diagnostic: does r_do (display order) FE matter?
## Refit the §2 main HR and Eng regressions:
##   (A) baseline FE block (with r_do)
##   (B) FE block without r_do
##   (C) r_do as i() on RHS for an F-test on the 17 dummies
## Also extract absorbed FE coefficients in (A) and report distribution.

library(pacman)
p_load(haven, here, base, readr, data.table, stringr, labelled,
       tidyverse, dplyr, fixest)

setwd(here())
options(readr.show_types = FALSE)
date      <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"

source(here("1_Codes", "2_Import.R"))
source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R"))
source(here("1_Codes", "5_Cleaning.R"))

cat("\n#####################################################\n")
cat("# r_do FE diagnostic on the §2 main regression\n")
cat("#####################################################\n")

run_diag <- function(role_lbl, dt) {
  cat("\n========== Role:", role_lbl, "==========\n")

  ## (A) Baseline: r_do in FE block
  m_A <- feols(sc_overall_z ~ i(race_gender, ref = "Blind") +
                              resp_age + I(resp_age^2) + I(resp_age^3)
               | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income,
               data = dt, vcov = ~ responseid)

  ## (B) Drop r_do from FE
  m_B <- feols(sc_overall_z ~ i(race_gender, ref = "Blind") +
                              resp_age + I(resp_age^2) + I(resp_age^3)
               | resume_index + resp_gender + resp_race + s2_educ + s2_income,
               data = dt, vcov = ~ responseid)

  ## (C) r_do as i() on RHS to recover joint F-test on 17 dummies
  m_C <- feols(sc_overall_z ~ i(race_gender, ref = "Blind") +
                              i(r_do, ref = 1) +
                              resp_age + I(resp_age^2) + I(resp_age^3)
               | resume_index + resp_gender + resp_race + s2_educ + s2_income,
               data = dt, vcov = ~ responseid)

  cat("\n--- Adj R^2 comparison ---\n")
  cat(sprintf("(A) FE includes r_do        : adj R^2 = %.5f, within R^2 = %.5f\n",
              fitstat(m_A, "ar2", simplify = TRUE),
              fitstat(m_A, "wr2", simplify = TRUE)))
  cat(sprintf("(B) FE drops r_do           : adj R^2 = %.5f, within R^2 = %.5f\n",
              fitstat(m_B, "ar2", simplify = TRUE),
              fitstat(m_B, "wr2", simplify = TRUE)))
  cat(sprintf("(C) r_do as i() on RHS      : adj R^2 = %.5f, within R^2 = %.5f\n",
              fitstat(m_C, "ar2", simplify = TRUE),
              fitstat(m_C, "wr2", simplify = TRUE)))

  ## Joint Wald test of r_do dummies (17 of them)
  rdo_terms <- grep("^r_do::", names(coef(m_C)), value = TRUE)
  cat(sprintf("\n--- Wald test: all %d r_do dummies = 0 (model C) ---\n", length(rdo_terms)))
  W <- wald(m_C, keep = "^r_do::", print = FALSE)
  print(W)

  ## Distribution of absorbed r_do FE coefficients (model A)
  fe_A <- fixef(m_A)$r_do
  cat("\n--- Absorbed r_do FE coefficients (model A), summary ---\n")
  print(round(summary(as.numeric(fe_A)), 4))
  cat(sprintf("SD across r_do FEs:        %.4f\n", sd(as.numeric(fe_A))))
  cat(sprintf("Range across r_do FEs:     %.4f\n", diff(range(as.numeric(fe_A)))))
  cat(sprintf("(For reference, sc_overall_z is z-scored so SD = 1)\n"))

  invisible(NULL)
}

run_diag("HR",  firm_long_dt[role == "HR"])
run_diag("Eng", firm_long_dt[role == "Eng"])

cat("\n--- Done ---\n")
