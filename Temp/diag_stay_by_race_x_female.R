## diag_stay_by_race_x_female.R
## Question: Does the assigned-Female effect on sc_stay_z differ across
## race cells -- is White Female penalized less than Asian/Hispanic Female?
## Use *assigned* race_gender (resume_ver), nonblind only.

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
firm_long_dt[, resume_id := paste(resume_ver, resume_index, sep = "_")]

nb <- firm_long_dt[treat == "nonblind"]
nb[, race_gender := factor(race_gender,
                            levels = c("White_Male", "White_Female",
                                       "Asian_Male", "Asian_Female",
                                       "Hispanic_Male", "Hispanic_Female"))]

cat("\n================================================================\n")
cat(" SC_STAY_Z BY ASSIGNED RACE x GENDER, NONBLIND ONLY\n")
cat("================================================================\n")

for (rl in c("HR", "Eng")) {
  cat("\n\n================ Role:", rl, "================\n")
  d <- nb[role == rl]

  ## (A) Each race-gender vs White_Male baseline (level)
  cat("\n--- (A) Each race-gender vs White_Male baseline ---\n")
  cat("    sc_stay_z ~ i(race_gender, ref='White_Male') | resume_index + r_do\n")
  m_a <- feols(sc_stay_z ~ i(race_gender, ref = "White_Male") | resume_index + r_do,
               data = d, vcov = ~ responseid + resume_id)
  print(coeftable(m_a))

  ## (B) Female cells contrast: each Female cell vs the same-race Male cell
  ## i.e., compare White_Female - White_Male, Asian_Female - Asian_Male,
  ## Hispanic_Female - Hispanic_Male using linear combinations.
  cf <- coef(m_a); V <- vcov(m_a)
  cat("\n--- (B) Female-minus-same-race-Male within each race ---\n")
  female_cells <- list(
    "White_Female - White_Male"       = list(pos = "race_gender::White_Female",     neg = NULL),
    "Asian_Female - Asian_Male"       = list(pos = "race_gender::Asian_Female",     neg = "race_gender::Asian_Male"),
    "Hispanic_Female - Hispanic_Male" = list(pos = "race_gender::Hispanic_Female",  neg = "race_gender::Hispanic_Male")
  )
  for (lab in names(female_cells)) {
    pos <- female_cells[[lab]]$pos
    neg <- female_cells[[lab]]$neg
    v <- numeric(length(cf))
    if (pos %in% names(cf)) v[match(pos, names(cf))] <-  1
    if (!is.null(neg) && neg %in% names(cf)) v[match(neg, names(cf))] <- -1
    est <- sum(v * cf); se <- sqrt(c(v %*% V %*% v))
    p   <- 2 * pnorm(-abs(est / se))
    cat(sprintf("    %-35s : %+.4f (SE %.4f), p = %.3f\n", lab, est, se, p))
  }

  ## (C) Female main effect interacted with race (cleaner spec):
  ## Make Race (3 levels: White / Asian / Hispanic) and Female binary.
  d[, race3 := fcase(
    race_gender %in% c("White_Male", "White_Female"),       "White",
    race_gender %in% c("Asian_Male", "Asian_Female"),       "Asian",
    race_gender %in% c("Hispanic_Male", "Hispanic_Female"), "Hispanic")]
  d[, race3 := factor(race3, levels = c("White", "Asian", "Hispanic"))]
  d[, female := as.integer(race_gender %in% c("White_Female", "Asian_Female", "Hispanic_Female"))]

  cat("\n--- (C) Female effect by race (interaction) ---\n")
  cat("    sc_stay_z ~ female * race3 | resume_index + r_do\n")
  m_c <- feols(sc_stay_z ~ female * race3 | resume_index + r_do,
               data = d, vcov = ~ responseid + resume_id)
  print(coeftable(m_c))

  cf <- coef(m_c); V <- vcov(m_c)
  fmain <- "female"
  cat("\n--- (D) Implied Female effect within each race (linear combinations from C) ---\n")
  for (r in c("White", "Asian", "Hispanic")) {
    inter <- paste0("female:race3", r)
    v <- numeric(length(cf))
    if (fmain %in% names(cf)) v[match(fmain, names(cf))] <- 1
    if (r != "White" && inter %in% names(cf)) v[match(inter, names(cf))] <- 1
    est <- sum(v * cf); se <- sqrt(c(v %*% V %*% v))
    p   <- 2 * pnorm(-abs(est / se))
    cat(sprintf("    Female effect in %-9s : %+.4f (SE %.4f), p = %.3f\n", r, est, se, p))
  }

  ## (E) Triple: Female x race x quality
  cat("\n--- (E) Female x race x quality (true_coding_z) interaction ---\n")
  cat("    sc_stay_z ~ female * race3 * true_coding_z | r_do\n")
  m_e <- feols(sc_stay_z ~ female * race3 * true_coding_z | r_do,
               data = d, vcov = ~ responseid + resume_id)
  print(coeftable(m_e))
}

cat("\n================================================================\n")
cat(" END\n")
cat("================================================================\n")
