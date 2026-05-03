## Phase 3: Inter-departmental backlash / compensation regressions.
##
## Engineer backlash hypothesis:
##   eng_perceives_hr_overreach = aligndiffeng_1 OR aligndiffeng_4
##   (Engineer marked "HR cares more about race DEI" OR "HR cares more about
##   gender DEI"). Tested on Engineer sample only.
##
## HR compensation hypothesis:
##   hr_sees_eng_apathy = s5_engineer_care in {0, 1}
##   (HR rates Eng's DEI prioritization as "Not at all" or "Very little").
##   Tested on HR sample only.
##
## Per-cell TE: beta_D + beta_{D x perception=1} for the perception=1 bucket
##              beta_D                            for the perception=0 bucket
## Joint Wald test: all race_gender x perception coefs = 0.
##
## Pooled spec, two-way cluster on responseid + resume_index, linear age,
## same FE block as §6 final.

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

## Merge perception variables onto firm_long_dt.
perc_vars <- c("s5_view_align", "s5_view_aligndiffeng", "s5_view_aligndiffhr",
               "s5_hr_influence", "s5_engineer_care",
               grep("^aligndiff(hr|eng)_", names(firm_dt), value = TRUE))
fld <- merge(firm_long_dt,
             firm_dt[, c("responseid", perc_vars), with = FALSE],
             by = "responseid", all.x = TRUE)

## ---- Construct perception indicators ------------------------------------
fld[, eng_perceives_hr_overreach := as.integer(
        (!is.na(aligndiffeng_1) & aligndiffeng_1 == 1) |
        (!is.na(aligndiffeng_4) & aligndiffeng_4 == 1))]
fld[, hr_sees_eng_apathy := as.integer(
        !is.na(s5_engineer_care) & s5_engineer_care %in% c(0, 1))]

## URM-axis factor for cleaner URM-premium framing.
fld[, urm_axis_blind := fcase(
       treat == "blind",                                                            "Blind",
       treat == "nonblind" & race_gender %in% c("White_Male", "Asian_Male"),         "Nonblind_MaleNonHisp",
       treat == "nonblind" & race_gender %in% c("White_Female", "Asian_Female"),     "Nonblind_FemaleNonHisp",
       treat == "nonblind" & race_gender %in% c("Hispanic_Male", "Hispanic_Female"), "Nonblind_Hispanic",
       default = NA_character_)]
fld[, urm_axis_blind := factor(urm_axis_blind,
       levels = c("Blind", "Nonblind_MaleNonHisp", "Nonblind_FemaleNonHisp", "Nonblind_Hispanic"))]
fld[, race_gender := factor(race_gender,
       levels = c("Blind", "White_Male", "White_Female",
                  "Asian_Male", "Asian_Female",
                  "Hispanic_Male", "Hispanic_Female"))]

## ---- Bucket sizes -------------------------------------------------------
cat("\n========================================================\n")
cat("Phase 3: Perception heterogeneity regressions\n")
cat("========================================================\n")

cat("\n--- Engineer sample: eng_perceives_hr_overreach ---\n")
print(unique(fld[role == "Eng", .(responseid, eng_perceives_hr_overreach)])[
  , .N, by = eng_perceives_hr_overreach])

cat("\n--- HR sample: hr_sees_eng_apathy ---\n")
print(unique(fld[role == "HR", .(responseid, hr_sees_eng_apathy)])[
  , .N, by = hr_sees_eng_apathy])

## ---- Fit + report helper -----------------------------------------------
fit_perception <- function(d, factor_col, perc_col) {
  fml <- as.formula(sprintf(
    "I(-sc_overall_z) ~ %s * %s + resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income",
    factor_col, perc_col))
  suppressMessages(suppressWarnings(
    feols(fml, data = d, vcov = ~ responseid + resume_index)
  ))
}

extract_te <- function(model, factor_col, perc_col,
                       demo_levels_no_blind, demo_labels) {
  cf <- coef(model);  V <- vcov(model)
  np <- length(cf);   cf_names <- names(cf)
  rows <- list()
  for (i in seq_along(demo_levels_no_blind)) {
    for (p in c(0, 1)) {
      d <- demo_levels_no_blind[i]
      v <- numeric(np)
      idx_main <- match(paste0(factor_col, d), cf_names)
      if (!is.na(idx_main)) v[idx_main] <- 1
      if (p == 1) {
        idx_int <- match(paste0(factor_col, d, ":", perc_col), cf_names)
        if (!is.na(idx_int)) v[idx_int] <- 1
      }
      est <- sum(v * cf); se <- sqrt(c(v %*% V %*% v))
      pv  <- 2 * pnorm(-abs(est / se))
      rows[[length(rows) + 1L]] <- data.frame(
        demo = demo_labels[i],
        bucket = ifelse(p == 0, "Perc=0 (no rebellion)", "Perc=1 (rebellion)"),
        beta = -est, se = se, p = pv,  # flip back to "effect of blinding raises score"
        stringsAsFactors = FALSE)
    }
  }
  out <- do.call(rbind, rows)
  out$stars <- ifelse(out$p < 0.01, "***",
               ifelse(out$p < 0.05, "** ",
               ifelse(out$p < 0.10, "*  ", "")))
  out
}

joint_test <- function(model, factor_col, perc_col, demo_levels_no_blind) {
  ## Wald test: all race_gender x perception interaction coefs = 0.
  int_pat <- paste0("^", factor_col, "(", paste(demo_levels_no_blind, collapse = "|"),
                    "):", perc_col, "$")
  cat(sprintf("\nJoint Wald test (interaction == 0) for %s x %s: ",
              factor_col, perc_col))
  ans <- tryCatch(
    wald(model, keep = int_pat, print = FALSE),
    error = function(e) NULL)
  if (is.null(ans)) cat("[wald failed]\n") else {
    cat(sprintf("F = %.3f, p = %.4f, df1 = %d, df2 = %d\n",
                ans$stat, ans$p, ans$df1, ans$df2))
  }
}

run_panel <- function(role_lbl, perc_col, role_data, factor_col, levels, labels) {
  cat(sprintf("\n--- %s | %s x %s ---\n", role_lbl, factor_col, perc_col))
  m <- fit_perception(role_data, factor_col, perc_col)
  out <- extract_te(m, factor_col, perc_col, levels, labels)
  print(out, row.names = FALSE, digits = 3)
  joint_test(m, factor_col, perc_col, levels)
  invisible(out)
}

## ---- Run regressions ----------------------------------------------------
e_dt <- fld[role == "Eng"]
h_dt <- fld[role == "HR"]

rg_levels <- c("White_Male", "White_Female", "Asian_Male", "Asian_Female",
               "Hispanic_Male", "Hispanic_Female")
rg_labels <- c("White Male", "White Female", "Asian Male", "Asian Female",
               "Hispanic Male", "Hispanic Female")
ax_levels <- c("Nonblind_MaleNonHisp", "Nonblind_FemaleNonHisp", "Nonblind_Hispanic")
ax_labels <- c("Male (NH)", "Female (NH)", "Hispanic")

cat("\n=== ENGINEER BACKLASH HYPOTHESIS ===\n")
run_panel("Eng", "eng_perceives_hr_overreach", e_dt, "race_gender",    rg_levels, rg_labels)
run_panel("Eng", "eng_perceives_hr_overreach", e_dt, "urm_axis_blind", ax_levels, ax_labels)

cat("\n=== HR COMPENSATION HYPOTHESIS ===\n")
run_panel("HR",  "hr_sees_eng_apathy",         h_dt, "race_gender",    rg_levels, rg_labels)
run_panel("HR",  "hr_sees_eng_apathy",         h_dt, "urm_axis_blind", ax_levels, ax_labels)

cat("\n--- Done ---\n")
