## Treatment-effect heterogeneity by candidate quality.
## Mirrors §2 demographic-cell regression but split by GPA tier (Low/Mid/Top)
## and coding tier (Bottom half / Top half on lab test_case).
## Specification (per subsample, per role):
##   I(-sc_overall_z) ~ i(race_gender, ref='Blind') + resp_age + I(resp_age^2) + I(resp_age^3)
##                      | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income
## LHS sign-flipped so positive => effect of blinding raises the score.
## Standard errors clustered by responseid.

suppressPackageStartupMessages({
  library(pacman)
  p_load(haven, here, base, readr, data.table, stringr, labelled,
         tidyverse, dplyr, fixest, knitr)
})
options(readr.show_types = FALSE)
setwd(here())
date      <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
source(here("1_Codes", "2_Import.R"))
source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R"))
source(here("1_Codes", "5_Cleaning.R"))

dt_hr  <- firm_long_dt[role == "HR"]
dt_eng <- firm_long_dt[role == "Eng"]

groups <- c("White_Male", "White_Female",
            "Asian_Male", "Asian_Female",
            "Hispanic_Male", "Hispanic_Female")

fit_demo <- function(d) {
  suppressMessages(suppressWarnings(feols(
    I(-sc_overall_z) ~ i(race_gender, ref = "Blind") +
                       resp_age + I(resp_age^2) + I(resp_age^3)
    | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income,
    data = d, vcov = ~ responseid
  )))
}

extract_te <- function(fit) {
  ct <- coeftable(fit); rn <- rownames(ct)
  out <- data.table(group = groups, est = NA_real_, se = NA_real_, p = NA_real_)
  for (g in groups) {
    rname <- paste0("race_gender::", g)
    if (rname %in% rn) {
      out[group == g, est := ct[rname, "Estimate"]]
      out[group == g, se  := ct[rname, "Std. Error"]]
      out[group == g, p   := ct[rname, "Pr(>|t|)"]]
    }
  }
  out[, n := fit$nobs]
  out
}

stars <- function(p) ifelse(is.na(p), "",
                     ifelse(p < .01, "***",
                     ifelse(p < .05, "**",
                     ifelse(p < .10, "*", ""))))
fmt <- function(est, se, p) sprintf("%+7.3f (%5.3f)%-3s", est, se, stars(p))

print_block <- function(label, dt) {
  cat(sprintf("\n--- %s (N = %d) ---\n", label, dt$n[1]))
  for (g in groups) {
    row <- dt[group == g]
    cat(sprintf("  %-18s %s\n", gsub("_", " ", g),
                fmt(row$est, row$se, row$p)))
  }
}

run_split <- function(role_name, d, split_var, split_levels, split_lbl) {
  cat(sprintf("\n\n#### %s | split by %s ####\n", role_name, split_lbl))
  for (lv in split_levels) {
    sub <- d[get(split_var) == lv | (split_var == "top_half_coder" & get(split_var) == lv)]
    if (nrow(sub) == 0) {
      cat(sprintf("  [%s = %s]: no observations\n", split_var, lv))
      next
    }
    fit <- fit_demo(sub)
    te  <- extract_te(fit)
    print_block(paste0(split_var, " = ", lv), te)
  }
}

cat("\n#######################################################\n")
cat("§2-style demographic TE by GPA tier (gpa_tier3)\n")
cat("Cells: White Male / Female, Asian Male / Female, Hispanic Male / Female\n")
cat("#######################################################\n")

for (role_name in c("HR", "Eng")) {
  d <- if (role_name == "HR") dt_hr else dt_eng
  cat(sprintf("\n==== ROLE: %s ====", role_name))
  for (lv in c("Low_GPA", "Mid_GPA", "Top_GPA")) {
    sub <- d[gpa_tier3 == lv]
    if (nrow(sub) == 0) next
    fit <- fit_demo(sub)
    te  <- extract_te(fit)
    print_block(paste0("GPA = ", lv), te)
  }
}

cat("\n\n#######################################################\n")
cat("§2-style demographic TE by Coding tier (top_half_coder)\n")
cat("#######################################################\n")

for (role_name in c("HR", "Eng")) {
  d <- if (role_name == "HR") dt_hr else dt_eng
  cat(sprintf("\n==== ROLE: %s ====", role_name))
  for (lv in c(0, 1)) {
    sub <- d[top_half_coder == lv]
    if (nrow(sub) == 0) next
    fit <- fit_demo(sub)
    te  <- extract_te(fit)
    lbl <- ifelse(lv == 1, "Coding = Top half", "Coding = Bottom half")
    print_block(lbl, te)
  }
}

cat("\nDone.\n")
