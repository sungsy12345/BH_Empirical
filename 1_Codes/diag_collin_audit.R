## diag_collin_audit.R
## Refit every regression that lands in a table in 8B_Stage_BH.Rmd and
## report any variables dropped by fixest for collinearity. Flag any
## silently-dropped FE / regressor that the table claims is in the spec.
suppressPackageStartupMessages({
rm(list = ls())
date <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
library(pacman)
p_load(haven, here, base, readr, data.table, stringr, fixest)
options(tigris_use_cache = TRUE); setwd(here())
source(here("1_Codes", "2_Import.R")); source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R")); source(here("1_Codes", "5_Cleaning.R"))
source(here("1_Codes", "6_Hiring_Simulation.R"))
})

firm_long_dt[, true_coding_z := (overall_score - mean(overall_score, na.rm=TRUE)) /
                                  sd(overall_score, na.rm=TRUE)]
firm_long_dt[, resume_id := paste(resume_ver, resume_index, sep = "_")]
firm_long_dt[, firm_targets_fh := as.integer(firm_targets_female == 1L | firm_targets_hispanic == 1L)]
firm_long_dt[, blind_concordance_race := fcase(
  treat == "blind", "Blind",
  treat == "nonblind" & concordance_race_detailed == 1, "Nonblind_Concordant",
  treat == "nonblind" & concordance_race_detailed == 0, "Nonblind_Nonconcordant",
  default = NA_character_)]
firm_long_dt[, blind_concordance_race := factor(blind_concordance_race,
  levels = c("Blind", "Nonblind_Concordant", "Nonblind_Nonconcordant"))]

audit_one <- function(label, fml_str, data, vcov_arg = ~ responseid, claimed_vars = NULL) {
  m <- tryCatch(
    suppressMessages(suppressWarnings(feols(as.formula(fml_str), data = data, vcov = vcov_arg))),
    error = function(e) {
      cat(sprintf("  [ERROR] %s : %s\n", label, conditionMessage(e)))
      return(NULL)
    })
  if (is.null(m)) return(invisible())
  dropped <- m$collin.var
  if (length(dropped) == 0L) {
    cat(sprintf("  [OK]    %-55s  no collinearity drops\n", label))
  } else {
    cat(sprintf("  [DROP]  %-55s  dropped: %s\n", label, paste(dropped, collapse=", ")))
  }
  if (!is.null(claimed_vars)) {
    bad <- intersect(dropped, claimed_vars)
    if (length(bad) > 0L) {
      cat(sprintf("        ***** TABLE CLAIMS THESE BUT THEY WERE DROPPED: %s\n",
                  paste(bad, collapse=", ")))
    }
  }
}

cat("\n========== §1.1.1 race-gender ==========\n")
audit_one("§1.1.1 HR",
  "sc_overall_z ~ i(race_gender, ref='Blind') + resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income",
  firm_long_dt[role == "HR"])
audit_one("§1.1.1 Eng",
  "sc_overall_z ~ i(race_gender, ref='Blind') + resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income + resp_python + resp_java",
  firm_long_dt[role == "Eng"])

cat("\n========== §1.1.2 concordance ==========\n")
for (rl in c("HR", "Eng")) {
  fe_str <- "resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income"
  if (rl == "Eng") fe_str <- paste0(fe_str, " + resp_python + resp_java")
  audit_one(sprintf("§1.1.2 %s", rl),
    paste0("sc_overall_z ~ i(blind_concordance_race, ref='Blind') + resp_age | ", fe_str),
    firm_long_dt[role == rl])
}

cat("\n========== §1.2 composition / selection (k=1, 6, 12, 18) ==========\n")
for (rl in c("HR", "Eng")) {
  for (k in c(1, 6, 12, 18)) {
    col <- if (rl == "HR") paste0("hr_pass", k) else paste0("eng_top", k)
    fe_str <- "resume_index + q_version + resp_gender + resp_race + s2_educ + s2_income"
    extra_age <- if (rl == "HR") "" else " + resp_age"
    if (rl == "Eng") fe_str <- paste0(fe_str, " + resp_python + resp_java")
    audit_one(sprintf("§1.2 %s k=%d", rl, k),
      sprintf("%s ~ i(race_gender, ref='Blind')%s | %s", col, extra_age, fe_str),
      firm_long_dt[role == rl])
  }
}

cat("\n========== §2.2 signal substitution ==========\n")
hr_signals <- c("gpa_z","num_proj","num_exp","num_lead","num_awards",
                "work_1000","work_1000_twice","work_newstartup","work_research","work_teaching",
                "javascript","c_base","cplusplus","csharp","sql")
eng_only <- c("test_case_z")
audit_one("§2.2 HR",
  paste0("sc_overall_z ~ (", paste(hr_signals, collapse=" + "), ") * i(treat, ref='nonblind') | resume_index + responseid + r_do"),
  firm_long_dt[role == "HR"], vcov_arg = ~ responseid + resume_id)
audit_one("§2.2 Eng",
  paste0("sc_overall_z ~ (", paste(c(hr_signals, eng_only), collapse=" + "), ") * i(treat, ref='nonblind') | resume_index + responseid + r_do"),
  firm_long_dt[role == "Eng"], vcov_arg = ~ responseid + resume_id)

cat("\n========== §2.3 internal rubric ==========\n")
for (rl in c("HR", "Eng")) {
  audit_one(sprintf("§2.3 %s", rl),
    "sc_overall_z ~ (sc_coding_z + sc_fit_z + sc_stay_z) * i(treat, ref='nonblind') | responseid + resume_index + r_do",
    firm_long_dt[role == rl], vcov_arg = ~ responseid + resume_id)
}

cat("\n========== §4 DEI heterogeneity (race-gender simple + triple) ==========\n")
for (rl in c("HR", "Eng")) {
  fe_str <- "resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income"
  if (rl == "Eng") fe_str <- paste0(fe_str, " + resp_python + resp_java")
  audit_one(sprintf("§4 race-gender simple %s", rl),
    paste0("sc_overall_z ~ dei_index_z + i(race_gender, ref='Blind') + i(race_gender, dei_index_z, ref='Blind') + resp_age | ", fe_str),
    firm_long_dt[role == rl])
  audit_one(sprintf("§4 race-gender triple HR firm_targets_fh %s", rl),
    paste0("sc_overall_z ~ dei_index_z + firm_targets_fh + I(dei_index_z * firm_targets_fh) + i(race_gender, ref='Blind') + i(race_gender, dei_index_z, ref='Blind') + i(race_gender, firm_targets_fh, ref='Blind') + i(race_gender, I(dei_index_z * firm_targets_fh), ref='Blind') + resp_age | ", fe_str),
    firm_long_dt[role == rl])
}

cat("\n========== §6.7 Alt A.1 HR ==========\n")
hr_dt <- firm_long_dt[role == "HR"]
audit_one("§6.7 HR Pool",
  "sc_advance ~ true_coding_z * i(treat, ref='nonblind') | responseid + r_do",
  hr_dt, vcov_arg = ~ responseid + resume_id)
audit_one("§6.7 HR DEI",
  "sc_advance ~ true_coding_z * i(treat, ref='nonblind') * dei_index_z | responseid + r_do",
  hr_dt[!is.na(dei_index_z)], vcov_arg = ~ responseid + resume_id)
audit_one("§6.7 HR FT",
  "sc_advance ~ true_coding_z * i(treat, ref='nonblind') * firm_targets_fh | responseid + r_do",
  hr_dt[!is.na(firm_targets_fh)], vcov_arg = ~ responseid + resume_id)
audit_one("§6.7 HR MS",
  "sc_advance ~ true_coding_z * i(treat, ref='nonblind') * misalign_z | responseid + r_do",
  hr_dt[!is.na(misalign_z)], vcov_arg = ~ responseid + resume_id)

cat("\n========== §6.9 Alt B (HR + Eng x 4 specs each) ==========\n")
acc_dt <- firm_long_dt[
  !is.na(sc_overall_z) & !is.na(overall_score),
  .(accuracy = suppressWarnings(cor(sc_overall_z, overall_score, method = "spearman")),
    n_resumes = .N), by = .(responseid, role)]
acc_dt <- acc_dt[n_resumes >= 5 & !is.na(accuracy)]
acc_dt[, accuracy_fz := atanh(pmax(pmin(accuracy, 0.999), -0.999))]
resp_attrs <- unique(firm_long_dt[, .(responseid, treat, dei_index_z, firm_targets_fh,
                                       misalign_z, resp_age, resp_gender, resp_race,
                                       s2_educ, s2_income, q_version, resp_python, resp_java)])
acc_dt <- acc_dt[resp_attrs, on = "responseid", nomatch = NULL]
for (rl in c("HR", "Eng")) {
  fe_str <- "q_version + resp_gender + resp_race + s2_educ + s2_income"
  if (rl == "Eng") fe_str <- paste0(fe_str, " + resp_python + resp_java")
  d <- acc_dt[role == rl]
  audit_one(sprintf("§6.9 %s Pool", rl),
    paste0("accuracy_fz ~ i(treat, ref='nonblind') + resp_age | ", fe_str),
    d, vcov_arg = "hetero")
  audit_one(sprintf("§6.9 %s DEI", rl),
    paste0("accuracy_fz ~ i(treat, ref='nonblind') * dei_index_z + resp_age | ", fe_str),
    d[!is.na(dei_index_z)], vcov_arg = "hetero")
  audit_one(sprintf("§6.9 %s FT", rl),
    paste0("accuracy_fz ~ i(treat, ref='nonblind') * firm_targets_fh + resp_age | ", fe_str),
    d[!is.na(firm_targets_fh)], vcov_arg = "hetero")
  audit_one(sprintf("§6.9 %s MS", rl),
    paste0("accuracy_fz ~ i(treat, ref='nonblind') * misalign_z + resp_age | ", fe_str),
    d[!is.na(misalign_z)], vcov_arg = "hetero")
}

cat("\n========== END OF AUDIT ==========\n")
