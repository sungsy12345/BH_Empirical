## Compare §4.2 / §4.3 estimates with and without r_do FE.
## For a few representative k values per role, fit both specs and tabulate.

library(pacman)
p_load(haven, here, base, readr, data.table, stringr,
       tidyverse, dplyr, fixest, labelled)
setwd(here())
date <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
source(here("1_Codes", "2_Import.R"))
source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R"))
source(here("1_Codes", "5_Cleaning.R"))
source(here("1_Codes", "6_Hiring_Simulation.R"))

ks_to_check <- c(1, 3, 5, 9, 12, 15, 18)

extract <- function(m, label) {
  ct <- coeftable(m)
  if (!"treat::blind" %in% rownames(ct)) return(NULL)
  data.table(
    spec     = label,
    est_main = ct["treat::blind", "Estimate"],
    se_main  = ct["treat::blind", "Std. Error"],
    est_int  = if ("dei_index_z:treat::blind" %in% rownames(ct))
                  ct["dei_index_z:treat::blind", "Estimate"] else NA_real_,
    se_int   = if ("dei_index_z:treat::blind" %in% rownames(ct))
                  ct["dei_index_z:treat::blind", "Std. Error"] else NA_real_
  )
}

run_one <- function(role_lbl, role_filter, pass_col_prefix, eng_extra_fe) {
  out <- list()
  for (k in ks_to_check) {
    pass_col <- paste0(pass_col_prefix, k)
    d        <- firm_long_dt[role == role_filter & get(pass_col) == 1]
    cm       <- d[treat == "nonblind", mean(overall_score, na.rm = TRUE)]
    cs       <- d[treat == "nonblind", sd(overall_score,   na.rm = TRUE)]
    if (is.na(cs) || cs == 0) next
    d[, overall_score_std := (overall_score - cm) / cs]

    base_fe   <- "resp_gender + resp_race + s2_educ + s2_income"
    fe_with    <- paste("r_do + q_version", base_fe, sep = " + ")
    fe_without <- paste("q_version", base_fe, sep = " + ")
    if (eng_extra_fe) {
      fe_with    <- paste(fe_with,    "resp_python + resp_java", sep = " + ")
      fe_without <- paste(fe_without, "resp_python + resp_java", sep = " + ")
    }

    ## §4.2 spec (no DEI interaction)
    m_with_42    <- tryCatch(suppressMessages(suppressWarnings(feols(
      as.formula(paste("overall_score_std ~ i(treat, ref = 'nonblind') + resp_age |", fe_with)),
      data = d, vcov = ~ responseid))), error = function(e) NULL)
    m_without_42 <- tryCatch(suppressMessages(suppressWarnings(feols(
      as.formula(paste("overall_score_std ~ i(treat, ref = 'nonblind') + resp_age |", fe_without)),
      data = d, vcov = ~ responseid))), error = function(e) NULL)

    ## §4.3 spec (with DEI interaction)
    m_with_43    <- tryCatch(suppressMessages(suppressWarnings(feols(
      as.formula(paste("overall_score_std ~ i(treat, ref = 'nonblind') * dei_index_z + resp_age |", fe_with)),
      data = d, vcov = ~ responseid))), error = function(e) NULL)
    m_without_43 <- tryCatch(suppressMessages(suppressWarnings(feols(
      as.formula(paste("overall_score_std ~ i(treat, ref = 'nonblind') * dei_index_z + resp_age |", fe_without)),
      data = d, vcov = ~ responseid))), error = function(e) NULL)

    add <- function(model, sec, drop) {
      r <- extract(model, paste0(sec, "_", ifelse(drop, "no_rdo", "with_rdo")))
      if (is.null(r)) return(NULL)
      r[, role := role_lbl];  r[, k := k];  r[, sec := sec]
      r[, drop_rdo := drop]
      r[, n := nobs(model)]
      r
    }
    out[[length(out)+1]] <- add(m_with_42,    "S42", FALSE)
    out[[length(out)+1]] <- add(m_without_42, "S42", TRUE)
    out[[length(out)+1]] <- add(m_with_43,    "S43", FALSE)
    out[[length(out)+1]] <- add(m_without_43, "S43", TRUE)
  }
  rbindlist(out, fill = TRUE)
}

res <- rbind(
  run_one("HR",  "HR",  "hr_pass", FALSE),
  run_one("Eng", "Eng", "eng_top", TRUE)
)

## Print: side-by-side comparison per (role, k, sec) of with vs without r_do.
res[, est_low  := est_main - 1.96 * se_main]
res[, est_high := est_main + 1.96 * se_main]

wide_main <- dcast(res[sec == "S42"], role + k + n ~ drop_rdo,
                    value.var = c("est_main", "se_main"))
setnames(wide_main, c("role","k","n",
                       "est_with","est_no","se_with","se_no"))
wide_main[, est_delta_pct := round(100 * (est_no - est_with) / abs(est_with), 1)]
wide_main[, se_delta_pct  := round(100 * (se_no  - se_with) / abs(se_with),  1)]

cat("\n==========================================\n")
cat("§4.2 (TE main) -- with r_do vs without r_do\n")
cat("==========================================\n")
print(wide_main, digits = 4)

wide_int <- dcast(res[sec == "S43"], role + k + n ~ drop_rdo,
                   value.var = c("est_main", "se_main", "est_int", "se_int"))
setnames(wide_int, c("role","k","n",
                      "est_main_with","est_main_no",
                      "se_main_with",  "se_main_no",
                      "est_int_with",  "est_int_no",
                      "se_int_with",   "se_int_no"))
wide_int[, est_main_delta_pct := round(100 * (est_main_no - est_main_with) / abs(est_main_with), 1)]
wide_int[, se_main_delta_pct  := round(100 * (se_main_no  - se_main_with)  / abs(se_main_with),  1)]
wide_int[, est_int_delta_pct  := round(100 * (est_int_no  - est_int_with)  / abs(est_int_with),  1)]
wide_int[, se_int_delta_pct   := round(100 * (se_int_no   - se_int_with)   / abs(se_int_with),   1)]

cat("\n==========================================\n")
cat("§4.3 (TE main + interaction) -- with r_do vs without r_do\n")
cat("==========================================\n")
print(wide_int, digits = 4)

cat("\n--- Done ---\n")
