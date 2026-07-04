## ISOLATED SIDE-WORK (part 2): productivity TE full vs. dropping engineer 0jLRJv,
## plus that respondent's responsiveness to the shown test-case signal.
## Does NOT modify or render the main report. Console output only.
suppressMessages({library(pacman); p_load(haven, here, readr, data.table, stringr, fixest)})
options(warn = -1, readr.show_types = FALSE)
date <- "18mar2026"; data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
setwd(here::here())
invisible(capture.output({
  for (f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning"))
    source(here::here("1_Codes", paste0(f, ".R")))
  source(here::here("1_Codes", "6_Hiring_Simulation.R"))   # creates eng_top{k}, hr_pass{k}
}))

DROP <- "R_6F6ThuZMI0jLRJv"

## ---- (A) That respondent's engagement with the SHOWN test-case signal ----
e0 <- firm_long_dt[role == "Eng" & responseid == DROP]
cat("================ FLAGGED RESPONDENT ================\n")
cat("responseid:", DROP, "| arm:", as.character(e0$treat[1]), "| n candidates rated:", nrow(e0), "\n")
## correlation between this engineer's assigned overall score and the visible test-case pass rate
r0 <- cor(e0$sc_overall_z, e0$test_case, use = "complete.obs")
## engineer-population distribution of the same within-evaluator correlation
allr <- firm_long_dt[role == "Eng", .(r = cor(sc_overall_z, test_case, use="complete.obs")), by = responseid]
cat(sprintf("Their score-vs-shown-testcase correlation r = %+.2f  (engineer median %+.2f, mean %+.2f)\n",
    r0, median(allr$r, na.rm=TRUE), mean(allr$r, na.rm=TRUE)))
cat(sprintf("Percentile among engineers: %.0f%% (fraction of engineers with lower r)\n",
    100*mean(allr$r < r0, na.rm=TRUE)))
## the test-case pass rates (shown) of the candidates they put at rank 1-3
e0[, rk := frank(-sc_overall_z, ties.method="min")]
cat("Their top-ranked picks and the test-case pass rate shown for each:\n")
print(e0[rk <= 3][order(rk), .(rank = rk, resume_index, shown_test_case_pct = round(test_case,1), overall_true = round(overall_score,1))])

## ---- (B) Productivity TE at each k: full vs. dropped ----
prod_te <- function(dt, k) {
  col <- paste0("eng_top", k)
  d <- dt[role == "Eng" & get(col) == 1]
  cm <- d[treat == "nonblind", mean(overall_score, na.rm=TRUE)]
  cs <- d[treat == "nonblind", sd(overall_score, na.rm=TRUE)]
  if (is.na(cs) || cs == 0) return(c(est=NA, se=NA, n=nrow(d)))
  d[, y := (overall_score - cm)/cs]
  m <- tryCatch(feols(y ~ i(treat, ref="nonblind") + resp_age |
       q_version + resp_gender + resp_race + s2_educ + s2_income + resp_python + resp_java,
       data = d, vcov = ~ responseid), error=function(e) NULL)
  if (is.null(m) || !("treat::blind" %in% rownames(coeftable(m)))) return(c(est=NA, se=NA, n=nrow(d)))
  ct <- coeftable(m)["treat::blind", ]
  c(est = unname(ct["Estimate"]), se = unname(ct["Std. Error"]), n = nrow(d))
}

cat("\n================ ENGINEER PRODUCTIVITY TE: full vs drop 0jLRJv ================\n")
cat("(coef = effect of blinding on standardized TRUE productivity of the engineer's top-k hires)\n")
cat(sprintf("%4s | %22s | %22s | %s\n", "k", "FULL est (se) [n]", "DROP 0jLRJv est (se) [n]", "delta"))
full_dt <- firm_long_dt
drop_dt <- firm_long_dt[responseid != DROP]
for (k in 1:10) {
  f <- prod_te(full_dt, k); d <- prod_te(drop_dt, k)
  cat(sprintf("k=%2d | %+7.3f (%.3f) [%3.0f]    | %+7.3f (%.3f) [%3.0f]    | %+.3f\n",
      k, f["est"], f["se"], f["n"], d["est"], d["se"], d["n"], d["est"]-f["est"]))
}
cat("\nDONE.\n")
