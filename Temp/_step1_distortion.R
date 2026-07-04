## STEP 1: back out REVEALED demographic distortion by evaluator group (non-blind arm).
## Distortion = response of assigned score to candidate-evaluator CONCORDANCE, net of
## resume-content FE. Identified between-evaluator (labels randomized onto fixed content).
suppressMessages({library(pacman); p_load(haven, here, readr, data.table, stringr, fixest)})
options(warn = -1, readr.show_types = FALSE)
date <- "18mar2026"; data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
setwd(here::here())
invisible(capture.output({
  for (f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning"))
    source(here::here("1_Codes", paste0(f, ".R")))
}))

## available concordance / evaluator-demographic vars
cat("concordance vars:", paste(grep("concordance", names(firm_long_dt), value=TRUE), collapse=", "), "\n")
cat("resp demo vars:", paste(grep("^resp_(gender|race)", names(firm_long_dt), value=TRUE), collapse=", "), "\n\n")

nb <- firm_long_dt[treat == "nonblind"]
## coarse evaluator groups
nb[, ev_gender := resp_gender]
nb[, ev_majmale := as.integer(resp_gender == "Male" & resp_race %in% c("White/Caucasian, non-Hispanic","Asian or Asian American"))]
nb[, dei_ter := cut(dei_index_z, quantile(unique(nb[, .(responseid, dei_index_z)])$dei_index_z, c(0,1/3,2/3,1), na.rm=TRUE),
                    labels=c("Low","Mid","High"), include.lowest=TRUE)]

## distortion estimator: score ~ gender-conc + race-conc | resume content, clustered by respondent
dist <- function(d, lbl) {
  if (uniqueN(d$responseid) < 4) { cat(sprintf("  %-26s (too few evaluators: %d)\n", lbl, uniqueN(d$responseid))); return(invisible()) }
  m <- tryCatch(feols(sc_overall_z ~ concordance_gender + concordance_race_detailed | resume_index,
        data = d, vcov = ~ responseid), error=function(e) NULL)
  if (is.null(m)) { cat(sprintf("  %-26s (not estimable)\n", lbl)); return(invisible()) }
  ct <- coeftable(m); g <- function(n) if (n %in% rownames(ct)) ct[n, c(1,4)] else c(NA,NA)
  cg <- g("concordance_gender"); cr <- g("concordance_race_detailed")
  cat(sprintf("  %-26s | gender-conc %+.3f (p=%.2f) | race-conc %+.3f (p=%.2f) | |sum|=%.3f | resp=%d\n",
      lbl, cg[1], cg[2], cr[1], cr[2], abs(cg[1])+abs(cr[1]), uniqueN(d$responseid)))
}

cat("REVEALED CONCORDANCE DISTORTION (non-blind). Negative = penalizes own-group; positive = own-group premium.\n")
cat("\n== By role ==\n"); for (r in c("Eng","HR")) dist(nb[role==r], r)
cat("\n== Engineers by evaluator gender ==\n"); for (gg in c("Male","Female")) dist(nb[role=="Eng" & ev_gender==gg], paste0("Eng ", gg))
cat("== HR by evaluator gender ==\n"); for (gg in c("Male","Female")) dist(nb[role=="HR" & ev_gender==gg], paste0("HR ", gg))
cat("\n== By majority-male evaluator (White/Asian male) vs rest ==\n")
for (r in c("Eng","HR")) for (mm in c(1,0)) dist(nb[role==r & ev_majmale==mm], sprintf("%s %s", r, ifelse(mm==1,"MajMale","Rest")))
cat("\n== By DEI tercile (does stated DEI track revealed distortion?) ==\n")
for (r in c("Eng","HR")) for (tt in c("Low","High")) dist(nb[role==r & dei_ter==tt], sprintf("%s DEI-%s", r, tt))
cat("\nDONE.\n")
