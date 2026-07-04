## Reconcile: report's §6.2 productivity TE, FULL vs DROP-0jLRJv computed the
## report's way (drop BEFORE sourcing the simulation), and a seed-sensitivity probe.
suppressMessages({library(pacman); p_load(haven, here, readr, data.table, stringr, fixest)})
options(warn = -1, readr.show_types = FALSE)
date <- "18mar2026"; data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
setwd(here::here())
DROP <- "R_6F6ThuZMI0jLRJv"

prod_te <- function(dt, k) {
  col <- paste0("eng_top", k)
  d <- dt[role == "Eng" & get(col) == 1]
  cm <- d[treat=="nonblind", mean(overall_score, na.rm=TRUE)]; cs <- d[treat=="nonblind", sd(overall_score, na.rm=TRUE)]
  if (is.na(cs) || cs==0) return(NA_real_)
  d[, y := (overall_score-cm)/cs]
  m <- tryCatch(feols(y ~ i(treat, ref="nonblind") + resp_age |
       q_version+resp_gender+resp_race+s2_educ+s2_income+resp_python+resp_java, data=d, vcov=~responseid),
       error=function(e) NULL)
  if (is.null(m) || !("treat::blind" %in% rownames(coeftable(m)))) return(NA_real_)
  unname(coeftable(m)["treat::blind","Estimate"])
}

build <- function(drop_first) {
  invisible(capture.output({
    for (f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning"))
      source(here::here("1_Codes", paste0(f, ".R")))
    if (drop_first) assign("firm_long_dt", firm_long_dt[responseid != DROP], envir=.GlobalEnv)
    source(here::here("1_Codes", "6_Hiring_Simulation.R"))
  }))
  firm_long_dt
}

cat("================ REPORT PATH: full vs drop-0jLRJv (drop before simulation) ================\n")
full <- build(FALSE); fk <- sapply(1:5, function(k) prod_te(full, k))
drp  <- build(TRUE);  dk <- sapply(1:5, function(k) prod_te(drp, k))
cat(sprintf("%4s %10s %12s %8s\n","k","FULL","DROP","delta"))
for (k in 1:5) cat(sprintf("k=%2d %+10.3f %+12.3f %+8.3f\n", k, fk[k], dk[k], dk[k]-fk[k]))

## ---- Seed-sensitivity probe: re-randomize tie-breaks on the SAME full data ----
## Shows how much the low-k TE bounces purely from random tie-breaking of evaluators
## who assign tied top scores (0jLRJv gave a 6-way tied top score).
cat("\n================ TIE-BREAK INSTABILITY (same full data, different seeds) ================\n")
eng <- full[role=="Eng"]
k1 <- numeric(8)
for (s in 1:8) {
  set.seed(1000*s + 7)
  e <- copy(eng)
  e[, eng_rank2 := frank(list(-sc_overall_z, -sc_coding_z), ties.method="random"), by=responseid]
  e[, top1 := as.integer(eng_rank2 <= 1)]
  d <- e[top1==1]
  cm <- d[treat=="nonblind", mean(overall_score, na.rm=TRUE)]; cs <- d[treat=="nonblind", sd(overall_score, na.rm=TRUE)]
  d[, y := (overall_score-cm)/cs]
  m <- tryCatch(feols(y ~ i(treat, ref="nonblind") + resp_age |
       q_version+resp_gender+resp_race+s2_educ+s2_income+resp_python+resp_java, data=d, vcov=~responseid), error=function(e) NULL)
  k1[s] <- if (is.null(m)) NA else unname(coeftable(m)["treat::blind","Estimate"])
}
cat("k=1 engineer TE across 8 random tie-break seeds (FULL data, nobody dropped):\n  ")
cat(paste(sprintf("%+.3f", k1), collapse="  "), "\n")
cat(sprintf("  range [%.3f, %.3f], sd = %.3f\n", min(k1,na.rm=T), max(k1,na.rm=T), sd(k1,na.rm=T)))
cat("\nDONE.\n")
