## Deeper look at ties: (A) does HR even have a coding score, (B) per-evaluator
## NON-DIFFERENTIATION, (C) how much the random draw churns who advances, (D) extremes.
suppressMessages({library(pacman); p_load(haven, here, readr, data.table, stringr)})
options(warn = -1, readr.show_types = FALSE)
date <- "18mar2026"; data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
setwd(here::here())
invisible(capture.output({
  for (f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning"))
    source(here::here("1_Codes", paste0(f, ".R")))
}))

## ===== (A) Does HR have a usable coding score (the rank's secondary key)? =====
cat("================ (A) sc_coding_z by role ================\n")
for (rl in c("Eng","HR")) {
  d <- firm_long_dt[role == rl]
  cat(sprintf("%s: rows=%d | sc_coding_z: %.0f%% missing, %d distinct non-missing | sc_overall_z: %.0f%% missing\n",
    rl, nrow(d), 100*mean(is.na(d$sc_coding_z)), uniqueN(d$sc_coding_z[!is.na(d$sc_coding_z)]),
    100*mean(is.na(d$sc_overall_z))))
}

## ===== (B) Per-evaluator non-differentiation =====
cat("\n================ (B) Non-differentiation per evaluator (over 18 ratings) ================\n")
ndiff <- function(rl) {
  d <- firm_long_dt[role == rl & !is.na(sc_overall_z)]
  s <- d[, .(n = .N,
             distinct_overall = uniqueN(round(sc_overall_z,6)),
             max_tie = max(.SD[, .N, by=.(round(sc_overall_z,6), round(sc_coding_z,6))]$N),
             ## Herfindahl on overall-score groups: 1 = all identical, ~1/n = all distinct
             hhi = sum((.SD[, .N, by=round(sc_overall_z,6)]$N / .N)^2)),
        by = responseid]
  cat(sprintf("\n[%s] n evaluators = %d\n", rl, nrow(s)))
  cat(sprintf("  distinct overall scores used (of ~18): median %d, p10 %d, min %d\n",
      as.integer(median(s$distinct_overall)), as.integer(quantile(s$distinct_overall,.1)), min(s$distinct_overall)))
  cat(sprintf("  largest tie group: median %d, p90 %d, max %d\n",
      as.integer(median(s$max_tie)), as.integer(quantile(s$max_tie,.9)), max(s$max_tie)))
  cat(sprintf("  evaluators giving the SAME score to >= half (>=9) of candidates: %d (%.0f%%)\n",
      sum(s$max_tie >= 9), 100*mean(s$max_tie >= 9)))
  cat(sprintf("  evaluators using <= 5 distinct overall scores: %d (%.0f%%)\n",
      sum(s$distinct_overall <= 5), 100*mean(s$distinct_overall <= 5)))
  s[, role := rl][]
}
S <- rbind(ndiff("Eng"), ndiff("HR"))

## ===== (C) Selection churn: share of advancing seats decided by the coin flip =====
## For each evaluator at cutoff k: straddling group has t tied, s=k-A slots.
## Expected share of the k seats that DIFFER between two independent random draws
## within that group = s(t-s)/t. Aggregate over evaluators.
cat("\n================ (C) Churn: expected share of advancing seats that flip between two random seeds ================\n")
churn <- function(rl) {
  d <- firm_long_dt[role == rl & !is.na(sc_overall_z)]
  info <- unique(d[, grp := .GRP, by=.(responseid, round(sc_overall_z,6), round(sc_coding_z,6))][
                 , .(responseid, grp, so = round(sc_overall_z,6), sc = round(sc_coding_z,6),
                     t = .N), by=.(responseid, grp)][, .(responseid, grp, so, sc, t)])
  info <- unique(info)
  setorder(info, responseid, -so, -sc)
  info[, A := cumsum(c(0, head(t,-1))), by=responseid]
  out <- sapply(1:5, function(k) {
    info[, s := pmin(pmax(k - A, 0), t)]            # slots this group contributes to top-k
    frag <- info[t > 1 & A < k & (A+t) > k, sum(s*(t-s)/t), by=responseid]
    nresp <- uniqueN(info$responseid)
    total_frag <- sum(frag$V1)
    total_frag / (k * nresp)                         # share of all k*nresp seats that flip
  })
  cat(sprintf("[%s]  ", rl)); cat(paste(sprintf("k=%d:%.0f%%", 1:5, 100*out), collapse="  "), "\n")
}
churn("Eng"); churn("HR")

## ===== (D) The extreme non-differentiators =====
cat("\n================ (D) Extreme HR non-differentiators (largest tie >= 9) ================\n")
ext <- S[role=="HR" & max_tie >= 9][order(-max_tie)]
print(head(ext[, .(responseid, distinct_overall, max_tie, hhi = round(hhi,2))], 12))
cat("\nDONE.\n")
