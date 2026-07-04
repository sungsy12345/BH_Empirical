## How common are the ties that the simulation breaks randomly?
## A "true tie" = candidates an evaluator scores identically on BOTH keys the rank uses:
## (sc_overall_z, sc_coding_z). Those are the only ones ties.method="random" touches.
suppressMessages({library(pacman); p_load(haven, here, readr, data.table, stringr)})
options(warn = -1, readr.show_types = FALSE)
date <- "18mar2026"; data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
setwd(here::here())
invisible(capture.output({
  for (f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning"))
    source(here::here("1_Codes", paste0(f, ".R")))
}))

analyze <- function(role_) {
  d <- firm_long_dt[role == role_, .(responseid, treat, sc_overall_z, sc_coding_z)]
  d <- d[!is.na(sc_overall_z)]
  cat("\n##################  ROLE:", role_, " ##################\n")
  cat("evaluators:", uniqueN(d$responseid), "| candidates each (median):",
      median(d[, .N, by=responseid]$N), "\n")

  ## granularity of the assigned-score scales
  cat(sprintf("distinct values: sc_overall_z = %d, sc_coding_z = %d (across all rows)\n",
      uniqueN(round(d$sc_overall_z,6)), uniqueN(round(d$sc_coding_z,6))))

  ## ---- composite-key tie groups within evaluator ----
  d[, grp := .GRP, by = .(responseid, round(sc_overall_z,6), round(sc_coding_z,6))]
  gsz <- d[, .N, by = .(responseid, grp)]            # size of each tie group
  ## per-candidate tie-group size
  d <- merge(d, gsz, by = c("responseid","grp"))
  cat(sprintf("Share of candidate-rows that sit in a tie (group size > 1): %.1f%%\n",
      100*mean(d$N > 1)))
  cat("Distribution of tie-group sizes (over candidate-rows):\n")
  print(round(100*prop.table(table(pmin(d$N, 6))), 1))   # cap at 6 for display

  ## ---- ranking & straddle analysis at each k ----
  ## sort by composite key desc; compute A (strictly above) and t (group size) per group
  setorder(d, responseid, -sc_overall_z, -sc_coding_z)
  ## group rank info per evaluator
  info <- unique(d[, .(responseid, grp, sc_overall_z, sc_coding_z, t = N)])
  setorder(info, responseid, -sc_overall_z, -sc_coding_z)
  info[, A := cumsum(c(0, head(t,-1))), by = responseid]    # candidates strictly above this group

  ## top-score tie (affects k=1): top group size > 1
  topg <- info[, .SD[1], by = responseid]
  cat(sprintf("Evaluators whose TOP score is tied (>=2 candidates) -> affects k=1: %d of %d (%.0f%%); ",
      sum(topg$t > 1), nrow(topg), 100*mean(topg$t > 1)))
  cat(sprintf("median top-tie size when tied = %d, max = %d\n",
      as.integer(median(topg[t>1]$t)), max(topg$t)))

  ## for each k, fraction of evaluators with a STRADDLING tie at the cutoff
  cat("Share of evaluators where the rank-k cutoff lands inside a tie group (random draw changes who is hired):\n")
  for (k in 1:5) {
    strad <- info[A < k & (A + t) > k & t > 1]
    cat(sprintf("   k=%d : %.0f%%\n", k, 100*uniqueN(strad$responseid)/uniqueN(info$responseid)))
  }
  invisible(NULL)
}

for (r in c("Eng","HR")) analyze(r)

## ---- the extreme case: 0jLRJv ----
cat("\n================ reference: 0jLRJv tie structure ================\n")
z <- firm_long_dt[responseid == "R_6F6ThuZMI0jLRJv", .(resume_index, sc_overall_z, sc_coding_z)]
z[, grp := .GRP, by = .(round(sc_overall_z,6), round(sc_coding_z,6))]
setorder(z, -sc_overall_z, -sc_coding_z)
cat("Their tie-group sizes (top to bottom):", paste(z[, .N, by=grp]$N, collapse=" "), "\n")
cat("\nDONE.\n")
