library(pacman)
p_load(haven, here, base, readr, data.table, stringr, labelled, tidyverse, dplyr)
options(readr.show_types = FALSE)
setwd(here())
date <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
source(here("1_Codes", "2_Import.R"))
source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R"))
source(here("1_Codes", "5_Cleaning.R"))

## Per respondent within tier: concordance over rankable pairs (|true diff| >= delta).
##   C   = correct order (rating direction matches true direction)
##   D   = wrong order (rating direction reversed)
##   T_y = tied rating (rating_i == rating_j) -- counted as wrong since the
##         true gap was large enough that they should have differentiated.
##   rate = C / (C + D + T_y)
compute_one_resp <- function(sc, true_score, delta) {
  if (length(sc) < 2) return(c(C = 0, D = 0, T_y = 0, rate = NA_real_, n_pairs = 0))
  pairs <- combn(length(sc), 2)
  i <- pairs[1, ]; j <- pairs[2, ]
  td <- true_score[i] - true_score[j]
  rd <- sc[i] - sc[j]
  rankable <- abs(td) >= delta
  if (sum(rankable) == 0) return(c(C = 0, D = 0, T_y = 0, rate = NA_real_, n_pairs = 0))
  td <- td[rankable]; rd <- rd[rankable]
  C   <- sum(rd != 0 & sign(td) == sign(rd))
  D   <- sum(rd != 0 & sign(td) == -sign(rd))
  T_y <- sum(rd == 0)
  total <- C + D + T_y
  c(C = C, D = D, T_y = T_y,
    rate = if (total > 0) C / total else NA_real_,
    n_pairs = total)
}

aggregate_cell <- function(d, tier_var, tier_val, role_val, arm_val, delta) {
  sub <- d[role == role_val & treat == arm_val & get(tier_var) == tier_val,
           .(responseid, sc_coding, overall_score)]
  if (nrow(sub) == 0) return(data.frame(mean_rate = NA, n_resp = 0, mean_n_pairs = NA))
  per_resp <- sub[, {
    out <- compute_one_resp(sc_coding, overall_score, delta)
    list(rate = out["rate"], n_pairs = out["n_pairs"])
  }, by = responseid]
  valid <- per_resp[!is.na(rate)]
  data.frame(
    mean_rate    = mean(valid$rate),
    n_resp       = nrow(valid),
    mean_n_pairs = mean(valid$n_pairs)
  )
}

deltas <- c(5, 7, 10)

cat("\n=========================================================\n")
cat("Concordance rate on RANKABLE pairs (|true diff| >= delta).\n")
cat("Per-respondent rate, then averaged across respondents in cell.\n")
cat("Tied evaluator ratings COUNT AS WRONG when true gap >= delta.\n")
cat("=========================================================\n")

for (delta in deltas) {
  cat(sprintf("\n--------- delta = %d ---------\n", delta))
  cat("\n[Coding tier: top_half_coder]\n")
  for (rl in c("HR", "Eng")) {
    for (arm in c("nonblind", "blind")) {
      for (tier_val in c(1, 0)) {
        tier_lbl <- if (tier_val == 1) "Top half" else "Bottom half"
        out <- aggregate_cell(firm_long_dt, "top_half_coder", tier_val, rl, arm, delta)
        cat(sprintf("  %-3s | %-8s | %-11s | rate = %.3f | N_resp = %2d | avg_pairs = %5.1f\n",
                    rl, arm, tier_lbl, out$mean_rate, out$n_resp, out$mean_n_pairs))
      }
    }
  }

  cat("\n[GPA tier: gpa_tier3]\n")
  for (rl in c("HR", "Eng")) {
    for (arm in c("nonblind", "blind")) {
      for (tier_val in c("Low_GPA", "Mid_GPA", "Top_GPA")) {
        out <- aggregate_cell(firm_long_dt, "gpa_tier3", tier_val, rl, arm, delta)
        cat(sprintf("  %-3s | %-8s | %-7s | rate = %.3f | N_resp = %2d | avg_pairs = %5.1f\n",
                    rl, arm, tier_val, out$mean_rate, out$n_resp, out$mean_n_pairs))
      }
    }
  }
}
