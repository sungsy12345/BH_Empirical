## Tabulate NA counts by role for the candidate misalignment-index components.
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

## firm_long_dt has multiple obs per respondent; collapse to respondent level.
extra_perc <- c("s5_view_align", "s5_view_aligndiffeng", "s5_view_aligndiffhr",
                "s5_hr_influence", "s5_engineer_care",
                grep("^aligndiff(hr|eng)_", names(firm_dt), value = TRUE))
extra_perc <- intersect(extra_perc, names(firm_dt))

resp_dt <- unique(merge(
  firm_long_dt[, .(responseid, role)],
  firm_dt[, c("responseid", extra_perc), with = FALSE],
  by = "responseid"))

## Cast labelled cols to numeric so checks work uniformly.
for (col in extra_perc) {
  set(resp_dt, j = col, value = as.numeric(resp_dt[[col]]))
}

## Compute the count for each role (rowSums over the 6 dummies).
eng_items <- grep("^aligndiffeng_", extra_perc, value = TRUE)
hr_items  <- grep("^aligndiffhr_",  extra_perc, value = TRUE)
resp_dt[, eng_count := rowSums(.SD, na.rm = FALSE), .SDcols = eng_items]
resp_dt[, hr_count  := rowSums(.SD, na.rm = FALSE), .SDcols = hr_items]

## ===== Per-role NA tabulation across the three relevant components =====
roles <- c("Eng", "HR")
for (rl in roles) {
  cat(sprintf("\n========== Role: %s ==========\n", rl))
  d <- resp_dt[role == rl]
  n_total <- nrow(d)
  cat(sprintf("Total respondents: %d\n\n", n_total))

  if (rl == "Eng") {
    legs <- list(
      "s5_view_align"        = d$s5_view_align,
      "s5_view_aligndiffeng" = d$s5_view_aligndiffeng,
      "s5_hr_influence"      = d$s5_hr_influence,
      "count(aligndiffeng_*)" = d$eng_count
    )
  } else {
    legs <- list(
      "s5_view_align"       = d$s5_view_align,
      "s5_view_aligndiffhr" = d$s5_view_aligndiffhr,
      "s5_engineer_care"    = d$s5_engineer_care,
      "count(aligndiffhr_*)" = d$hr_count
    )
  }

  ## Per-leg NA count
  cat("--- Per-leg NA counts ---\n")
  for (nm in names(legs)) {
    x <- legs[[nm]]
    n_na <- sum(is.na(x))
    cat(sprintf("  %-30s NA = %3d / %d  (%.1f%%)\n",
                nm, n_na, length(x), 100 * n_na / length(x)))
  }

  ## Joint missingness across the three Likert/ordinal items (excluding count).
  cat("\n--- Joint missingness (3 Likert/ordinal items) ---\n")
  if (rl == "Eng") {
    triplet <- data.frame(view_align = is.na(d$s5_view_align),
                          adiff      = is.na(d$s5_view_aligndiffeng),
                          influence  = is.na(d$s5_hr_influence))
    cat("  Pattern (view_align, adiff, hr_influence):\n")
  } else {
    triplet <- data.frame(view_align = is.na(d$s5_view_align),
                          adiff      = is.na(d$s5_view_aligndiffhr),
                          eng_care   = is.na(d$s5_engineer_care))
    cat("  Pattern (view_align, adiff, eng_care):\n")
  }
  triplet$pattern <- apply(triplet, 1, function(r) paste(ifelse(r, "NA", " ok"), collapse = " | "))
  pat_tab <- as.data.frame(table(pattern = triplet$pattern))
  pat_tab <- pat_tab[order(-pat_tab$Freq), ]
  for (i in seq_len(nrow(pat_tab))) {
    cat(sprintf("    %s -> n = %d\n",
                as.character(pat_tab$pattern[i]),
                pat_tab$Freq[i]))
  }

  ## Sample sizes under candidate inclusion rules.
  cat("\n--- Sample under candidate filters ---\n")
  cat(sprintf("  All respondents:                                 n = %d\n", n_total))
  cat(sprintf("  Drop if s5_view_align NA:                        n = %d\n",
              sum(!is.na(d$s5_view_align))))
  cat(sprintf("  Drop if s5_view_align NA OR third-leg NA:        n = %d\n",
              if (rl == "Eng") sum(!is.na(d$s5_view_align) & !is.na(d$s5_hr_influence))
              else             sum(!is.na(d$s5_view_align) & !is.na(d$s5_engineer_care))))
  cat(sprintf("  Drop if all 3 Likert items NA (pairwise sample): n = %d\n",
              if (rl == "Eng")
                sum(!(is.na(d$s5_view_align) & is.na(d$s5_view_aligndiffeng) & is.na(d$s5_hr_influence)))
              else
                sum(!(is.na(d$s5_view_align) & is.na(d$s5_view_aligndiffhr)  & is.na(d$s5_engineer_care)))))
  cat(sprintf("  Listwise: all 3 Likert items non-NA:             n = %d\n",
              if (rl == "Eng")
                sum(!is.na(d$s5_view_align) & !is.na(d$s5_view_aligndiffeng) & !is.na(d$s5_hr_influence))
              else
                sum(!is.na(d$s5_view_align) & !is.na(d$s5_view_aligndiffhr)  & !is.na(d$s5_engineer_care))))
}

cat("\n--- Done ---\n")
