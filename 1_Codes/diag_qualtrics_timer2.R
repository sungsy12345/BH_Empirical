date <- "18mar2026"; data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
suppressPackageStartupMessages({library(pacman); p_load(haven, here, data.table, readr, dplyr, tidyverse, stringr, labelled, splitstackshape, lubridate)})
setwd(here()); source(here('1_Codes','2_Import.R')); source(here('1_Codes','3_Firm_Cleaning.R'))

nm <- names(firm_dt)

## All section-prefixes under timer_*
all_t <- grep("(?i)^timer_", nm, value = TRUE)
sects <- unique(sub("^timer_([^_]+)_.*", "\\1", all_t))
cat("\nAll section-prefixes under timer_*:\n"); print(sects)

## Look for resume-evaluation timers under various candidate patterns
patterns <- c("(?i)timer_(res|resume|eval|cand|r[0-9])", "(?i)res[0-9]+_(pagesubmit|firstclick|lastclick|clickcount)", "(?i)avg_res")
for (pat in patterns) {
  hits <- grep(pat, nm, value = TRUE, perl = TRUE)
  cat(sprintf("\n--- pattern: %s  (%d hits) ---\n", pat, length(hits)))
  print(head(hits, 60))
}

## Section s4 covers resume evaluation (s4_resume_time exists). Check timer_s4_*
s4 <- grep("(?i)^timer_s4_", nm, value = TRUE)
cat(sprintf("\nAll timer_s4_* columns (%d):\n", length(s4))); print(s4)

## Also list any other "_pagesubmit"/"_firstclick" suffix not under timer_*
ps <- grep("(?i)pagesubmit$", nm, value = TRUE)
ps <- setdiff(ps, all_t)
cat(sprintf("\nNon-timer pagesubmit cols (%d):\n", length(ps))); print(head(ps, 50))
