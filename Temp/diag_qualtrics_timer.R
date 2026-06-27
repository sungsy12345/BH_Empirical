## diag_qualtrics_timer.R
## Look for Qualtrics native page-timer columns in firm_dt (raw, pre-merge).
## Qualtrics convention is "<TimerVar>_PageSubmit", "_FirstClick", "_LastClick",
## "_ClickCount". Print every column matching those suffixes plus a sample value.
date <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
suppressPackageStartupMessages({
  library(pacman)
  p_load(haven, here, data.table, fixest, readr, dplyr, tidyverse, stringr,
         labelled, splitstackshape, lubridate)
})
setwd(here())
source(here("1_Codes", "2_Import.R"))
source(here("1_Codes", "3_Firm_Cleaning.R"))

nm <- names(firm_dt)
hits <- grep("(?i)pagesubmit|firstclick|lastclick|clickcount|_t_dur|duration", nm, value = TRUE)
cat("\n##### Qualtrics-native timer columns in firm_dt #####\n")
if (length(hits) == 0) {
  cat("  (none found)\n")
} else {
  cat(sprintf("  Found %d columns:\n", length(hits)))
  print(hits)
  cat("\n  First-row values for each:\n")
  for (h in hits) {
    v <- firm_dt[[h]][1]
    cat(sprintf("    %-50s  %s\n", h, as.character(v)))
  }
}

## Also inspect anything obviously time-like that we might have missed.
extra <- grep("(?i)time|secs|seconds|elapsed|dur", nm, value = TRUE)
extra <- setdiff(extra, hits)
cat(sprintf("\n##### Other time-ish columns (%d) #####\n", length(extra)))
print(head(extra, 30))
