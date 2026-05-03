## Count cluster sizes for responseid and resume_index in §7-§9 regressions.
library(pacman)
p_load(haven, here, base, readr, data.table, stringr,
       tidyverse, dplyr, fixest)
setwd(here())
options(readr.show_types = FALSE)
date <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
source(here("1_Codes", "2_Import.R"))
source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R"))
source(here("1_Codes", "5_Cleaning.R"))

cat("\n--- Cluster counts on the §7-§9 sample ---\n")
for (rl in c("HR", "Eng")) {
  d <- firm_long_dt[role == rl]
  cat(sprintf("\nRole = %s:\n", rl))
  cat(sprintf("  N obs                       : %d\n", nrow(d)))
  cat(sprintf("  Unique responseid clusters  : %d\n", uniqueN(d$responseid)))
  cat(sprintf("  Unique resume_index clusters: %d\n", uniqueN(d$resume_index)))
  cat(sprintf("  Mean rows / responseid      : %.1f\n", nrow(d) / uniqueN(d$responseid)))
  cat(sprintf("  Mean rows / resume_index    : %.1f\n", nrow(d) / uniqueN(d$resume_index)))
}
cat("\n--- Done ---\n")
