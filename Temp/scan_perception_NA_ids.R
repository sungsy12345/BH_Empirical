## Pull a handful of responseids per NA category for hand-inspection
## of the raw Qualtrics responses.
library(pacman)
p_load(haven, here, base, readr, data.table, stringr, tidyverse, dplyr)
setwd(here())
options(readr.show_types = FALSE)
date <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
source(here("1_Codes", "2_Import.R"))
source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R"))
source(here("1_Codes", "5_Cleaning.R"))

role_map <- unique(firm_long_dt[, .(responseid, role_long = role)])
resp_dt  <- merge(firm_dt, role_map, by = "responseid", all.x = TRUE)
resp_dt[, role := role_long]

cat("\n==================================================\n")
cat("Sample responseids for hand inspection in Qualtrics\n")
cat("==================================================\n")

## --- Engineers with NA on s5_hr_influence ---
e <- resp_dt[role == "Eng"]
e_attrit <- e[is.na(s5_hr_influence) & is.na(s5_view_align)]
e_item   <- e[is.na(s5_hr_influence) & !is.na(s5_view_align)]

cat(sprintf("\n[Eng | s5_hr_influence NA, also NA on s5_view_align (stage-5 attrition)]  n=%d\n", nrow(e_attrit)))
cat("First 5 responseids:\n")
print(head(e_attrit$responseid, 5))

cat(sprintf("\n[Eng | s5_hr_influence NA, but answered s5_view_align (item-specific)]  n=%d\n", nrow(e_item)))
cat("First 5 responseids (with their s5_view_align value):\n")
print(head(e_item[, .(responseid, s5_view_align)], 5))

## --- HR with NA on s5_engineer_care ---
h <- resp_dt[role == "HR"]
h_attrit <- h[is.na(s5_engineer_care) & is.na(s5_view_align)]
h_item   <- h[is.na(s5_engineer_care) & !is.na(s5_view_align)]

cat(sprintf("\n[HR | s5_engineer_care NA, also NA on s5_view_align (stage-5 attrition)]  n=%d\n", nrow(h_attrit)))
cat("First 5 responseids:\n")
print(head(h_attrit$responseid, 5))

cat(sprintf("\n[HR | s5_engineer_care NA, but answered s5_view_align (item-specific)]  n=%d\n", nrow(h_item)))
cat("First 5 responseids (with their s5_view_align value):\n")
print(head(h_item[, .(responseid, s5_view_align)], 5))

cat("\n--- Done ---\n")
