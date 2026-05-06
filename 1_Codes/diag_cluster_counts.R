## -------------------- --------------------
## diag_cluster_counts.R
##
## Quick diagnostic: how many unique respondents / resumes / (resume_ver,
## resume_index) combinations exist in firm_long_dt, broken out by role?
## Used to decide whether two-way clustering on (responseid, resume_*) is
## viable for §4.7 / §4.8 (need a non-trivial number of resume clusters).
## -------------------- --------------------

rm(list = ls())

date <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"

library(pacman)
p_load(haven, here, base, readr, data.table, stringr, labelled, writexl,
       tidyverse, dplyr, ggplot2, ggbreak, ggthemes, ggrepel, ggtext, ggpattern, ggforce, patchwork, cowplot, corrplot,
       tikzDevice, lubridate, scales, RCT, Hmisc,
       fixest, sandwich, broom, marginaleffects, modelsummary, xtable, kableExtra,
       splitstackshape, spatstat, knitr, kableExtra, rmarkdown, tinytex, ezknitr,
       pdflscape, gridExtra, patchwork,
       zipcodeR, tigris, sf, scales, hexbin, patchwork,
       broom)
options(tigris_use_cache = TRUE)
setwd(here())

source(here("1_Codes", "2_Import.R"))
source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R"))
source(here("1_Codes", "5_Cleaning.R"))
source(here("1_Codes", "6_Hiring_Simulation.R"))

cat("\n=================== CLUSTER DIAGNOSTICS ===================\n")
cat("\nTotal rows in firm_long_dt:", nrow(firm_long_dt), "\n")

cat("\n--- By role ---\n")
print(firm_long_dt[, .(
  N           = .N,
  n_resp      = uniqueN(responseid),
  n_resume_idx = uniqueN(resume_index),
  n_resume_ver = uniqueN(resume_ver),
  n_resume_combo = uniqueN(paste(resume_ver, resume_index, sep = "_")),
  n_q_version = uniqueN(q_version)
), by = role])

cat("\n--- By role x treat ---\n")
print(firm_long_dt[, .(
  N           = .N,
  n_resp      = uniqueN(responseid),
  n_resume_idx = uniqueN(resume_index),
  n_resume_combo = uniqueN(paste(resume_ver, resume_index, sep = "_"))
), by = .(role, treat)])

cat("\n--- HR: distribution of resume_index (positions) ---\n")
print(firm_long_dt[role == "HR", .N, by = resume_index][order(resume_index)])

cat("\n--- HR: distribution of (resume_ver, resume_index) combos ---\n")
hr_combo <- firm_long_dt[role == "HR", .N, by = .(resume_ver, resume_index)]
cat("  distinct combos: ", nrow(hr_combo), "\n",
    " mean n per combo: ", round(mean(hr_combo$N), 1), "\n",
    "  min n per combo: ", min(hr_combo$N), "\n",
    "  max n per combo: ", max(hr_combo$N), "\n", sep = "")

cat("\n--- HR: do (resume_ver, resume_index) combos line up with race_gender? ---\n")
print(unique(firm_long_dt[role == "HR", .(resume_ver, resume_index, race_gender)])[order(resume_ver, resume_index)][1:30])

cat("\n=================== END ===================\n")
