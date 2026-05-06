## -------------------- --------------------
## diag_readability_dist.R
##
## What is the distribution of `readability` across the 18 resumes?
## Print one row per unique resume_index showing the lab subscores.
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

cat("\n=================== READABILITY DISTRIBUTION ===================\n\n")

cat("--- Per resume_index (one row per unique resume slot) ---\n")
resume_lab <- unique(firm_long_dt[, .(resume_index, test_case, readability,
                                       time_efficiency, space_efficiency,
                                       overall_score)])
resume_lab[, resume_index := as.integer(as.character(resume_index))]
setorder(resume_lab, resume_index)
print(resume_lab)

cat("\n--- Five-number + mean/sd summary of readability across the 18 resumes ---\n")
print(resume_lab[, .(N = .N,
                     min  = min(readability,  na.rm = TRUE),
                     p25  = quantile(readability, 0.25, na.rm = TRUE),
                     median = median(readability, na.rm = TRUE),
                     mean   = mean(readability,   na.rm = TRUE),
                     p75  = quantile(readability, 0.75, na.rm = TRUE),
                     max  = max(readability,  na.rm = TRUE),
                     sd   = sd(readability,   na.rm = TRUE),
                     n_unique_values = uniqueN(readability))])

cat("\n--- Tabulation of unique readability values + counts ---\n")
print(resume_lab[, .N, by = readability][order(readability)])

cat("\n--- Same for the other three components (for context) ---\n")
for (cmp in c("test_case", "time_efficiency", "space_efficiency")) {
  cat(sprintf("\n[%s]\n", cmp))
  print(resume_lab[, .(N = .N,
                       min  = min(get(cmp),  na.rm = TRUE),
                       median = median(get(cmp), na.rm = TRUE),
                       mean   = mean(get(cmp),   na.rm = TRUE),
                       max  = max(get(cmp),  na.rm = TRUE),
                       sd   = sd(get(cmp),   na.rm = TRUE),
                       n_unique_values = uniqueN(get(cmp)))])
}

cat("\n=================== END ===================\n")
