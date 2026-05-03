## Inspect the test_case (coding score) distribution across the 18 resumes
## to assess whether a 3-tier cut is defensible.
library(pacman)
p_load(haven, here, base, readr, data.table, stringr, labelled,
       tidyverse, dplyr, fixest)

setwd(here())
options(readr.show_types = FALSE)

date      <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"

source(here("1_Codes", "2_Import.R"))
source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R"))
source(here("1_Codes", "5_Cleaning.R"))

cat("\n================================================================\n")
cat("Per-resume test_case (coding score), gpa, and current 2-tier cut\n")
cat("================================================================\n")
resume_scores <- unique(firm_long_dt[, .(resume_index, test_case, gpa, gpa_tier3, top_half_coder)])
setorder(resume_scores, test_case)
print(resume_scores)

cat("\n--- Summary stats of test_case ---\n")
print(summary(resume_scores$test_case))

cat("\n--- Quantiles ---\n")
print(round(quantile(resume_scores$test_case,
                     probs = c(0, 0.10, 0.25, 0.33, 0.50, 0.67, 0.75, 0.90, 1),
                     na.rm = TRUE), 4))

cat("\n--- Sorted scores with gaps to next (look for natural breaks) ---\n")
sorted <- sort(resume_scores$test_case)
gaps   <- diff(sorted)
gap_dt <- data.frame(rank        = seq_along(gaps),
                     score_below = round(sorted[-length(sorted)], 4),
                     score_above = round(sorted[-1], 4),
                     gap         = round(gaps, 4))
print(gap_dt)

cat("\n--- Three candidate 3-tier cuts (6/6/6, 33rd/67th, equal-spacing) ---\n")
n <- nrow(resume_scores)
cat("\n[A] Equal-count tertiles (6 / 6 / 6):\n")
cut_A <- cut(resume_scores$test_case, breaks = quantile(resume_scores$test_case, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
             include.lowest = TRUE, labels = c("Low", "Mid", "Top"))
tabA <- resume_scores[, .(resume_index, test_case)][, tier := cut_A][]
setorder(tabA, test_case)
print(tabA)
print(table(tabA$tier))

cat("\n[B] 33rd/67th percentile cut on test_case:\n")
breaks_B <- quantile(resume_scores$test_case, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE)
cat("breaks:", round(breaks_B, 4), "\n")

cat("\n[C] Equal-spacing cut on test_case (range / 3):\n")
rng <- range(resume_scores$test_case, na.rm = TRUE)
breaks_C <- seq(rng[1], rng[2], length.out = 4)
cat("breaks:", round(breaks_C, 4), "\n")
cut_C <- cut(resume_scores$test_case, breaks = breaks_C, include.lowest = TRUE,
             labels = c("Low", "Mid", "Top"))
tabC <- resume_scores[, .(resume_index, test_case)][, tier := cut_C][]
setorder(tabC, test_case)
print(tabC)
print(table(tabC$tier))

cat("\n--- Done ---\n")
