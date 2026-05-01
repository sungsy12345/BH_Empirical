library(pacman)
p_load(haven, here, base, readr, data.table, stringr, labelled,
       tidyverse, dplyr, fixest, knitr)
options(readr.show_types = FALSE)
setwd(here())

date      <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"

source(here("1_Codes", "2_Import.R"))
source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R"))
source(here("1_Codes", "5_Cleaning.R"))

firm_long_dt[, true_coding_z := (overall_score - mean(overall_score, na.rm = TRUE)) /
                                  sd(overall_score, na.rm = TRUE)]

fit_subsample <- function(d) {
  feols(sc_coding_z ~ true_coding_z * i(treat, ref = "nonblind") | responseid + r_do,
        data = d, vcov = ~ responseid)
}
extract_slopes <- function(m) {
  ct <- coeftable(m); V <- vcov(m)
  b_main <- ct["true_coding_z", "Estimate"]
  s_main <- ct["true_coding_z", "Std. Error"]
  b_int  <- ct["true_coding_z:treat::blind", "Estimate"]
  s_int  <- ct["true_coding_z:treat::blind", "Std. Error"]
  cov_mi <- V["true_coding_z", "true_coding_z:treat::blind"]
  list(b_nb = b_main, s_nb = s_main,
       b_b  = b_main + b_int,
       s_b  = sqrt(s_main^2 + s_int^2 + 2 * cov_mi))
}

# Coding tier
cat("\n========== Coding tier ==========\n")
for (rl in c("HR", "Eng")) for (tier in c(1, 0)) {
  d <- firm_long_dt[role == rl & top_half_coder == tier]
  m <- suppressMessages(suppressWarnings(fit_subsample(d)))
  s <- extract_slopes(m)
  tier_lbl <- ifelse(tier == 1, "Top half", "Bottom half")
  cat(sprintf("%-9s | %-12s | nonblind: %.3f (%.3f) | blind: %.3f (%.3f)\n",
              rl, tier_lbl, s$b_nb, s$s_nb, s$b_b, s$s_b))
}

# GPA tier
cat("\n========== GPA tier ==========\n")
for (rl in c("HR", "Eng")) for (tier in c("Low_GPA", "Mid_GPA", "Top_GPA")) {
  d <- firm_long_dt[role == rl & gpa_tier3 == tier]
  m <- suppressMessages(suppressWarnings(fit_subsample(d)))
  s <- extract_slopes(m)
  cat(sprintf("%-9s | %-7s | nonblind: %.3f (%.3f) | blind: %.3f (%.3f)\n",
              rl, tier, s$b_nb, s$s_nb, s$b_b, s$s_b))
}
