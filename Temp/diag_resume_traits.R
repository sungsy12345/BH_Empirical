## -------------------- --------------------
## diag_resume_traits.R
##
## Enumerate candidate / resume-trait variables available on
## firm_long_dt. The aim is to identify everything that varies at the
## resume level (constant within `resume_index`) so we can pick RHS
## controls / signal-substitution moderators for §X.
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

cat("\n=================== RESUME / CANDIDATE-TRAIT INVENTORY ===================\n\n")

cat("--- All columns on firm_long_dt ---\n")
print(names(firm_long_dt))

cat("\n--- All columns on student_dt (the source of resume traits) ---\n")
if (exists("student_dt")) {
  print(names(student_dt))
} else {
  cat("(student_dt not in the workspace; was rm'd after the cleaning merge)\n")
}

cat("\n--- For each col on firm_long_dt, does it vary within resume_index? (resume-level vars only) ---\n")
candidate_vars <- setdiff(names(firm_long_dt),
                          c("responseid", "treat", "role", "q_version", "r_do", "resume_ver", "resume_index"))
within_resume_var <- function(v) {
  if (!is.numeric(firm_long_dt[[v]]) && !is.character(firm_long_dt[[v]]) &&
      !is.factor(firm_long_dt[[v]]) && !is.logical(firm_long_dt[[v]]))
    return(NA_real_)
  uv <- firm_long_dt[, uniqueN(get(v)), by = resume_index]$V1
  ## A resume-level var has uniqueN == 1 in every resume_index group (allowing NAs).
  if (any(is.na(uv))) return(NA_real_)
  mean(uv == 1L)
}
trait_dt <- data.table(
  var = candidate_vars,
  share_resume_constant = vapply(candidate_vars, within_resume_var, numeric(1))
)
trait_dt <- trait_dt[order(-share_resume_constant)]
print(trait_dt[share_resume_constant > 0.95])

cat("\n--- Probable resume-level traits (constant within resume_index in 100% of slots) ---\n")
print(trait_dt[share_resume_constant == 1])

cat("\n=================== END ===================\n")
