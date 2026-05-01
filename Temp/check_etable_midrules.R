library(pacman)
p_load(haven, here, base, readr, data.table, stringr, labelled,
       tidyverse, dplyr, fixest)
options(readr.show_types = FALSE)
setwd(here())
date      <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
source(here("1_Codes", "2_Import.R"))
source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R"))
source(here("1_Codes", "5_Cleaning.R"))

dt_hr <- firm_long_dt[role == "HR"]
dt_hr[, urm_tech_blind := fcase(
  treat == "blind",                    "Blind",
  treat == "nonblind" & urm_tech == 1, "Nonblind_URM",
  treat == "nonblind" & urm_tech == 0, "Nonblind_NonURM",
  default = NA_character_)]
dt_hr[, urm_tech_blind := factor(urm_tech_blind,
  levels = c("Blind", "Nonblind_URM", "Nonblind_NonURM"))]

m <- feols(I(-sc_overall_z) ~ dei_index_A_z
              + i(urm_tech_blind, ref = 'Blind')
              + i(urm_tech_blind, dei_index_A_z, ref = 'Blind')
              + resp_age + I(resp_age^2) + I(resp_age^3)
            | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income,
            data = dt_hr, vcov = ~ responseid)

chk <- "\\checkmark"
nox <- "$\\times$"

tbl <- etable(
  list("HR" = m), tex = TRUE, style.tex = style.tex("aer"),
  placement = "H", depvar = FALSE,
  keep = c("%urm_tech_blind", "%dei_index"),
  fitstat = c("n", "r2", "ar2"),
  drop.section = "fixef",
  digits = 3, se.below = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  extralines = list(
    "Resume (Content) FE"        = chk,
    "Display Position FE"        = chk,
    "Respondent FE"              = nox,
    "Respondent Characteristics" = chk
  )
)
cat("==== RAW etable TEX ====\n")
cat(tbl)
cat("\n==== END ====\n")
