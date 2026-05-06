## -------------------- --------------------
## Script name: render_8B_only.R
##
## One-shot render of just 8B_Stage_BH.Rmd (and the upstream data
## pipeline it depends on). Mirrors 1_Master.R but skips the Descriptive
## and Preference renders. Use this when the only thing that changed is
## inside 8B and re-rendering the other two PDFs would be wasted work.
## Output PDF name follows the YYYYMMDD_HHMM convention.
## -------------------- --------------------

rm(list = ls())

## -------------------- SECTION FOR CUSTOMIZATION --------------------
date <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"

## -------------------- PREAMBLE --------------------
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
options(readr.show_types = FALSE)
knitr::opts_chunk$set(echo = FALSE)

## -------------------- RUN UPSTREAM PIPELINE --------------------
source(here("1_Codes", "2_Import.R"))
source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R"))
source(here("1_Codes", "5_Cleaning.R"))
source(here("1_Codes", "6_Hiring_Simulation.R"))

## -------------------- RENDER 8B --------------------
today_str <- format(Sys.time(), "%Y%m%d_%H%M")

rmarkdown::render(
  here("1_Codes", "8B_Stage_BH.Rmd"),
  output_file       = sprintf("Main_Results_%s.pdf", today_str),
  output_dir        = here("2_Reports"),
  intermediates_dir = tempdir()
)
