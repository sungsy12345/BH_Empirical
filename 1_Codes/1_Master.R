## -------------------- --------------------
##
## Script name: 1_Master.R
##
## Email: sysung@berkeley.edu
##
## -------------------- --------------------


## Empty Work-space / Environment
rm(list=ls())


## -------------------- SECTION FOR CUSTOMIZATION --------------------

# Identify Data Version We Want To Use
date <- "18mar2026"

# Root of the canonical (read-only) Dropbox data folder. All scripts read
# raw and intermediate data from here. Nothing is ever written back.
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"


## -------------------- PREAMBLE --------------------

## Load Packages
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

## Working Directories
setwd(here())

## Other Settings
options(readr.show_types = FALSE) # When reading in csv, suppresses messages 
knitr::opts_chunk$set(echo = FALSE) # When knitting, print the code

## -------------------- RUN CODES --------------------

## Import Firm Side Survey Data
source(here("1_Codes", "2_Import.R"))

## Firm Name Cleaning
source(here("1_Codes", "3_Firm_Cleaning.R"))

## Distribution Cleaning
source(here("1_Codes", "4_Distribution_Cleaning.R"))

## Data Cleaning
source(here("1_Codes", "5_Cleaning.R"))

## Create Matching Output
source(here("1_Codes", "6_Hiring_Simulation.R"))

## Analysis
rmarkdown::render(
  here("1_Codes", "7_Descriptive.Rmd"),
  output_dir        = here("2_Reports"),
  intermediates_dir = tempdir()
)

## ----- Steps below are temporarily disabled. -----
## Re-enable them one at a time as each Rmd is migrated.
rmarkdown::render(here("1_Codes", "8A_Preferences.Rmd"),     output_dir = here("2_Reports"), intermediates_dir = tempdir())
rmarkdown::render(here("1_Codes", "8B_Stage_BH.Rmd"),        output_dir = here("2_Reports"), intermediates_dir = tempdir())
# rmarkdown::render(here("1_Codes", "8C_Dynamic_BH.Rmd"),      output_dir = here("2_Reports"), intermediates_dir = tempdir())
# rmarkdown::render(here("1_Codes", "8C_Dynamic_BH_Jobs.Rmd"), output_dir = here("2_Reports"), intermediates_dir = tempdir())

## Results
# rmarkdown::render(here("1_Codes", "9_Final_Results.Rmd"),    output_dir = here("2_Reports"), intermediates_dir = tempdir())
