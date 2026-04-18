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
source(here("2_Import.R"))

## Firm Name Cleaning
source(here("3_Firm_Cleaning.R"))

## Distribution Cleaning
source(here("4_Distribution_Cleaning.R"))

## Data Cleaning
source(here("5_Cleaning.R"))

## Create Matching Output
source(here("6_Hiring_Simulation.R"))

## Analysis
rmarkdown::render("7_Descriptive.Rmd")
rmarkdown::render("8A_Preferences.Rmd")
rmarkdown::render("8B_Stage_BH.Rmd")
# rmarkdown::render("8C_Dynamic_BH.Rmd")
# rmarkdown::render("8C_Dynamic_BH_Jobs.Rmd")

## Results
rmarkdown::render("9_Final_Results.Rmd")
