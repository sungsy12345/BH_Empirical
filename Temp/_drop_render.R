## SIDE PROJECT: render 8B dropping one Engineer evaluator (R_6F6ThuZMI0jLRJv).
## Does NOT modify 1_Codes/8B_Stage_BH.Rmd. Output is a separately-named PDF.
Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")
date <- "18mar2026"; data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
library(pacman)
suppressMessages(p_load(haven, here, readr, data.table, stringr, labelled, writexl,
  tidyverse, ggplot2, ggbreak, ggthemes, ggrepel, ggtext, ggpattern, ggforce, patchwork, cowplot, corrplot,
  tikzDevice, lubridate, scales, RCT, Hmisc, fixest, sandwich, broom, marginaleffects, modelsummary, xtable, kableExtra,
  splitstackshape, spatstat, knitr, rmarkdown, tinytex, ezknitr, pdflscape, gridExtra, zipcodeR, tigris, sf, hexbin))
options(tigris_use_cache = TRUE, readr.show_types = FALSE)
setwd(here())
for (f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here("1_Codes", paste0(f, ".R")))

## ---- DROP the evaluator (before the hiring simulation so everything is on the reduced data) ----
drop_id <- "R_6F6ThuZMI0jLRJv"
n0 <- nrow(firm_long_dt)
firm_long_dt <- firm_long_dt[responseid != drop_id]
cat("Dropped", drop_id, ": rows", n0, "->", nrow(firm_long_dt),
    "| still present?", any(firm_long_dt$responseid == drop_id), "\n")
if (exists("resp_dt")) resp_dt <- resp_dt[responseid != drop_id]

source(here("1_Codes", "6_Hiring_Simulation.R"))

today_str <- format(Sys.time(), "%Y%m%d_%H%M")
out <- sprintf("SideReport_DropEng0jLRJv_%s.pdf", today_str)
rmarkdown::render(here("1_Codes", "8B_Stage_BH.Rmd"),
  output_file = out, output_dir = here("2_Reports"),
  intermediates_dir = tempdir(), quiet = TRUE, envir = globalenv())
cat("RENDERED", out, "\n")
