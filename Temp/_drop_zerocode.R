## SIDE PROJECT 2: render 8B dropping all Engineer evaluators who opened 0% of code.
## Does NOT modify 1_Codes/8B_Stage_BH.Rmd. Separately-named output PDF.
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

## ---- Identify Engineer evaluators with 0% code-opening, then drop them ----
.eng <- firm_long_dt[role == "Eng"]
.eng[, oc := as.integer((!is.na(open_code1) & open_code1 == 1) | (!is.na(open_code2) & open_code2 == 1))]
zerocode <- .eng[, .(cod = mean(oc)), by = responseid][cod == 0, responseid]
n0 <- nrow(firm_long_dt)
firm_long_dt <- firm_long_dt[!(responseid %in% zerocode)]
if (exists("resp_dt")) resp_dt <- resp_dt[!(responseid %in% zerocode)]
cat("Dropped", length(zerocode), "zero-code Engineer evaluators | rows", n0, "->", nrow(firm_long_dt), "\n")

source(here("Temp", "_6_relaxed.R"))

today_str <- format(Sys.time(), "%Y%m%d_%H%M")
out <- sprintf("SideReport_DropZeroCodeEng_%s.pdf", today_str)
rmarkdown::render(here("1_Codes", "8B_Stage_BH.Rmd"),
  output_file = out, output_dir = here("2_Reports"),
  intermediates_dir = tempdir(), quiet = TRUE, envir = globalenv())
cat("RENDERED", out, "\n")
