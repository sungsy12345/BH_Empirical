## Driver: render the fractional-weight report. arg = "full" or "drop".
## Isolated: does not touch 1_Codes/8B_Stage_BH.Rmd. Outputs a separately-named PDF.
mode <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(mode)) mode <- "full"
Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")
date <- "18mar2026"; data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
library(pacman)
suppressMessages(p_load(haven, here, readr, data.table, stringr, labelled, writexl,
  tidyverse, ggplot2, ggbreak, ggthemes, ggrepel, ggtext, ggpattern, ggforce, patchwork, cowplot, corrplot,
  tikzDevice, lubridate, scales, RCT, Hmisc, fixest, sandwich, broom, marginaleffects, modelsummary, xtable, kableExtra,
  splitstackshape, spatstat, knitr, rmarkdown, tinytex, ezknitr, pdflscape, gridExtra, zipcodeR, tigris, sf, hexbin))
options(tigris_use_cache = TRUE, readr.show_types = FALSE)
setwd(here())

FLAG <- c("R_6F6ThuZMI0jLRJv","R_7KD5TNkimOZGNCx","R_1GCYghAkB1YzutY","R_6m8207oT8ZCXMZN")

for (f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning"))
  source(here("1_Codes", paste0(f, ".R")))

if (mode == "drop") {
  n0 <- nrow(firm_long_dt)
  firm_long_dt <- firm_long_dt[!(responseid %in% FLAG)]
  if (exists("resp_dt")) resp_dt <- resp_dt[!(responseid %in% FLAG)]
  cat("Dropped", length(FLAG), "flagged respondents | rows", n0, "->", nrow(firm_long_dt), "\n")
}

source(here("Temp", "_6_relaxed.R"))   # sim with two-stage assertion relaxed (drop changes arm counts)

## ---- overwrite single-stage selection cols with deterministic FRACTIONAL weights ----
overwrite_fracw <- function(role_, prefix) {
  d <- firm_long_dt[role == role_, .(responseid, resume_index, sc_overall_z, sc_coding_z)]
  d[, grp := .GRP, by = .(responseid, round(sc_overall_z, 8), round(sc_coding_z, 8))]
  gs <- d[, .(t = .N, so = sc_overall_z[1], sc = sc_coding_z[1]), by = .(responseid, grp)]
  setorder(gs, responseid, -so, -sc)
  gs[, A := cumsum(c(0, head(t, -1))), by = responseid]
  d <- merge(d, gs[, .(responseid, grp, t, A)], by = c("responseid", "grp"))
  for (k in 1:18) {
    d[, wtmp := pmin(1, pmax(0, (k - A) / t))]
    firm_long_dt[d, on = .(responseid, resume_index), (paste0(prefix, k)) := i.wtmp]
  }
}
overwrite_fracw("HR",  "hr_pass")
overwrite_fracw("Eng", "eng_top")
cat("Overwrote hr_pass/eng_top with fractional weights. sum(eng_top1) per Eng resp (should~1):",
    round(mean(firm_long_dt[role=="Eng", sum(eng_top1, na.rm=TRUE), by=responseid]$V1), 3), "\n")

## weighted-moment helpers used by the converted standardization lines
assign(".wmean", function(x, w) { ok <- !is.na(x) & !is.na(w); if (!any(ok)) return(NA_real_); sum(x[ok]*w[ok])/sum(w[ok]) }, envir = .GlobalEnv)
assign(".wsd",   function(x, w) { ok <- !is.na(x) & !is.na(w); if (sum(ok) < 2) return(NA_real_); m <- sum(x[ok]*w[ok])/sum(w[ok]); sqrt(sum(w[ok]*(x[ok]-m)^2)/sum(w[ok])) }, envir = .GlobalEnv)

today_str <- format(Sys.time(), "%Y%m%d_%H%M")
out <- sprintf("SideReport_FracWeight_%s_%s.pdf", ifelse(mode=="drop","DropFlagged","Full"), today_str)
rmarkdown::render(here("Temp", "_8B_fracweight.Rmd"),
  output_file = out, output_dir = here("2_Reports"),
  knit_root_dir = here(), intermediates_dir = tempdir(), quiet = TRUE, envir = globalenv())
cat("RENDERED", out, "\n")
