## Parameterized branch driver.  args: <dropset> <tiemode>
##   dropset : "eng0j" (drop R_6F6ThuZMI0jLRJv only) | "all4" (drop all flagged)
##   tiemode : "random" (original Rmd, random tie-break) | "frac" (fractional weighting)
## Isolated: never edits 1_Codes/8B_Stage_BH.Rmd. Outputs a separately-named PDF.
a <- commandArgs(trailingOnly = TRUE); dropset <- a[1]; tiemode <- a[2]
Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")
date <- "18mar2026"; data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
library(pacman)
suppressMessages(p_load(haven, here, readr, data.table, stringr, labelled, writexl,
  tidyverse, ggplot2, ggbreak, ggthemes, ggrepel, ggtext, ggpattern, ggforce, patchwork, cowplot, corrplot,
  tikzDevice, lubridate, scales, RCT, Hmisc, fixest, sandwich, broom, marginaleffects, modelsummary, xtable, kableExtra,
  splitstackshape, spatstat, knitr, rmarkdown, tinytex, ezknitr, pdflscape, gridExtra, zipcodeR, tigris, sf, hexbin, lme4))
options(tigris_use_cache = TRUE, readr.show_types = FALSE)
setwd(here())

FLAG_ALL <- c("R_6F6ThuZMI0jLRJv","R_7KD5TNkimOZGNCx","R_1GCYghAkB1YzutY","R_6m8207oT8ZCXMZN")
drop_ids <- if (dropset == "all4") FLAG_ALL else "R_6F6ThuZMI0jLRJv"

for (f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning"))
  source(here("1_Codes", paste0(f, ".R")))

n0 <- nrow(firm_long_dt)
firm_long_dt <- firm_long_dt[!(responseid %in% drop_ids)]
if (exists("resp_dt")) resp_dt <- resp_dt[!(responseid %in% drop_ids)]
cat("BRANCH", dropset, tiemode, "| dropped", length(drop_ids), "| rows", n0, "->", nrow(firm_long_dt), "\n")

source(here("Temp", "_6_relaxed.R"))     # two-stage assertion relaxed (drop changes arm counts)

if (tiemode == "frac") {
  overwrite_fracw <- function(role_, prefix) {
    d <- firm_long_dt[role == role_, .(responseid, resume_index, sc_overall_z, sc_coding_z)]
    d[, grp := .GRP, by = .(responseid, round(sc_overall_z, 8), round(sc_coding_z, 8))]
    gs <- d[, .(t = .N, so = sc_overall_z[1], sc = sc_coding_z[1]), by = .(responseid, grp)]
    setorder(gs, responseid, -so, -sc)
    gs[, A := cumsum(c(0, head(t, -1))), by = responseid]
    d <- merge(d, gs[, .(responseid, grp, t, A)], by = c("responseid", "grp"))
    for (k in 1:18) { d[, wtmp := pmin(1, pmax(0, (k - A) / t))]
      firm_long_dt[d, on = .(responseid, resume_index), (paste0(prefix, k)) := i.wtmp] }
  }
  overwrite_fracw("HR", "hr_pass"); overwrite_fracw("Eng", "eng_top")
  assign(".wmean", function(x, w) { ok <- !is.na(x) & !is.na(w); if (!any(ok)) return(NA_real_); sum(x[ok]*w[ok])/sum(w[ok]) }, envir = .GlobalEnv)
  assign(".wsd",   function(x, w) { ok <- !is.na(x) & !is.na(w); if (sum(ok) < 2) return(NA_real_); m <- sum(x[ok]*w[ok])/sum(w[ok]); sqrt(sum(w[ok]*(x[ok]-m)^2)/sum(w[ok])) }, envir = .GlobalEnv)
  rmd <- here("Temp", "_8B_fracweight.Rmd")
} else {
  rmd <- here("1_Codes", "8B_Stage_BH.Rmd")
}

lbl <- c("eng0j" = "Drop0jLRJv", "all4" = "DropAll4Flagged")[dropset]
tlb <- c("random" = "RandomTie", "frac" = "FracWeight")[tiemode]
today_str <- format(Sys.time(), "%Y%m%d_%H%M")
out <- sprintf("Main_Results_%s_%s_%s.pdf", today_str, lbl, tlb)
rmarkdown::render(rmd, output_file = out, output_dir = here("2_Reports"),
  knit_root_dir = here(), intermediates_dir = tempdir(), quiet = TRUE, envir = globalenv())
cat("RENDERED", out, "\n")
