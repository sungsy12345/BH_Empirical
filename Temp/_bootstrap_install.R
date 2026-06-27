## One-off environment bootstrap for the new machine (R 4.6.0).
## Installs every package 1_Master.R's p_load() needs, then TinyTeX.
options(repos = c(CRAN = "https://cloud.r-project.org"))
options(Ncpus = max(1L, parallel::detectCores() - 1L))

lib <- "C:/Users/sungs/AppData/Local/R/win-library/4.6"
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(lib)

pkgs <- c(
  "pacman","haven","here","readr","data.table","stringr","labelled","writexl",
  "tidyverse","dplyr","ggplot2","ggbreak","ggthemes","ggrepel","ggtext","ggpattern",
  "ggforce","patchwork","cowplot","corrplot","tikzDevice","lubridate","scales","RCT",
  "Hmisc","fixest","sandwich","broom","marginaleffects","modelsummary","xtable",
  "kableExtra","splitstackshape","spatstat","knitr","rmarkdown","tinytex","ezknitr",
  "pdflscape","gridExtra","zipcodeR","tigris","sf","hexbin"
)

have <- rownames(installed.packages())
need <- setdiff(pkgs, have)
cat("Already have:", length(have), "| To install:", length(need), "\n")
cat("Installing:", paste(need, collapse=", "), "\n\n")

if (length(need)) install.packages(need, lib = lib)

## Verify what loaded
have2 <- rownames(installed.packages())
still_missing <- setdiff(pkgs, have2)
cat("\n=== INSTALL SUMMARY ===\n")
cat("Still MISSING (", length(still_missing), "):", paste(still_missing, collapse=", "), "\n")

## LaTeX via TinyTeX (only if tinytex package installed and TinyTeX not present)
if ("tinytex" %in% have2) {
  if (!tinytex::is_tinytex()) {
    cat("\nInstalling TinyTeX (LaTeX engine)...\n")
    tinytex::install_tinytex()
  } else {
    cat("\nTinyTeX already present.\n")
  }
} else {
  cat("\ntinytex package not installed; skipping LaTeX setup.\n")
}
cat("\n=== BOOTSTRAP DONE ===\n")
