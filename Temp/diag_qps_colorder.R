date <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
suppressPackageStartupMessages({
  library(pacman)
  p_load(haven, here, data.table, readr, dplyr, tidyverse, stringr,
         labelled, splitstackshape, lubridate)
})
setwd(here())
source(here("1_Codes","2_Import.R"))
source(here("1_Codes","3_Firm_Cleaning.R"))

cat("\nColumn POSITIONS of r1..r18 in firm_dt:\n")
r_cols <- paste0("r", 1:18)
positions <- match(r_cols, names(firm_dt))
print(data.table(col = r_cols, position = positions)[order(position)])

cat("\nSequential names AROUND those positions:\n")
seq_pos <- sort(unique(positions))
all_names <- names(firm_dt)
for (p in seq_pos[1:5]) {
  cat(sprintf("Position %d: %s\n", p, all_names[p]))
}
cat("...\n")
for (p in tail(seq_pos, 5)) {
  cat(sprintf("Position %d: %s\n", p, all_names[p]))
}

cat("\n##### Same for res<i>_timer_pagesubmit columns #####\n")
ps_cols <- paste0("res", 1:18, "_timer_pagesubmit")
positions <- match(ps_cols, names(firm_dt))
print(data.table(col = ps_cols, position = positions)[order(position)])

cat("\n##### What's the column order: numeric or alphabetic? #####\n")
## If alphabetic: r1, r10, r11, ..., r18, r2, r3, ..., r9
## If numeric: r1, r2, r3, ..., r18
r_pos <- match(r_cols, names(firm_dt))
ord_by_pos <- r_cols[order(r_pos)]
cat("r columns sorted by their position in firm_dt:\n")
print(ord_by_pos)
