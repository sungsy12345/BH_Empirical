## Quick check: do any 99 values leak into s5_hr_influence / s5_engineer_care
## / s5_view_align after the haven import?
library(pacman)
p_load(haven, here, data.table, readr, tidyverse, dplyr, labelled)
setwd(here())
date <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"

firm_dt <- as.data.table(
  read_dta(paste0(data_root, "3output_data/main_empsurvey_cleaned_", date, ".dta")))

for (v in c("s5_view_align", "s5_hr_influence", "s5_engineer_care")) {
  if (v %in% names(firm_dt)) {
    x <- as.numeric(firm_dt[[v]])
    cat(sprintf("%-20s  range = [%s, %s]   has 99 = %s   N(NA) = %d / %d\n",
                v,
                ifelse(all(is.na(x)), "NA", format(min(x, na.rm = TRUE))),
                ifelse(all(is.na(x)), "NA", format(max(x, na.rm = TRUE))),
                any(x == 99, na.rm = TRUE),
                sum(is.na(x)),
                length(x)))
  } else {
    cat(sprintf("%-20s  not present in firm_dt\n", v))
  }
}
