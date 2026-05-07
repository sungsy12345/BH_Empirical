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
source(here("1_Codes","4_Distribution_Cleaning.R"))
source(here("1_Codes","5_Cleaning.R"))
source(here("1_Codes","6_Hiring_Simulation.R"))

cat("\nRAW TaskMaster timeonpage distribution (per resp-resume):\n")
print(firm_long_dt[!is.na(timeonpage),
  .(N=.N, p1=round(quantile(timeonpage,0.01),1),
    p50=round(median(timeonpage),1),
    p90=round(quantile(timeonpage,0.90),1),
    p99=round(quantile(timeonpage,0.99),1),
    p999=round(quantile(timeonpage,0.999),1),
    max=round(max(timeonpage),1)), by=role])

cat("\nRAW TaskMaster timeoffpage:\n")
print(firm_long_dt[!is.na(timeoffpage),
  .(N=.N, p50=round(median(timeoffpage),1),
    p90=round(quantile(timeoffpage,0.90),1),
    p99=round(quantile(timeoffpage,0.99),1),
    p999=round(quantile(timeoffpage,0.999),1),
    max=round(max(timeoffpage),1)), by=role])

cat("\nN obs with timeonpage > X (sec):\n")
for (c in c(100, 300, 600, 1800, 3600)) {
  cat(sprintf("  > %5d:  %4d / %d  (%.2f%%)\n",
              c,
              sum(firm_long_dt$timeonpage > c, na.rm=TRUE),
              sum(!is.na(firm_long_dt$timeonpage)),
              100*sum(firm_long_dt$timeonpage > c, na.rm=TRUE)/sum(!is.na(firm_long_dt$timeonpage))))
}
cat("\nN obs with timeoffpage > X (sec):\n")
for (c in c(100, 300, 600, 1800, 3600, 14400)) {
  cat(sprintf("  > %5d:  %4d / %d  (%.2f%%)\n",
              c,
              sum(firm_long_dt$timeoffpage > c, na.rm=TRUE),
              sum(!is.na(firm_long_dt$timeoffpage)),
              100*sum(firm_long_dt$timeoffpage > c, na.rm=TRUE)/sum(!is.na(firm_long_dt$timeoffpage))))
}
