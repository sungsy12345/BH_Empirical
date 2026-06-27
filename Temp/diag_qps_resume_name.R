## Inspect what resume_index, resume_name, r_do actually mean.
## Hypothesis: resume_index = survey slot (positional, 1..18 fixed in
## designed order); resume_name = the actual resume content shown at that
## slot (varies per resume_ver). If true, then res<i>_timer_pagesubmit
## keyed by survey-designed slot == resume_index, and the merge by
## resume_index SHOULD work.
date <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
suppressPackageStartupMessages({
  library(pacman)
  p_load(haven, here, data.table, fixest, readr, dplyr, tidyverse, stringr,
         labelled, splitstackshape, lubridate)
})
setwd(here())
source(here("1_Codes","2_Import.R"))
source(here("1_Codes","3_Firm_Cleaning.R"))
source(here("1_Codes","4_Distribution_Cleaning.R"))
source(here("1_Codes","5_Cleaning.R"))
source(here("1_Codes","6_Hiring_Simulation.R"))

cat("\n##### Raw r<i> column values for ONE respondent (R_13gCNEgP6bVMHhn) #####\n")
rid <- "R_13gCNEgP6bVMHhn"
r_cols <- paste0("r", 1:18)
exists_r <- intersect(r_cols, names(firm_dt))
cat(sprintf("Columns r1..r18 present in firm_dt: %d\n", length(exists_r)))
if (length(exists_r) > 0) {
  print(unlist(firm_dt[responseid == rid, ..exists_r]))
}

cat("\n##### Same respondent, r<i>_DO values (display position of slot i) #####\n")
do_cols <- paste0("r", 1:18, "_DO")
exists_do <- intersect(do_cols, names(firm_dt))
if (length(exists_do) > 0) {
  print(unlist(firm_dt[responseid == rid, ..exists_do]))
}

cat("\n##### firm_long_dt for same respondent: resume_index, resume_name, r_do, total #####\n")
fl <- firm_long_dt[responseid == rid][order(resume_index)]
fl[, total := timeonpage + timeoffpage]
print(fl[, .(resume_index, resume_name = if ("resume_name" %in% names(fl)) get("resume_name") else NA,
             race_gender, r_do,
             ton = round(timeonpage,1), toff = round(timeoffpage,1), total = round(total,1))])

cat("\n##### qps res1..res18 for same respondent #####\n")
qcols <- paste0("res", 1:18, "_timer_pagesubmit")
print(unlist(firm_dt[responseid == rid, ..qcols]))

cat("\n##### Test: does qps res<i> equal TaskMaster total for resume_index = i (this respondent)? #####\n")
ps <- data.table(i = 1:18,
                 qps = as.numeric(firm_dt[responseid == rid, ..qcols]))
ts <- fl[, .(i = as.integer(as.character(resume_index)), tm_total = total)]
chk <- merge(ps, ts, by = "i")
chk[, abs_diff := abs(qps - tm_total)]
print(chk[order(i)])
cat(sprintf("\n  Median |qps - tm_total| keyed by resume_index = %.2f sec\n", median(chk$abs_diff)))

cat("\n##### Same test, keyed by r_do (Hypothesis B) #####\n")
ts2 <- fl[, .(i = r_do, tm_total = total)]
chk2 <- merge(ps, ts2, by = "i")
chk2[, abs_diff := abs(qps - tm_total)]
print(chk2[order(i)])
cat(sprintf("\n  Median |qps - tm_total| keyed by r_do = %.2f sec\n", median(chk2$abs_diff)))

cat("\n##### What IF qps res<i> is keyed by resume_NAME (not resume_index)? #####\n")
if ("resume_name" %in% names(firm_long_dt)) {
  fl2 <- firm_long_dt[responseid == rid]
  fl2[, total := timeonpage + timeoffpage]
  cat("Sample of resume_name values:\n")
  print(fl2[, .(resume_index, resume_name, total = round(total,1))])
  ts3 <- fl2[, .(i = as.integer(as.character(resume_name)), tm_total = round(total,1))]
  if (any(!is.na(ts3$i))) {
    chk3 <- merge(ps, ts3, by = "i", allow.cartesian = TRUE)
    chk3[, abs_diff := abs(qps - tm_total)]
    print(chk3[order(i)])
    cat(sprintf("\n  Median |qps - tm_total| keyed by resume_name = %.2f sec\n",
                median(chk3$abs_diff)))
  }
} else {
  cat("  resume_name not present in firm_long_dt\n")
}
