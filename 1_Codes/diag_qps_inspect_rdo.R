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

cat("\nfirm_long_dt time/order columns:\n")
print(grep("(?i)time|do|order|page|res|name", names(firm_long_dt),
           value=TRUE, perl=TRUE))

rid <- "R_13gCNEgP6bVMHhn"
rdo_cols <- paste0("r", 1:18, "_DO")

cat("\nfirm_dt original r1_DO ... r18_DO values for ", rid, ":\n")
print(unlist(firm_dt[responseid == rid, ..rdo_cols]))

cat("\nfirm_dt r1 ... r18 values:\n")
r_cols <- paste0("r", 1:18)
print(unlist(firm_dt[responseid == rid, ..r_cols]))

cat("\nfirm_long_dt rows for this responseid: resume_index | r_do | ton | toff\n")
fl <- firm_long_dt[responseid == rid][order(as.integer(as.character(resume_index)))]
print(fl[, .(resume_index, r_do, ton = round(timeonpage, 1),
             toff = round(timeoffpage, 1))])

cat("\n##### Test: does qps res<i> match TaskMaster total when keyed by ORIGINAL r<i>_DO position?\n")
qcols <- paste0("res", 1:18, "_timer_pagesubmit")
qps_vals <- as.numeric(firm_dt[responseid == rid, ..qcols])
rdo_vals <- as.integer(firm_dt[responseid == rid, ..rdo_cols])
fl[, total := timeonpage + timeoffpage]
fl[, idx_int := as.integer(as.character(resume_index))]

## For each i in 1..18, look up:
##   resume_index = i  (i.e., r<i>'s "content slot")
##   r<i>_DO = position at which content slot i was shown
## Compare qps res<i> against TaskMaster total at resume_index = i AND
## TaskMaster total at r_do = r<i>_DO.
out <- data.table(
  i = 1:18,
  qps_resi = qps_vals,
  rdo_for_idx_i = rdo_vals,
  tm_total_at_idx_i = fl[match(1:18, idx_int)]$total,
  tm_total_at_rdo_eq_rdo_i = fl[match(rdo_vals, fl$r_do)]$total
)
out[, diff_idx := abs(qps_resi - tm_total_at_idx_i)]
out[, diff_rdo := abs(qps_resi - tm_total_at_rdo_eq_rdo_i)]
print(out)

cat(sprintf("\n  Median |diff| (key = resume_index)   = %.2f sec\n", median(out$diff_idx)))
cat(sprintf("  Median |diff| (key = r<i>_DO position)= %.2f sec\n", median(out$diff_rdo)))

cat("\n##### Cleaner test: WHICH RAW r<i>_DO PATTERN matches qps res<i>?\n")
cat("If qps res<i> is keyed by survey-DESIGNED slot, but the TaskMaster slot\n")
cat("is the survey-DISPLAYED slot, then we expect agreement when the survey\n")
cat("design = display (not randomized).  But the survey IS randomized, so\n")
cat("res<i>_timer is on whatever slot Qualtrics calls 'i' in DESIGN.  That\n")
cat("DESIGN slot has no relationship to resume_index unless the design slot\n")
cat("is statically assigned to a resume_index.\n\n")

## A direct alignment test: maybe res<i>_timer corresponds to the resume that
## was assigned to "r<i>" in firm_dt, and r<i>'s VALUE encodes the resume_index
## that landed at design-slot i.
##
## Inspect: r<i>'s values are a permutation of 1..18, varying by respondent.
## Then res<i>_timer should match TaskMaster total for resume_index = r<i>.
cat("##### Hypothesis C: qps res<i> keyed via r<i>'s VALUE (which is resume_index at design slot i) #####\n")
r_vals <- as.integer(firm_dt[responseid == rid, ..r_cols])
out_C <- data.table(
  i = 1:18,
  r_i_value = r_vals,
  qps_resi = qps_vals,
  tm_total_at_resume_index_eq_r_i = fl[match(r_vals, idx_int)]$total
)
out_C[, diff := abs(qps_resi - tm_total_at_resume_index_eq_r_i)]
print(out_C)
cat(sprintf("\n  Median |diff| (key = r<i>'s VALUE) = %.2f sec\n", median(out_C$diff, na.rm = TRUE)))
