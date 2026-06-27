## Empirically determine whether res<i>_timer_pagesubmit is keyed by:
##   (a) resume_index (content i)
##   (b) r_do (display position i)
##   (c) something else (e.g., a positional index in the survey flow)
## Compare correlations with TaskMaster timeonpage + timeoffpage under each
## key. The correct key should give a much higher correlation -- ideally
## near 1, since both timers fire on the same page event (page load -> Next).
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

ps_cols <- paste0("res", 1:18, "_timer_pagesubmit")
ps_long <- melt(firm_dt[, c("responseid", ps_cols), with = FALSE],
                id.vars = "responseid",
                measure.vars = ps_cols,
                variable.name = "raw", value.name = "qps")
ps_long[, i_in_resname := as.integer(sub("res([0-9]+)_timer_pagesubmit", "\\1", raw))]
ps_long[, raw := NULL]

## firm_long_dt has both resume_index (content) and r_do (display position).
## Bring both into the merge candidate.
fl <- firm_long_dt[, .(responseid, role, resume_index, r_do,
                        timeonpage, timeoffpage)]
fl[, resume_index := as.integer(as.character(resume_index))]
fl[, taskmaster_total := timeonpage + timeoffpage]

## Hypothesis A: i_in_resname == resume_index (CONTENT keying)
ps_A <- copy(ps_long); ps_A[, resume_index := i_in_resname]
dt_A <- merge(fl, ps_A[, .(responseid, resume_index, qps)],
              by = c("responseid", "resume_index"), all.x = TRUE)

## Hypothesis B: i_in_resname == r_do (DISPLAY-POSITION keying)
ps_B <- copy(ps_long); ps_B[, r_do := i_in_resname]
dt_B <- merge(fl, ps_B[, .(responseid, r_do, qps)],
              by = c("responseid", "r_do"), all.x = TRUE)

cat("\n##### Correlation of qps with TaskMaster (timeon + timeoff), by keying hypothesis #####\n")
cat("(higher cor wins; raw, NO outlier filtering)\n\n")
for (lab in c("A: content (resume_index)", "B: position (r_do)")) {
  d <- if (substr(lab, 1, 1) == "A") dt_A else dt_B
  out <- d[!is.na(qps) & !is.na(taskmaster_total),
           .(cor_total = round(cor(qps, taskmaster_total), 3),
             cor_ton   = round(cor(qps, timeonpage), 3),
             cor_toff  = round(cor(qps, timeoffpage), 3),
             N = .N), by = role]
  cat(sprintf("--- Hypothesis %s ---\n", lab))
  print(out)
  cat("\n")
}

cat("##### Correlation by role -- TRIMMED (qps capped at p99 within role) #####\n")
cat("(robust to walked-away outliers)\n\n")
for (lab in c("A: content (resume_index)", "B: position (r_do)")) {
  d <- if (substr(lab, 1, 1) == "A") dt_A else dt_B
  d <- copy(d)[!is.na(qps) & !is.na(taskmaster_total)]
  d[, qps_cap := pmin(qps, quantile(qps, 0.99, na.rm=TRUE)), by = role]
  d[, tm_cap  := pmin(taskmaster_total, quantile(taskmaster_total, 0.99, na.rm=TRUE)), by = role]
  out <- d[, .(cor_total_capped = round(cor(qps_cap, tm_cap), 3),
               N = .N), by = role]
  cat(sprintf("--- Hypothesis %s ---\n", lab))
  print(out); cat("\n")
}

cat("##### Per-respondent within-respondent correlation #####\n")
cat("(rank correlation of qps vs taskmaster_total across the 18 resumes\n")
cat(" -- per respondent. If keying is right, median should be near 1.)\n\n")
for (lab in c("A: content (resume_index)", "B: position (r_do)")) {
  d <- if (substr(lab, 1, 1) == "A") dt_A else dt_B
  d <- d[!is.na(qps) & !is.na(taskmaster_total)]
  by_resp <- d[, .(rho = if (.N >= 5) suppressWarnings(cor(qps, taskmaster_total, method = "spearman")) else NA_real_,
                   N = .N), by = .(responseid, role)]
  by_resp <- by_resp[!is.na(rho)]
  cat(sprintf("--- Hypothesis %s ---\n", lab))
  print(by_resp[, .(N_resp = .N,
                    rho_p10 = round(quantile(rho, 0.10, na.rm=TRUE), 2),
                    rho_p25 = round(quantile(rho, 0.25, na.rm=TRUE), 2),
                    rho_p50 = round(median(rho, na.rm=TRUE), 2),
                    rho_p75 = round(quantile(rho, 0.75, na.rm=TRUE), 2),
                    rho_p90 = round(quantile(rho, 0.90, na.rm=TRUE), 2)),
                by = role])
  cat("\n")
}

cat("##### Single-respondent diagnostic: pick a respondent and show the alignment #####\n")
rid <- fl[role == "Eng" & !is.na(timeonpage)][1, responseid]
cat(sprintf("Respondent: %s\n\n", rid))
cat("All 18 res<i>_timer_pagesubmit values, in name order (i = 1..18):\n")
ps_one <- ps_long[responseid == rid][order(i_in_resname)]
print(ps_one[, .(i_in_resname, qps = round(qps, 1))])

cat("\nfirm_long_dt rows for this respondent (role, resume_index, r_do, timeonpage, timeoffpage, total):\n")
fl_one <- fl[responseid == rid][order(resume_index)]
print(fl_one[, .(role, resume_index, r_do, ton = round(timeonpage,1),
                 toff = round(timeoffpage,1), total = round(taskmaster_total,1))])
