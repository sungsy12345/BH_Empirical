## diag_time_outliers.R
## Investigate odd patterns in evaluator time-spent (Qualtrics pagesubmit
## and TaskMaster timeonpage/timeoffpage) at the resp-resume and respondent
## levels, to inform a defensible cleaning rule for §2.1.
date <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
suppressPackageStartupMessages({
  library(pacman)
  p_load(haven, here, data.table, fixest, readr, dplyr, tidyverse, stringr,
         labelled, splitstackshape, lubridate)
})
setwd(here())
source(here("1_Codes", "2_Import.R"))
source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R"))
source(here("1_Codes", "5_Cleaning.R"))
source(here("1_Codes", "6_Hiring_Simulation.R"))

ps_cols <- paste0("res", 1:18, "_timer_pagesubmit")
ps_long <- melt(firm_dt[, c("responseid", ps_cols), with = FALSE],
                id.vars = "responseid",
                measure.vars = ps_cols,
                variable.name = "raw", value.name = "qps")
ps_long[, r_do := as.integer(sub("res([0-9]+)_timer_pagesubmit", "\\1", raw))]
ps_long[, raw := NULL]

dt <- merge(firm_long_dt, ps_long, by = c("responseid", "r_do"), all.x = TRUE)
dt[, taskmaster_total := timeonpage + timeoffpage]

cat("\n##############################################################\n")
cat("##  1. RAW DISTRIBUTION OF qualtrics_pagesubmit (sec)        ##\n")
cat("##############################################################\n")
cat("(per resp-resume observation, by role)\n\n")
print(dt[, .(N=.N,
             min  = round(min(qps, na.rm=TRUE), 1),
             p1   = round(quantile(qps, 0.01, na.rm=TRUE), 1),
             p10  = round(quantile(qps, 0.10, na.rm=TRUE), 1),
             p50  = round(median(qps, na.rm=TRUE), 1),
             mean = round(mean(qps, na.rm=TRUE), 1),
             p90  = round(quantile(qps, 0.90, na.rm=TRUE), 1),
             p99  = round(quantile(qps, 0.99, na.rm=TRUE), 1),
             p999 = round(quantile(qps, 0.999, na.rm=TRUE), 1),
             max  = round(max(qps, na.rm=TRUE), 1)),
         by = role])

cat("\n##############################################################\n")
cat("##  2. EXTREME OUTLIER OBSERVATIONS (resp-resume level)      ##\n")
cat("##############################################################\n")
cat("How many resp-resume rows where Qualtrics PageSubmit > X seconds?\n\n")
cuts <- c(120, 300, 600, 1800, 3600, 7200, 14400)  # 2 min, 5, 10, 30, 1h, 2h, 4h
for (c in cuts) {
  n_total <- nrow(dt[!is.na(qps)])
  n_over  <- nrow(dt[qps > c])
  cat(sprintf("  qps > %5d sec (%-5s):  %4d / %d  (%.2f%%)\n",
              c,
              if (c < 60) paste0(c, "s") else if (c < 3600) paste0(c %/% 60, "min") else paste0(c %/% 3600, "h"),
              n_over, n_total, 100 * n_over / n_total))
}

cat("\n##############################################################\n")
cat("##  3. CROSS-VALIDATION:  Qualtrics LONG, TaskMaster SHORT   ##\n")
cat("##############################################################\n")
cat("(rows where Qualtrics says 'long' but TaskMaster active focus is small\n")
cat(" -> respondent likely walked away with the tab open)\n\n")
diverge <- dt[!is.na(qps) & !is.na(taskmaster_total) &
              qps > 600 & taskmaster_total < 60]
cat(sprintf("  qps > 600s (10 min) AND TaskMaster total < 60s:  %d rows\n", nrow(diverge)))
cat("  (sample of 10 such rows:)\n")
print(head(diverge[order(-qps), .(responseid, role, r_do, qps = round(qps,0),
                                  ton = round(timeonpage,1),
                                  toff = round(timeoffpage,1),
                                  total = round(taskmaster_total,1))], 10))

cat("\n##############################################################\n")
cat("##  4. PER-RESPONDENT PATTERNS                               ##\n")
cat("##############################################################\n")
resp_stats <- dt[!is.na(qps), .(
  n_resumes  = .N,
  qps_max    = round(max(qps), 0),
  qps_p90    = round(quantile(qps, 0.90), 0),
  qps_med    = round(median(qps), 1),
  qps_min    = round(min(qps), 1),
  qps_total  = round(sum(qps), 0)
), by = .(responseid, role, treat)]

cat("\n[A] How many respondents have at least one resume with qps > 30 min (1800s)?\n")
print(resp_stats[, .(n_total = .N,
                     n_with_30min_plus = sum(qps_max > 1800),
                     pct = round(100 * sum(qps_max > 1800) / .N, 1)),
                 by = .(role, treat)][order(role, treat)])

cat("\n[B] Distribution of per-respondent MAX qps (sec):\n")
print(resp_stats[, .(p25 = round(quantile(qps_max, 0.25), 0),
                     p50 = round(median(qps_max), 0),
                     p75 = round(quantile(qps_max, 0.75), 0),
                     p90 = round(quantile(qps_max, 0.90), 0),
                     p99 = round(quantile(qps_max, 0.99), 0),
                     max = round(max(qps_max), 0)),
                 by = role])

cat("\n[C] Respondents with extreme MEDIAN qps (the 'always slow' or 'always fast'):\n")
cat("  Top 10 by median qps:\n")
print(resp_stats[order(-qps_med)][1:10])
cat("\n  Bottom 10 by median qps:\n")
print(resp_stats[order(qps_med)][1:10])

cat("\n##############################################################\n")
cat("##  5. CANDIDATE CLEANING RULES (resp-resume vs respondent)  ##\n")
cat("##############################################################\n")
cat("(How many obs / respondents drop under each candidate rule?)\n\n")

eval_rule <- function(label, drop_obs_idx, drop_resp_idx = NULL) {
  n_obs_pre  <- nrow(dt[!is.na(qps)])
  n_resp_pre <- length(unique(resp_stats$responseid))
  n_obs_drop <- if (is.null(drop_obs_idx)) 0 else sum(drop_obs_idx, na.rm=TRUE)
  n_resp_drop<- if (is.null(drop_resp_idx)) 0 else sum(drop_resp_idx, na.rm=TRUE)
  cat(sprintf("  %-60s  drops %4d obs (%.1f%%)  /  %3d resp (%.1f%%)\n",
              label,
              n_obs_drop, 100 * n_obs_drop / n_obs_pre,
              n_resp_drop, 100 * n_resp_drop / n_resp_pre))
}

eval_rule("RR1: drop obs with qps > 600s (10 min)",
          dt$qps > 600)
eval_rule("RR2: drop obs with qps > 1800s (30 min)",
          dt$qps > 1800)
eval_rule("RR3: clip obs at 95th pct (winsor)",
          dt$qps > quantile(dt$qps, 0.95, na.rm=TRUE))
eval_rule("RR4: drop obs where qps > 600 AND tm_total < 60",
          dt$qps > 600 & dt$taskmaster_total < 60)

eval_rule("R1: drop respondents with any qps > 30 min", NULL,
          resp_stats$qps_max > 1800)
eval_rule("R2: drop respondents with qps_max > 60 min", NULL,
          resp_stats$qps_max > 3600)
eval_rule("R3: drop respondents with median qps > 5 min", NULL,
          resp_stats$qps_med > 300)

cat("\n##############################################################\n")
cat("##  6. RECOMMENDED RULE: PER-OBSERVATION CLIP                ##\n")
cat("##############################################################\n")
cat("(rationale: most outliers are isolated 'walked-away' obs,\n")
cat(" not respondents who walked away on every resume)\n\n")
cat("  Spread of qps_max per respondent vs qps_med per respondent:\n")
cat("  Are extreme obs concentrated in 'always slow' respondents (rule R)\n")
cat("  or scattered across otherwise-normal respondents (rule RR)?\n\n")
suspect_resp <- resp_stats[qps_max > 1800]
cat(sprintf("  Of %d respondents with qps_max > 30 min:\n", nrow(suspect_resp)))
cat(sprintf("    - median of THEIR qps_med:   %.0f sec   (vs full sample %.0f)\n",
            median(suspect_resp$qps_med), median(resp_stats$qps_med)))
cat(sprintf("    - mean ratio (qps_max / qps_med):  %.1fx\n",
            mean(suspect_resp$qps_max / suspect_resp$qps_med, na.rm=TRUE)))
cat("  -> if ratio is large, the 'long obs' is isolated; clip per-obs.\n")
cat("     if ratio is near 1, respondent was uniformly long; drop respondent.\n")
