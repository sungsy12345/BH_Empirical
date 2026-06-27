## Recover the true mapping from "i in res<i>_timer_pagesubmit" to whatever
## index in firm_long_dt it actually corresponds to. Approach: per respondent,
## match the 18 qps values to the 18 TaskMaster totals (timeon + timeoff)
## by nearest-value within a tight tolerance. Then look at the resume_index
## (and r_do) of the matched row. If a stable mapping exists -- e.g.,
## i_in_resname == 5 always corresponds to resume_index = 7 -- that reveals
## the keying convention.
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

fl <- firm_long_dt[, .(responseid, role, resume_index, r_do, timeonpage, timeoffpage)]
fl[, resume_index := as.integer(as.character(resume_index))]
fl[, total := timeonpage + timeoffpage]

## Per respondent: greedy nearest-value match between qps and total,
## with a tolerance (e.g., |diff| < 2 sec). Record the mapping.
match_one <- function(rid) {
  q  <- ps_long[responseid == rid][order(i_in_resname)]
  tm <- fl[responseid == rid][order(resume_index)]
  if (nrow(q) != 18 || nrow(tm) != 18 || any(is.na(tm$total))) return(NULL)
  used <- rep(FALSE, 18)
  out  <- vector("list", 18)
  for (j in seq_len(18)) {
    candidates <- which(!used)
    diffs <- abs(tm$total[candidates] - q$qps[j])
    best <- which.min(diffs)
    idx_in_tm <- candidates[best]
    out[[j]] <- data.table(responseid = rid,
                            i_in_resname = q$i_in_resname[j],
                            qps = q$qps[j],
                            matched_resume_index = tm$resume_index[idx_in_tm],
                            matched_r_do         = tm$r_do[idx_in_tm],
                            matched_total        = tm$total[idx_in_tm],
                            abs_diff             = abs(tm$total[idx_in_tm] - q$qps[j]))
    used[idx_in_tm] <- TRUE
  }
  rbindlist(out)
}

resp_list <- unique(fl$responseid)
maps <- rbindlist(lapply(resp_list, match_one))

cat("\n##### Quality of greedy match (abs|diff| in seconds) #####\n")
print(maps[, .(p25 = round(quantile(abs_diff, 0.25), 2),
               p50 = round(median(abs_diff), 2),
               p75 = round(quantile(abs_diff, 0.75), 2),
               p90 = round(quantile(abs_diff, 0.90), 2),
               p99 = round(quantile(abs_diff, 0.99), 2),
               max = round(max(abs_diff), 2),
               n_within_2s   = sum(abs_diff < 2),
               n_total       = .N)])

cat("\n##### For matches with abs_diff < 2 sec (high confidence), what is the modal mapping i_in_resname -> resume_index? #####\n")
high_conf <- maps[abs_diff < 2]
cat(sprintf("(%d / %d high-confidence matches)\n\n", nrow(high_conf), nrow(maps)))

## Cross-tab: i_in_resname vs matched_resume_index
ct1 <- high_conf[, .N, by = .(i_in_resname, matched_resume_index)][
  order(i_in_resname, -N)]
cat("Top mapping per i_in_resname (most-frequent matched_resume_index):\n")
top1 <- ct1[, .SD[1], by = i_in_resname][order(i_in_resname)]
print(top1)

cat("\nIs i_in_resname == matched_resume_index across the table?\n")
print(all(top1$i_in_resname == top1$matched_resume_index))

## Same against r_do
ct2 <- high_conf[, .N, by = .(i_in_resname, matched_r_do)][
  order(i_in_resname, -N)]
cat("\nTop mapping per i_in_resname -> r_do:\n")
top2 <- ct2[, .SD[1], by = i_in_resname][order(i_in_resname)]
print(top2)

cat("\nIs i_in_resname == matched_r_do across the table?\n")
print(all(top2$i_in_resname == top2$matched_r_do))

## Distribution: how often does the modal mapping hold?
## Stratify by resume_ver: maybe res<i> -> resume_index is keyed differently
## per version. (Each respondent has one resume_ver in {v1..v8}.)
ver_dt <- unique(firm_long_dt[, .(responseid, resume_ver)])
hc_v <- merge(high_conf, ver_dt, by = "responseid")
cat("\n##### Modal i_in_resname -> resume_index, BY resume_ver #####\n")
for (v in sort(unique(hc_v$resume_ver))) {
  cat(sprintf("\n--- resume_ver = %s (N respondents = %d) ---\n", v, length(unique(hc_v[resume_ver == v]$responseid))))
  sub <- hc_v[resume_ver == v]
  ct  <- sub[, .N, by = .(i_in_resname, matched_resume_index)][order(i_in_resname, -N)]
  topv<- ct[, .SD[1], by = i_in_resname][order(i_in_resname)]
  topv[, total_for_i := sub[, .N, by = i_in_resname][match(topv$i_in_resname, i_in_resname)]$N]
  topv[, pct_modal := round(100 * N / total_for_i, 1)]
  print(topv[, .(i_in_resname, mode_idx = matched_resume_index, n = N,
                 total = total_for_i, pct = pct_modal)])
}

cat("\n##### Per-i_in_resname mapping consistency #####\n")
total_per_i <- high_conf[, .(N = .N), by = i_in_resname]
top1_match <- merge(top1[, .(i_in_resname, modal_idx = matched_resume_index, modal_n = N)],
                    total_per_i, by = "i_in_resname")
top1_match[, pct_modal := round(100 * modal_n / N, 1)]
cat("\n[mapping vs resume_index]\n")
print(top1_match[order(i_in_resname)])

top2_match <- merge(top2[, .(i_in_resname, modal_rdo = matched_r_do, modal_n = N)],
                    total_per_i, by = "i_in_resname")
top2_match[, pct_modal := round(100 * modal_n / N, 1)]
cat("\n[mapping vs r_do]\n")
print(top2_match[order(i_in_resname)])
