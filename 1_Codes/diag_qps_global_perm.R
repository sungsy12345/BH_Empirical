## Hypothesis: res<i>_timer_pagesubmit corresponds to a FIXED survey-design
## permutation of resume_index (different from 1..18 identity).  Test by
## restricting to high-confidence matches (|qps - tm_total| < 0.5 sec) and
## inspecting the modal mapping i_in_resname -> resume_index globally and
## by role.
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

fl <- firm_long_dt[, .(responseid, role, resume_index, r_do,
                        timeonpage, timeoffpage)]
fl[, resume_index := as.integer(as.character(resume_index))]
fl[, total := timeonpage + timeoffpage]

## Per respondent: Hungarian-style greedy match on |diff|, restricted to
## tight pairs (|diff| < 0.5 sec).
match_strict <- function(rid) {
  q  <- ps_long[responseid == rid][order(i_in_resname)]
  tm <- fl[responseid == rid][order(resume_index)]
  if (nrow(q) != 18 || nrow(tm) != 18 || any(is.na(tm$total))) return(NULL)
  pairs <- expand.grid(qj = 1:18, tk = 1:18)
  pairs$diff <- abs(q$qps[pairs$qj] - tm$total[pairs$tk])
  pairs <- pairs[order(pairs$diff), ]
  used_q <- rep(FALSE, 18); used_t <- rep(FALSE, 18)
  out <- list()
  for (rr in seq_len(nrow(pairs))) {
    if (pairs$diff[rr] > 5) break
    j <- pairs$qj[rr]; k <- pairs$tk[rr]
    if (used_q[j] || used_t[k]) next
    used_q[j] <- TRUE; used_t[k] <- TRUE
    out[[length(out)+1]] <- data.table(
      responseid = rid, role = tm$role[k],
      i_in_resname = q$i_in_resname[j],
      matched_resume_index = tm$resume_index[k],
      matched_r_do = tm$r_do[k],
      diff = pairs$diff[rr])
  }
  rbindlist(out)
}

resp_list <- unique(fl$responseid)
maps <- rbindlist(lapply(resp_list, match_strict))
high <- maps[diff < 0.5]

cat(sprintf("\nN total resp-resume rows: 18 * %d = %d\n", length(resp_list), 18 * length(resp_list)))
cat(sprintf("N high-confidence matches (|diff|<0.5): %d (%.1f%%)\n",
            nrow(high), 100 * nrow(high) / (18 * length(resp_list))))

cat("\n##### High-confidence modal mapping i_in_resname -> resume_index, GLOBAL #####\n")
ct <- high[, .N, by = .(i_in_resname, matched_resume_index)][
  order(i_in_resname, -N)]
top <- ct[, .SD[1], by = i_in_resname]
top_n <- high[, .N, by = i_in_resname]
top <- merge(top, top_n[, .(i_in_resname, total_N = N)], by = "i_in_resname")
top[, pct := round(100 * N / total_N, 1)]
print(top[order(i_in_resname),
          .(i_in_resname, mode_idx = matched_resume_index, n = N,
            total_N, pct)])

cat("\n##### Same, by role #####\n")
for (rl in c("HR", "Eng")) {
  cat(sprintf("\n--- role = %s ---\n", rl))
  h <- high[role == rl]
  ct <- h[, .N, by = .(i_in_resname, matched_resume_index)][order(i_in_resname, -N)]
  top <- ct[, .SD[1], by = i_in_resname]
  total_n <- h[, .N, by = i_in_resname]
  top <- merge(top, total_n[, .(i_in_resname, total_N = N)], by = "i_in_resname")
  top[, pct := round(100 * N / total_N, 1)]
  print(top[order(i_in_resname),
            .(i_in_resname, mode_idx = matched_resume_index,
              n = N, total_N, pct)])
}

cat("\n##### Sanity: how many respondents have ALL 18 high-conf matches? #####\n")
cnt_per_resp <- maps[, .(n_match = .N, n_high = sum(diff < 0.5)), by = responseid]
print(cnt_per_resp[, .(N_resp = .N,
                       n_with_18_high = sum(n_high == 18),
                       pct_with_18 = round(100*sum(n_high==18)/.N, 1),
                       n_with_17plus = sum(n_high >= 17),
                       n_with_15plus = sum(n_high >= 15))])
