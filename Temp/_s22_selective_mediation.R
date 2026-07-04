suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,tidyverse,lubridate,fixest)})
options(warn=-1, readr.show_types=FALSE); date<-"18mar2026"; data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
d<-firm_long_dt[role=="Eng"]; d[, blind:=as.integer(treat=="blind")]
d[, test_z:=as.numeric(scale(test_case))]; d[, true_z:=as.numeric(scale(overall_score))]
d[, dpos:=as.numeric(str_extract(as.character(r_do),"[0-9]+"))]
d[, test_z2:=test_z^2]; d[, oc:=as.integer((!is.na(open_code1)&open_code1==1)|(!is.na(open_code2)&open_code2==1))]
cat("===== #5 Selective attention: do engineers open more for BORDERLINE candidates? =====\n")
## inverted-U in the visible quality signal (test cases) => peak at middle = borderline focus
m<-feols(oc ~ test_z + test_z2 + dpos | responseid, d, vcov=~responseid); ct<-coeftable(m)
cat(sprintf("  open_code ~ test + test^2:  linear=%+.4f (p=%.3f), quadratic=%+.4f (p=%.3f)\n", ct["test_z",1],ct["test_z",4],ct["test_z2",1],ct["test_z2",4]))
cat("  (negative quadratic = inverted-U = open most for middle/borderline; positive/null = extremes/monotonic)\n")
## distance from the candidate pool median test (closeness to a generic threshold)
d[, mid_dist:= -abs(test_z)]  # higher = closer to the middle
m2<-feols(oc ~ mid_dist + dpos | responseid, d, vcov=~responseid); ct2<-coeftable(m2)
cat(sprintf("  open_code ~ closeness-to-middle:  %+.4f (p=%.3f)  (positive = more opening for middling candidates)\n", ct2["mid_dist",1], ct2["mid_dist",4]))

cat("\n===== #2 Mediation: does the per-candidate CHANGE in opening track the CHANGE in score under blinding? =====\n")
agg<-d[, .(open_b=mean(oc[blind==1]), open_n=mean(oc[blind==0]), sc_b=mean(sc_overall_z[blind==1],na.rm=T), sc_n=mean(sc_overall_z[blind==0],na.rm=T)), by=resume_index]
agg[, d_open:=open_b-open_n][, d_score:=sc_b-sc_n]
ct<-cor.test(agg$d_open, agg$d_score)
cat(sprintf("  corr( delta-opening , delta-score ) across 18 candidates = %+.3f (p=%.3f)\n", ct$estimate, ct$p.value))
cat("  (positive => candidates inspected more under blinding also scored higher => inspection mediates; null => parallel channels)\n")
