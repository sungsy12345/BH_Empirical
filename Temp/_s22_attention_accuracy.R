## #1 Attention -> accuracy: do engineers' scores track true ability BETTER for
## candidates whose code they opened? Within-evaluator (responseid FE).
## true_z:open = does opening improve validity. Margin: full, top-half, drop-top.
suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,tidyverse,lubridate,fixest)})
options(warn=-1, readr.show_types=FALSE); date<-"18mar2026"; data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
d<-firm_long_dt[role=="Eng"]; d[, blind:=as.integer(treat=="blind")]
d[, true_z:=as.numeric(scale(overall_score))]
d[, oc:=as.integer((!is.na(open_code1)&open_code1==1)|(!is.na(open_code2)&open_code2==1))]
cand<-unique(d[,.(resume_index,overall_score)])[order(-overall_score)]; top_id<-cand$resume_index[1]; tophalf<-cand$resume_index[1:9]
vp<-function(ids,lbl){ s<-d[resume_index %in% ids]
  m<-feols(sc_overall_z ~ true_z*oc | responseid, s, vcov=~responseid); ct<-coeftable(m)
  cat(sprintf("  %-20s validity if NOT opened=%.3f | extra-validity-if-opened (true_z:oc)=%+.3f (p=%.3f)\n", lbl, ct["true_z",1], ct["true_z:oc",1], ct["true_z:oc",4])) }
cat("ENGINEER: does opening the code improve score validity?\n")
vp(cand$resume_index,"FULL (18)"); vp(setdiff(cand$resume_index,top_id),"DROP top"); vp(tophalf,"TOP-HALF ability")
cat("\nAlso split by arm (blind candidates' code more likely opened):\n")
for(b in c(0,1)){ s<-d[blind==b]; m<-feols(sc_overall_z ~ true_z*oc | responseid, s, vcov=~responseid); ct<-coeftable(m)
  cat(sprintf("  %-9s true_z:oc = %+.3f (p=%.3f)\n", if(b==1)"BLIND" else "NONBLIND", ct["true_z:oc",1], ct["true_z:oc",4])) }
