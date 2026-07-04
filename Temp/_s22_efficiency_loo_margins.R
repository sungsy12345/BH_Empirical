## Efficiency of blinding, margin-aware + leave-one-out.
##  (A) VALIDITY: does the evaluator score track TRUE coding ability? slope of
##      within-respondent sc_overall_z on true ability, blind vs nonblind.
##  (B) SELECTION: true ability of each respondent's top-K picks, blind vs nonblind.
## Run FULL, DROP-top-true-ability-candidate, and TOP-HALF margin.
suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,tidyverse,lubridate,fixest)})
options(warn=-1, readr.show_types=FALSE)
date<-"18mar2026"; data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
d<-copy(firm_long_dt); d[, blind:=as.integer(treat=="blind")]
d[, true_z:=as.numeric(scale(overall_score))]
cand<-unique(d[,.(resume_index, overall_score, true_z)])[order(-overall_score)]
top_id<-cand$resume_index[1]; allids<-cand$resume_index; tophalf<-cand$resume_index[1:9]
cat("Top true-ability candidate: resume",top_id," (overall_score=",round(cand$overall_score[1],1),", next=",round(cand$overall_score[2],1),")\n\n")

vslope<-function(rl,ids,lbl){ s<-d[role==rl & resume_index %in% ids]
  m<-feols(sc_overall_z ~ true_z*blind | responseid, s, vcov=~responseid); ct<-coeftable(m)
  nb<-ct["true_z",1]; di<-ct["true_z:blind",1]; p<-ct["true_z:blind",4]
  cat(sprintf("    %-22s nonblind=%.3f  blind=%.3f  (blinding effect %+.3f, p=%.3f)\n", lbl, nb, nb+di, di, p)) }
cat("=========== (A) VALIDITY slope: score on true ability ===========\n")
for(rl in c("HR","Eng")){ cat(rl,":\n"); vslope(rl,allids,"FULL (18)"); vslope(rl,setdiff(allids,top_id),"DROP top candidate"); vslope(rl,tophalf,"TOP-HALF ability") }

cat("\n=========== (B) SELECTION: true ability of top-K picks (blind vs nonblind) ===========\n")
sel<-function(rl,K,pool){ s<-d[role==rl & resume_index %in% pool]
  picks<-s[, .(tz=true_z[order(-sc_overall)][1:min(K,.N)]), by=.(responseid,blind)]
  agg<-picks[, .(m=mean(tz,na.rm=TRUE)), by=.(responseid,blind)]
  bb<-agg[blind==1,mean(m,na.rm=TRUE)]; nn<-agg[blind==0,mean(m,na.rm=TRUE)]
  pv<-t.test(m~blind,agg)$p.value
  cat(sprintf("    %-22s top-%d: nonblind=%.3f  blind=%.3f  (diff %+.3f, p=%.3f)\n", "", K, nn, bb, bb-nn, pv)) }
for(rl in c("HR","Eng")){ cat(rl,":\n")
  for(K in c(1,3,5)) sel(rl,K,allids)
  cat("    -- pool excludes top true-ability candidate --\n"); for(K in c(1,3,5)) sel(rl,K,setdiff(allids,top_id)) }
