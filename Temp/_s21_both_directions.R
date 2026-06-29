## Two mismatch-direction binaries (reference = concordant candidates):
##  dir_paper = strong paper, weak coding  (paper_z>0 & test_z<0)
##  dir_code  = strong coding, weak paper  (test_z>0 & paper_z<0)
## Engineer opening rates; within-evaluator (responseid FE) + position.
suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,tidyverse,lubridate,fixest)})
options(warn=-1, readr.show_types=FALSE)
date<-"18mar2026"; data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
d<-firm_long_dt[role=="Eng"]
d[, prest:=fcase(as.character(max_firm_signal_tier)=="Top",2, as.character(max_firm_signal_tier)%in%c("Ordinary","Growth"),1,default=0)]
d[, paper_z:=as.numeric(scale(rowMeans(cbind(scale(gpa),scale(num_awards),scale(prest)))))]
d[, test_z:=as.numeric(scale(test_case))]; d[, dpos:=as.numeric(str_extract(as.character(r_do),"[0-9]+"))]
d[, open_code1_or_2:=as.integer((!is.na(open_code1)&open_code1==1)|(!is.na(open_code2)&open_code2==1))]
d[, dir_paper:=as.integer(paper_z>0 & test_z<0)]   # strong paper, weak coding
d[, dir_code :=as.integer(test_z>0 & paper_z<0)]   # strong coding, weak paper
cat("candidates flagged -- Strong Paper/Weak Coding:", uniqueN(d[dir_paper==1,resume_index]),
    "| Strong Coding/Weak Paper:", uniqueN(d[dir_code==1,resume_index]),
    "| concordant reference:", uniqueN(d[dir_paper==0 & dir_code==0,resume_index]), "\n\n")
P<-function(dv){ base<-mean(d[[dv]]); m<-feols(as.formula(paste0(dv,"~ dir_paper + dir_code + dpos | responseid")),d,vcov=~responseid); ct<-coeftable(m)
  cat("OUTCOME:",dv," (base=",sprintf("%.3f",base),")\n",sep="")
  for(t in c("dir_paper","dir_code")) cat(sprintf("   %-10s %+.1f pp (%+.0f%% of base)  p=%.3f\n", t, 100*ct[t,1], 100*ct[t,1]/base, ct[t,4])) }
P("open_resume"); P("open_code1_or_2")
