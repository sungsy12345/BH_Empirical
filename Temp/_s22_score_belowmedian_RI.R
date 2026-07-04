## Below-median GPA + top-exp (resumes 2,4,8,14): blinding effect on score,
## with randomization inference (permute which 4 candidates are "tel").
suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,tidyverse,lubridate,fixest)})
options(warn=-1, readr.show_types=FALSE)
date<-"18mar2026"; data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
d<-copy(firm_long_dt); d[, blind:=as.integer(treat=="blind")]
d[, top_exp:=as.integer(as.character(max_firm_signal_tier)=="Top")]
d[, gpa_med:=median(unique(.SD[,.(resume_index,gpa)])$gpa)]
tel_ids<-sort(unique(d[top_exp==1 & gpa<=gpa_med, resume_index])); cat("tel (below-median) resumes:",paste(tel_ids,collapse=","),"\n")
allc<-sort(unique(d$resume_index))
FE_HR<-"resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income"
FE_EN<-paste(FE_HR,"+ resp_python + resp_java")
coef_bt<-function(dat,fe,ids){ dat<-copy(dat); dat[, telx:=as.integer(resume_index %in% ids)]; dat[, bt:=blind*telx]
  m<-tryCatch(feols(as.formula(paste0("sc_overall_z ~ blind + bt + resp_age | ",fe)),dat,vcov=~responseid),error=function(e)NULL)
  if(is.null(m)||!"bt"%in%rownames(coeftable(m))) return(c(NA,NA)); ct<-coeftable(m); c(ct["bt",1],ct["bt",4]) }
set.seed(42); B<-600
for(rl in c("HR","Eng")){
  fe<-if(rl=="Eng")FE_EN else FE_HR; dat<-d[role==rl]
  act<-coef_bt(dat,fe,tel_ids)
  perm<-replicate(B, coef_bt(dat,fe,sample(allc,length(tel_ids)))[1])
  perm<-perm[!is.na(perm)]
  pri<-mean(perm>=act[1]); pti<-mean(abs(perm)>=abs(act[1]))
  cat(sprintf("[%s] blind x tel = %+.3f SD (analytic p=%.3f) | RI: one-sided p=%.3f, two-sided p=%.3f  (vs %d random 4-cand groups)\n",
      rl, act[1], act[2], pri, pti, length(perm)))
  # descriptive: mean score for tel candidates, blind vs nonblind
  dd<-dat[resume_index %in% tel_ids]
  cat(sprintf("     tel candidates mean sc_overall_z: blind=%+.3f, nonblind=%+.3f\n", mean(dd[blind==1,sc_overall_z],na.rm=T), mean(dd[blind==0,sc_overall_z],na.rm=T)))
}
