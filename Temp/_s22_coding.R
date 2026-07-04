suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,fixest)})
options(warn=-1, readr.show_types=FALSE); date<-"18mar2026"
data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
d<-firm_long_dt[role=="Eng"]
d[, true_z:=as.numeric(scale(overall_score))]
d[, oc:=as.integer((!is.na(open_code1)&open_code1==1)|(!is.na(open_code2)&open_code2==1))]
cand<-unique(d[,.(resume_index,overall_score)])[order(-overall_score)]
top<-cand$resume_index[1:9]
pc<-function(sv,ids,o){ s<-d[resume_index%in%ids & oc==o & !is.na(get(sv))]
  s[, sc_d:=get(sv)-mean(get(sv)), by=responseid]; s[, tr_d:=true_z-mean(true_z), by=responseid]
  cor(s$sc_d,s$tr_d) }
sl<-function(sv,ids){ s<-d[resume_index%in%ids]; m<-feols(as.formula(paste0(sv," ~ true_z*oc | responseid")),s,vcov=~responseid); ct<-coeftable(m)
  sprintf("base=%.2f +opened=%+.2f (p=%.2f)", ct["true_z",1], ct["true_z:oc",1], ct["true_z:oc",4]) }
for(sv in c("sc_overall_z","sc_coding_z")){
  cat("==== Assigned score =", sv, "vs TRUE coding ability ====\n")
  cat(sprintf("  Correlation  FULL:     not-opened r=%.2f  opened r=%.2f\n", pc(sv,cand$resume_index,0), pc(sv,cand$resume_index,1)))
  cat(sprintf("  Correlation  TOP-half: not-opened r=%.2f  opened r=%.2f\n", pc(sv,top,0), pc(sv,top,1)))
  cat(sprintf("  Slope(score~true) FULL: %s | TOP: %s\n\n", sl(sv,cand$resume_index), sl(sv,top)))
}
