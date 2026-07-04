suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,fixest)})
options(warn=-1, readr.show_types=FALSE); date<-"18mar2026"
data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
d<-firm_long_dt[role=="Eng"]
d[, true_z:=as.numeric(scale(overall_score))]
d[, oc:=as.integer((!is.na(open_code1)&open_code1==1)|(!is.na(open_code2)&open_code2==1))]
cand<-unique(d[,.(resume_index,overall_score)])[order(-overall_score)]
top<-cand$resume_index[1:9]; bot<-cand$resume_index[10:18]
pc<-function(ids,o){ s<-d[resume_index%in%ids & oc==o]
  s[, sc_d:=sc_overall_z-mean(sc_overall_z), by=responseid]; s[, tr_d:=true_z-mean(true_z), by=responseid]
  cor(s$sc_d,s$tr_d) }
sdtrue<-function(ids) sd(d[resume_index%in%ids, true_z-mean(true_z), by=responseid]$V1)
cat("Within-respondent correlation(score, true) by opened, and within-resp SD of true ability:\n")
cat(sprintf("  FULL (18):    not-opened r=%.2f  opened r=%.2f   | true SD=%.2f\n", pc(cand$resume_index,0), pc(cand$resume_index,1), sdtrue(cand$resume_index)))
cat(sprintf("  TOP-half (9): not-opened r=%.2f  opened r=%.2f   | true SD=%.2f\n", pc(top,0), pc(top,1), sdtrue(top)))
cat(sprintf("  BOT-half (9): not-opened r=%.2f  opened r=%.2f   | true SD=%.2f\n", pc(bot,0), pc(bot,1), sdtrue(bot)))
# slopes too
sl<-function(ids,fml,reg){ s<-d[resume_index%in%ids]; m<-feols(as.formula(fml),s,vcov=~responseid); ct<-coeftable(m); i<-paste0(reg,":oc")
  sprintf("base=%.2f +opened=%+.2f (p=%.2f)", ct[reg,1], ct[i,1], ct[i,4]) }
cat("\nDirection A (score~true) BOT-half: ", sl(bot,"sc_overall_z ~ true_z*oc | responseid","true_z"),"\n")
cat("Direction B (true~score) BOT-half: ", sl(bot,"true_z ~ sc_overall_z*oc | responseid","sc_overall_z"),"\n")
