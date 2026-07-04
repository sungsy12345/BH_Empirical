suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,fixest)})
options(warn=-1, readr.show_types=FALSE); date<-"18mar2026"
data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
d<-firm_long_dt[role=="Eng"]
d[, true_z:=as.numeric(scale(overall_score))]
d[, oc:=as.integer((!is.na(open_code1)&open_code1==1)|(!is.na(open_code2)&open_code2==1))]
cand<-unique(d[,.(resume_index,overall_score)])[order(-overall_score)]; tophalf<-cand$resume_index[1:9]
slope<-function(ids,fml,reg,lbl){ s<-d[resume_index%in%ids]
  m<-feols(as.formula(fml),s,vcov=~responseid); ct<-coeftable(m)
  i<-paste0(reg,":oc")
  cat(sprintf("  %-12s base=%.3f  +opened=%+.3f (p=%.2f)\n", lbl, ct[reg,1], ct[i,1], ct[i,4])) }
cat("=== Direction A: score ~ true (slope = SD-of-score per SD-of-true) ===\n")
slope(cand$resume_index,"sc_overall_z ~ true_z*oc | responseid","true_z","FULL")
slope(tophalf,         "sc_overall_z ~ true_z*oc | responseid","true_z","TOP-HALF")
cat("=== Direction B: true ~ score (slope = SD-of-true per SD-of-assigned-score) ===\n")
slope(cand$resume_index,"true_z ~ sc_overall_z*oc | responseid","sc_overall_z","FULL")
slope(tophalf,         "true_z ~ sc_overall_z*oc | responseid","sc_overall_z","TOP-HALF")
cat("=== Symmetric: within-respondent correlation(score, true), by opened ===\n")
pc<-function(ids,o){ s<-d[resume_index%in%ids & oc==o]
  # partial out respondent means
  s[, sc_d:=sc_overall_z-mean(sc_overall_z), by=responseid]; s[, tr_d:=true_z-mean(true_z), by=responseid]
  cor(s$sc_d,s$tr_d) }
cat(sprintf("  FULL:     not-opened r=%.2f  opened r=%.2f\n", pc(cand$resume_index,0), pc(cand$resume_index,1)))
cat(sprintf("  TOP-HALF: not-opened r=%.2f  opened r=%.2f\n", pc(tophalf,0), pc(tophalf,1)))
cat(sprintf("\nWithin-resp SD: true(full)=%.2f true(top)=%.2f  score=%.2f (standardized)\n",
   sd(d[, true_z-mean(true_z), by=responseid]$V1), sd(d[resume_index%in%tophalf, true_z-mean(true_z), by=responseid]$V1), 1))
