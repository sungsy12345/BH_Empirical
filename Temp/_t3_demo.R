suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,fixest)})
options(warn=-1, readr.show_types=FALSE); date<-"18mar2026"
data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
d<-copy(firm_long_dt)
d[, ok:=!is.na(timeonpage)&timeonpage<=600]
d[, gpa_z:=as.numeric(scale(gpa))]; d[, awards_z:=as.numeric(scale(num_awards))]
d[, top_exp:=as.integer(as.character(max_firm_signal_tier)=="Top")]
d[, test_z:=as.numeric(scale(test_case))]
d[, prest:=fcase(as.character(max_firm_signal_tier)=="Top",2, as.character(max_firm_signal_tier)%in%c("Ordinary","Growth"),1, default=0)]
d[, paper_z:=as.numeric(scale(rowMeans(cbind(scale(gpa),scale(num_awards),scale(prest)))))]
d[, dir_paper:=as.integer(paper_z>0 & test_z<0)]; d[, dir_code:=as.integer(test_z>0 & paper_z<0)]
d[, dpos:=as.numeric(str_extract(as.character(r_do),"[0-9]+"))]
d<-d[ok & dpos>=4]
cat("Assigned race_gender values (nonblind):", paste(sort(unique(d[treat=="nonblind",race_gender])),collapse=", "), "\n\n")
dn<-d[treat=="nonblind"]
dn[, rg:=relevel(factor(as.character(race_gender)), ref="White_Male")]
P<-function(m,lbl){ ct<-coeftable(m)
  cat("---", lbl, " (N=", m$nobs, ") ---\n", sep="")
  for(v in rownames(ct)){ p<-ct[v,4]; st<-if(p<.01)"***" else if(p<.05)"**" else if(p<.1)"*" else ""
    cat(sprintf("  %-26s %+7.2f%-3s (SE %.2f, p=%.3f)\n", v, ct[v,1], st, ct[v,2], p)) }
  w<-tryCatch(fixest::wald(m, "rg", print=FALSE), error=function(e) NULL)
  if(!is.null(w)) cat(sprintf("  >> Joint test of the 5 demographic dummies: p=%.3f\n", w$p)) }
cat(sprintf("Mean time (nonblind, pos 4-18): HR=%.1fs  Eng=%.1fs\n\n", mean(dn[role=="HR",timeonpage]), mean(dn[role=="Eng",timeonpage])))
mHR <-feols(timeonpage ~ gpa_z+awards_z+top_exp+dpos + rg | responseid, dn[role=="HR"],  vcov=~responseid)
mEng<-feols(timeonpage ~ gpa_z+awards_z+top_exp+test_z+dir_paper+dir_code+dpos + rg | responseid, dn[role=="Eng"], vcov=~responseid)
P(mHR,"HR: time ~ quality + assigned demographics (ref = White Male)")
cat("\n"); P(mEng,"Eng: time ~ quality + assigned demographics (ref = White Male)")
