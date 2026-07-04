## Heterogeneous treatment effect of BLINDING on engagement, by candidate
## CHARACTERISTICS only (no true demographics). Causal FE (mirrors S2.2):
## resume_index + r_do + respondent-characteristic FE. blind = treatment
## indicator (blind reference / pooled nonblind), so blind:X is the
## heterogeneity of the *effect of blinding* by X.
suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,tidyverse,lubridate,fixest)})
options(warn=-1, readr.show_types=FALSE)
date<-"18mar2026"; data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
d<-copy(firm_long_dt)
d[, ok:=!is.na(timeonpage)&timeonpage<=600]; d[, blind:=as.integer(treat=="blind")]
d[, gpa_z:=as.numeric(scale(gpa))]; d[, test_z:=as.numeric(scale(test_case))]; d[, awards_z:=as.numeric(scale(num_awards))]
d[, top_exp:=as.integer(as.character(max_firm_signal_tier)=="Top")]
d[, prest:=fcase(as.character(max_firm_signal_tier)=="Top",2, as.character(max_firm_signal_tier)%in%c("Ordinary","Growth"),1,default=0)]
d[, paper_z:=as.numeric(scale(rowMeans(cbind(scale(gpa),scale(num_awards),scale(prest)))))]
d[, qm1:=as.integer(paper_z>0 & test_z<0)]; d[, qm2:=as.integer(test_z>0 & paper_z<0)]
d[, oc:=as.integer((!is.na(open_code1)&open_code1==1)|(!is.na(open_code2)&open_code2==1))]
FE_HR<-"resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income"
FE_EN<-paste(FE_HR,"+ resp_python + resp_java")
het<-function(dat,dv,x,fe,unit){ dat<-copy(dat); dat[, bx:=blind*get(x)]
  m<-feols(as.formula(paste0(dv," ~ blind + bx + resp_age | ",fe)), dat, vcov=~responseid); ct<-coeftable(m)
  st<-if(ct["bx",4]<.01)"***" else if(ct["bx",4]<.05)"**" else if(ct["bx",4]<.1)"*" else ""
  cat(sprintf("   Blinding effect x %-9s = %+.2f%s %s (p=%.3f)\n", x, if(unit=="pp")100*ct["bx",1] else ct["bx",1], st, unit, ct["bx",4])) }
cat("############ OPENING (Engineer, pp); base blind ATE printed first ############\n")
for(dv in c("open_resume","oc")){
  a<-feols(as.formula(paste0(dv," ~ blind + resp_age | ",FE_EN)), d[role=="Eng"], vcov=~responseid)
  cat("== ",dv," (blind ATE = ",sprintf("%+.1f pp",100*coef(a)["blind"]),", p=",sprintf("%.3f",coeftable(a)["blind",4]),") ==\n",sep="")
  for(x in c("test_z","gpa_z","awards_z","top_exp","qm1","qm2")) het(d[role=="Eng"], dv, x, FE_EN, "pp") }
cat("\n############ TIME (seconds) ############\n")
cat("== ENG ==\n"); for(x in c("test_z","gpa_z","awards_z","top_exp","qm1","qm2")) het(d[role=="Eng"&ok],"timeonpage",x,FE_EN,"s")
cat("== HR (resume only) ==\n"); for(x in c("gpa_z","awards_z","top_exp")) het(d[role=="HR"&ok],"timeonpage",x,FE_HR,"s")
