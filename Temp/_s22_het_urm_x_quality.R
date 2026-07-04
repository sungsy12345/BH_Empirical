suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,tidyverse,lubridate,fixest)})
options(warn=-1, readr.show_types=FALSE)
date<-"18mar2026"; data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
d<-copy(firm_long_dt); d[, ok:=!is.na(timeonpage)&timeonpage<=600]; d[, blind:=as.integer(treat=="blind")]
d[, gpa_z:=as.numeric(scale(gpa))]; d[, test_z:=as.numeric(scale(test_case))]
d[, oc:=as.integer((!is.na(open_code1)&open_code1==1)|(!is.na(open_code2)&open_code2==1))]
d[, rt:=relevel(factor(fcase(blind==1,"Blind", grepl("Hispanic",as.character(race_gender)),"NB_Hispanic", default="NB_Other")),ref="Blind")]
d[, gt:=relevel(factor(fcase(blind==1,"Blind", grepl("Female",as.character(race_gender)),"NB_Female", default="NB_Male")),ref="Blind")]
FE_EN<-"resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income + resp_python + resp_java"
hx<-function(dat,dv,fac,q,grpU,grpM,unit){
  m<-feols(as.formula(paste0(dv," ~ ",fac,"*",q," + resp_age | ",FE_EN)), dat, vcov=~responseid); ct<-coeftable(m); rn<-rownames(ct)
  pick<-function(g){t<-rn[grepl(g,rn)&grepl(q,rn)]; if(length(t)){b<- -ct[t[1],1];p<-ct[t[1],4];st<-if(p<.01)"***" else if(p<.05)"**" else if(p<.1)"*" else "";sprintf("%+.2f%s (p=%.3f)",if(unit=="pp")100*b else b,st,p)} else "dropped"}
  cat(sprintf("   blinding-effect slope on %-6s:  URM=%-18s | Majority=%s\n", q, pick(grpU), pick(grpM))) }
cat("####### OPEN CODE (Engineer; blinding-effect slope on quality, pp) #######\n")
cat("-- RACE axis (URM=Hispanic-assigned) --\n"); for(q in c("test_z","gpa_z")) hx(d[role=="Eng"],"oc","rt",q,"NB_Hispanic","NB_Other","pp")
cat("-- GENDER axis (URM=Female-assigned) --\n"); for(q in c("test_z","gpa_z")) hx(d[role=="Eng"],"oc","gt",q,"NB_Female","NB_Male","pp")
cat("\n####### OPEN RESUME (pp) #######\n")
hx(d[role=="Eng"],"open_resume","rt","test_z","NB_Hispanic","NB_Other","pp"); hx(d[role=="Eng"],"open_resume","gt","test_z","NB_Female","NB_Male","pp")
cat("\n####### ENG TIME (s) #######\n")
hx(d[role=="Eng"&ok],"timeonpage","rt","test_z","NB_Hispanic","NB_Other","s"); hx(d[role=="Eng"&ok],"timeonpage","gt","test_z","NB_Female","NB_Male","s")
