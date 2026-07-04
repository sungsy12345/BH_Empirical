## Heterogeneous treatment effect of BLINDING on engagement (time, opening).
## Causal FE structure (mirrors the S2.2 tables): resume_index + r_do +
## respondent-characteristic FE; NOT evaluator FE (blind is between-evaluator).
## Heterogeneity = blind x candidate feature. Demographics use the TRUE identity.
suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,tidyverse,lubridate,fixest)})
options(warn=-1, readr.show_types=FALSE)
date<-"18mar2026"; data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
d<-copy(firm_long_dt)
d[, ok:=!is.na(timeonpage)&timeonpage<=600]
d[, blind:=as.integer(treat=="blind")]
d[, gpa_z:=as.numeric(scale(gpa))]; d[, test_z:=as.numeric(scale(test_case))]
d[, prest:=fcase(as.character(max_firm_signal_tier)=="Top",2, as.character(max_firm_signal_tier)%in%c("Ordinary","Growth"),1,default=0)]
d[, paper_z:=as.numeric(scale(rowMeans(cbind(scale(gpa),scale(num_awards),scale(prest)))))]
d[, qm1:=as.integer(paper_z>0 & test_z<0)]   # Quality Mismatch I: strong resume, weak coding
d[, qm2:=as.integer(test_z>0 & paper_z<0)]   # Quality Mismatch II: strong coding, weak resume
d[, true_urm_race:=as.integer(grepl("Hispanic", as.character(demo_group)))]   # TRUE identity
d[, true_urm_female:=as.integer(grepl("Female", as.character(demo_group)))]
d[, oc:=as.integer((!is.na(open_code1)&open_code1==1)|(!is.na(open_code2)&open_code2==1))]
FE_HR<-"resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income"
FE_EN<-paste(FE_HR,"+ resp_python + resp_java")
het<-function(dat,dv,x,fe,unit){ dat<-copy(dat); dat[, xx:=get(x)]; dat[, bx:=blind*xx]
  m<-feols(as.formula(paste0(dv," ~ blind + bx + resp_age | ",fe)), dat, vcov=~responseid); ct<-coeftable(m)
  f<-function(t) if(t%in%rownames(ct)) sprintf("%+.2f%s", if(unit=="pp")100*ct[t,1] else ct[t,1], if(ct[t,4]<.01)"***" else if(ct[t,4]<.05)"**" else if(ct[t,4]<.1)"*" else "") else "NA"
  cat(sprintf("   blind x %-16s = %-9s (p=%.3f)   [blind ATE=%s]\n", x, f("bx"), ct["bx",4], f("blind"))) }

cat("############ OPENING (Engineer, pp) ############\n")
for(dv in c("open_resume","oc")){ cat("== OUTCOME:",dv,"==\n")
  for(x in c("test_z","gpa_z","qm1","qm2","true_urm_race","true_urm_female")) het(d[role=="Eng"], dv, x, FE_EN, "pp") }
cat("\n############ TIME ON PAGE (seconds) ############\n")
cat("== ENG (saw test/code) ==\n")
for(x in c("test_z","gpa_z","qm2","true_urm_race","true_urm_female")) het(d[role=="Eng"&ok], "timeonpage", x, FE_EN, "s")
cat("== HR (saw resume only) ==\n")
for(x in c("gpa_z","true_urm_race","true_urm_female")) het(d[role=="HR"&ok], "timeonpage", x, FE_HR, "s")
