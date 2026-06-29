## Does the race/gender margin turn ON conditional on a visible quality feature?
## Interactions demographic x quality, NONBLIND only, within-evaluator (responseid FE).
## Eng conditions on TEST CASES (and GPA); HR conditions on GPA (never saw test cases).
suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,tidyverse,lubridate,fixest)})
options(warn=-1, readr.show_types=FALSE)
date<-"18mar2026"; data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
d<-copy(firm_long_dt)
d[, ok:=!is.na(timeonpage)&timeonpage<=600]
d[, gpa_z:=as.numeric(scale(gpa))]; d[, test_z:=as.numeric(scale(test_case))]
d[, dpos:=as.numeric(str_extract(as.character(r_do),"[0-9]+"))]
d[, oc:=as.integer((!is.na(open_code1)&open_code1==1)|(!is.na(open_code2)&open_code2==1))]
d[, urm_race:=as.integer(grepl("Hispanic",as.character(race_gender)))]
d[, urm_gender:=as.integer(grepl("Female",as.character(race_gender)))]
nb<-d[treat=="nonblind"]
PR<-function(dat,dv,rhs,terms,lbl,unit){ m<-feols(as.formula(paste0(dv,"~",rhs,"|responseid")),dat,vcov=~responseid); ct<-coeftable(m)
  cat("  [",lbl,"]\n",sep="")
  for(t in terms) if(t%in%rownames(ct)) cat(sprintf("     %-26s %+7.3f %s (p=%.3f)\n", t, if(unit=="pp")100*ct[t,1] else ct[t,1], unit, ct[t,4])) }

cat("###################### ENGINEER: condition on TEST CASES ######################\n")
cat("---- OPENING (pp) ----\n")
for(dv in c("open_resume","oc")){
  cat("OUTCOME:",dv,"\n")
  PR(nb[role=="Eng"], dv, "urm_race*test_z + dpos", c("urm_race","test_z","urm_race:test_z"), "URM-race x test", "pp")
  PR(nb[role=="Eng"], dv, "urm_gender*test_z + dpos", c("urm_gender","test_z","urm_gender:test_z"), "URM-gender x test", "pp")
}
cat("---- TIME (seconds) ----\n")
PR(nb[role=="Eng"&ok], "timeonpage", "urm_race*test_z + dpos", c("urm_race","test_z","urm_race:test_z"), "Eng time: URM-race x test", "s")
PR(nb[role=="Eng"&ok], "timeonpage", "urm_gender*test_z + dpos", c("urm_gender","test_z","urm_gender:test_z"), "Eng time: URM-gender x test", "s")

cat("\n###################### Also: Eng condition on GPA ######################\n")
for(dv in c("open_resume","oc"))
  PR(nb[role=="Eng"], dv, "urm_race*gpa_z + urm_gender*gpa_z + dpos", c("urm_race:gpa_z","urm_gender:gpa_z"), paste(dv,"x GPA"), "pp")

cat("\n###################### HR: condition on GPA (never saw test) ######################\n")
PR(nb[role=="HR"&ok], "timeonpage", "urm_race*gpa_z + dpos", c("urm_race","gpa_z","urm_race:gpa_z"), "HR time: URM-race x GPA", "s")
PR(nb[role=="HR"&ok], "timeonpage", "urm_gender*gpa_z + dpos", c("urm_gender","gpa_z","urm_gender:gpa_z"), "HR time: URM-gender x GPA", "s")
