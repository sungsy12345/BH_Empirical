## Non-demographic interactions, FULL sample, within-evaluator (responseid FE).
## Outcomes: Time on page (HR+Eng) and Opening (Eng). Magnitudes: time in s,
## opening in pp; quality vars standardized so coefs are per SD.
suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,tidyverse,lubridate,fixest)})
options(warn=-1, readr.show_types=FALSE)
date<-"18mar2026"; data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
d<-copy(firm_long_dt)
d[, ok:=!is.na(timeonpage)&timeonpage<=600]
d[, gpa_z:=as.numeric(scale(gpa))]; d[, test_z:=as.numeric(scale(test_case))]
d[, prest:=fcase(as.character(max_firm_signal_tier)=="Top",2, as.character(max_firm_signal_tier)%in%c("Ordinary","Growth"),1,default=0)]
d[, paper_z:=as.numeric(scale(rowMeans(cbind(scale(gpa),scale(num_awards),scale(prest)))))]
d[, dpos:=as.numeric(str_extract(as.character(r_do),"[0-9]+"))]
d[, oc:=as.integer((!is.na(open_code1)&open_code1==1)|(!is.na(open_code2)&open_code2==1))]
d[, blind:=as.integer(treat=="blind")]
d[, dir_code:=as.integer(test_z>0 & paper_z<0)]
PR<-function(dat,dv,rhs,terms,lbl,unit){ m<-feols(as.formula(paste0(dv,"~",rhs,"|responseid")),dat,vcov=~responseid); ct<-coeftable(m)
  cat("  [",lbl,"]\n",sep="")
  for(t in terms) if(t%in%rownames(ct)) cat(sprintf("     %-22s %+7.3f %s (p=%.3f)\n", t, if(unit=="pp")100*ct[t,1] else ct[t,1], unit, ct[t,4])) }
E<-d[role=="Eng"]; H<-d[role=="HR"]

cat("############### OPENING RATES (Engineer, pp) ###############\n")
for(dv in c("open_resume","oc")){
  cat("===== OUTCOME:",dv,"=====\n")
  PR(E, dv, "blind*test_z + dpos", "blind:test_z", "Blinding x Test (promise-chasing)", "pp")
  PR(E, dv, "blind*dir_code + dpos", "blind:dir_code", "Blinding x StrongCoding-WeakPaper", "pp")
  PR(E, dv, "test_z*dpos", "test_z:dpos", "Test x Display Position (fatigue)", "pp")
  PR(E, dv, "test_z*gpa_z + dpos", "test_z:gpa_z", "Test x GPA (complementarity)", "pp")
  PR(E, dv, "test_z*resp_python + dpos", "test_z:resp_python", "Test x Evaluator-codes-Python", "pp")
}
cat("\n############### TIME ON PAGE (seconds) ###############\n")
cat("===== ENG time =====\n")
PR(E[ok==TRUE], "timeonpage", "blind*test_z + dpos", "blind:test_z", "Blinding x Test", "s")
PR(E[ok==TRUE], "timeonpage", "test_z*dpos", "test_z:dpos", "Test x Display Position (fatigue)", "s")
PR(E[ok==TRUE], "timeonpage", "test_z*gpa_z + dpos", "test_z:gpa_z", "Test x GPA", "s")
cat("===== HR time (HR saw GPA, not test) =====\n")
PR(H[ok==TRUE], "timeonpage", "blind*gpa_z + dpos", "blind:gpa_z", "Blinding x GPA", "s")
PR(H[ok==TRUE], "timeonpage", "gpa_z*dpos", "gpa_z:dpos", "GPA x Display Position (fatigue)", "s")
