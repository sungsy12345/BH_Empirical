suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,fixest,knitr,kableExtra,tidyverse)})
options(warn=-1, readr.show_types=FALSE); date<-"18mar2026"
data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
firm_long_dt[, ok_ton := !is.na(timeonpage) & timeonpage <= 600]
firm_long_dt[, blind := as.integer(treat == "blind")]
firm_long_dt[, dpos := as.numeric(str_extract(as.character(r_do), "[0-9]+"))]
firm_long_dt[, test_case_z := as.numeric(scale(test_case))]
firm_long_dt[, open_code1_or_2 := as.integer((!is.na(open_code1)&open_code1==1)|(!is.na(open_code2)&open_code2==1))]
firm_long_dt[, gpa_z := as.numeric(scale(gpa))]; firm_long_dt[, awards_z := as.numeric(scale(num_awards))]
firm_long_dt[, top_exp := as.integer(as.character(max_firm_signal_tier)=="Top")]
firm_long_dt[, prest := fcase(as.character(max_firm_signal_tier)=="Top",2, as.character(max_firm_signal_tier)%in%c("Ordinary","Growth"),1,default=0)]
firm_long_dt[, paper_z := as.numeric(scale(rowMeans(cbind(scale(gpa),scale(num_awards),scale(prest)))))]
firm_long_dt[, qm1 := as.integer(paper_z>0 & test_case_z<0)]; firm_long_dt[, qm2 := as.integer(test_case_z>0 & paper_z<0)]
FE_HR<-"resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income"; FE_EN<-paste(FE_HR,"+ resp_python + resp_java")
chk<-"\\checkmark"; nox<-"$\\times$"; XS<-c("test_case_z","gpa_z","awards_z","top_exp","qm1","qm2")
xrow<-c("Test Cases Passed (Z)","GPA (Z)","\\# Awards (Z)","Top Experience","Quality Mismatch I","Quality Mismatch II")
fcell<-function(ct,term,unit){if(!term%in%rownames(ct))return("---");e<-ct[term,1];s<-ct[term,2];p<-ct[term,4];st<-if(p<.01)"***" else if(p<.05)"**" else if(p<.1)"*" else "";sprintf("%+.1f%s (%.1f)",if(unit=="pp")100*e else e,st,if(unit=="pp")100*s else s)}
basec<-function(dat,dv,fe,unit){m<-feols(as.formula(paste0(dv," ~ blind + resp_age | ",fe)),dat,vcov=~responseid);list(cell=fcell(coeftable(m),"blind",unit),n=m$nobs)}
intx<-function(dat,dv,fe,x,unit){d2<-copy(dat);d2[,bx:=blind*get(x)];m<-feols(as.formula(paste0(dv," ~ blind + bx + resp_age | ",fe)),d2,vcov=~responseid);fcell(coeftable(m),"bx",unit)}
dT_hr<-firm_long_dt[role=="HR"&ok_ton&dpos>=4]; dT_en<-firm_long_dt[role=="Eng"&ok_ton&dpos>=4]; dO<-firm_long_dt[role=="Eng"]
b_hr<-basec(dT_hr,"timeonpage",FE_HR,"s");b_en<-basec(dT_en,"timeonpage",FE_EN,"s");b_or<-basec(dO,"open_resume",FE_EN,"pp");b_oc<-basec(dO,"open_code1_or_2",FE_EN,"pp")
het_body<-data.frame(Term=xrow,
  HR=sapply(XS,function(x) if(x%in%c("test_case_z","qm1","qm2"))"---" else intx(dT_hr,"timeonpage",FE_HR,x,"s"),USE.NAMES=FALSE),
  EN=sapply(XS,function(x) intx(dT_en,"timeonpage",FE_EN,x,"s"),USE.NAMES=FALSE),
  OR=sapply(XS,function(x) intx(dO,"open_resume",FE_EN,x,"pp"),USE.NAMES=FALSE),
  OC=sapply(XS,function(x) intx(dO,"open_code1_or_2",FE_EN,x,"pp"),USE.NAMES=FALSE), check.names=FALSE, stringsAsFactors=FALSE)
cat("BASELINE row:  HR",b_hr$cell," EN",b_en$cell," OR",b_or$cell," OC",b_oc$cell,"\n")
cat("N: ",b_hr$n,b_en$n,b_or$n,b_oc$n,"\n")
cat("rownames(het_body) default (should be 1..6):", paste(rownames(het_body),collapse=","),"\n")
print(het_body)
k<-kbl(het_body,format="latex",booktabs=TRUE,escape=FALSE,align="lcccc",col.names=c("Blinding $\\times$","HR","Engineer","Open Resume","Open Code"))
cat("\nLaTeX header line check (count '&' in body rows -> should be 4 per row, i.e. 5 cols):\n")
ll<-strsplit(as.character(k),"\n")[[1]]; for(L in ll) if(grepl("Test Cases|Quality Mismatch I ",L)) cat("  ",L,"\n")
cat("DONE OK\n")
