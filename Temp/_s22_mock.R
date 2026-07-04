suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,fixest)})
options(warn=-1, readr.show_types=FALSE); date<-"18mar2026"
data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
d<-copy(firm_long_dt); d[, blind:=as.integer(treat=="blind")]; d[, ok:=!is.na(timeonpage)&timeonpage<=600]
d[, dpos:=as.numeric(str_extract(as.character(r_do),"[0-9]+"))]
d[, oc:=as.integer((!is.na(open_code1)&open_code1==1)|(!is.na(open_code2)&open_code2==1))]
d[, test_z:=as.numeric(scale(test_case))]; d[, gpa_z:=as.numeric(scale(gpa))]; d[, awards_z:=as.numeric(scale(num_awards))]
d[, top_exp:=as.integer(as.character(max_firm_signal_tier)=="Top")]
d[, prest:=fcase(as.character(max_firm_signal_tier)=="Top",2, as.character(max_firm_signal_tier)%in%c("Ordinary","Growth"),1,default=0)]
d[, paper_z:=as.numeric(scale(rowMeans(cbind(scale(gpa),scale(num_awards),scale(prest)))))]
d[, qm1:=as.integer(paper_z>0 & test_z<0)]; d[, qm2:=as.integer(test_z>0 & paper_z<0)]
FE_HR<-"resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income"
FE_EN<-paste(FE_HR,"+ resp_python + resp_java")
fmt<-function(e,s,p,unit){st<-if(p<.01)"***" else if(p<.05)"**" else if(p<.1)"*" else ""; sprintf("%+.1f%s (%.1f)",if(unit=="pp")100*e else e,st,if(unit=="pp")100*s else s)}
cellc<-function(dat,dv,fe){m<-feols(as.formula(paste0(dv,"~ blind + resp_age |",fe)),dat,vcov=~responseid);ct<-coeftable(m);fmt(ct["blind",1],ct["blind",2],ct["blind",4],if(dv=="timeonpage")"s" else "pp")}
cellx<-function(dat,dv,fe,x){dat<-copy(dat);dat[,bx:=blind*get(x)];m<-feols(as.formula(paste0(dv,"~ blind + bx + resp_age |",fe)),dat,vcov=~responseid);ct<-coeftable(m);fmt(ct["bx",1],ct["bx",2],ct["bx",4],if(dv=="timeonpage")"s" else "pp")}
rows<-c("test_z","gpa_z","awards_z","top_exp","qm1","qm2")
cat("===== TABLE 5: Effect of Blinding on Time Spent (seconds) =====\n")
cat(sprintf("%-34s %14s %14s\n","","HR","Engineer"))
cat(sprintf("%-34s %14s %14s\n","Blinding effect (baseline)", cellc(d[role=="HR"&ok&dpos>=4],"timeonpage",FE_HR), cellc(d[role=="Eng"&ok&dpos>=4],"timeonpage",FE_EN)))
for(x in rows){ hr<-if(x %in% c("test_z","qm1","qm2")) "---" else cellx(d[role=="HR"&ok&dpos>=4],"timeonpage",FE_HR,x)
  cat(sprintf("  x %-30s %14s %14s\n", x, hr, cellx(d[role=="Eng"&ok&dpos>=4],"timeonpage",FE_EN,x))) }
cat("\n===== TABLE 6: Material Engagement -- Opening Rate (Engineer, pp) =====\n")
cat(sprintf("%-34s %14s %14s\n","","Open Resume","Open Code"))
cat(sprintf("%-34s %14s %14s\n","Blinding effect (baseline)", cellc(d[role=="Eng"],"open_resume",FE_EN), cellc(d[role=="Eng"],"oc",FE_EN)))
for(x in rows){ cat(sprintf("  x %-30s %14s %14s\n", x, cellx(d[role=="Eng"],"open_resume",FE_EN,x), cellx(d[role=="Eng"],"oc",FE_EN,x))) }
