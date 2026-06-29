## Dev/build-test for the S2.1 "Determinants of Opening Rates" table (2 panels,
## 2 sample sizes). Verifies the 8 regressions + the stacked-panel kable build.
suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,tidyverse,lubridate,fixest,modelsummary,kableExtra)})
options(warn=-1, readr.show_types=FALSE)
date<-"18mar2026"; data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
od <- copy(firm_long_dt[role=="Eng"])
od[, gpa_z:=as.numeric(scale(gpa))]; od[, awards_z:=as.numeric(scale(num_awards))]
od[, prest:=fcase(as.character(max_firm_signal_tier)=="Top",2, as.character(max_firm_signal_tier)%in%c("Ordinary","Growth"),1,default=0)]
od[, top_exp:=as.integer(as.character(max_firm_signal_tier)=="Top")]
od[, paper_z:=as.numeric(scale(rowMeans(cbind(scale(gpa),scale(num_awards),scale(prest)))))]
od[, test_z:=as.numeric(scale(test_case))]; od[, dpos:=as.numeric(str_extract(as.character(r_do),"[0-9]+"))]
od[, oc:=as.integer((!is.na(open_code1)&open_code1==1)|(!is.na(open_code2)&open_code2==1))]
od[, dir_paper:=as.integer(paper_z>0 & test_z<0)]; od[, dir_code:=as.integer(test_z>0 & paper_z<0)]
od[, urm_race:=as.integer(grepl("Hispanic",as.character(race_gender)))]; od[, urm_gender:=as.integer(grepl("Female",as.character(race_gender)))]
od[, or_pp:=100*open_resume]; od[, oc_pp:=100*oc]
fA<-function(dv) feols(as.formula(paste0(dv," ~ gpa_z + test_z + awards_z + top_exp + dir_paper + dir_code + dpos | responseid")), od, vcov=~responseid)
fB<-function(dv) feols(as.formula(paste0(dv," ~ urm_race + urm_gender + dpos | responseid")), od[treat=="nonblind"], vcov=~responseid)
mA<-list("Open Resume"=fA("or_pp"),"Open Code"=fA("oc_pp")); mB<-list("Open Resume"=fB("or_pp"),"Open Code"=fB("oc_pp"))
cat("Panel A N:", sapply(mA,nobs), " | Panel B N:", sapply(mB,nobs), "\n")
cmA<-c("gpa_z"="GPA (Z)","test_z"="Test Cases Passed (Z)","awards_z"="\\# Awards (Z)","top_exp"="Top Experience","dir_paper"="Strong Paper, Weak Coding","dir_code"="Strong Coding, Weak Paper")
cmB<-c("urm_race"="URM --- Race","urm_gender"="URM --- Gender"); chk<-"\\checkmark"
g<-tribble(~raw,~clean,~fmt,"nobs","Observations",0,"r.squared","R$^2$",3)
feA<-data.frame(term=c("Mean Open Rate (\\%)","Respondent FE","Display Position Control"),
  "Open Resume"=c(sprintf("%.1f",mean(od$or_pp)),chk,chk),"Open Code"=c(sprintf("%.1f",mean(od$oc_pp)),chk,chk),check.names=FALSE,stringsAsFactors=FALSE)
attr(feA,"position")<-c(7,8,9)
feB<-data.frame(term=c("Respondent FE","Display Position Control"),"Open Resume"=c(chk,chk),"Open Code"=c(chk,chk),check.names=FALSE,stringsAsFactors=FALSE)
attr(feB,"position")<-c(3,4)
strip<-function(k){s<-paste(as.character(k),collapse="\n"); s<-gsub("\\\\begin\\{table\\}[^\n]*\n?","",s); s<-gsub("\\\\end\\{table\\}\\s*","",s); s}
kA<-modelsummary(mA,estimate="{estimate}{stars} ({std.error})",statistic=NULL,fmt=1,coef_map=cmA,gof_map=g,add_rows=feA,escape=FALSE,stars=c('*'=.1,'**'=.05,'***'=.01),output="kableExtra")%>%
  kable_styling(latex_options="HOLD_position",font_size=8)%>%row_spec(6,extra_latex_after="\\midrule")%>%row_spec(9,extra_latex_after="\\midrule")
kB<-modelsummary(mB,estimate="{estimate}{stars} ({std.error})",statistic=NULL,fmt=1,coef_map=cmB,gof_map=g,add_rows=feB,escape=FALSE,stars=c('*'=.1,'**'=.05,'***'=.01),output="kableExtra")%>%
  kable_styling(latex_options="HOLD_position",font_size=8)%>%row_spec(2,extra_latex_after="\\midrule")
sA<-strip(kA); sB<-strip(kB)
cat("kA latex chars:",nchar(sA)," kB latex chars:",nchar(sB),"\n")
cat("contains tabular A:", grepl("tabular",sA), " B:", grepl("tabular",sB), "\n")
cat("Strong Coding row present:", grepl("Strong Coding, Weak Paper",sA), " | URM row present:", grepl("URM --- Race",sB), "\n")
cat("[BUILD OK]\n")
