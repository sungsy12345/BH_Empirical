suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,tidyverse,lubridate,fixest,kableExtra)})
options(warn=-1, readr.show_types=FALSE)
date<-"18mar2026"; data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
od<-copy(firm_long_dt[role=="Eng"])
od[, gpa_z:=as.numeric(scale(gpa))]; od[, awards_z:=as.numeric(scale(num_awards))]
od[, prest:=fcase(as.character(max_firm_signal_tier)=="Top",2, as.character(max_firm_signal_tier)%in%c("Ordinary","Growth"),1,default=0)]
od[, top_exp:=as.integer(as.character(max_firm_signal_tier)=="Top")]
od[, paper_z:=as.numeric(scale(rowMeans(cbind(scale(gpa),scale(num_awards),scale(prest)))))]
od[, test_z:=as.numeric(scale(test_case))]; od[, dpos:=as.numeric(str_extract(as.character(r_do),"[0-9]+"))]
od[, oc:=as.integer((!is.na(open_code1)&open_code1==1)|(!is.na(open_code2)&open_code2==1))]
od[, dir_paper:=as.integer(paper_z>0 & test_z<0)]; od[, dir_code:=as.integer(test_z>0 & paper_z<0)]
od[, urm_race:=as.integer(grepl("Hispanic",as.character(race_gender)))]; od[, urm_gender:=as.integer(grepl("Female",as.character(race_gender)))]
gc<-function(dat,dv,rhs,v){ m<-feols(as.formula(paste0(dv," ~ ",rhs," | responseid")),dat,vcov=~responseid); ct<-coeftable(m)
  e<-ct[v,1]; s<-ct[v,2]; p<-ct[v,4]; st<-if(p<.01)"***" else if(p<.05)"**" else if(p<.1)"*" else ""; sprintf("%.1f%s (%.1f)",100*e,st,100*s) }
qA<-function(dv) c(gc(od,dv,"gpa_z + dpos","gpa_z"), gc(od,dv,"test_z + dpos","test_z"), gc(od,dv,"awards_z + dpos","awards_z"),
                   gc(od,dv,"top_exp + dpos","top_exp"), gc(od,dv,"dir_paper + dir_code + dpos","dir_paper"), gc(od,dv,"dir_paper + dir_code + dpos","dir_code"))
nb<-od[treat=="nonblind"]; qB<-function(dv) c(gc(nb,dv,"urm_race + urm_gender + dpos","urm_race"), gc(nb,dv,"urm_race + urm_gender + dpos","urm_gender"))
aR<-qA("open_resume"); aC<-qA("oc"); bR<-qB("open_resume"); bC<-qB("oc")
rn<-c("GPA","Test Cases","# Awards","Top Exp","Strong Paper/Weak Coding","Strong Coding/Weak Paper"); cat("ROW                         OpenResume   OpenCode\n"); for(i in 1:6) cat(sprintf("%-26s  %-12s %-12s\n",rn[i],aR[i],aC[i])); cat(sprintf("%-26s  %-12s %-12s\n","URM-Race(nb)",bR[1],bC[1])); cat(sprintf("%-26s  %-12s %-12s\n","URM-Gender(nb)",bR[2],bC[2]))
chk<-"\\checkmark"; meanOR<-sprintf("%.1f",100*mean(od$open_resume)); meanOC<-sprintf("%.1f",100*mean(od$oc))
nA<-format(nrow(od),big.mark=","); nB<-format(nrow(nb),big.mark=",")
body<-data.frame(Determinant=c("GPA (Z)","Test Cases Passed (Z)","\\# Awards (Z)","Top Experience","Strong Paper, Weak Coding","Strong Coding, Weak Paper",
  "Mean Open Rate (\\%)","Display Position Control","Respondent FE","Observations","URM --- Race","URM --- Gender","Observations"),
  "Open Resume"=c(aR,meanOR,chk,chk,nA,bR,nB), "Open Code"=c(aC,meanOC,chk,chk,nA,bC,nB), check.names=FALSE, stringsAsFactors=FALSE)
k<-kbl(body, format="latex", booktabs=TRUE, escape=FALSE, align="lcc", caption="Determinants of Opening Rates (Engineer Phase Only)",
       col.names=c(" ","Open Resume","Open Code")) %>%
  kable_styling(latex_options="HOLD_position", font_size=8) %>%
  pack_rows("Panel A: Quality, Mismatch \\& Fatigue (all rounds)",1,10,escape=FALSE) %>%
  pack_rows("Panel B: Demographics (Nonblind only)",11,13,escape=FALSE) %>%
  row_spec(6, extra_latex_after="\\midrule") %>% row_spec(10, extra_latex_after="\\midrule")
cat("\nLaTeX chars:",nchar(paste(as.character(k),collapse=""))," contains tabular:",grepl("tabular",paste(as.character(k),collapse="")),"\n[BUILD OK]\n")
