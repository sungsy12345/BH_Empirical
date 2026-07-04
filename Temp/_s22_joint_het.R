suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,fixest)})
options(warn=-1, readr.show_types=FALSE); date<-"18mar2026"
data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
d<-copy(firm_long_dt[role=="Eng"]); d[, blind:=as.integer(treat=="blind")]
d[, oc:=as.integer((!is.na(open_code1)&open_code1==1)|(!is.na(open_code2)&open_code2==1))]
d[, test_z:=as.numeric(scale(test_case))]; d[, gpa_z:=as.numeric(scale(gpa))]; d[, awards_z:=as.numeric(scale(num_awards))]
d[, top_exp:=as.integer(as.character(max_firm_signal_tier)=="Top")]
d[, prest:=fcase(as.character(max_firm_signal_tier)=="Top",2, as.character(max_firm_signal_tier)%in%c("Ordinary","Growth"),1,default=0)]
d[, paper_z:=as.numeric(scale(rowMeans(cbind(scale(gpa),scale(num_awards),scale(prest)))))]
d[, qm1:=as.integer(paper_z>0 & test_z<0)]; d[, qm2:=as.integer(test_z>0 & paper_z<0)]
FE<-"resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income + resp_python + resp_java"

cat("=== Correlations among the 6 candidate characteristics (across 18 resumes) ===\n")
cand<-unique(d[,.(resume_index,test_z,gpa_z,awards_z,top_exp,qm1,qm2)])
print(round(cor(cand[,.(test_z,gpa_z,awards_z,top_exp,qm1,qm2)]),2))

mk<-function(x) {d[, paste0("b_",x):=blind*get(x)]}
for(x in c("test_z","gpa_z","awards_z","top_exp","qm1","qm2")) mk(x)
cat("\n=== JOINT: all 6 Blind x characteristic interactions in ONE regression ===\n")
mj<-feols(oc ~ blind + b_test_z + b_gpa_z + b_awards_z + b_top_exp + b_qm1 + b_qm2 + resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income + resp_python + resp_java, d, vcov=~responseid)
print(round(coeftable(mj)[grepl("b_|blind",rownames(coeftable(mj))),],3))
cat("\nfixest collinearity removal note (if any):\n"); print(mj$collin.var)

cat("\n=== FOCUSED: each Blind x characteristic alone (SE for comparison) ===\n")
for(x in c("test_z","gpa_z","awards_z","top_exp","qm1","qm2")){
  d[, bx:=blind*get(x)]
  m<-feols(oc ~ blind + bx + resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income + resp_python + resp_java, d, vcov=~responseid)
  ct<-coeftable(m); cat(sprintf("  b_%-9s focused: %+.1f pp (SE %.1f)\n", x, 100*ct["bx",1], 100*ct["bx",2])) }
