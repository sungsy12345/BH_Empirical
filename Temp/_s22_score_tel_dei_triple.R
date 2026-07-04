## Triple interaction: blind x tel x pro-DEI on HR score. URM-inference predicts
## blind:tel:pro_dei > 0 (high-DEI HR give the top-exp/low-GPA group a bigger
## blinding boost); signal re-weighting predicts ~0 triple. tel = below-median.
suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,tidyverse,lubridate,fixest)})
options(warn=-1, readr.show_types=FALSE)
date<-"18mar2026"; data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
d<-copy(firm_long_dt); d[, blind:=as.integer(treat=="blind")]
d[, top_exp:=as.integer(as.character(max_firm_signal_tier)=="Top")]
gmed<-median(unique(d[,.(resume_index,gpa)])$gpa)
d[, tel:=as.integer(top_exp==1 & gpa<=gmed)]
## pick a pro-DEI measure
cand<-c("pro_dei_index_z","dei_index_z","z_dei_index5","dei_index_pca_z"); dv<-cand[cand %in% names(d)][1]
cat("Using pro-DEI measure:", dv, "\n")
d[, dei_z:=as.numeric(scale(get(dv)))]
d[, dei_hi:=as.integer(dei_z>0)]   # above-median pro-DEI
FE_HR<-"resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income"
FE_EN<-paste(FE_HR,"+ resp_python + resp_java")
P<-function(m,t){ct<-coeftable(m); if(t%in%rownames(ct)){p<-ct[t,4];st<-if(p<.01)"***" else if(p<.05)"**" else if(p<.1)"*" else "";sprintf("%+.3f%s (p=%.3f)",ct[t,1],st,p)}else "NA"}
for(rl in c("HR","Eng")){
  fe<-if(rl=="Eng")FE_EN else FE_HR; dat<-copy(d[role==rl & !is.na(dei_z)])
  dat[, `:=`(bt=blind*tel, bd=blind*dei_z, td=tel*dei_z, btd=blind*tel*dei_z)]
  m<-feols(as.formula(paste0("sc_overall_z ~ blind + tel + dei_z + bt + bd + td + btd + resp_age | ",fe)),dat,vcov=~responseid)
  cat(sprintf("\n[%s] blind x tel = %s | blind x tel x proDEI = %s\n", rl, P(m,"bt"), P(m,"btd")))
  ## binary split: blind x tel among high vs low pro-DEI
  for(g in c(1,0)){ sub<-dat[dei_hi==g]; sub[, bt2:=blind*tel]
    ms<-feols(as.formula(paste0("sc_overall_z ~ blind + bt2 + resp_age | ",fe)),sub,vcov=~responseid)
    cat(sprintf("     blind x tel among %s pro-DEI %s: %s  (N=%d)\n", if(g==1)"HIGH" else "LOW ", rl, P(ms,"bt2"), nrow(sub))) }
}
