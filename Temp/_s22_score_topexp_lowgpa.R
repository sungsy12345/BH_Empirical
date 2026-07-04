## Does the blinding effect on EVALUATION SCORE differ for "top-experience but
## low-GPA" candidates? If evaluators infer URM from this profile even when
## blinded, blind x (top-exp/low-GPA) should be non-zero. Causal spec: blind
## indicator, resume_index + r_do + respondent-characteristic FE (incl. display
## order), cluster by respondent. Outcome = sc_overall_z (within-respondent z).
suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,tidyverse,lubridate,fixest)})
options(warn=-1, readr.show_types=FALSE)
date<-"18mar2026"; data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
d<-copy(firm_long_dt); d[, blind:=as.integer(treat=="blind")]
d[, top_exp:=as.integer(as.character(max_firm_signal_tier)=="Top")]
gpa_med<-median(unique(d[,.(resume_index,gpa)])$gpa); gpa_t1<-quantile(unique(d[,.(resume_index,gpa)])$gpa,1/3)
d[, tel_tercile:=as.integer(top_exp==1 & gpa<=gpa_t1)]    # resumes 4,8
d[, tel_median :=as.integer(top_exp==1 & gpa<=gpa_med)]   # resumes 4,8,14,2
cat("tel_tercile candidates:", paste(sort(unique(d[tel_tercile==1,resume_index])),collapse=","),
    " | tel_median:", paste(sort(unique(d[tel_median==1,resume_index])),collapse=","), "\n\n")
FE_HR<-"resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income"
FE_EN<-paste(FE_HR,"+ resp_python + resp_java")
run<-function(rl,tel,fe){ dat<-copy(d[role==rl]); dat[, bt:=blind*get(tel)]
  m<-feols(as.formula(paste0("sc_overall_z ~ blind + bt + resp_age | ",fe)), dat, vcov=~responseid); ct<-coeftable(m)
  f<-function(t){p<-ct[t,4];st<-if(p<.01)"***" else if(p<.05)"**" else if(p<.1)"*" else "";sprintf("%+.3f%s (p=%.3f)",ct[t,1],st,p)}
  cat(sprintf("  [%s, %s]  blind(others)=%s | blind x tel=%s | blinding eff for tel = %+.3f\n",
      rl, tel, f("blind"), f("bt"), ct["blind",1]+ct["bt",1])) }
cat("=== Effect of blinding on sc_overall_z, by top-exp/low-GPA ===\n")
cat("-- HR --\n");  run("HR","tel_tercile",FE_HR); run("HR","tel_median",FE_HR)
cat("-- Engineer --\n"); run("Eng","tel_tercile",FE_EN); run("Eng","tel_median",FE_EN)
cat("\n(For reference, recall engineers give an out-group/URM PREMIUM when demographic is visible;\n if blinding raises tel scores (positive blind x tel), it is consistent with inferred-URM under blinding.)\n")
