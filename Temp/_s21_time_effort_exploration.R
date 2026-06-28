## Section 2.1 exploration: drivers of TIME (timeonpage) and EFFORT (open_*),
## and whether the blinding treatment effect varies by those drivers.
## Candidate attributes are constant within resume_index, so we identify off
## cross-candidate variation WITHIN evaluator: responseid FE + display order,
## clustered by responseid. (resume_index FE would absorb candidate attributes.)
suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,tidyverse,lubridate,fixest,splitstackshape,Hmisc,zipcodeR,sf,tigris)})
options(warn=-1, readr.show_types=FALSE)
date <- "18mar2026"; data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
setwd(here::here())
invisible(capture.output({ for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R"))) }))

d <- copy(firm_long_dt)
d[, blind := as.integer(treat=="blind")]
d[, ok_ton := !is.na(timeonpage) & timeonpage <= 600]
d[, test_case_z := as.numeric(scale(test_case))]
d[, open_code1_or_2 := as.integer((!is.na(open_code1)&open_code1==1)|(!is.na(open_code2)&open_code2==1))]
## composite resume-paper signal (academic + experience), candidate-level
for(v in c("gpa","num_proj","num_exp","num_lead","num_awards")) d[, paste0(v,"_s"):=as.numeric(scale(get(v)))]
d[, resume_z := rowMeans(.SD), .SDcols=paste0(c("gpa","num_proj","num_exp","num_lead","num_awards"),"_s")]
d[, resume_z := as.numeric(scale(resume_z))]
## signal divergence: resume paper signal vs coding signal
d[, mismatch := abs(resume_z - test_case_z)]
## display-order / fatigue proxy from r_do
d[, pos := as.numeric(str_extract(as.character(r_do), "[0-9]+"))]
cat("pos (display order) range:", paste(range(d$pos,na.rm=TRUE),collapse="-"), " | has concordance_race_gender:", "concordance_race_gender" %in% names(d), " | urm_tech:", "urm_tech" %in% names(d), "\n\n")

cl <- ~responseid
pr <- function(m, terms, lbl){ ct<-coeftable(m); cat("  [",lbl,"] N=",nobs(m),"\n",sep="")
  for(t in terms) if(t %in% rownames(ct)) cat(sprintf("     %-26s %+.4f (p=%.3f)\n", t, ct[t,1], ct[t,4])) }

cat("=================== (i) TIME: which candidates get more time? ===================\n")
for(rl in c("HR","Eng")){
  dd <- d[role==rl & ok_ton]
  m1 <- feols(timeonpage ~ test_case_z + I(test_case_z^2) + resume_z + blind | responseid + pos, dd, vcov=cl)
  pr(m1, c("test_case_z","I(test_case_z^2)","resume_z","blind"), paste(rl,"quality+quadratic"))
  m2 <- feols(timeonpage ~ (test_case_z + resume_z)*blind | responseid + pos, dd, vcov=cl)
  pr(m2, c("test_case_z","resume_z","blind","test_case_z:blind","resume_z:blind"), paste(rl,"x blind"))
}

cat("\n=================== (ii) EFFORT (Eng): which candidates get opened? ===================\n")
de <- d[role=="Eng"]
for(dv in c("open_resume","open_code1_or_2")){
  m1 <- feols(as.formula(paste0(dv,"~ test_case_z + I(test_case_z^2) + resume_z + mismatch + blind | responseid + pos")), de, vcov=cl)
  pr(m1, c("test_case_z","I(test_case_z^2)","resume_z","mismatch","blind"), paste(dv,"drivers"))
}

cat("\n=================== (iii) TREATMENT-EFFECT HETEROGENEITY (Eng opening) ===================\n")
for(dv in c("open_resume","open_code1_or_2")){
  m <- feols(as.formula(paste0(dv,"~ blind*test_case_z + blind*resume_z + blind*mismatch | responseid + pos")), de, vcov=cl)
  pr(m, c("blind","blind:test_case_z","blind:resume_z","blind:mismatch"), paste(dv,"x candidate quality"))
}

cat("\n=================== Demographic scrutiny under NONBLIND (assigned demo) ===================\n")
## extra time/opening by assigned demographic (nonblind only); ref = White Male
for(rl in c("HR","Eng")){
  dn <- d[role==rl & treat=="nonblind" & ok_ton]
  dn[, demo_f := relevel(factor(demo_group), ref="1")]
  m <- feols(timeonpage ~ i(demo_f, ref="1") | responseid + pos, dn, vcov=cl)
  ct<-coeftable(m); cat("  [",rl," TIME by assigned demo, ref=WhiteMale] N=",nobs(m),"\n",sep="")
  for(t in grep("demo_f", rownames(ct), value=TRUE)) cat(sprintf("     %-22s %+.2f (p=%.3f)\n", sub("demo_f::","grp",t), ct[t,1], ct[t,4]))
}
de_nb <- d[role=="Eng" & treat=="nonblind"]
de_nb[, demo_f := relevel(factor(demo_group), ref="1")]
m <- feols(open_code1_or_2 ~ i(demo_f, ref="1") | responseid + pos, de_nb, vcov=cl)
ct<-coeftable(m); cat("  [Eng OPEN-CODE by assigned demo, ref=WhiteMale] N=",nobs(m),"\n",sep="")
for(t in grep("demo_f", rownames(ct), value=TRUE)) cat(sprintf("     %-22s %+.4f (p=%.3f)\n", sub("demo_f::","grp",t), ct[t,1], ct[t,4]))
cat("\n(demo codes: 1 WM 2 WF 3 AM 4 AF 5 HM 6 HF)\n")
