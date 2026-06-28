## Section 2.1 follow-up: does TIME / EFFORT vary by the ASSIGNED demographic
## (race_gender) and by evaluator-candidate concordance, under NONBLIND?
suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,tidyverse,lubridate,fixest,splitstackshape,Hmisc,zipcodeR,sf,tigris)})
options(warn=-1, readr.show_types=FALSE)
date <- "18mar2026"; data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
setwd(here::here())
invisible(capture.output({ for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R"))) }))
d <- copy(firm_long_dt)
d[, blind := as.integer(treat=="blind")]
d[, ok_ton := !is.na(timeonpage) & timeonpage <= 600]
d[, open_code1_or_2 := as.integer((!is.na(open_code1)&open_code1==1)|(!is.na(open_code2)&open_code2==1))]
d[, pos := as.numeric(str_extract(as.character(r_do), "[0-9]+"))]
d[, rg := as.character(race_gender)]
cat("race_gender levels:", paste(sort(unique(d$rg)), collapse=" | "), "\n")
cat("concordance_race_gender values:\n"); print(table(d$concordance_race_gender, d$treat, useNA="ifany"))
cl <- ~responseid
prdemo <- function(m, lbl){ ct<-coeftable(m); cat("  [",lbl,"] N=",nobs(m),"\n",sep="")
  for(t in grep("rg::|urm|concordance", rownames(ct), value=TRUE)) cat(sprintf("     %-26s %+.4f (p=%.3f)\n", sub("^rg::","",t), ct[t,1], ct[t,4])) }

cat("\n===== Extra TIME by ASSIGNED demographic (nonblind, ref = White_Male) =====\n")
for(rl in c("HR","Eng")){
  dn <- d[role==rl & treat=="nonblind" & ok_ton & !is.na(rg)]
  dn[, rg := relevel(factor(rg), ref="White_Male")]
  prdemo(feols(timeonpage ~ i(rg, ref="White_Male") | responseid + pos, dn, vcov=cl), paste(rl,"time"))
}
cat("\n===== Eng OPEN-CODE by ASSIGNED demographic (nonblind, ref = White_Male) =====\n")
de <- d[role=="Eng" & treat=="nonblind" & !is.na(rg)]
de[, rg := relevel(factor(rg), ref="White_Male")]
prdemo(feols(open_code1_or_2 ~ i(rg, ref="White_Male") | responseid + pos, de, vcov=cl), "open_code")
prdemo(feols(open_resume ~ i(rg, ref="White_Male") | responseid + pos, de, vcov=cl), "open_resume")

cat("\n===== Concordance (homophily): does sharing demo with candidate change time/effort? (nonblind) =====\n")
for(rl in c("HR","Eng")){
  dn <- d[role==rl & treat=="nonblind" & ok_ton]
  prdemo(feols(timeonpage ~ concordance_race_gender | responseid + pos, dn, vcov=cl), paste(rl,"time~concordance"))
}
prdemo(feols(open_code1_or_2 ~ concordance_race_gender | responseid + pos, d[role=="Eng"&treat=="nonblind"], vcov=cl), "Eng open_code~concordance")

cat("\n===== Does blinding RAISE opening more where the demographic cue was 'doing work'? =====\n")
## URM candidates: if demographic substituted for verification, blinding should raise opening more for URM
de2 <- d[role=="Eng" & !is.na(urm_tech_blind)]
m <- feols(open_code1_or_2 ~ blind*urm_tech_blind | responseid + pos, de2, vcov=cl)
ct<-coeftable(m); cat("  [Eng open_code: blind x URM] N=",nobs(m),"\n",sep="")
for(t in c("blind","urm_tech_blind","blind:urm_tech_blind")) if(t%in%rownames(ct)) cat(sprintf("     %-26s %+.4f (p=%.3f)\n",t,ct[t,1],ct[t,4]))
cat("(demo codes: WM WF AM AF HM HF; urm_tech_blind: 1 = URM-in-tech)\n")
