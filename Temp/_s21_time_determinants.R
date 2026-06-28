## (0) Determinants of Attention -- TIME ON PAGE
## Identify off cross-candidate variation WITHIN evaluator: responseid FE +
## display position. Candidate attributes are constant within resume_index.
## NOTE: HR see resume only (no test_case/code); Engineers see test_case + code.
suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,tidyverse,lubridate,fixest)})
options(warn=-1, readr.show_types=FALSE)
date <- "18mar2026"; data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
setwd(here::here())
invisible(capture.output({ for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R"))) }))
d <- copy(firm_long_dt)
d[, ok := !is.na(timeonpage) & timeonpage <= 600]
d[, gpa_z   := as.numeric(scale(gpa))]
d[, awards_z:= as.numeric(scale(num_awards))]
d[, test_z  := as.numeric(scale(test_case))]
d[, prest   := fcase(as.character(max_firm_signal_tier)=="Top",2,
                     as.character(max_firm_signal_tier) %in% c("Ordinary","Growth"),1, default=0)]
d[, top_exp := as.integer(as.character(max_firm_signal_tier)=="Top")]
d[, paper_z := as.numeric(scale(rowMeans(cbind(scale(gpa), scale(num_awards), scale(prest)))))]
d[, mismatch:= abs(paper_z - test_z)]
d[, pos     := as.numeric(str_extract(as.character(r_do),"[0-9]+"))]
d[, urm_race_a   := as.integer(grepl("Hispanic", as.character(race_gender)))]   # assigned URM race
d[, urm_gender_a := as.integer(grepl("Female",   as.character(race_gender)))]   # assigned female
cl <- ~responseid
mu <- function(rl) mean(d[role==rl & ok, timeonpage], na.rm=TRUE)
cat(sprintf("MEAN time/resume (capped 600s): HR = %.1fs | Eng = %.1fs\n", mu("HR"), mu("Eng")))
cat(sprintf("Nonblind mean: HR = %.1fs | Eng = %.1fs\n\n",
   mean(d[role=="HR"&ok&treat=='nonblind',timeonpage]), mean(d[role=="Eng"&ok&treat=='nonblind',timeonpage])))
P <- function(m, terms, lbl){ ct<-coeftable(m); cat("  [",lbl,"] N=",nobs(m),"\n",sep="")
  for(t in terms) if(t %in% rownames(ct)) cat(sprintf("     %-14s %+7.2f s (p=%.3f)\n", t, ct[t,1], ct[t,4])) }

for(rl in c("HR","Eng")){
  dd <- d[role==rl & ok]
  cat("################# ROLE:",rl," #################\n")
  cat("-- (1) Quality: univariate (control: responseid FE + display position FE) --\n")
  sigs <- c("gpa_z","awards_z","top_exp"); if(rl=="Eng") sigs <- c(sigs,"test_z")
  for(s in sigs) P(feols(as.formula(paste0("timeonpage ~ ",s," | responseid + pos")), dd, vcov=cl), s, paste("uni",s))
  if(rl=="Eng"){
    cat("-- (2) Quality mismatch (resume credentials vs test case) [Eng only] --\n")
    P(feols(timeonpage ~ mismatch | responseid + pos, dd, vcov=cl), "mismatch","uni mismatch")
    P(feols(timeonpage ~ test_z + paper_z + mismatch | responseid + pos, dd, vcov=cl),
      c("test_z","paper_z","mismatch"), "test+paper+mismatch")
  }
  cat("-- (3) Race / Gender [NONBLIND only, assigned] --\n")
  dn <- dd[treat=="nonblind"]
  P(feols(timeonpage ~ urm_race_a | responseid + pos, dn, vcov=cl), "urm_race_a","uni URM-race")
  P(feols(timeonpage ~ urm_gender_a | responseid + pos, dn, vcov=cl), "urm_gender_a","uni URM-gender")
  P(feols(timeonpage ~ urm_race_a + urm_gender_a | responseid + pos, dn, vcov=cl),
    c("urm_race_a","urm_gender_a"), "race+gender")
  cat("-- (4) Fatigue: time vs display position (resume_index FE absorbs candidate) --\n")
  P(feols(timeonpage ~ pos | responseid + resume_index, dd, vcov=cl), "pos","fatigue slope (per step)")
  cat("-- (5) COMBINED kitchen-sink (nonblind; incl. fatigue as linear pos) --\n")
  rhs <- if(rl=="Eng") "gpa_z + awards_z + top_exp + test_z + mismatch + urm_race_a + urm_gender_a + pos" else
                       "gpa_z + awards_z + top_exp + urm_race_a + urm_gender_a + pos"
  P(feols(as.formula(paste0("timeonpage ~ ",rhs," | responseid")), dd[treat=="nonblind"], vcov=cl),
    c("gpa_z","awards_z","top_exp","test_z","mismatch","urm_race_a","urm_gender_a","pos"), "combined")
  cat("\n")
}
