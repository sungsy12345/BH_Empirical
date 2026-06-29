## Determinants of ENGINEER opening rates: promise-chasing (linear + U-shape),
## quality mismatch (directional hinge), race/gender, and combined.
## Within-evaluator (responseid FE) + display position; cluster by respondent.
suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,tidyverse,lubridate,fixest)})
options(warn=-1, readr.show_types=FALSE)
date <- "18mar2026"; data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
setwd(here::here())
invisible(capture.output({ for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R"))) }))
d <- firm_long_dt[role=="Eng"]
d[, gpa_z := as.numeric(scale(gpa))]; d[, awards_z := as.numeric(scale(num_awards))]
d[, prest := fcase(as.character(max_firm_signal_tier)=="Top",2, as.character(max_firm_signal_tier)%in%c("Ordinary","Growth"),1, default=0)]
d[, top_exp := as.integer(as.character(max_firm_signal_tier)=="Top")]
d[, paper_z := as.numeric(scale(rowMeans(cbind(scale(gpa),scale(num_awards),scale(prest)))))]
d[, test_z := as.numeric(scale(test_case))]
d[, dpos := as.numeric(str_extract(as.character(r_do),"[0-9]+"))]
d[, open_code1_or_2 := as.integer((!is.na(open_code1)&open_code1==1)|(!is.na(open_code2)&open_code2==1))]
d[, code_over_paper := pmax(test_z - paper_z, 0)]   # coding exceeds resume (the live mismatch direction)
d[, urm_race := as.integer(grepl("Hispanic", as.character(race_gender)))]
d[, urm_gender := as.integer(grepl("Female", as.character(race_gender)))]

P <- function(dv, rhs, terms, lbl, dat=d){ base<-mean(dat[[dv]],na.rm=TRUE)
  m<-feols(as.formula(paste0(dv," ~ ",rhs," | responseid")), dat, vcov=~responseid); ct<-coeftable(m)
  cat("  [",lbl,"] base=",sprintf("%.3f",base),"\n",sep="")
  for(t in terms) if(t %in% rownames(ct)) cat(sprintf("     %-22s %+.1f pp (%+.0f%% of base)  p=%.3f\n", t, 100*ct[t,1], 100*ct[t,1]/base, ct[t,4])) }

for(dv in c("open_resume","open_code1_or_2")){
  cat("\n##################### OUTCOME:",dv," #####################\n")
  cat("-- Promise-chasing: linear + U-shape --\n")
  P(dv, "gpa_z + I(gpa_z^2) + dpos", c("gpa_z","I(gpa_z^2)"), "GPA linear+quad")
  P(dv, "test_z + I(test_z^2) + dpos", c("test_z","I(test_z^2)"), "Test linear+quad")
  P(dv, "gpa_z + test_z + awards_z + top_exp + dpos", c("gpa_z","test_z","awards_z","top_exp"), "All quality (linear)")
  cat("-- Quality mismatch (verification on conflict): code-exceeds-paper --\n")
  P(dv, "code_over_paper + dpos", "code_over_paper", "code>paper hinge")
  cat("-- Race/Gender (NONBLIND, assigned) --\n")
  P(dv, "urm_race + urm_gender + dpos", c("urm_race","urm_gender"), "URM race + gender", dat=d[treat=="nonblind"])
  cat("-- COMBINED (nonblind: quality + mismatch + demo + position) --\n")
  P(dv, "gpa_z + test_z + awards_z + top_exp + code_over_paper + urm_race + urm_gender + dpos",
    c("gpa_z","test_z","awards_z","top_exp","code_over_paper","urm_race","urm_gender"), "Combined", dat=d[treat=="nonblind"])
}
