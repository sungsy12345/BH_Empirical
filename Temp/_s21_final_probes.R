suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,tidyverse,lubridate,fixest,splitstackshape,Hmisc,zipcodeR,sf,tigris)})
options(warn=-1, readr.show_types=FALSE)
date <- "18mar2026"; data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
setwd(here::here())
invisible(capture.output({ for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R"))) }))
d <- copy(firm_long_dt)
d[, blind := as.integer(treat=="blind")]
d[, test_case_z := as.numeric(scale(test_case))]
d[, open_code1_or_2 := as.integer((!is.na(open_code1)&open_code1==1)|(!is.na(open_code2)&open_code2==1))]
d[, pos := as.numeric(str_extract(as.character(r_do), "[0-9]+"))]
## TRUE underlying URM-in-tech = Hispanic (constant within candidate, defined in both arms)
d[, true_urm := as.integer(grepl("Hispanic", as.character(demo_group)))]
cl <- ~responseid
P <- function(m, terms, lbl){ ct<-coeftable(m); cat("  [",lbl,"] N=",nobs(m),"\n",sep="")
  for(t in terms) if(t%in%rownames(ct)) cat(sprintf("     %-26s %+.4f (p=%.3f)\n",t,ct[t,1],ct[t,4])) }
de <- d[role=="Eng"]

cat("===== (a) Opening gradient in test score: monotonic vs inverted-U (ambiguity) =====\n")
P(feols(open_resume    ~ test_case_z + I(test_case_z^2) | responseid + pos, de, vcov=cl),
  c("test_case_z","I(test_case_z^2)"), "open_resume")
P(feols(open_code1_or_2~ test_case_z + I(test_case_z^2) | responseid + pos, de, vcov=cl),
  c("test_case_z","I(test_case_z^2)"), "open_code")

cat("\n===== (b) Does blinding raise opening MORE for genuinely-URM (Hispanic) candidates? =====\n")
cat("  share URM (Hispanic) among 18:", round(mean(unique(d[,.(resume_index,true_urm)])$true_urm),3), "\n")
P(feols(open_resume    ~ blind*true_urm | responseid + pos, de, vcov=cl),
  c("blind","true_urm","blind:true_urm"), "open_resume x URM")
P(feols(open_code1_or_2~ blind*true_urm | responseid + pos, de, vcov=cl),
  c("blind","true_urm","blind:true_urm"), "open_code x URM")

cat("\n===== (c) Same, but by Female (gender cue) =====\n")
d[, true_female := as.integer(grepl("Female", as.character(demo_group)))]
de <- d[role=="Eng"]
P(feols(open_code1_or_2~ blind*true_female | responseid + pos, de, vcov=cl),
  c("blind","true_female","blind:true_female"), "open_code x Female")
