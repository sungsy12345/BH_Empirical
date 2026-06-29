## Test the user's percentile-conjunction mismatch flags on engineer opening.
## Direction = "strong on paper, weak in code" (paper>code). Cutoffs from the
## 18-candidate distribution: bottom tercile test cases; top-half GPA; top-firm.
suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,tidyverse,lubridate,fixest)})
options(warn=-1, readr.show_types=FALSE)
date<-"18mar2026"; data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
d<-firm_long_dt[role=="Eng"]
d[, top_exp:=as.integer(as.character(max_firm_signal_tier)=="Top")]
d[, dpos:=as.numeric(str_extract(as.character(r_do),"[0-9]+"))]
d[, open_code1_or_2:=as.integer((!is.na(open_code1)&open_code1==1)|(!is.na(open_code2)&open_code2==1))]
## candidate-level distribution cutoffs (over the 18 unique candidates)
cand<-unique(d[,.(resume_index,gpa,test_case,top_exp)])
test_lo <- quantile(cand$test_case, 1/3); gpa_med <- median(cand$gpa); test_hi <- quantile(cand$test_case,2/3)
cat(sprintf("cutoffs: test bottom-tercile <= %.1f | test top-tercile >= %.1f | GPA median = %.2f\n", test_lo, test_hi, gpa_med))
## flags (paper>code: strong paper, weak code)
d[, f_topexp_lowtest  := as.integer(top_exp==1 & test_case<=test_lo)]
d[, f_highgpa_lowtest := as.integer(gpa>=gpa_med & test_case<=test_lo)]
d[, f_strongpaper_lowtest := as.integer((top_exp==1 | gpa>=gpa_med) & test_case<=test_lo)]
## for contrast: code>paper "hidden gem" (weak paper, strong code)
d[, f_lowpaper_hightest := as.integer(top_exp==0 & gpa<gpa_med & test_case>=test_hi)]
for(v in c("f_topexp_lowtest","f_highgpa_lowtest","f_strongpaper_lowtest","f_lowpaper_hightest"))
  cat(sprintf("  %-24s flags %d of 18 candidates\n", v, uniqueN(d[get(v)==1, resume_index])))
P<-function(dv,var){base<-mean(d[[dv]]); m<-feols(as.formula(paste0(dv,"~",var,"+dpos|responseid")),d,vcov=~responseid); ct<-coeftable(m)
  cat(sprintf("    %-24s on %-15s: %+.1f pp (%+.0f%% of base) p=%.3f\n", var, dv, 100*ct[var,1], 100*ct[var,1]/base, ct[var,4]))}
cat("\n=== PAPER>CODE: strong paper, weak code (your idea) ===\n")
for(v in c("f_topexp_lowtest","f_highgpa_lowtest","f_strongpaper_lowtest")){ P("open_resume",v); P("open_code1_or_2",v) }
cat("\n=== CONTRAST  CODE>PAPER: weak paper, strong code (hidden gem) ===\n")
P("open_resume","f_lowpaper_hightest"); P("open_code1_or_2","f_lowpaper_hightest")
