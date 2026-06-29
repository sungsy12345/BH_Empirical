## Quality Mismatch -> ENGINEER opening rates (open_resume, open_code1_or_2).
## Within-evaluator (responseid FE) + display-position control; cluster by
## respondent. Mismatch vars standardized to SD=1 -> coefs are pp per SD of gap.
## Sign convention: positive gap = stronger on PAPER than in CODE.
suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,tidyverse,lubridate,fixest)})
options(warn=-1, readr.show_types=FALSE)
date <- "18mar2026"; data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
setwd(here::here())
invisible(capture.output({ for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R"))) }))
d <- firm_long_dt[role=="Eng"]
d[, gpa_z := as.numeric(scale(gpa))]; d[, awards_z := as.numeric(scale(num_awards))]
d[, prest := fcase(as.character(max_firm_signal_tier)=="Top",2, as.character(max_firm_signal_tier)%in%c("Ordinary","Growth"),1, default=0)]
d[, paper_z := as.numeric(scale(rowMeans(cbind(scale(gpa),scale(num_awards),scale(prest)))))]
d[, test_z := as.numeric(scale(test_case))]
d[, dpos := as.numeric(str_extract(as.character(r_do),"[0-9]+"))]
d[, open_code1_or_2 := as.integer((!is.na(open_code1)&open_code1==1)|(!is.na(open_code2)&open_code2==1))]
d[, mm_sym_comp  := as.numeric(scale(abs(paper_z - test_z)))]
d[, mm_sign_comp := as.numeric(scale(paper_z - test_z))]
d[, mm_sym_gpa   := as.numeric(scale(abs(gpa_z - test_z)))]
d[, mm_sign_gpa  := as.numeric(scale(gpa_z - test_z))]
d[, gap := paper_z - test_z]
d[, gap_paper_over_code := pmax(gap, 0)]
d[, gap_code_over_paper := pmax(-gap, 0)]

run_outcome <- function(dv){
  base <- mean(d[[dv]], na.rm=TRUE)
  cat(sprintf("\n########## OUTCOME: %s  (baseline open rate = %.3f) ##########\n", dv, base))
  P <- function(rhs, terms, lbl){ m<-feols(as.formula(paste0(dv," ~ ",rhs," | responseid")), d, vcov=~responseid); ct<-coeftable(m)
    cat("  [",lbl,"]\n",sep="")
    for(t in terms) if(t %in% rownames(ct)) cat(sprintf("     %-22s %+.4f (%+.1f pp, %+.0f%% of base)  p=%.3f\n", t, ct[t,1], 100*ct[t,1], 100*ct[t,1]/base, ct[t,4])) }
  cat("  -- Composite paper-index vs coding --\n")
  P("mm_sym_comp + dpos",  "mm_sym_comp",  "Symmetric |paper-code| (ref)")
  P("mm_sign_comp + dpos", "mm_sign_comp", "Signed (paper-code)")
  P("gap_paper_over_code + gap_code_over_paper + dpos", c("gap_paper_over_code","gap_code_over_paper"), "Hinge paper>code vs code>paper")
  cat("  -- Pure GPA vs Test --\n")
  P("mm_sym_gpa + dpos",  "mm_sym_gpa",  "Symmetric |GPA-test| (ref)")
  P("mm_sign_gpa + dpos", "mm_sign_gpa", "Signed (GPA-test)")
}
run_outcome("open_resume")
run_outcome("open_code1_or_2")
