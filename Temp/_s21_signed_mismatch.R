## Quality Mismatch -> time on page (Engineers, positions 4-18, responseid FE).
## Compare SYMMETRIC |gap| vs SIGNED gap, for the composite paper index and for
## a pure GPA-vs-test-cases gap. Mismatch vars standardized to SD=1 so coefs are
## "seconds per SD of the gap". Sign convention: positive = stronger on PAPER
## than in CODE (resume looks better than the coding performance).
suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,tidyverse,lubridate,fixest)})
options(warn=-1, readr.show_types=FALSE)
date <- "18mar2026"; data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
setwd(here::here())
invisible(capture.output({ for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R"))) }))
d <- firm_long_dt[role=="Eng"]
d[, ok := !is.na(timeonpage) & timeonpage <= 600]
d[, gpa_z := as.numeric(scale(gpa))]; d[, awards_z := as.numeric(scale(num_awards))]
d[, prest := fcase(as.character(max_firm_signal_tier)=="Top",2, as.character(max_firm_signal_tier)%in%c("Ordinary","Growth"),1, default=0)]
d[, paper_z := as.numeric(scale(rowMeans(cbind(scale(gpa),scale(num_awards),scale(prest)))))]
d[, test_z := as.numeric(scale(test_case))]
d[, dpos := as.numeric(str_extract(as.character(r_do),"[0-9]+"))]
## gap vars (standardized to SD=1)
d[, mm_sym_comp  := as.numeric(scale(abs(paper_z - test_z)))]
d[, mm_sign_comp := as.numeric(scale(paper_z - test_z))]
d[, mm_sym_gpa   := as.numeric(scale(abs(gpa_z - test_z)))]
d[, mm_sign_gpa  := as.numeric(scale(gpa_z - test_z))]
## hinge (true asymmetry): split signed composite into paper>code vs code>paper
d[, gap := paper_z - test_z]
d[, gap_paper_over_code := pmax(gap, 0)]   # resume better than code
d[, gap_code_over_paper := pmax(-gap, 0)]  # code better than resume

e <- d[ok & dpos >= 4]
mu <- mean(e$timeonpage); cat(sprintf("Engineer mean time (pos 4-18) = %.1f s | N = %d\n\n", mu, nrow(e)))
P <- function(f, terms, lbl){ m<-feols(as.formula(f), e, vcov=~responseid); ct<-coeftable(m)
  cat("  [",lbl,"]\n",sep="")
  for(t in terms) if(t %in% rownames(ct)) cat(sprintf("     %-22s %+6.2f s/SD (p=%.3f)  = %+.0f%% of mean\n", t, ct[t,1], ct[t,4], 100*ct[t,1]/mu)) }

cat("===== COMPOSITE paper-index vs coding =====\n")
P("timeonpage ~ mm_sym_comp + dpos | responseid",  "mm_sym_comp",  "Symmetric |paper - code|  (reference)")
P("timeonpage ~ mm_sign_comp + dpos | responseid", "mm_sign_comp", "Signed (paper - code)")
P("timeonpage ~ gap_paper_over_code + gap_code_over_paper + dpos | responseid",
  c("gap_paper_over_code","gap_code_over_paper"), "Hinge: paper>code vs code>paper (s per SD-unit of gap)")

cat("\n===== PURE GPA vs Test Cases =====\n")
P("timeonpage ~ mm_sym_gpa + dpos | responseid",  "mm_sym_gpa",  "Symmetric |GPA - test|  (reference)")
P("timeonpage ~ mm_sign_gpa + dpos | responseid", "mm_sign_gpa", "Signed (GPA - test)")
