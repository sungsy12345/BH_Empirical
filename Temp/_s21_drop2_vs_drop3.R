## Decide drop-first-2 (pos>=3) vs drop-first-3 (pos>=4) for the Determinants
## of Time on Page table. Same spec as the table; report the Display Position
## (fatigue) slope + quality coefs, with economic magnitude.
suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,tidyverse,lubridate,fixest)})
options(warn=-1, readr.show_types=FALSE)
date <- "18mar2026"; data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
setwd(here::here())
invisible(capture.output({ for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R"))) }))
d <- copy(firm_long_dt)
d[, ok := !is.na(timeonpage) & timeonpage <= 600]
d[, gpa_z := as.numeric(scale(gpa))]; d[, awards_z := as.numeric(scale(num_awards))]
d[, top_exp := as.integer(as.character(max_firm_signal_tier)=="Top")]; d[, test_z := as.numeric(scale(test_case))]
d[, prest := fcase(as.character(max_firm_signal_tier)=="Top",2, as.character(max_firm_signal_tier)%in%c("Ordinary","Growth"),1, default=0)]
d[, paper_z := as.numeric(scale(rowMeans(cbind(scale(gpa),scale(num_awards),scale(prest)))))]
d[, mismatch := abs(paper_z - test_z)]; d[, dpos := as.numeric(str_extract(as.character(r_do),"[0-9]+"))]

## position profile reminder
for(rl in c("HR","Eng")){
  pr <- d[role==rl & ok, .(m=round(mean(timeonpage),0)), by=dpos][order(dpos)]
  cat(rl," mean time by position 1..6:", paste(head(pr$m,6),collapse=" "), " ... plateau ~", round(mean(pr$m[pr$dpos>=6])), "s\n", sep="")
}
cat("\n")
fit <- function(rl, kmin){
  s <- d[role==rl & ok & dpos>=kmin]
  rhs <- if(rl=="Eng") "gpa_z + awards_z + top_exp + test_z + mismatch + dpos" else "gpa_z + awards_z + top_exp + dpos"
  m <- feols(as.formula(paste0("timeonpage ~ ",rhs," | responseid")), s, vcov=~responseid)
  ct <- coeftable(m); b <- ct["dpos",1]; p <- ct["dpos",4]
  steps <- 18-kmin; base <- mean(s$timeonpage)
  cat(sprintf("  %-4s keep pos %d-18: Display Position = %+.2f s/step (p=%.3f) | cumulative %+.1fs over %d steps = %.0f%% of mean(%.1f) | N=%d\n",
      rl, kmin, b, p, b*steps, steps, 100*abs(b*steps)/base, base, nobs(m)))
  # also show whether any quality coef is significant
  q <- setdiff(rownames(ct),"dpos"); sig <- q[ct[q,4]<0.10]
  cat("        quality/other coefs significant at 10%: ", if(length(sig)) paste(sig,collapse=", ") else "NONE", "\n")
}
cat("================= DROP FIRST 2 (positions 3-18) =================\n"); fit("HR",3); fit("Eng",3)
cat("\n================= DROP FIRST 3 (positions 4-18) =================\n"); fit("HR",4); fit("Eng",4)
cat("\n(For reference: ALL 18 -> HR Display Pos ~ -1.12***, Eng ~ -3.67***)\n")
