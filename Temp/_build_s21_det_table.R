## Dev/test for the S2.1 "Determinants of Time on Page" table before inserting
## the chunk into 8B. Verifies the 4 regressions + the modelsummary build.
suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,tidyverse,lubridate,fixest,modelsummary,kableExtra)})
options(warn=-1, readr.show_types=FALSE)
date <- "18mar2026"; data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
setwd(here::here())
invisible(capture.output({ for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R"))) }))

det <- copy(firm_long_dt)
det[, ok := !is.na(timeonpage) & timeonpage <= 600]
det[, gpa_z   := as.numeric(scale(gpa))]
det[, awards_z:= as.numeric(scale(num_awards))]
det[, top_exp := as.integer(as.character(max_firm_signal_tier)=="Top")]
det[, test_z  := as.numeric(scale(test_case))]
det[, prest   := fcase(as.character(max_firm_signal_tier)=="Top",2, as.character(max_firm_signal_tier)%in%c("Ordinary","Growth"),1, default=0)]
det[, paper_z := as.numeric(scale(rowMeans(cbind(scale(gpa),scale(num_awards),scale(prest)))))]
det[, mismatch:= abs(paper_z - test_z)]
det[, dpos    := as.numeric(str_extract(as.character(r_do),"[0-9]+"))]

fit_det <- function(rl, drop12){
  s <- det[role==rl & ok]; if(drop12) s <- s[dpos>=3]
  rhs <- if(rl=="Eng") "gpa_z + awards_z + top_exp + test_z + mismatch + dpos" else "gpa_z + awards_z + top_exp + dpos"
  feols(as.formula(paste0("timeonpage ~ ",rhs," | responseid")), s, vcov=~responseid)
}
mods <- list("(1)"=fit_det("HR",FALSE), "(2)"=fit_det("HR",TRUE), "(3)"=fit_det("Eng",FALSE), "(4)"=fit_det("Eng",TRUE))
cat("==== coefficients (sanity) ====\n")
for(nm in names(mods)){ ct<-coeftable(mods[[nm]]); cat(nm," dpos=",sprintf("%.2f (p=%.3f)",ct["dpos",1],ct["dpos",4])," N=",nobs(mods[[nm]]),"\n",sep="") }

coef_map <- c("gpa_z"="GPA (Z)","awards_z"="\\# Awards (Z)","top_exp"="Top Experience",
              "test_z"="Test Cases Passed (Z)","mismatch"="Quality Mismatch","dpos"="Display Position")
chk <- "\\checkmark"
mt <- function(rl,d12){ s<-det[role==rl&ok]; if(d12) s<-s[dpos>=3]; sprintf("%.1f",mean(s$timeonpage)) }
fe_rows <- data.frame(term=c("Mean Time (Seconds)","Respondent ID FE"),
  "(1)"=c(mt("HR",F),chk), "(2)"=c(mt("HR",T),chk), "(3)"=c(mt("Eng",F),chk), "(4)"=c(mt("Eng",T),chk),
  check.names=FALSE, stringsAsFactors=FALSE)
attr(fe_rows,"position") <- c(7,8)   # after the 6 coef rows
gof <- tribble(~raw,~clean,~fmt, "nobs","Observations",0, "r.squared","R$^2$",3)

tbl <- modelsummary(mods, estimate="{estimate}{stars} ({std.error})", statistic=NULL,
  coef_map=coef_map, gof_map=gof, add_rows=fe_rows, escape=FALSE, output="kableExtra",
  stars=c('*'=0.1,'**'=0.05,'***'=0.01), title="Determinants of Time on Page (seconds)") |>
  kable_styling(latex_options="HOLD_position", font_size=8) |>
  add_header_above(c(" "=1,"Panel A: Resume-Screening"=2,"Panel B: Technical-Evaluation"=2)) |>
  row_spec(6, extra_latex_after="\\midrule") |>
  row_spec(8, extra_latex_after="\\midrule") |>
  footnote(general="est$^{*}$ (SE) inline; * p<.10, ** p<.05, *** p<.01; SE clustered by respondent. ``Drop 1--2'' excludes each candidate's first two display positions (warm-up). URM-race and URM-gender (nonblind) are also insignificant (not shown).",
           general_title="", threeparttable=TRUE, footnote_as_chunk=FALSE, escape=FALSE)
cat("\n==== LaTeX builds OK? first lines ====\n")
cat(substr(paste(as.character(tbl),collapse="\n"),1,600),"\n")
cat("\n[BUILD OK]\n")
