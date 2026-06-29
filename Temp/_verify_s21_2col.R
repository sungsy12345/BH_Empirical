suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,tidyverse,lubridate,fixest,modelsummary,kableExtra)})
options(warn=-1, readr.show_types=FALSE)
date <- "18mar2026"; data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
setwd(here::here())
invisible(capture.output({ for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R"))) }))
det <- copy(firm_long_dt)
det[, ok := !is.na(timeonpage) & timeonpage <= 600]
det[, gpa_z := as.numeric(scale(gpa))]; det[, awards_z := as.numeric(scale(num_awards))]
det[, top_exp := as.integer(as.character(max_firm_signal_tier)=="Top")]; det[, test_z := as.numeric(scale(test_case))]
det[, prest := fcase(as.character(max_firm_signal_tier)=="Top",2, as.character(max_firm_signal_tier)%in%c("Ordinary","Growth"),1,default=0)]
det[, paper_z := as.numeric(scale(rowMeans(cbind(scale(gpa),scale(num_awards),scale(prest)))))]
det[, mismatch := abs(paper_z - test_z)]; det[, dpos := as.numeric(str_extract(as.character(r_do),"[0-9]+"))]
det <- det[ok & dpos >= 4]
fit_det <- function(rl){ rhs <- if(rl=="Eng") "gpa_z + awards_z + top_exp + test_z + mismatch + dpos" else "gpa_z + awards_z + top_exp + dpos"
  feols(as.formula(paste0("timeonpage ~ ",rhs," | responseid")), det[role==rl], vcov=~responseid) }
mods <- list("(1)"=fit_det("HR"), "(2)"=fit_det("Eng"))
for(nm in names(mods)){ m<-mods[[nm]]; ct<-coeftable(m)
  cat(nm," dpos=",sprintf("%.2f (p=%.3f)",ct["dpos",1],ct["dpos",4])," R2=",sprintf("%.3f",fitstat(m,"r2",verbose=FALSE)$r2)," N=",nobs(m),"\n",sep="") }
cm <- c("gpa_z"="GPA (Z)","awards_z"="\\# Awards (Z)","top_exp"="Top Experience","test_z"="Test Cases Passed (Z)","mismatch"="Quality Mismatch","dpos"="Display Position")
fe <- data.frame(term=c("Mean Time (Seconds)","Respondent ID FE"),
  "(1)"=c(sprintf("%.1f",mean(det[role=="HR",timeonpage])),"\\checkmark"),
  "(2)"=c(sprintf("%.1f",mean(det[role=="Eng",timeonpage])),"\\checkmark"), check.names=FALSE)
attr(fe,"position")<-c(7,8)
g <- tribble(~raw,~clean,~fmt,"nobs","Observations",0,"r.squared","R$^2$",3)
t <- modelsummary(mods, estimate="{estimate}{stars} ({std.error})", statistic=NULL, coef_map=cm, gof_map=g,
   add_rows=fe, escape=FALSE, stars=c('*'=.1,'**'=.05,'***'=.01), title="Determinants of Time on Page (Seconds)", output="kableExtra") %>%
   kable_styling(latex_options="HOLD_position", font_size=8) %>%
   add_header_above(c(" "=1,"Panel A: Resume-Screening"=1,"Panel B: Technical-Evaluation"=1)) %>%
   row_spec(6, extra_latex_after="\\midrule") %>% row_spec(8, extra_latex_after="\\midrule")
cat("\n[TABLE BUILD OK]  ncols=", length(mods), "\n")
