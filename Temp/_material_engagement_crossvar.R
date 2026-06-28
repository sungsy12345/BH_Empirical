## Explore Table 4 (Material Engagement) interaction variable:
##  - current code crosses blind x test_case_z (labeled "Coding Ability (Z)")
##  - check for a "code compiles" variable / derivable binary
suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,tidyverse,lubridate,fixest,splitstackshape,Hmisc,zipcodeR,sf,tigris)})
options(warn=-1, readr.show_types=FALSE)
date <- "18mar2026"; data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
setwd(here::here())
invisible(capture.output({ for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R"))) }))

cat("==== any compile/run columns in firm_long_dt? ====\n")
print(grep("compile|runs|run_", names(firm_long_dt), value=TRUE, ignore.case=TRUE))
cat("runtime present:", "runtime" %in% names(firm_long_dt), " | test_case present:", "test_case" %in% names(firm_long_dt), "\n")

cat("\n==== runtime & test_case across the 18 candidates (is a compiles binary derivable?) ====\n")
u <- unique(firm_long_dt[, .(resume_index, test_case, runtime, overall_score, code_opt_out)])[order(as.integer(resume_index))]
print(u)
cat("\nruntime summary:\n"); print(summary(u$runtime))
cat("test_case summary:\n"); print(summary(u$test_case))
cat("# candidates with runtime==0:", sum(u$runtime==0, na.rm=TRUE), " | test_case==0:", sum(u$test_case==0, na.rm=TRUE), " | code_opt_out==1:", sum(u$code_opt_out==1, na.rm=TRUE), "\n")

## ---- Run the Material Engagement regressions with blind x test_case_z (current/correct spec) ----
firm_long_dt[, blind := as.integer(treat=="blind")]
firm_long_dt[, test_case_z := as.numeric(scale(test_case))]
firm_long_dt[, open_code1_or_2 := as.integer((!is.na(open_code1)&open_code1==1)|(!is.na(open_code2)&open_code2==1))]
eng <- firm_long_dt[role=="Eng"]
fit_o <- function(dv, xvar){
  d <- copy(eng[!is.na(get(xvar))]); d[, q_int := blind * get(xvar)]
  feols(as.formula(sprintf("%s ~ blind + q_int + resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income + resp_python + resp_java", dv)),
        data=d, vcov=~responseid)
}
show <- function(m, lbl){ ct<-coeftable(m); r<-c("blind","q_int"); cat(sprintf("  %-22s blind=%.4f (p=%.3f) | q_int=%.4f (p=%.3f) | N=%d\n", lbl, ct["blind",1],ct["blind",4],ct["q_int",1],ct["q_int",4], nobs(m))) }
cat("\n==== Material Engagement: blind x test_case_z (the info shown to engineers) ====\n")
show(fit_o("open_resume","test_case_z"),     "open_resume")
show(fit_o("open_code1_or_2","test_case_z"), "open_code1_or_2")
