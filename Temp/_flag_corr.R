suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr)})
options(warn=-1, readr.show_types=FALSE); date<-"18mar2026"
data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
## candidate coding-score variable names (raw, ~0-10) and test-case var
cat("coding/test-ish columns:\n"); print(grep("cod|test", names(firm_long_dt), value=TRUE, ignore.case=TRUE))
FLAG <- c("R_6F6ThuZMI0jLRJv","R_7KD5TNkimOZGNCx","R_1GCYghAkB1YzutY","R_6m8207oT8ZCXMZN")
## show ranges to identify the 0-10 coding score
d <- firm_long_dt[responseid==FLAG[1]]
cat("\nranges for FLAG1:\n")
for(v in grep("^sc_cod|^cod|coding", names(firm_long_dt), value=TRUE)) cat(sprintf("  %s: [%.2f, %.2f]\n", v, min(firm_long_dt[[v]],na.rm=T), max(firm_long_dt[[v]],na.rm=T)))
cat(sprintf("  test_case: [%.2f, %.2f]\n", min(firm_long_dt$test_case,na.rm=T), max(firm_long_dt$test_case,na.rm=T)))
