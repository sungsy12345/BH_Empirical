suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr)})
options(warn=-1, readr.show_types=FALSE); date<-"18mar2026"
data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
FLAG <- c("R_6F6ThuZMI0jLRJv","R_7KD5TNkimOZGNCx","R_1GCYghAkB1YzutY","R_6m8207oT8ZCXMZN")
res <- firm_long_dt[responseid %in% FLAG,
  .(role=role[1], arm=as.character(treat[1]), n=.N,
    corr_testcase_coding = round(cor(test_case, sc_coding, use="complete.obs"),2),
    mean_coding=round(mean(sc_coding,na.rm=T),1), sd_coding=round(sd(sc_coding,na.rm=T),2)),
  by=responseid]
setorder(res, role, arm)
print(res)
cat("\nCounts by role x arm:\n"); print(res[, .N, by=.(role, arm)])
## population reference: median within-evaluator corr(test_case, sc_coding) by role
ref <- firm_long_dt[, .(r=cor(test_case, sc_coding, use="complete.obs")), by=.(role,responseid)][
       , .(median_r=round(median(r,na.rm=T),2), mean_r=round(mean(r,na.rm=T),2)), by=role]
cat("\nPopulation within-evaluator corr(test_case, coding) by role:\n"); print(ref)
