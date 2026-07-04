suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr)})
options(warn=-1, readr.show_types=FALSE); date<-"18mar2026"
data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
d<-firm_long_dt[!is.na(sc_overall_z)]
mt<-d[,.(max_tie=max(.SD[,.N,by=.(round(sc_overall_z,6),round(sc_coding_z,6))]$N),
         treat=treat[1]), by=.(role,responseid)]
cat("Rule B failures (max_tie>=9):\n"); print(mt[max_tie>=9][order(-max_tie)])
cat("\nBy role, counts at >=9:\n"); print(mt[max_tie>=9, .N, by=role])
cat("\n0jLRJv max_tie (Rule A flag, shown for reference):\n")
print(mt[responseid=="R_6F6ThuZMI0jLRJv"])
