suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr)})
options(warn=-1, readr.show_types=FALSE); date<-"18mar2026"
data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
nm <- names(firm_long_dt)
cat("cols matching dei/pro_/belief/discrim/merit/politic/ideol:\n")
print(grep("dei|pro_|belief|discrim|merit|politic|ideol|attitude|fair|diverse|equit", nm, value=TRUE, ignore.case=TRUE))
