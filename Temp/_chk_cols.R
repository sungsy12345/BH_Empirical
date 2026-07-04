suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr)})
options(warn=-1, readr.show_types=FALSE); date<-"18mar2026"
data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
need<-c("gpa_major","count_cs_work_experience","ind_cs_research","num_proj","num_awards","tier_top","tier_ordinary","test_case_z","overall_score","readability","time_efficiency","space_efficiency","runtime","resume_index")
cat("in firm_long_dt?\n"); for(v in need) cat(sprintf("  %-28s %s\n",v, v %in% names(firm_long_dt)))
cat("\nresume_index type in firm_long_dt:", class(firm_long_dt$resume_index),"| distinct:", uniqueN(firm_long_dt$resume_index),"\n")
cat("gpa_major present? if so range on 18:\n"); if("gpa_major"%in%names(firm_long_dt)) print(range(firm_long_dt$gpa_major,na.rm=TRUE))
