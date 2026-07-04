suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,fixest)})
options(warn=-1, readr.show_types=FALSE); date<-"18mar2026"
data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))
  source(here::here("1_Codes","6_Hiring_Simulation.R"))}))
d<-firm_long_dt[role=="Eng" & eng_top3==1]
cat("n=",nrow(d)," treat levels:",paste(levels(factor(d$treat)),collapse="/"),"\n")
cat("exists q_version?",("q_version"%in%names(d)),"| eng_top3?",("eng_top3"%in%names(d)),"\n")
cm<-d[treat=="nonblind",mean(overall_score,na.rm=T)]; cs<-d[treat=="nonblind",sd(overall_score,na.rm=T)]
d[,y:=(overall_score-cm)/cs]
m<-feols(y~i(treat,ref="nonblind")+resp_age | q_version+resp_gender+resp_race+s2_educ+s2_income+resp_python+resp_java, data=d, vcov=~responseid)
print(coeftable(m))
