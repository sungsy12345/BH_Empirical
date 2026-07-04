suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,fixest)})
options(warn=-1, readr.show_types=FALSE); date<-"18mar2026"
data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
for(v in c("gpa_major","count_cs_work_experience","num_proj","num_awards")) firm_long_dt[,(paste0(v,"_z")):=as.numeric(scale(get(v)))]
sigs<-c("gpa_major_z","count_cs_work_experience_z","num_proj_z","ind_cs_research","num_awards_z","tier_top","test_case_z")
## resume-level (18 shown) values
rl<-unique(firm_long_dt[,c("resume_index",sigs),with=FALSE])
cat("N resumes:",nrow(rl),"\n=== resume-level correlation (18 shown) ===\n")
print(round(cor(rl[,..sigs],use="pairwise.complete.obs"),2))
cat("\n=== which get dropped by fixest (eng interaction) ===\n")
firm_long_dt[,resume_id:=paste(resume_ver,resume_index,sep="_")]
m<-feols(as.formula(paste0("sc_overall_z ~ (",paste(sigs,collapse=" + "),") * i(treat,ref='nonblind') | resume_index + responseid + r_do")), firm_long_dt[role=="Eng"],vcov=~responseid+resume_id)
cat("collinear removed:\n"); print(m$collin.var)
