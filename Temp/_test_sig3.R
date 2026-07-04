suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,fixest)})
options(warn=-1,readr.show_types=FALSE); date<-"18mar2026"; data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
firm_long_dt[,resume_id:=paste(resume_ver,resume_index,sep="_")]
for(v in c("count_cs_work_experience","num_proj","num_awards")) firm_long_dt[,(paste0(v,"_z")):=as.numeric(scale(get(v)))]
hr<-c("gpa_z","count_cs_work_experience_z","num_proj_z","ind_cs_research","num_awards_z","tier_top"); eng<-c(hr,"test_case_z")
m_hr<-feols(as.formula(paste0("sc_overall_z ~ (",paste(hr,collapse=" + "),") * i(treat,ref='nonblind') | resume_index + responseid + r_do")),firm_long_dt[role=="HR"],vcov=~responseid+resume_id)
m_eng<-feols(as.formula(paste0("sc_overall_z ~ (",paste(eng,collapse=" + "),") * i(treat,ref='nonblind') | resume_index + responseid + r_do")),firm_long_dt[role=="Eng"],vcov=~responseid+resume_id)
cat("HR interaction terms:",length(grep(":treat::blind$",rownames(coeftable(m_hr)),value=T)),"(expect 6)\n")
cat("Eng interaction terms:",length(grep(":treat::blind$",rownames(coeftable(m_eng)),value=T)),"(expect 7)\n")
cat("Eng terms:",paste(sub(":treat::blind","",grep(":treat::blind$",rownames(coeftable(m_eng)),value=T)),collapse=","),"\n")
v<-fread(file.path(data_root,"3output_data/Main/student_data/main_all69_student_sys.csv"))
v[,gpa:=ifelse(gpa>=0&gpa<=4,gpa,NA_real_)]
v[,`:=`(gpa_z=as.numeric(scale(gpa)),count_cs_work_experience_z=as.numeric(scale(count_cs_work_experience)),num_proj_z=as.numeric(scale(count_projects)),num_awards_z=as.numeric(scale(count_award)),ind_cs_research=as.integer(ind_cs_research),tier_top=as.integer(max_firm_signal_tier=="Top"),test_case_z=as.numeric(scale(test_case)),true_coding_z=as.numeric(scale(overall_score)),true_noncode_z=as.numeric(scale(readability+time_efficiency+space_efficiency+runtime)))]
uv<-rbindlist(lapply(eng,function(s){o<-if(s=="test_case_z")"true_noncode_z" else "true_coding_z";m<-feols(as.formula(paste0(o," ~ ",s)),v,vcov="hetero");ct<-coeftable(m);data.table(signal=s,val=round(ct[s,1],2),p=round(ct[s,4],2),n=nobs(m))}))
cat("\nValidity (69):\n"); print(uv)
