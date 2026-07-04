suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,fixest)})
options(warn=-1, readr.show_types=FALSE); date<-"18mar2026"
data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
if(!"resume_id"%in%names(firm_long_dt)) firm_long_dt[,resume_id:=paste(resume_ver,resume_index,sep="_")]
for(v in c("gpa_major","count_cs_work_experience","num_proj","num_awards")) if(!(paste0(v,"_z")%in%names(firm_long_dt))) firm_long_dt[,(paste0(v,"_z")):=as.numeric(scale(get(v)))]
hr_signals<-c("gpa_major_z","count_cs_work_experience_z","num_proj_z","ind_cs_research","num_awards_z","tier_top")
eng_only<-c("test_case_z"); eng_signals<-c(hr_signals,eng_only)
fml_hr<-as.formula(paste0("sc_overall_z ~ (",paste(hr_signals,collapse=" + "),") * i(treat, ref='nonblind') | resume_index + responseid + r_do"))
fml_eng<-as.formula(paste0("sc_overall_z ~ (",paste(eng_signals,collapse=" + "),") * i(treat, ref='nonblind') | resume_index + responseid + r_do"))
m_hr<-feols(fml_hr,firm_long_dt[role=="HR"],vcov=~responseid+resume_id); m_eng<-feols(fml_eng,firm_long_dt[role=="Eng"],vcov=~responseid+resume_id)
bh<-grep(":treat::blind$",rownames(coeftable(m_hr)),value=TRUE); be<-grep(":treat::blind$",rownames(coeftable(m_eng)),value=TRUE)
cat("HR signalxblind terms:",length(bh),"(expect 6):",paste(sub(":treat::blind","",bh),collapse=","),"\n")
cat("Eng signalxblind terms:",length(be),"(expect 7):",paste(sub(":treat::blind","",be),collapse=","),"\n\n")
## validity on 69
val69<-fread(file.path(data_root,"3output_data/Main/student_data/main_all69_student_sys.csv"))
val69[,gpa_major:=ifelse(gpa_major<0,NA_real_,gpa_major)]
val69[,`:=`(gpa_major_z=as.numeric(scale(gpa_major)),count_cs_work_experience_z=as.numeric(scale(count_cs_work_experience)),num_proj_z=as.numeric(scale(count_projects)),num_awards_z=as.numeric(scale(count_award)),ind_cs_research=as.integer(ind_cs_research),tier_top=as.integer(max_firm_signal_tier=="Top"),test_case_z=as.numeric(scale(test_case)),true_coding_z=as.numeric(scale(overall_score)),true_noncode_z=as.numeric(scale(readability+time_efficiency+space_efficiency+runtime)))]
uv<-rbindlist(lapply(eng_signals,function(s){outcome<-if(s=="test_case_z")"true_noncode_z" else "true_coding_z"; m<-feols(as.formula(paste0(outcome," ~ ",s)),val69,vcov="hetero"); ct<-coeftable(m); data.table(signal=s,est=round(ct[s,1],3),p=round(ct[s,4],2),n=nobs(m))}))
cat("Validity (all 69):\n"); print(uv)
