suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,fixest)})
options(warn=-1,readr.show_types=FALSE); date<-"18mar2026"; data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
flags<-c("ind_cs_research","ind_cs_publication","ind_cs_teaching","ind_cs_entrepreneurship","ind_cs_leadership","tier_top")
cat("=== which experience-type flags are in firm_long_dt? + prevalence among 18 shown ===\n")
rl<-unique(firm_long_dt[,c("resume_index",intersect(flags,names(firm_long_dt))),with=FALSE])
for(v in flags) if(v%in%names(firm_long_dt)) cat(sprintf("  %-26s in DT: yes | positives among 18: %d/18\n",v,sum(rl[[v]]==1,na.rm=TRUE))) else cat(sprintf("  %-26s in DT: NO\n",v))
firm_long_dt[,resume_id:=paste(resume_ver,resume_index,sep="_")]
for(v in c("count_cs_work_experience","num_proj","num_awards")) firm_long_dt[,(paste0(v,"_z")):=as.numeric(scale(get(v)))]
sig<-c("gpa_z","count_cs_work_experience_z","num_proj_z","num_awards_z","tier_top","ind_cs_research","ind_cs_publication","ind_cs_teaching","test_case_z")
sig<-intersect(sig,names(firm_long_dt))
cat("\n=== weight-shift survival with expanded set (Eng, expect all):\n")
m<-feols(as.formula(paste0("sc_overall_z ~ (",paste(sig,collapse=" + "),") * i(treat,ref='nonblind') | resume_index + responseid + r_do")),firm_long_dt[role=="Eng"],vcov=~responseid+resume_id)
surv<-sub(":treat::blind","",grep(":treat::blind$",rownames(coeftable(m)),value=T))
cat("  survived:",paste(surv,collapse=","),"\n  DROPPED:",paste(setdiff(sig,surv),collapse=","),"\n")
cat("\n=== validity on 69 ===\n")
v<-fread(file.path(data_root,"3output_data/Main/student_data/main_all69_student_sys.csv"))
v[,gpa:=ifelse(gpa>=0&gpa<=4,gpa,NA_real_)]
mk<-function(x) as.numeric(scale(x))
v[,`:=`(gpa_z=mk(gpa),count_cs_work_experience_z=mk(count_cs_work_experience),num_proj_z=mk(count_projects),num_awards_z=mk(count_award),tier_top=as.integer(max_firm_signal_tier=="Top"),ind_cs_research=as.integer(ind_cs_research),ind_cs_publication=as.integer(ind_cs_publication),ind_cs_teaching=as.integer(ind_cs_teaching),test_case_z=mk(test_case),tc_z=mk(overall_score),nc_z=mk(readability+time_efficiency+space_efficiency+runtime))]
uv<-rbindlist(lapply(sig,function(s){o<-if(s=="test_case_z")"nc_z" else "tc_z";m<-feols(as.formula(paste0(o," ~ ",s)),v,vcov="hetero");ct<-coeftable(m);data.table(signal=s,val=round(ct[s,1],2),p=round(ct[s,4],2),n=nobs(m))}))
print(uv)
