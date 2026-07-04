suppressMessages({library(pacman); p_load(data.table,fixest)})
data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
v<-fread(file.path(data_root,"3output_data/Main/student_data/main_all69_student_sys.csv"))
v[,gpa:=ifelse(gpa>=0&gpa<=4,gpa,NA_real_)]
z<-function(x) as.numeric(scale(x))
v[,`:=`(gpa_z=z(gpa), num_awards_z=z(count_award), count_cs_work_experience_z=z(count_cs_work_experience),
        num_proj_z=z(count_projects), tier_top=as.integer(max_firm_signal_tier=="Top"),
        ind_cs_research=as.integer(ind_cs_research), ind_cs_publication=as.integer(ind_cs_publication),
        ind_cs_teaching=as.integer(ind_cs_teaching), test_case_z=z(test_case),
        true_coding_z=z(overall_score), true_noncode_z=z(readability+time_efficiency+space_efficiency+runtime))]
sig<-c("gpa_z","num_awards_z","count_cs_work_experience_z","num_proj_z","tier_top","ind_cs_research","ind_cs_publication","ind_cs_teaching","test_case_z")
lab<-c("GPA","# Awards","CS Work Exp","# Projects","Top Firm","CS Research","CS Publication","CS Teaching","Test Cases")
## univariate (marginal)
uni<-sapply(sig,function(s){o<-if(s=="test_case_z")"true_noncode_z" else "true_coding_z";coeftable(feols(as.formula(paste0(o,"~",s)),v,vcov="hetero"))[s,c(1,4)]})
## multivariate on overall productivity (test_case tautological here)
m<-feols(as.formula(paste0("true_coding_z ~ ",paste(sig,collapse="+"))),v,vcov="hetero")
## multivariate on leave-out (non-code) productivity -> test_case clean
m2<-feols(as.formula(paste0("true_noncode_z ~ ",paste(sig,collapse="+"))),v,vcov="hetero")
ct<-coeftable(m); ct2<-coeftable(m2)
out<-data.table(Signal=lab,
  Univar_est=round(uni[1,],2), Univar_p=round(uni[2,],2),
  Multivar_est=round(ct[sig,1],2), Multivar_p=round(ct[sig,4],2),
  Multivar_noncode_est=round(ct2[sig,1],2), Multivar_noncode_p=round(ct2[sig,4],2))
cat(sprintf("N = %d | Multivar R2 (overall) = %.2f | Multivar R2 (non-code) = %.2f\n\n", nobs(m), r2(m,"r2"), r2(m2,"r2")))
print(out)
## VIF (collinearity)
cat("\n=== VIF (multicollinearity among the 9 signals) ===\n")
X<-as.matrix(v[complete.cases(v[,..sig]),..sig]); vif<-diag(solve(cor(X)))
print(round(sort(vif,decreasing=TRUE),1))
