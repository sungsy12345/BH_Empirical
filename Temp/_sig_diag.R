## Diagnostic to prune the signal set: prevalence, collinearity, and relevance (corr with productivity), all 69.
suppressMessages({library(pacman); p_load(data.table)})
data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
a<-fread(file.path(data_root,"3output_data/Main/student_data/main_all69_student_sys.csv"))
a[,tier_top:=as.integer(max_firm_signal_tier=="Top")]
a[,tier_ord:=as.integer(max_firm_signal_tier %in% c("Ordinary","Growth"))]
sig<-c("gpa","gpa_major","count_award","count_projects","count_work_experience","count_cs_work_experience",
       "count_leadership","count_cs_leadership","tier_top","tier_ord","repeated_top_signal_corporate",
       "repeated_high_signal_corporate","ind_cs_research","ind_cs_publication","ind_cs_entrepreneurship",
       "ind_cs_teaching","ind_cs_leadership","research_at_prestigious_lab_arbitrary","test_case")
sig<-intersect(sig,names(a))
cat("=== prevalence / mean (binaries: share of 1s) ===\n")
for(s in sig){v<-a[[s]]; if(all(v %in% c(0,1,NA))) cat(sprintf("  %-38s share1=%.2f\n",s,mean(v,na.rm=TRUE)))
  else cat(sprintf("  %-38s mean=%.2f sd=%.2f\n",s,mean(v,na.rm=TRUE),sd(v,na.rm=TRUE)))}
cat("\n=== relevance: correlation with true productivity (overall_score) ===\n")
rel<-sort(sapply(sig,function(s) cor(a[[s]],a$overall_score,use="complete.obs")),decreasing=TRUE)
print(round(rel,2))
cat("\n=== high pairwise correlations among signals (|r|>=0.5) ===\n")
M<-cor(a[,..sig],use="pairwise.complete.obs"); M[lower.tri(M,diag=TRUE)]<-NA
hi<-which(abs(M)>=0.5,arr.ind=TRUE)
if(nrow(hi)) for(i in 1:nrow(hi)) cat(sprintf("  %-30s ~ %-30s r=%.2f\n",rownames(M)[hi[i,1]],colnames(M)[hi[i,2]],M[hi[i,1],hi[i,2]]))
