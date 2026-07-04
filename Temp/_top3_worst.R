suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr)})
options(warn=-1, readr.show_types=FALSE); date<-"18mar2026"
data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
d<-copy(firm_long_dt)
cand<-unique(d[,.(resume_index, overall_score, test_case)])[order(overall_score)]
worst<-cand$resume_index[1]
zero<-cand[test_case==0, resume_index]
cat("Worst candidate (lowest true score): resume", worst, " overall_score=", round(cand$overall_score[1],1), " test_case=", cand$test_case[1], "\n")
cat("Candidates with ZERO test cases passed:", if(length(zero)) paste(zero,collapse=",") else "none", "\n")
cat("Bottom-3 true candidates: resumes", paste(cand$resume_index[1:3],collapse=","), " (scores", paste(round(cand$overall_score[1:3],1),collapse=","), ")\n\n")
for(rl in c("HR","Eng")){
  s<-d[role==rl & !is.na(sc_overall)]
  top3<-s[, .(rk=resume_index[order(-sc_overall)][1:min(3,.N)]), by=responseid]
  nresp<-uniqueN(top3$responseid)
  w<-top3[, any(rk==worst), by=responseid]; nw<-sum(w$V1)
  z<-if(length(zero)) top3[, any(rk %in% zero), by=responseid][, sum(V1)] else 0
  top1<-s[, .(r1=resume_index[order(-sc_overall)][1]), by=responseid]
  w1<-sum(top1$r1==worst); z1<-if(length(zero)) sum(top1$r1 %in% zero) else 0
  cat("==", rl, "(", nresp, "evaluators) ==\n")
  cat("   worst candidate in TOP-3:", nw, "evaluators (", round(100*nw/nresp), "% ) | ranked #1:", w1, "\n")
  cat("   any zero-test candidate in TOP-3:", z, "evaluators (", round(100*z/nresp), "% ) | ranked #1:", z1, "\n")
}
