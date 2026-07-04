suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr)})
options(warn=-1, readr.show_types=FALSE); date<-"18mar2026"
data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
d<-copy(firm_long_dt)
cand<-unique(d[,.(resume_index, overall_score, test_case)])[order(overall_score)]
worst<-cand$resume_index[1]; zero<-cand[test_case==0, resume_index]
for(rl in c("HR","Eng")){
  cat("================  ", rl, "  ================\n")
  for(arm in c("nonblind","blind")){
    s<-d[role==rl & treat==arm & !is.na(sc_overall)]
    top3<-s[, .(rk=resume_index[order(-sc_overall)][1:min(3,.N)]), by=responseid]
    top1<-s[, .(r1=resume_index[order(-sc_overall)][1]), by=responseid]
    nresp<-uniqueN(top3$responseid)
    nz<-top3[, any(rk %in% zero), by=responseid][, sum(V1)]
    nw<-top3[, any(rk==worst), by=responseid][, sum(V1)]
    z1<-sum(top1$r1 %in% zero); w1<-sum(top1$r1==worst)
    cat(sprintf("  %-9s (n=%3d):  zero-test in TOP-3 = %2d (%2.0f%%), #1=%d  |  worst in TOP-3 = %d, #1=%d\n",
        arm, nresp, nz, 100*nz/nresp, z1, nw, w1))
  }
}
