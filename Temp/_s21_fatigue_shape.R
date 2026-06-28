suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,tidyverse,lubridate,fixest)})
options(warn=-1, readr.show_types=FALSE)
date <- "18mar2026"; data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
setwd(here::here())
invisible(capture.output({ for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R"))) }))
d <- copy(firm_long_dt)
d[, ok := !is.na(timeonpage) & timeonpage <= 600]
d[, pos := as.numeric(str_extract(as.character(r_do),"[0-9]+"))]
cl <- ~responseid
for(rl in c("HR","Eng")){
  dd <- d[role==rl & ok]
  cat("################# ROLE:",rl," (mean = ",sprintf("%.1f",mean(dd$timeonpage)),"s) #################\n",sep="")
  prof <- dd[, .(m=round(mean(timeonpage),1)), by=pos][order(pos)]
  cat("  pos: ", paste(sprintf("%2d",prof$pos), collapse=" "), "\n")
  cat("  sec: ", paste(sprintf("%4.0f",prof$m), collapse=" "), "\n-- fatigue slope --\n")
  for(k in 1:3){
    sub <- dd[pos>=k]
    m <- feols(timeonpage ~ pos | responseid + resume_index, sub, vcov=cl)
    ct<-coeftable(m); b<-ct["pos",1]; p<-ct["pos",4]; steps<-diff(range(sub$pos)); base<-mean(sub$timeonpage)
    cat(sprintf("  [from pos %d (%d-18)] slope=%+.2f s/step (p=%.3f); cumulative %+.1fs = %.0f%% of submean(%.1f); N=%d\n",
        k, k, b, p, b*steps, 100*abs(b*steps)/base, base, nobs(m)))
  }
  cat("\n")
}
