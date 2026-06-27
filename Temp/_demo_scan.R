suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,tidyverse,lubridate,fixest,splitstackshape,Hmisc,zipcodeR,sf,tigris)})
options(warn=-1, readr.show_types=FALSE)
date <- "18mar2026"; data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
setwd(here::here())
invisible(capture.output({
  for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))
}))

rx <- "race|gender|demo|chinese|indian|asian|white|black|hispan|urm|minorit|ethnic|true_rg|true_race"
cat("===== firm_long_dt demographic-related columns =====\n")
print(grep(rx, names(firm_long_dt), value=TRUE, ignore.case=TRUE))

# classify each by variation level
d <- firm_long_dt
has <- function(v) v %in% names(d)
lvl <- function(v){
  if(!has(v)) return("absent")
  # constant within candidate (resume_index)?
  by_cand <- d[!is.na(get(v)), .(n=uniqueN(get(v))), by=resume_index][, max(n)]
  # constant within candidate x resume_ver?
  by_cv   <- d[!is.na(get(v)), .(n=uniqueN(get(v))), by=.(resume_index, resume_ver)][, max(n)]
  # varies across evaluators (responseid)?
  by_eval <- d[!is.na(get(v)), .(n=uniqueN(get(v))), by=responseid][, max(n)]
  if(by_cand==1) "TRUE-underlying (constant within candidate)"
  else if(by_cv==1) "ASSIGNED (constant within candidate x resume_ver)"
  else if(by_eval==1) "EVALUATOR-own (constant within responseid)"
  else "varies-other"
}
cands <- grep(rx, names(d), value=TRUE, ignore.case=TRUE)
cat("\n===== classification by variation level =====\n")
for(v in cands) cat(sprintf("  %-26s -> %s\n", v, lvl(v)))

cat("\n===== sample value sets =====\n")
for(v in intersect(c("race_r","gender_r","demo_group","chinese_r","indian_r","true_race_gender",
                     "race_gender","demo_group_asian","s2_race","s2_gender"), names(d)))
  cat(sprintf("  %-20s: %s\n", v, paste(head(sort(unique(as.character(d[[v]]))),8), collapse=" | ")))
