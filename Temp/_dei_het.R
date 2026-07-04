## Heterogeneity of the blinding->productivity effect by evaluator DEI index.
## Deterministic fractional tie weights; base sample = full minus 0jLRJv. Console only.
suppressMessages({library(pacman); p_load(haven, here, readr, data.table, stringr, fixest)})
options(warn = -1, readr.show_types = FALSE)
date <- "18mar2026"; data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
setwd(here::here())
invisible(capture.output({
  for (f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning"))
    source(here::here("1_Codes", paste0(f, ".R")))
}))
firm_long_dt <- firm_long_dt[responseid != "R_6F6ThuZMI0jLRJv"]

## sanity: dei_index_z evaluator-level & coverage
chk <- firm_long_dt[, .(ndistinct = uniqueN(round(dei_index_z,6)), miss = mean(is.na(dei_index_z))), by = .(role, responseid)]
cat(sprintf("dei_index_z: within-evaluator distinct values max = %d (should be 1) | evaluators missing DEI: Eng %d, HR %d\n",
    max(chk$ndistinct, na.rm=TRUE),
    firm_long_dt[role=="Eng" & is.na(dei_index_z), uniqueN(responseid)],
    firm_long_dt[role=="HR"  & is.na(dei_index_z), uniqueN(responseid)]))

add_fracw <- function(d) {
  d <- copy(d); d[, grp := .GRP, by = .(responseid, round(sc_overall_z,8), round(sc_coding_z,8))]
  gs <- d[, .(t=.N, so=sc_overall_z[1], sc=sc_coding_z[1]), by=.(responseid, grp)]
  setorder(gs, responseid, -so, -sc); gs[, A := cumsum(c(0, head(t,-1))), by=responseid]
  d <- merge(d, gs[, .(responseid, grp, t, A)], by=c("responseid","grp"))
  for (k in 1:18) d[, (paste0("w",k)) := pmin(1, pmax(0,(k-A)/t))]; d[]
}
W <- rbind(add_fracw(firm_long_dt[role=="HR"]), add_fracw(firm_long_dt[role=="Eng"]), fill=TRUE)
W[, blind := as.integer(treat == "blind")]

het <- function(role_, k, moderator = "dei_index_z") {
  wk <- paste0("w", k); d <- W[role==role_ & get(wk) > 0 & !is.na(get(moderator))]
  cm <- d[treat=="nonblind", weighted.mean(overall_score, get(wk))]
  cs <- d[treat=="nonblind", {w<-get(wk); sqrt(sum(w*(overall_score-cm)^2)/sum(w))}]
  if (is.na(cs)||cs==0) return(NULL)
  d[, ystd := (overall_score-cm)/cs]
  fe <- if (role_=="Eng") "q_version+resp_gender+resp_race+s2_educ+s2_income+resp_python+resp_java" else
                          "q_version+resp_gender+resp_race+s2_educ+s2_income"
  f <- as.formula(sprintf("ystd ~ blind*%s + resp_age | %s", moderator, fe))
  m <- tryCatch(feols(f, d, weights=as.formula(paste0("~",wk)), vcov=~responseid), error=function(e) NULL)
  if (is.null(m)) return(NULL)
  ct <- coeftable(m); get_r <- function(nm) if (nm %in% rownames(ct)) ct[nm,c(1,2,4)] else c(NA,NA,NA)
  ix <- paste0("blind:", moderator)
  list(blind=get_r("blind"), mod=get_r(moderator), inter=get_r(ix), n=nrow(d), nresp=uniqueN(d$responseid))
}

cat("\n============ Blinding x DEI on hired-cohort TRUE productivity (base: full - 0jLRJv, fractional) ============\n")
cat("coef of interest = blind:dei_index_z  (>0 => blinding raises hire productivity MORE for pro-DEI evaluators)\n")
for (role_ in c("Eng","HR")) {
  cat(sprintf("\n--- %s stage ---\n", role_))
  cat(sprintf("%3s | %-22s | %-22s | %-24s | resp\n","k","blind (se, p)","dei_index_z (se, p)","blind:dei (se, p)"))
  for (k in c(1,3,5)) {
    r <- het(role_, k); if (is.null(r)) { cat(sprintf("k=%d  (not estimable)\n",k)); next }
    f <- function(v) sprintf("%+.3f (%.3f, p=%.2f)", v[1], v[2], v[3])
    cat(sprintf("k=%2d | %-22s | %-22s | %-24s | %d\n", k, f(r$blind), f(r$mod), f(r$inter), r$nresp))
  }
}

## tercile contrast for interpretability: does blinding help the top-DEI third specifically?
cat("\n============ Tercile view: blind effect within LOW vs HIGH DEI third (Eng & HR, k=3) ============\n")
W[, dei_ter := cut(dei_index_z, quantile(unique(firm_long_dt[, .(responseid, dei_index_z)])$dei_index_z, c(0,1/3,2/3,1), na.rm=TRUE),
                   labels=c("Low","Mid","High"), include.lowest=TRUE)]
for (role_ in c("Eng","HR")) for (tt in c("Low","High")) {
  k<-3; wk<-paste0("w",k); d <- W[role==role_ & get(wk)>0 & dei_ter==tt]
  cm <- d[treat=="nonblind", weighted.mean(overall_score, get(wk))]; cs <- d[treat=="nonblind",{w<-get(wk);sqrt(sum(w*(overall_score-cm)^2)/sum(w))}]
  d[, ystd:=(overall_score-cm)/cs]
  fe <- if(role_=="Eng") "q_version+resp_gender+resp_race+s2_educ+s2_income+resp_python+resp_java" else "q_version+resp_gender+resp_race+s2_educ+s2_income"
  m <- tryCatch(feols(as.formula(paste0("ystd ~ i(treat,ref='nonblind') + resp_age | ",fe)), d, weights=as.formula(paste0("~",wk)), vcov=~responseid), error=function(e) NULL)
  est <- if(!is.null(m) && "treat::blind"%in%rownames(coeftable(m))) coeftable(m)["treat::blind",c(1,2,4)] else c(NA,NA,NA)
  cat(sprintf("  %-3s %-4s DEI: blind TE = %+.3f (se %.3f, p=%.2f)  [resp=%d]\n", role_, tt, est[1], est[2], est[3], uniqueN(d$responseid)))
}
cat("\nDONE.\n")
