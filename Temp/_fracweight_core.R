## CORE verification: deterministic fractional tie-weights (replicate-and-inverse-weight),
## then the §6.2 productivity TE computed WEIGHTED, full vs. dropping the 4 flagged.
suppressMessages({library(pacman); p_load(haven, here, readr, data.table, stringr, fixest)})
options(warn = -1, readr.show_types = FALSE)
date <- "18mar2026"; data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
setwd(here::here())
invisible(capture.output({
  for (f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning"))
    source(here::here("1_Codes", paste0(f, ".R")))
}))

FLAG <- c("R_6F6ThuZMI0jLRJv",   # Rule A: top-ranked template submissions (Eng)
          "R_7KD5TNkimOZGNCx",   # Rule B: 15-tie (HR)
          "R_1GCYghAkB1YzutY",   # Rule B: 14-tie (HR)
          "R_6m8207oT8ZCXMZN")   # Rule B: 9-tie  (HR)

## ---- fractional inclusion weight w_k(i) = min(1, max(0,(k-A)/t)) on (sc_overall_z, sc_coding_z) ----
add_fracw <- function(d, kmax = 18) {
  d <- copy(d)
  ## grouping by identical (overall, coding) within responseid
  d[, grp := .GRP, by = .(responseid, round(sc_overall_z, 8), round(sc_coding_z, 8))]
  gs <- d[, .(t = .N, so = sc_overall_z[1], sc = sc_coding_z[1]), by = .(responseid, grp)]
  setorder(gs, responseid, -so, -sc)
  gs[, A := cumsum(c(0, head(t, -1))), by = responseid]
  d <- merge(d, gs[, .(responseid, grp, t, A)], by = c("responseid", "grp"))
  for (k in 1:kmax) d[, (paste0("w", k)) := pmin(1, pmax(0, (k - A) / t))]
  d[]
}

## weighted productivity TE at cutoff k (replicates report spec, but weighted)
wprod_te <- function(dt, role_, k) {
  wk <- paste0("w", k)
  d <- dt[role == role_ & get(wk) > 0]
  cm <- d[treat == "nonblind", weighted.mean(overall_score, get(wk))]
  v  <- d[treat == "nonblind", {w <- get(wk); sum(w*(overall_score-cm)^2)/sum(w)}]
  cs <- sqrt(v)
  if (is.na(cs) || cs == 0) return(c(est = NA, se = NA))
  d[, ystd := (overall_score - cm)/cs]
  fe <- if (role_ == "Eng")
    "q_version+resp_gender+resp_race+s2_educ+s2_income+resp_python+resp_java" else
    "q_version+resp_gender+resp_race+s2_educ+s2_income"
  m <- tryCatch(feols(as.formula(paste0("ystd ~ i(treat, ref='nonblind') + resp_age | ", fe)),
        data = d, weights = as.formula(paste0("~", wk)), vcov = ~ responseid),
        error = function(e) NULL)
  if (is.null(m) || !("treat::blind" %in% rownames(coeftable(m)))) return(c(est=NA, se=NA))
  ct <- coeftable(m)["treat::blind", ]; c(est = unname(ct["Estimate"]), se = unname(ct["Std. Error"]))
}

## build weighted firm_long_dt (full and dropped)
W_full <- rbind(add_fracw(firm_long_dt[role=="HR"]), add_fracw(firm_long_dt[role=="Eng"]), fill=TRUE)
W_drop <- W_full[!(responseid %in% FLAG)]

## sanity: weights sum to k per evaluator
chk <- W_full[role=="Eng", .(s1=sum(w1), s3=sum(w3)), by=responseid]
cat(sprintf("SANITY: per-engineer sum(w1) should=1 -> mean %.3f ; sum(w3) should=3 -> mean %.3f\n",
    mean(chk$s1), mean(chk$s3)))

cat("\n================ WEIGHTED productivity TE (deterministic ties): full vs drop-flagged ================\n")
for (role_ in c("Eng","HR")) {
  cat(sprintf("\n--- %s stage ---\n", role_))
  cat(sprintf("%4s | %18s | %18s\n","k","FULL est (se)","DROP flagged est (se)"))
  for (k in 1:5) {
    f <- wprod_te(W_full, role_, k); d <- wprod_te(W_drop, role_, k)
    cat(sprintf("k=%2d | %+7.3f (%.3f)    | %+7.3f (%.3f)\n", k, f["est"], f["se"], d["est"], d["se"]))
  }
}
cat("\nDONE.\n")
