## ISOLATED SIDE-WORK: template-catch-trial quality control for ENGINEER evaluators.
## HR are NOT touched (they never saw coding material). Does NOT modify or render
## the main report; sources only the read-only cleaning pipeline; prints to console.
suppressMessages({library(pacman)
  p_load(haven, here, readr, data.table, stringr, fixest)})
options(warn = -1, readr.show_types = FALSE)
date <- "18mar2026"; data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
setwd(here::here())
invisible(capture.output({
  for (f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning"))
    source(here::here("1_Codes", paste0(f, ".R")))
}))

## ---- The two template-only (zero-effort) displayed candidates ----
## resume_index 5  = Ayla Aeress  (Q1 stencil untouched)
## resume_index 11 = Kayle Marsh  (Q2 stencil untouched)
TEMPLATE_RESUMES <- c(5, 11)

eng <- firm_long_dt[role == "Eng"]
cat("Engineer rows:", nrow(eng), "| unique engineers:", uniqueN(eng$responseid), "\n")
cat("Arm split (engineers):\n"); print(eng[, .(n_eval = .N, n_resp = uniqueN(responseid)), by = treat])

## sanity: confirm resume 5 / 11 are the lowest-test-case displayed candidates
cat("\nTrue productivity of the two template candidates vs others (engineer view):\n")
print(unique(firm_long_dt[, .(resume_index, overall_score)])[order(overall_score)][1:6])

## ---- Within-engineer ranking of candidates by ASSIGNED overall score ----
## rank 1 = the engineer's top pick (highest assigned score). Ties: min rank.
eng[, rank_assigned := frank(-sc_overall_z, ties.method = "min"), by = responseid]
tmpl_rank <- eng[resume_index %in% TEMPLATE_RESUMES,
                 .(responseid, treat, resume_index, rank_assigned)]
## best (lowest) rank an engineer gave to EITHER template candidate
flag_tbl <- tmpl_rank[, .(best_tmpl_rank = min(rank_assigned),
                          treat = treat[1]), by = responseid]

cat("\n================ CATCH-TRIAL FLAGGING (engineers only) ================\n")
cat("How many engineers placed a TEMPLATE candidate (resume 5 or 11) in their top-K,\n",
    "by arm (denominator = engineers in that arm):\n", sep = "")
narm <- eng[, .(N = uniqueN(responseid)), by = treat]
setkey(narm, treat)
for (K in 1:4) {
  fl <- flag_tbl[best_tmpl_rank <= K]
  tab <- fl[, .N, by = treat]; setkey(tab, treat)
  nb <- narm["nonblind"]$N; bl <- narm["blind"]$N
  nb_f <- ifelse(nrow(tab["nonblind"][!is.na(N)])>0, tab["nonblind"]$N, 0)
  bl_f <- ifelse(nrow(tab["blind"][!is.na(N)])>0, tab["blind"]$N, 0)
  nb_f <- sum(fl$treat=="nonblind"); bl_f <- sum(fl$treat=="blind")
  cat(sprintf("  top-%d : nonblind %d/%d (%.0f%%)   blind %d/%d (%.0f%%)   total flagged %d\n",
    K, nb_f, nb, 100*nb_f/nb, bl_f, bl, 100*bl_f/bl, nrow(fl)))
}

cat("\n---- Specific flagged engineers (best template rank <= 4) ----\n")
det <- merge(tmpl_rank, dcast(tmpl_rank, responseid ~ resume_index, value.var = "rank_assigned"),
             by = "responseid")
det <- unique(det[, .(responseid, treat, rank_r5 = `5`, rank_r11 = `11`)])
print(det[pmin(rank_r5, rank_r11, na.rm = TRUE) <= 4][order(pmin(rank_r5, rank_r11, na.rm=TRUE))])

## ---- BALANCE TEST: is flagging differential by arm? (the fairness check) ----
cat("\n================ BALANCE: flagged ~ blind (engineers) ================\n")
flag_tbl[, blind := as.integer(treat == "blind")]
for (K in c(2,3,4)) {
  flag_tbl[, flagged := as.integer(best_tmpl_rank <= K)]
  ## include engineers never flagged at any K (they are in flag_tbl only if they
  ## evaluated a template candidate -- all engineers saw all 18, so all present)
  m <- lm(flagged ~ blind, data = flag_tbl)
  ft <- fisher.test(table(flag_tbl$blind, flag_tbl$flagged))
  cat(sprintf("  K=%d : P(flag|nonblind)=%.2f  P(flag|blind)=%.2f  diff(blind-nonblind)=%+.2f (lm p=%.3f) Fisher p=%.3f\n",
    K, mean(flag_tbl[blind==0]$flagged), mean(flag_tbl[blind==1]$flagged),
    coef(m)["blind"], summary(m)$coefficients["blind",4], ft$p.value))
}

## ---- HEADLINE ENGINEER TE: full vs. dropped-flagged (top-3) ----
## Replicates the report's engineer spec (chunk blind_hiring_treatment_effects_demo),
## coefficients flipped so positive = effect of blinding.
fit_eng <- function(dt) {
  m <- feols(sc_overall_z ~ i(race_gender, ref = "Blind") + resp_age
    | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income + resp_python + resp_java,
    data = dt, vcov = ~ responseid)
  -1 * coef(m)[grep("race_gender", names(coef(m)))]
}
Kdrop <- 3
drop_ids <- flag_tbl[best_tmpl_rank <= Kdrop, responseid]
cat(sprintf("\n================ ENGINEER DEMOGRAPHIC TE: full vs drop top-%d (%d engineers dropped) ================\n",
    Kdrop, length(drop_ids)))
full <- fit_eng(eng)
drop <- fit_eng(eng[!(responseid %in% drop_ids)])
cmp <- data.table(coef = names(full), full_SD = round(as.numeric(full),3),
                  dropped_SD = round(as.numeric(drop),3))
cmp[, delta := round(dropped_SD - full_SD, 3)]
print(cmp)
cat("\n(positive = blinding raises that group's score vs nonblind baseline; units = SD)\n")

## ---- ENGINEER CONCORDANCE spec: full vs dropped (recreate report's blind_concordance vars) ----
mk_conc <- function(d) {
  d <- copy(d)
  d[, bcr := fcase(treat == "blind", "Blind",
                   treat == "nonblind" & concordance_race_detailed == 1, "NB_Conc",
                   treat == "nonblind" & concordance_race_detailed == 0, "NB_Nonconc")]
  d[, bcr := factor(bcr, levels = c("Blind","NB_Conc","NB_Nonconc"))]
  d[, bcg := fcase(treat == "blind", "Blind",
                   treat == "nonblind" & concordance_gender == 1, "NB_Conc",
                   treat == "nonblind" & concordance_gender == 0, "NB_Nonconc")]
  d[, bcg := factor(bcg, levels = c("Blind","NB_Conc","NB_Nonconc"))]
  d
}
fit_conc <- function(dt, v) {
  m <- feols(as.formula(paste0("sc_overall_z ~ i(", v, ", ref='Blind') + resp_age",
    "| resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income")),
    data = dt, vcov = ~ responseid)
  ## flip so positive = effect of blinding relative to that nonblind cell
  cc <- coef(m); -cc[grep(v, names(cc))]
}
if (all(c("concordance_race_detailed","concordance_gender") %in% names(eng))) {
  ec  <- mk_conc(eng)
  ecd <- mk_conc(eng[!(responseid %in% drop_ids)])
  cat("\n================ ENGINEER CONCORDANCE TE: full vs drop top-3 ================\n")
  for (v in c("bcr","bcg")) {
    cat(sprintf("  [%s]  full: %s\n", ifelse(v=="bcr","RACE","GENDER"),
        paste(sprintf("%s=%+.3f", sub(".*::","",names(fit_conc(ec,v))), fit_conc(ec,v)), collapse="  ")))
    cat(sprintf("        drop: %s\n",
        paste(sprintf("%s=%+.3f", sub(".*::","",names(fit_conc(ecd,v))), fit_conc(ecd,v)), collapse="  ")))
  }
} else cat("\n(concordance vars not present post-cleaning; skipping concordance spec)\n")

cat("\nDONE.\n")
