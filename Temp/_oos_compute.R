## OUT-OF-SAMPLE REWEIGHTING SIMULATION -- fixed-behavior backbone (sweep rho only).
## Isolates the composition mechanism: how a realistic identity<->productivity correlation
## would translate the measured demographic bias into a productivity treatment effect.
suppressMessages({library(pacman); p_load(haven, here, readr, data.table, stringr, fixest, ggplot2)})
options(warn = -1, readr.show_types = FALSE)
date <- "18mar2026"; data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
setwd(here::here())
invisible(capture.output({
  for (f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning"))
    source(here::here("1_Codes", paste0(f, ".R")))
}))
firm_long_dt <- firm_long_dt[responseid != "R_6F6ThuZMI0jLRJv"]   # branch-2 base
set.seed(20260702)

## ============ Phase 1: identified primitives ============
sr <- fread(file.path(data_root, "3output_data/Main/student_data/student_matching/student_resume.csv"))
sr[, resume_index := suppressWarnings(as.integer(resume_index))]   # blank -> NA (the 51 unshown)
sr[, Y := overall_score]
sr[, urm_real := as.integer(grepl("Hispanic", race, ignore.case = TRUE))]
sr[, `:=`(tc_z = scale(test_case)[,1], gpa_z = scale(gpa)[,1])]

## blind-arm content score per shown resume (assigned-score z units), engineers
bl <- firm_long_dt[treat == "blind" & role == "Eng", .(bscore = mean(sc_overall_z, na.rm=TRUE)), by = resume_index]
bl[, resume_index := suppressWarnings(as.integer(as.character(resume_index)))]
fit_dt <- merge(sr[!is.na(resume_index)], bl, by = "resume_index")          # the 18 shown
shat_fit <- lm(bscore ~ tc_z + gpa_z, data = fit_dt)
## LOO validation on the 18
loo <- sapply(1:nrow(fit_dt), function(i) predict(lm(bscore ~ tc_z + gpa_z, data=fit_dt[-i]), fit_dt[i]))
loo_r2 <- 1 - sum((fit_dt$bscore - loo)^2) / sum((fit_dt$bscore - mean(fit_dt$bscore))^2)
sr[, shat := predict(shat_fit, sr)]                                          # content score for all 69

## u = productivity NOT captured by the score candidates are ranked on
sr[, u := resid(lm(Y ~ shat, data = sr))]
SDu <- sd(sr$u); SDY <- sd(sr$Y); inv_frac <- var(sr$u)/var(sr$Y)

## demographic bias of the URM (Hispanic) label, engineers, non-blind (assigned-score z units)
firm_long_dt[, urm_assigned := as.integer(grepl("Hispanic", race_gender, ignore.case = TRUE))]
m_beta <- feols(sc_overall_z ~ urm_assigned | resume_index, data = firm_long_dt[treat=="nonblind" & role=="Eng"], vcov=~responseid)
beta_bar <- coef(m_beta)["urm_assigned"]; beta_se <- se(m_beta)["urm_assigned"]
## evaluator heterogeneity in the bias (random slope SD); fallback 0 if lme4 missing
tau <- tryCatch({ suppressMessages(library(lme4))
  mm <- lmer(sc_overall_z ~ urm_assigned + (urm_assigned | responseid), data=firm_long_dt[treat=="nonblind" & role=="Eng"])
  as.data.frame(VarCorr(mm))[as.data.frame(VarCorr(mm))$var1=="urm_assigned" & is.na(as.data.frame(VarCorr(mm))$var2),"sdcor"][1] }, error=function(e) NA)
## idiosyncratic evaluation noise (within evaluator & content), blind engineers
sig_eps <- sd(resid(feols(sc_overall_z ~ 1 | resume_index + responseid, firm_long_dt[treat=="blind" & role=="Eng"])))

p_urm <- mean(sr$urm_real)

cat("================ PHASE 1 PRIMITIVES ================\n")
cat(sprintf("s(X) fit on 18: in-sample R2=%.2f | LOO R2=%.2f\n", summary(shat_fit)$r.squared, loo_r2))
cat(sprintf("SD(u)=%.2f  SD(Y)=%.2f  -> invisible share of productivity variance = %.0f%%\n", SDu, SDY, 100*inv_frac))
cat(sprintf("beta_bar (URM/Hispanic nonblind score effect, Eng) = %+.3f SD (se %.3f)\n", beta_bar, beta_se))
cat(sprintf("tau (evaluator SD of URM bias) = %s ; sigma_eps (eval noise) = %.3f ; P(URM)=%.2f\n",
    ifelse(is.na(tau),"NA(lme4 missing->0)",sprintf("%.3f",tau)), sig_eps, p_urm))
if (is.na(tau)) tau <- 0

## ============ Phases 2-3-4: simulate & sweep rho ============
Y <- sr$Y; shat <- sr$shat; uu <- as.numeric(scale(sr$u)); n <- nrow(sr); n_urm <- round(p_urm*n)
simTE <- function(rho, K, M=400) {
  te <- numeric(M); realcorr <- numeric(M)
  for (m in 1:M) {
    key <- -rho*uu + sqrt(max(0,1-rho^2))*rnorm(n)          # tilt URM toward low u
    urm <- as.integer(rank(-key, ties.method="random") <= n_urm)
    realcorr[m] <- cor(urm, sr$u)
    bias_m <- beta_bar + rnorm(1, 0, tau)                    # this evaluator's URM bias
    eps <- rnorm(n, 0, sig_eps)                              # common shock (matched pairs)
    sB <- shat + eps
    sN <- shat + bias_m*urm + eps
    hB <- order(-sB)[1:K]; hN <- order(-sN)[1:K]
    te[m] <- mean(Y[hN]) - mean(Y[hB])
  }
  c(te=mean(te), realcorr=mean(realcorr))
}

grid <- seq(0, 0.6, by=0.1)
res <- rbindlist(lapply(c(5,10), function(K) rbindlist(lapply(grid, function(r){
  o <- simTE(r, K); data.table(K=K, rho=r, realcorr=o["realcorr"], TE_prod=o["te"], TE_sd=o["te"]/SDY)}))))

cat("\n================ PHASE 0/4: TE(rho) ================\n")
cat("TE in true-productivity points and in SD(Y). rho=0 row is the null-recovery anchor.\n")
print(res[, .(K, rho, realized_corr=round(realcorr,2), TE_prodpts=round(TE_prod,2), TE_in_SD=round(TE_sd,3))])

## ceiling heuristic
cat(sprintf("\nFeasibility ceiling ~ beta_bar*SD(u) = %.2f SD-score * %.2f prod-pts = %.2f prod-pts (%.2f SD(Y))\n",
    abs(beta_bar), SDu, abs(beta_bar)*SDu, abs(beta_bar)*SDu/SDY))

## figure
gg <- ggplot(res, aes(realcorr, TE_sd, color=factor(K))) + geom_hline(yintercept=0, linetype=2, color="grey50") +
  geom_line() + geom_point() +
  labs(x="Realized corr(URM identity, unobserved productivity u)",
       y="Treatment effect of blinding on hired-cohort\ntrue productivity (SD of Y)",
       color="Top-K hired", title="Out-of-sample productivity TE vs. assumed identity-productivity correlation",
       subtitle="Fixed-behavior backbone; rho=0 recovers the measured null") + theme_minimal(base_size=11)
ggsave("Temp/_oos_fig.png", gg, width=8, height=4.6, dpi=150)
cat("\nsaved figure Temp/_oos_fig.png\nDONE.\n")
