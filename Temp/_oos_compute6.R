## OUT-OF-SAMPLE REWEIGHTING -- refined: all 6 race x gender groups; lme4 bias heterogeneity.
## Sweep = corr between the group-bias a candidate attracts and their unobserved productivity u.
suppressMessages({library(pacman); p_load(haven, here, readr, data.table, stringr, fixest, ggplot2, lme4)})
options(warn = -1, readr.show_types = FALSE)
date <- "18mar2026"; data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
setwd(here::here()); set.seed(20260702)
invisible(capture.output({for (f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning"))
  source(here::here("1_Codes", paste0(f, ".R")))}))
firm_long_dt <- firm_long_dt[responseid != "R_6F6ThuZMI0jLRJv"]

## ---- Phase 1: enriched s(X), u ----
sr <- fread(file.path(data_root,"3output_data/Main/student_data/student_matching/student_resume.csv"))
sr[, resume_index := suppressWarnings(as.integer(resume_index))]; sr[, Y := overall_score]
sr[, `:=`(tc_z=scale(test_case)[,1], gpa_z=scale(gpa)[,1], nexp_z=scale(num_exp)[,1], naw_z=scale(num_awards)[,1])]
bl <- firm_long_dt[treat=="blind" & role=="Eng", .(bscore=mean(sc_overall_z,na.rm=TRUE)), by=resume_index]
bl[, resume_index := suppressWarnings(as.integer(as.character(resume_index)))]
fit_dt <- merge(sr[!is.na(resume_index)], bl, by="resume_index")
shat_fit <- lm(bscore ~ tc_z+gpa_z+nexp_z+naw_z, fit_dt)
loo <- sapply(1:nrow(fit_dt), function(i) predict(lm(bscore~tc_z+gpa_z+nexp_z+naw_z, fit_dt[-i]), fit_dt[i]))
loo_r2 <- 1 - sum((fit_dt$bscore-loo)^2)/sum((fit_dt$bscore-mean(fit_dt$bscore))^2)
sr[, shat := predict(shat_fit, sr)]; sr[, u := resid(lm(Y~shat, sr))]
SDu <- sd(sr$u); SDY <- sd(sr$Y)

## ---- 6-group bias beta_g (engineers, nonblind), demeaned by realistic marginals ----
grps <- setdiff(unique(firm_long_dt[treat=="nonblind"]$race_gender), NA)
fe_g <- feols(sc_overall_z ~ i(race_gender, ref="White_Male") | resume_index,
              firm_long_dt[treat=="nonblind" & role=="Eng"], vcov=~responseid)
cf <- coef(fe_g); betas <- setNames(rep(0, length(grps)), grps)
for (g in grps) { nm <- paste0("race_gender::", g); if (nm %in% names(cf)) betas[g] <- cf[nm] }
## realistic marginals from the 69 (real demographics)
sr[, rg := gsub(" ", "_", demo_group)]
pg <- prop.table(table(factor(sr$rg, levels=grps)))
betas <- betas - sum(pg*betas[names(pg)])        # demean
cat("================ 6-group nonblind score bias (Eng), demeaned ================\n")
print(round(sort(betas),3)); cat("marginal shares (69):\n"); print(round(pg,2))

## ---- bias heterogeneity via lme4: random slope on the group-bias index ----
d <- firm_long_dt[treat=="nonblind" & role=="Eng"]
d[, bhat := betas[as.character(race_gender)]]
mm <- tryCatch(lmer(sc_overall_z ~ bhat + factor(resume_index) + (1 + bhat | responseid), data=d), error=function(e) NULL)
if (!is.null(mm)) { b1 <- fixef(mm)["bhat"]; tau <- attr(VarCorr(mm)$responseid,"stddev")["bhat"] } else { b1 <- 1; tau <- 0 }
sig_eps <- sd(resid(feols(sc_overall_z ~ 1 | resume_index + responseid, firm_long_dt[treat=="blind" & role=="Eng"])))
cat(sprintf("\ns(X) LOO R2=%.2f | SD(u)=%.2f SD(Y)=%.2f | bias-index fixed slope=%.2f, evaluator SD(tau)=%.2f | sigma_eps=%.2f\n",
    loo_r2, SDu, SDY, b1, tau, sig_eps))

## ---- Phases 2-4: sweep alignment corr(bhat_i, u_i) ----
Y<-sr$Y; shat<-sr$shat; uu<-as.numeric(scale(sr$u)); n<-nrow(sr)
ng <- pmax(1, round(as.numeric(pg)*n)); gord <- names(sort(betas))     # groups low->high beta
sim <- function(a, K, M=300) {
  te<-numeric(M); rc<-numeric(M)
  for (m in 1:M) {
    key <- a*uu + sqrt(max(0,1-a^2))*rnorm(n)          # high key -> high-beta group
    cand_order <- order(key)                            # low key first
    grp <- character(n); idx<-1
    for (gi in seq_along(gord)) { cnt <- if (gi==length(gord)) (n-idx+1) else ng[match(gord[gi], names(pg))]
      take <- cand_order[idx:(idx+cnt-1)]; grp[take] <- gord[gi]; idx<-idx+cnt }
    bhat_i <- betas[grp]; rc[m] <- cor(bhat_i, sr$u)
    slope_m <- rnorm(1, b1, tau); eps <- rnorm(n,0,sig_eps)
    sB <- shat+eps; sN <- shat + slope_m*bhat_i + eps
    te[m] <- mean(Y[order(-sN)[1:K]]) - mean(Y[order(-sB)[1:K]])
  }
  c(te=mean(te), rc=mean(rc))
}
grid <- seq(-0.6, 0.6, 0.2)
res <- rbindlist(lapply(c(5,10), function(K) rbindlist(lapply(grid, function(a){
  o<-sim(a,K); data.table(K=K, align=a, realcorr=o["rc"], TE_sd=o["te"]/SDY)}))))
## the 69's REAL group assignment as a reference point
realpt <- rbindlist(lapply(c(5,10), function(K){
  bhat_i<-betas[sr$rg]; te<-mean(sapply(1:300,function(m){eps<-rnorm(n,0,sig_eps)
    mean(Y[order(-(shat+b1*bhat_i+eps))[1:K]])-mean(Y[order(-(shat+eps))[1:K]])}))
  data.table(K=K, realcorr=cor(bhat_i,sr$u), TE_sd=te/SDY)}))

cat("\n================ TE(alignment) : blinding effect on hire productivity (SD of Y) ================\n")
cat("align=0 => null-recovery anchor. Positive realcorr = favored groups are HIGHER-u.\n")
print(res[, .(K, realized_corr=round(realcorr,2), TE_in_SD=round(TE_sd,3))])
cat("\n69's REAL demographic joint (reference point):\n"); print(realpt[, .(K, realized_corr=round(realcorr,2), TE_in_SD=round(TE_sd,3))])

gg <- ggplot(res, aes(realcorr, TE_sd, color=factor(K))) + geom_hline(yintercept=0, linetype=2, color="grey60") +
  geom_line() + geom_point() + geom_point(data=realpt, aes(realcorr, TE_sd), shape=8, size=3, show.legend=FALSE) +
  labs(x="corr(group-bias attracted, unobserved productivity u)", y="Blinding effect on hire productivity (SD of Y)",
       color="Top-K", title="Out-of-sample productivity effect of blinding (6-group, fixed behavior)",
       subtitle="Star = the 69's real demographic joint; align=0 recovers the null") + theme_minimal(base_size=11)
ggsave("Temp/_oos_fig6.png", gg, width=8, height=4.6, dpi=150)
cat("\nsaved Temp/_oos_fig6.png\nDONE.\n")
