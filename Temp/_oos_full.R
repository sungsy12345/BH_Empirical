## OUT-OF-SAMPLE REWEIGHTING -- full build: (a) firm prestige in s(X), (b) beta_g(signal)
## surface + shrinkage-consistency check, (f) k-curve + alignment envelope; Eng & HR stages
## separately; our-sample + US-representative marginals. Renders a standalone PDF.
Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")
suppressMessages({library(pacman); p_load(haven, here, readr, data.table, stringr, fixest, ggplot2,
  patchwork, lme4, knitr, rmarkdown, kableExtra)})
options(warn=-1, readr.show_types=FALSE); date<-"18mar2026"
data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here()); set.seed(20260702)
FIGDIR <- normalizePath(here::here("Temp"))
invisible(capture.output({for (f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning"))
  source(here::here("1_Codes", paste0(f, ".R")))}))
firm_long_dt <- firm_long_dt[responseid != "R_6F6ThuZMI0jLRJv"]

## ---------- 69-pool: signals + firm prestige + productivity + real demographics ----------
sr <- fread(file.path(data_root,"3output_data/Main/student_data/student_matching/student_resume.csv"))
a69<- fread(file.path(data_root,"3output_data/Main/student_data/main_all69_student_sys.csv"))
sr[, resume_index := suppressWarnings(as.integer(resume_index))]
sr[, os_key := round(overall_score,3)]; a69[, os_key := round(overall_score,3)]
a69u <- unique(a69, by="os_key")
sr <- merge(sr, a69u[, .(os_key, max_firm_signal_tier)], by="os_key", all.x=TRUE)
sr[, `:=`(tier_top=as.integer(max_firm_signal_tier=="Top"),
          tier_ord=as.integer(max_firm_signal_tier %in% c("Ordinary","Growth")))]
sr[is.na(tier_top), tier_top:=0L]; sr[is.na(tier_ord), tier_ord:=0L]   # missing firm -> None
sr[, Y := overall_score]; sr[, rg := gsub(" ","_",demo_group)]
for (v in c("test_case","gpa","num_exp","num_awards")) sr[, paste0(v,"_z") := scale(get(v))[,1]]
firm_merge_ok <- sum(!is.na(sr$max_firm_signal_tier))

## ---------- role-specific content-score map s(X); u = productivity not in the score ----------
grps <- c("White_Male","White_Female","Asian_Male","Asian_Female","Hispanic_Male","Hispanic_Female")
sig <- list(Eng=c("test_case_z","gpa_z","num_exp_z","num_awards_z","tier_top","tier_ord"),
            HR =c("gpa_z","num_exp_z","num_awards_z","tier_top","tier_ord"))       # HR sees resume, not code
prim <- list()
for (role_ in c("Eng","HR")) {
  bl <- firm_long_dt[treat=="blind" & role==role_, .(bscore=mean(sc_overall_z,na.rm=TRUE)), by=resume_index]
  bl[, resume_index := suppressWarnings(as.integer(as.character(resume_index)))]
  fitdt <- merge(sr[!is.na(resume_index)], bl, by="resume_index")
  ff <- as.formula(paste("bscore ~", paste(sig[[role_]], collapse="+")))
  m <- lm(ff, fitdt)
  loo <- sapply(1:nrow(fitdt), function(i) predict(lm(ff, fitdt[-i]), fitdt[i]))
  loo_r2 <- 1 - sum((fitdt$bscore-loo)^2)/sum((fitdt$bscore-mean(fitdt$bscore))^2)
  ## baseline (no firm) LOO for the (a) contrast
  ff0 <- as.formula(paste("bscore ~", paste(setdiff(sig[[role_]],c("tier_top","tier_ord")),collapse="+")))
  loo0 <- sapply(1:nrow(fitdt), function(i) predict(lm(ff0, fitdt[-i]), fitdt[i]))
  loo0_r2 <- 1 - sum((fitdt$bscore-loo0)^2)/sum((fitdt$bscore-mean(fitdt$bscore))^2)
  sr[[paste0("shat_",role_)]] <- predict(m, sr)
  lmu <- lm(as.formula(paste0("Y ~ shat_",role_)), sr)
  sr[[paste0("u_",role_)]] <- sr$Y - predict(lmu, sr)                # full length (69)
  ## constant 6-group bias + evaluator heterogeneity (lme4)
  d <- firm_long_dt[treat=="nonblind" & role==role_]
  feg <- feols(sc_overall_z ~ i(race_gender, ref="White_Male") | resume_index, d, vcov=~responseid)
  cf <- coef(feg); b <- setNames(rep(0,6),grps); for(g in grps){nm<-paste0("race_gender::",g); if(nm%in%names(cf)) b[g]<-cf[nm]}
  pg69 <- prop.table(table(factor(sr$rg,levels=grps))); b <- b - sum(pg69*b)
  d[, bhat := b[as.character(race_gender)]]
  mm <- tryCatch(lmer(sc_overall_z ~ bhat + factor(resume_index) + (1+bhat|responseid), d), error=function(e) NULL)
  tau <- if(!is.null(mm)) attr(VarCorr(mm)$responseid,"stddev")["bhat"] else 0
  b1  <- if(!is.null(mm)) fixef(mm)["bhat"] else 1
  sig_eps <- sd(resid(feols(sc_overall_z ~ 1 | resume_index+responseid, firm_long_dt[treat=="blind"&role==role_])))
  ## beta_g(signal): group premium slope in test_case (the (b) surface / shrinkage check)
  bt <- feols(sc_overall_z ~ i(race_gender,ref="White_Male") + i(race_gender, tc_z_cand, ref="White_Male") | resume_index,
              d[, tc_z_cand := scale(test_case)[,1]], vcov=~responseid)
  prim[[role_]] <- list(loo_r2=loo_r2, loo0_r2=loo0_r2, beta=b, tau=as.numeric(tau), b1=as.numeric(b1),
                        sig_eps=sig_eps, SDu=sd(sr[[paste0("u_",role_)]]), bt=bt)
}
SDY <- sd(sr$Y)

## ---------- simulation: sweep alignment corr(bhat,u); k-curve; our vs US marginals ----------
p_our <- prop.table(table(factor(sr$rg,levels=grps)))
p_us  <- setNames(c(0.30,0.08,0.30,0.12,0.15,0.05), grps)              # illustrative US-SWE-ish
sim <- function(role_, a, K, pg, M=250) {
  b<-prim[[role_]]$beta; b1<-prim[[role_]]$b1; tau<-prim[[role_]]$tau; se<-prim[[role_]]$sig_eps
  shat<-sr[[paste0("shat_",role_)]]; u<-sr[[paste0("u_",role_)]]; uu<-as.numeric(scale(u)); Y<-sr$Y; n<-nrow(sr)
  gord<-names(sort(b)); ng<-pmax(1,round(as.numeric(pg[gord])*n))
  te<-numeric(M); rc<-numeric(M)
  for(m in 1:M){ key<--a*uu+sqrt(max(0,1-a^2))*rnorm(n); ordc<-order(key); grp<-character(n); idx<-1
    for(gi in seq_along(gord)){cnt<-if(gi==length(gord))(n-idx+1) else ng[gi]; grp[ordc[idx:(idx+cnt-1)]]<-gord[gi]; idx<-idx+cnt}
    bh<-b[grp]; rc[m]<-cor(bh,u); slope<-rnorm(1,b1,tau); eps<-rnorm(n,0,se)
    sN<-shat+slope*bh+eps; sB<-shat+eps
    te[m]<-mean(Y[order(-sB)[1:K]])-mean(Y[order(-sN)[1:K]]) } # blind - nonblind = EFFECT OF BLINDING
  c(blindeff=mean(te)/SDY, rc=mean(rc))
}
grid<-seq(-0.6,0.6,0.2)
sweep<-rbindlist(lapply(c("Eng","HR"), function(r) rbindlist(lapply(c(5,10), function(K)
  rbindlist(lapply(grid,function(a){o<-sim(r,a,K,p_our); data.table(stage=r,K=K,realcorr=o["rc"],blindeff=o["blindeff"])}))))))
kcurve<-rbindlist(lapply(c("Eng","HR"), function(r) rbindlist(lapply(1:15,function(K){
  o<-sim(r,-0.4,K,p_our); data.table(stage=r,K=K,blindeff=o["blindeff"])}))))
sens<-rbindlist(lapply(c("Eng","HR"), function(r) rbindlist(lapply(c("Our (Berkeley)","US-representative"),function(lab){
  pg<-if(lab=="Our (Berkeley)") p_our else p_us; o<-sim(r,-0.4,10,pg); data.table(stage=r,pool=lab,blindeff=round(o["blindeff"],3))}))))

## ---------- figures ----------
figB <- ggplot(sweep, aes(realcorr, blindeff, color=factor(K))) + geom_hline(yintercept=0,linetype=2,color="grey60") +
  geom_line()+geom_point()+facet_wrap(~stage)+labs(x="corr(group bias attracted, unobserved productivity u)",
  y="Effect of blinding on hire productivity (SD of Y)", color="Top-K", title="(f) Sensitivity: productivity effect of blinding vs identity-productivity correlation") + theme_minimal(base_size=10)
figK <- ggplot(kcurve, aes(K, blindeff, color=stage)) + geom_hline(yintercept=0,linetype=2,color="grey60")+geom_line()+geom_point()+
  labs(x="Number hired (top-K of 69)", y="Effect of blinding (SD of Y)", title="(f) k-curve at a strong anti-alignment (corr = -0.4)")+theme_minimal(base_size=10)
## (b) surface: engineer group premium across test-case tiers
bt<-prim[["Eng"]]$bt; ct<-coef(bt); tcz<-seq(-1.5,1.5,0.5)
surf<-rbindlist(lapply(grps,function(g){a<-ifelse(g=="White_Male",0,ct[paste0("race_gender::",g)]);
  s<-ifelse(g=="White_Male",0,ifelse(paste0("race_gender::",g,":tc_z_cand")%in%names(ct),ct[paste0("race_gender::",g,":tc_z_cand")],0));
  data.table(group=g, tcz=tcz, premium=a+s*tcz)}))
figA <- ggplot(surf, aes(tcz, premium, color=group)) + geom_hline(yintercept=0,linetype=2,color="grey70")+geom_line()+
  labs(x="Candidate coding signal (test-case z)", y="Non-blind score premium (SD)", title="(b) Group premium across the ability signal (Eng): shrinkage-consistency check")+theme_minimal(base_size=10)
ggsave(file.path(FIGDIR,"_oosf_A.png"),figA,width=8,height=4,dpi=150); ggsave(file.path(FIGDIR,"_oosf_B.png"),figB,width=9,height=4,dpi=150); ggsave(file.path(FIGDIR,"_oosf_K.png"),figK,width=7,height=3.8,dpi=150)

## ---------- tables ----------
T1 <- data.table(Stage=c("Engineer","HR"),
  `s(X) LOO R2 (with firm)`=sapply(c("Eng","HR"),function(r)round(prim[[r]]$loo_r2,2)),
  `LOO R2 (no firm)`=sapply(c("Eng","HR"),function(r)round(prim[[r]]$loo0_r2,2)),
  `SD(u)`=sapply(c("Eng","HR"),function(r)round(prim[[r]]$SDu,2)),
  `invisible % var`=sapply(c("Eng","HR"),function(r)round(100*prim[[r]]$SDu^2/SDY^2)),
  `tau (bias heterog.)`=sapply(c("Eng","HR"),function(r)round(prim[[r]]$tau,2)))
T2 <- data.table(Group=gsub("_"," ",grps), `Eng bias`=round(prim[["Eng"]]$beta,3), `HR bias`=round(prim[["HR"]]$beta,3),
                 `pool share`=round(as.numeric(p_our[grps]),2))
cat("=========== KEY NUMBERS ===========\n"); cat("firm-tier merged for", firm_merge_ok, "of 69\n")
print(T1); print(T2); cat("\nsensitivity (blinding effect, SD Y, corr=-0.4, K=10):\n"); print(sens)
cat("\nsweep (our sample):\n"); print(sweep[, .(stage,K,realcorr=round(realcorr,2),blindeff=round(blindeff,3))])

## ---------- render PDF ----------
today <- format(Sys.time(),"%Y%m%d_%H%M")
out <- sprintf("OOS_Reweighting_ProductivityTE_%s.pdf", today)
rmarkdown::render(here("Temp","_oos_report.Rmd"), output_file=out, output_dir=here("2_Reports"),
  knit_root_dir=here(), intermediates_dir=tempdir(), quiet=TRUE, envir=globalenv())
cat("RENDERED", out, "\n")
