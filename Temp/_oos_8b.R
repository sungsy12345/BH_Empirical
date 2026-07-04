## OOS reweighting shown in the 8B house style: (1) compositional change of the hired
## cohort by demographic group across K; (2) treatment effect on true productivity across K.
## Real 69-pool demographics. Engineer & HR stages. Renders a standalone PDF.
Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")
suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,fixest,ggplot2,lme4,knitr,rmarkdown,kableExtra)})
options(warn=-1, readr.show_types=FALSE); date<-"18mar2026"
data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here()); set.seed(20260702)
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
firm_long_dt <- firm_long_dt[responseid!="R_6F6ThuZMI0jLRJv"]

## ---- 69-pool + firm prestige + role-specific s(X); primitives per stage ----
sr<-fread(file.path(data_root,"3output_data/Main/student_data/student_matching/student_resume.csv"))
a69<-fread(file.path(data_root,"3output_data/Main/student_data/main_all69_student_sys.csv"))
sr[,resume_index:=suppressWarnings(as.integer(resume_index))]; sr[,os_key:=round(overall_score,3)]; a69[,os_key:=round(overall_score,3)]
sr<-merge(sr, unique(a69,by="os_key")[,.(os_key,max_firm_signal_tier)], by="os_key", all.x=TRUE)
sr[,`:=`(tier_top=as.integer(max_firm_signal_tier=="Top"), tier_ord=as.integer(max_firm_signal_tier%in%c("Ordinary","Growth")))]
sr[is.na(tier_top),tier_top:=0L]; sr[is.na(tier_ord),tier_ord:=0L]
sr[,Y:=overall_score]; sr[,rg:=gsub(" ","_",demo_group)]
for(v in c("test_case","gpa","num_exp","num_awards")) sr[,paste0(v,"_z"):=scale(get(v))[,1]]
grps<-c("White_Male","White_Female","Asian_Male","Asian_Female","Hispanic_Male","Hispanic_Female")
sig<-list(Eng=c("test_case_z","gpa_z","num_exp_z","num_awards_z","tier_top","tier_ord"),
          HR=c("gpa_z","num_exp_z","num_awards_z","tier_top","tier_ord"))
prim<-list()
for(role_ in c("Eng","HR")){
  bl<-firm_long_dt[treat=="blind"&role==role_,.(bscore=mean(sc_overall_z,na.rm=TRUE)),by=resume_index]
  bl[,resume_index:=suppressWarnings(as.integer(as.character(resume_index)))]
  fitdt<-merge(sr[!is.na(resume_index)],bl,by="resume_index")
  m<-lm(as.formula(paste("bscore ~",paste(sig[[role_]],collapse="+"))),fitdt)
  sr[[paste0("shat_",role_)]]<-predict(m,sr)
  d<-firm_long_dt[treat=="nonblind"&role==role_]
  feg<-feols(sc_overall_z~i(race_gender,ref="White_Male")|resume_index,d,vcov=~responseid)
  cf<-coef(feg); b<-setNames(rep(0,6),grps); for(g in grps){nm<-paste0("race_gender::",g); if(nm%in%names(cf)) b[g]<-cf[nm]}
  pg<-prop.table(table(factor(sr$rg,levels=grps))); b<-b-sum(pg*b)
  d[,bhat:=b[as.character(race_gender)]]
  mm<-tryCatch(lmer(sc_overall_z~bhat+factor(resume_index)+(1+bhat|responseid),d),error=function(e)NULL)
  b1<-if(!is.null(mm))fixef(mm)["bhat"] else 1; tau<-if(!is.null(mm))attr(VarCorr(mm)$responseid,"stddev")["bhat"] else 0
  se<-sd(resid(feols(sc_overall_z~1|resume_index+responseid,firm_long_dt[treat=="blind"&role==role_])))
  prim[[role_]]<-list(b=b,b1=as.numeric(b1),tau=as.numeric(tau),se=se,n_eval=uniqueN(firm_long_dt[role==role_]$responseid))
}

## ---- simulate blind vs non-blind selection over K; real demographics ----
n<-nrow(sr); Kmax<-69; rg<-sr$rg; Y<-sr$Y; ng<-table(factor(rg,levels=grps))
Kgrid<-c(1:20, seq(25,65,5), 69)   # dense where selective; sparse in the flat tail
sim_stage<-function(role_,M=500){
  shat<-sr[[paste0("shat_",role_)]]; b<-prim[[role_]]$b; b1<-prim[[role_]]$b1; tau<-prim[[role_]]$tau; se<-prim[[role_]]$se
  bh<-b[rg]
  compS<-array(0,c(Kmax,6)); compSS<-array(0,c(Kmax,6))              # sum & sumsq of blinding effect on sel prob
  teS<-numeric(Kmax); teSS<-numeric(Kmax); ynSum<-numeric(Kmax); ynSq<-numeric(Kmax); ynN<-numeric(Kmax)
  for(m in 1:M){
    slope<-rnorm(1,b1,tau); eps<-rnorm(n,0,se)
    ob<-order(-(shat+eps)); on<-order(-(shat+slope*bh+eps))
    gb<-rg[ob]; gn<-rg[on]; yb<-cumsum(Y[ob])/(1:n); yn<-cumsum(Y[on])/(1:n)
    for(gi in seq_along(grps)){g<-grps[gi]
      rb<-cumsum(gb==g)/as.numeric(ng[g]); rn<-cumsum(gn==g)/as.numeric(ng[g])
      eff<-(rb-rn)[1:Kmax]; compS[,gi]<-compS[,gi]+eff; compSS[,gi]<-compSS[,gi]+eff^2}
    dte<-(yb-yn)[1:Kmax]; teS<-teS+dte; teSS<-teSS+dte^2
    for(K in 1:Kmax){ynSum[K]<-ynSum[K]+sum(Y[on[1:K]]); ynSq[K]<-ynSq[K]+sum(Y[on[1:K]]^2); ynN[K]<-ynN[K]+K}
  }
  ne<-prim[[role_]]$n_eval
  comp<-rbindlist(lapply(seq_along(grps),function(gi){mean<-compS[,gi]/M; sdd<-sqrt(pmax(0,compSS[,gi]/M-mean^2))
    se_<-sdd/sqrt(ne); data.table(demo=gsub("_"," ",grps[gi]),k=1:Kmax,estimate=mean,ci_low=mean-1.96*se_,ci_high=mean+1.96*se_,p=2*pnorm(-abs(mean/se_)))}))
  sdK<-sqrt(pmax(1e-9,ynSq/ynN-(ynSum/ynN)^2))            # nonblind productivity SD at each k
  tem<-teS/M; tesd<-sqrt(pmax(0,teSS/M-tem^2)); te_se<-tesd/sqrt(ne)
  prod<-data.table(k=1:Kmax, estimate=tem/sdK, ci_low=(tem-1.96*te_se)/sdK, ci_high=(tem+1.96*te_se)/sdK, p=2*pnorm(-abs(tem/te_se)))
  list(comp=comp[,Stage:=ifelse(role_=="Eng","Engineer Stage","HR Stage")],
       prod=prod[,Stage:=ifelse(role_=="Eng","Engineer Stage","HR Stage")])
}
E<-sim_stage("Eng"); H<-sim_stage("HR")
comp<-rbind(E$comp,H$comp)[k %in% Kgrid]; prod<-rbind(E$prod,H$prod)[k %in% Kgrid]
stars<-function(p) fcase(p<0.01,"***",p<0.05,"**",p<0.10,"*",default="")
comp[,`:=`(Stage=factor(Stage,c("HR Stage","Engineer Stage")),
  demo=factor(demo,c("White Male","White Female","Asian Male","Asian Female","Hispanic Male","Hispanic Female")),
  sig=stars(p), star_y=ifelse(estimate>=0,ci_high+0.01,ci_low-0.01))]
prod[,`:=`(Stage=factor(Stage,c("HR Stage","Engineer Stage")),sig=stars(p))]

## ---- 8B-style figures ----
figComp<-ggplot(comp,aes(k,estimate,color=Stage))+
  geom_hline(yintercept=0,linetype="dashed",color="gray50",linewidth=0.6)+
  geom_pointrange(aes(ymin=ci_low,ymax=ci_high),linewidth=0.6,size=0.4,fatten=3)+
  geom_text(aes(y=star_y,label=sig),size=3,color="black",show.legend=FALSE)+
  facet_grid(demo~Stage)+scale_color_manual(values=c("HR Stage"="#C44E52","Engineer Stage"="#4C72B0"))+
  scale_x_continuous(breaks=c(1,10,20,30,40,50,60,69),name="Number Hired (K, of 69 applicants;  K=7 ~ top 10%, K=14 ~ top 20%)")+
  scale_y_continuous(expand=expansion(mult=c(0.05,0.20)))+
  labs(title="Blinding Effect on Selection Probability by Group (Out-of-Sample 69-Pool)",
    subtitle="Blinding effect = P(selected | blind) - P(selected | non-blind). Positive = more likely selected under blinding.",
    y="Blinding Effect on Selection Probability",color="Evaluator Role",
    caption="Simulation bands (evaluator-clustered). Real 69-student demographics. Significance: * p<0.10, ** p<0.05, *** p<0.01.")+
  theme_bw()+theme(legend.position="bottom",legend.title=element_text(face="bold",size=10),legend.text=element_text(size=9),
    plot.title=element_text(face="bold",size=13),plot.subtitle=element_text(size=10,color="gray30"),
    strip.text=element_text(face="bold",size=10),strip.text.y=element_text(angle=0),panel.grid.minor.x=element_blank(),
    axis.title.x=element_text(margin=margin(t=10)),axis.title.y=element_text(margin=margin(r=10)),
    plot.caption=element_text(size=8,hjust=0),panel.spacing=unit(0.4,"lines"))
figProd<-ggplot(prod,aes(k,estimate,color=Stage))+
  geom_hline(yintercept=0,linetype="dashed",color="gray50",linewidth=0.8)+
  geom_pointrange(aes(ymin=ci_low,ymax=ci_high),linewidth=0.8,size=0.6,fatten=3)+
  geom_text(aes(y=ci_high,label=sig),vjust=-0.5,size=5,color="black",show.legend=FALSE)+
  facet_wrap(~Stage,ncol=2)+scale_color_manual(values=c("HR Stage"="#C44E52","Engineer Stage"="#4C72B0"))+
  scale_x_continuous(breaks=c(1,10,20,30,40,50,60,69),name="Number Hired (K, of 69 applicants;  K=7 ~ top 10%, K=14 ~ top 20%)")+coord_cartesian(ylim=c(-0.3,0.3))+
  labs(title="Treatment Effect on Productivity of Hired (Out-of-Sample, 69-Pool)",
    subtitle="Effect of blinding on true productivity of selected candidates, standardized relative to non-blind selected at each k",
    y="Effect on Actual Coding Score (SD, relative to nonblind)",
    caption="Positive = blind hiring selects higher-productivity candidates. Real 69-student demographics; fixed-behavior composition channel.\nSignificance: * p<0.10, ** p<0.05, *** p<0.01")+
  theme_bw()+theme(legend.position="none",plot.title=element_text(face="bold",size=13),plot.subtitle=element_text(size=10,color="gray30"),
    strip.text=element_text(face="bold",size=12),panel.grid.minor.x=element_blank(),
    axis.title.x=element_text(margin=margin(t=10)),axis.title.y=element_text(margin=margin(r=10)),plot.caption=element_text(size=8,hjust=0))

cat("=== composition effect (Eng), selected K ===\n"); print(comp[Stage=="Engineer Stage"&k%in%c(5,14,35,55),.(demo,k,est=round(estimate,3),p=round(p,2))])
cat("=== productivity TE across k (Eng) ===\n"); print(prod[Stage=="Engineer Stage"&k%in%c(1,7,14,20,35,55,69),.(k,est=round(estimate,3),p=round(p,2))])

today<-format(Sys.time(),"%Y%m%d_%H%M"); out<-sprintf("OOS_Composition_Productivity_8Bstyle_%s.pdf",today)
rmarkdown::render(here("Temp","_oos_8b.Rmd"),output_file=out,output_dir=here("2_Reports"),
  knit_root_dir=here(),intermediates_dir=tempdir(),quiet=TRUE,envir=globalenv())
cat("RENDERED",out,"\n")
