## Test the OOS-section compute in isolation before inlining into 8B_Stage_BH.Rmd.
suppressMessages({library(pacman); p_load(haven,here,readr,data.table,stringr,fixest,ggplot2,lme4)})
options(warn=-1, readr.show_types=FALSE); date<-"18mar2026"
data_root<-"C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"; setwd(here::here())
invisible(capture.output({for(f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning")) source(here::here("1_Codes",paste0(f,".R")))}))
set.seed(20260703)

local({
  sr_o <- fread(file.path(data_root,"3output_data/Main/student_data/student_matching/student_resume.csv"))
  a69_o<- fread(file.path(data_root,"3output_data/Main/student_data/main_all69_student_sys.csv"))
  sr_o[,resume_index:=suppressWarnings(as.integer(resume_index))]
  sr_o[,os_key:=round(overall_score,3)]; a69_o[,os_key:=round(overall_score,3)]
  sr_o<-merge(sr_o, unique(a69_o,by="os_key")[,.(os_key,max_firm_signal_tier)], by="os_key", all.x=TRUE)
  sr_o[,`:=`(tier_top=as.integer(max_firm_signal_tier=="Top"), tier_ord=as.integer(max_firm_signal_tier%in%c("Ordinary","Growth")))]
  sr_o[is.na(tier_top),tier_top:=0L]; sr_o[is.na(tier_ord),tier_ord:=0L]
  sr_o[,Yo:=overall_score]; sr_o[,rg:=gsub(" ","_",demo_group)]
  for(vv in c("test_case","gpa","num_exp","num_awards")) sr_o[,paste0(vv,"_zo"):=as.numeric(scale(get(vv)))]
  grpo<-c("White_Male","White_Female","Asian_Male","Asian_Female","Hispanic_Male","Hispanic_Female")
  sigo<-list(Eng=c("test_case_zo","gpa_zo","num_exp_zo","num_awards_zo","tier_top","tier_ord"),
             HR =c("gpa_zo","num_exp_zo","num_awards_zo","tier_top","tier_ord"))
  primo<-list()
  for(role_ in c("Eng","HR")){
    bl<-firm_long_dt[treat=="blind"&role==role_,.(bscore=mean(sc_overall_z,na.rm=TRUE)),by=resume_index]
    bl[,resume_index:=suppressWarnings(as.integer(as.character(resume_index)))]
    fitdt<-merge(sr_o[!is.na(resume_index)],bl,by="resume_index")
    mm0<-lm(as.formula(paste("bscore ~",paste(sigo[[role_]],collapse="+"))),fitdt)
    sr_o[[paste0("shat_",role_)]]<-predict(mm0,sr_o)
    sr_o[[paste0("u_",role_)]]<-sr_o$Yo - predict(lm(as.formula(paste0("Yo ~ shat_",role_)),sr_o),sr_o)
    d<-firm_long_dt[treat=="nonblind"&role==role_]
    feg<-feols(sc_overall_z~i(race_gender,ref="White_Male")|resume_index,d,vcov=~responseid)
    cf<-coef(feg); b<-setNames(rep(0,6),grpo); for(g in grpo){nm<-paste0("race_gender::",g); if(nm%in%names(cf)) b[g]<-cf[nm]}
    pg<-prop.table(table(factor(sr_o$rg,levels=grpo))); b<-b-sum(pg*b)
    d[,bhat:=b[as.character(race_gender)]]
    tau<-tryCatch(as.numeric(attr(lme4::VarCorr(lme4::lmer(sc_overall_z~bhat+factor(resume_index)+(1+bhat|responseid),d))$responseid,"stddev")["bhat"]),error=function(e) 0)
    se<-sd(resid(feols(sc_overall_z~1|resume_index+responseid,firm_long_dt[treat=="blind"&role==role_])))
    primo[[role_]]<-list(b=b,tau=tau,se=se,n_eval=uniqueN(firm_long_dt[role==role_]$responseid))
  }
  SDYo<-sd(sr_o$Yo); no<-nrow(sr_o); rgv<-sr_o$rg; Yv<-sr_o$Yo; ngo<-table(factor(rgv,levels=grpo)); Kg<-c(1:20,seq(25,65,5),69)
  simc<-function(role_,M=300){
    shat<-sr_o[[paste0("shat_",role_)]]; b<-primo[[role_]]$b; tau<-primo[[role_]]$tau; se<-primo[[role_]]$se; bh<-b[rgv]
    cS<-matrix(0,69,6); cSS<-matrix(0,69,6); teS<-numeric(69); teSS<-numeric(69); ynS<-numeric(69); ynQ<-numeric(69); ynN<-numeric(69)
    for(m in 1:M){slope<-rnorm(1,1,tau); eps<-rnorm(no,0,se); ob<-order(-(shat+eps)); on<-order(-(shat+slope*bh+eps))
      gb<-rgv[ob]; gn<-rgv[on]; yb<-cumsum(Yv[ob])/(1:no); yn<-cumsum(Yv[on])/(1:no)
      for(gi in 1:6){eff<-(cumsum(gb==grpo[gi])/as.numeric(ngo[gi])-cumsum(gn==grpo[gi])/as.numeric(ngo[gi]))[1:69]; cS[,gi]<-cS[,gi]+eff; cSS[,gi]<-cSS[,gi]+eff^2}
      dte<-(yb-yn)[1:69]; teS<-teS+dte; teSS<-teSS+dte^2
      for(K in 1:69){ynS[K]<-ynS[K]+sum(Yv[on[1:K]]); ynQ[K]<-ynQ[K]+sum(Yv[on[1:K]]^2); ynN[K]<-ynN[K]+K}}
    ne<-primo[[role_]]$n_eval; st<-ifelse(role_=="Eng","Engineer Stage","HR Stage")
    comp<-rbindlist(lapply(1:6,function(gi){mn<-cS[,gi]/M; sdd<-sqrt(pmax(0,cSS[,gi]/M-mn^2)); s_<-sdd/sqrt(ne)
      data.table(demo=gsub("_"," ",grpo[gi]),k=1:69,estimate=mn,ci_low=mn-1.96*s_,ci_high=mn+1.96*s_,p=2*pnorm(-abs(mn/s_)),Stage=st)}))[k%in%Kg]
    sdK<-sqrt(pmax(1e-9,ynQ/ynN-(ynS/ynN)^2)); tm<-teS/M; tsd<-sqrt(pmax(0,teSS/M-tm^2)); ts<-tsd/sqrt(ne)
    prod<-data.table(k=1:69,estimate=tm/sdK,ci_low=(tm-1.96*ts)/sdK,ci_high=(tm+1.96*ts)/sdK,p=2*pnorm(-abs(tm/ts)),Stage=st)[k%in%Kg]
    list(comp=comp,prod=prod)}
  Eo<-simc("Eng"); Ho<-simc("HR"); compo<-rbind(Eo$comp,Ho$comp); prodo<-rbind(Eo$prod,Ho$prod)
  star<-function(p) fcase(p<0.01,"***",p<0.05,"**",p<0.10,"*",default="")
  compo[,`:=`(Stage=factor(Stage,c("HR Stage","Engineer Stage")),demo=factor(demo,gsub("_"," ",grpo)),sig=star(p),star_y=ifelse(estimate>=0,ci_high+0.01,ci_low-0.01))]
  prodo[,`:=`(Stage=factor(Stage,c("HR Stage","Engineer Stage")),sig=star(p))]
  sens_sim<-function(role_,a,K,M=200){shat<-sr_o[[paste0("shat_",role_)]]; b<-primo[[role_]]$b; tau<-primo[[role_]]$tau; se<-primo[[role_]]$se
    u<-sr_o[[paste0("u_",role_)]]; uu<-as.numeric(scale(u)); pg<-prop.table(table(factor(rgv,levels=grpo))); gord<-names(sort(b)); ngg<-pmax(1,round(as.numeric(pg[gord])*no))
    te<-numeric(M); rc<-numeric(M)
    for(m in 1:M){key<--a*uu+sqrt(max(0,1-a^2))*rnorm(no); oc<-order(key); grp<-character(no); ix<-1
      for(gi in seq_along(gord)){cnt<-if(gi==length(gord))(no-ix+1) else ngg[gi]; grp[oc[ix:(ix+cnt-1)]]<-gord[gi]; ix<-ix+cnt}
      bh<-b[grp]; rc[m]<-cor(bh,u); slope<-rnorm(1,1,tau); eps<-rnorm(no,0,se)
      te[m]<-mean(Yv[order(-(shat+eps))[1:K]])-mean(Yv[order(-(shat+slope*bh+eps))[1:K]])}
    c(be=mean(te)/SDYo, rc=mean(rc))}
  senso<-rbindlist(lapply(c("Eng","HR"),function(r) rbindlist(lapply(c(5,10),function(K) rbindlist(lapply(seq(-0.6,0.6,0.2),function(a){o<-sens_sim(r,a,K); data.table(Stage=ifelse(r=="Eng","Engineer Stage","HR Stage"),K=K,realcorr=o["rc"],be=o["be"])}))))))
  cat("SANITY: SDY=",round(SDYo,1)," Eng beta spread=",round(diff(range(primo$Eng$b)),3)," compo rows=",nrow(compo)," prodo rows=",nrow(prodo)," senso rows=",nrow(senso),"\n")
  cat("Eng productivity TE at k=1,10,69:\n"); print(prodo[Stage=="Engineer Stage"&k%in%c(1,10,69),.(k,est=round(estimate,3),p=round(p,2))])
})
cat("DONE\n")
