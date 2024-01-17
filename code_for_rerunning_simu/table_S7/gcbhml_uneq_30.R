# > sessionInfo()
# R version 4.1.2 (2021-11-01)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS 14.0
# 
# Matrix products: default
# LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
#   [1] parallel  stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] runjags_2.2.0-3 rjags_4-14      coda_0.19-4    
# 
# loaded via a namespace (and not attached):
#   [1] compiler_4.1.2    tools_4.1.2       rstudioapi_0.15.0 grid_4.1.2        lattice_0.20-45  

library(rjags)
library(runjags)
library(parallel)
source('functions.R')
source('p_settings.R')

latent_model="
model{
  for(j in 1:Ngroup){ 
    for(i in 1:n[j]){
      Yt[i,j]~dbern(step(Zt[i,j])*0.99999+0.0000001)
      Ye[i,j]~dbern(step(Ze[i,j])*0.99999+0.0000001)
      Zt[i,j]~dnorm(muT[j],1)
      Ze[i,j]~dnorm((muE[j]+rho[j]*(Zt[i,j]-muT[j])),tau0[j])
    }
    tau0[j]<-1/(1-pow(rho[j],2))
    rho[j]~dunif(-0.9999,0.9999)
    muT[j]~dnorm(muT0,tauT)
    muE[j]~dnorm(muE0,tauE)
    pT[j]<-1-phi(-muT[j])
    pE[j]<-1-phi(-muE[j])
  }
  muT0~dnorm(0,0.01)
  muE0~dnorm(0,0.01)
}
"
a<-c(-5.31,-9.14)# E & T
b<-c(6.04,10.13)# E & T
##===============================Simulation===================================##
ngroup_all<-matrix(c(18,15,15,12,
                     36,30,30,24,
                     45,45,45,45),nrow = 3,byrow = T)
ninterim<-matrix(c(18,15,15,12,
                   18,15,15,12,
                   9,15,15,21),nrow = 3,byrow = T)

Ngroup<-4 # cancer groups/types
nstage<-nrow(ngroup_all) # trial stages(i.e.,#interim analyses=nstage-1)
rho=0.3
p<-p[,,30]
pE<-(p[1,]+p[2,])
pT<-(p[1,]+p[3,])
q1<-c(0.60,0.2)#H_1
q0<-c(0.45,0.3)#H_0
p_mid<-(q1+q0)/2
p_tilde_upper<-solve.level(rho,0.6,0.2)#H1
p_tilde_lower<-solve.level(rho,0.45,0.3)#H0
c_f<-0.05


jags_params<-c("muT","muE","pT","pE","rho")# parameters of interest without sigma2
#jags_inits<-list(theta=theta,mu=mu)# initial value 

#initialize
prE_futility<-prT_futility<-prE_eff<-prT_eff<-matrix(0,Ngroup,nsimu)
Pr_futility<-array(NA,dim=c(Ngroup,nsimu,nstage))
pE_hat<-pT_hat<-pE_hat_tmp<-pT_hat_tmp<-matrix(0,Ngroup,nsimu)
trial_process<-matrix(1,Ngroup,nsimu)
tp_expand<-sapply(1:Ngroup, function(r) array(NA,dim=c(0,nsimu)))
stopping<-integer(0)
n<-matrix(0,Ngroup,nsimu) #initial sample size n0 with **J** values in one column
Tstat<-matrix(0,2,nsimu) #T[1,] for E ; T[2,] for T
gT<-matrix(0,2,nsimu)#T[1,] for E ; T[2,] for T
sigma2_hat<-matrix(0,2,nstage)

set.seed(1234)
multi_Y0<-sapply(1:Ngroup,function(l) sapply(1:nsimu, function(m) sapply(1:ngroup_all[nstage,1], function(r) which(rmultinom(ngroup_all[nstage,1],1,p[,l])[,r]==1))),simplify = 'array')
Ye0<-(multi_Y0==1|multi_Y0==2)*1
Yt0<-(multi_Y0==1|multi_Y0==3)*1
for(t in 1:nstage){
  ##generate data set
  tp_expand0<-sapply(1:Ngroup, function(l) 
    sapply(1:nsimu, function(r) rep(trial_process[l,r],ninterim[t,l])))#expand trial_process to a ngroup0 x J x nsimu list
  tp_expand<-sapply(1:Ngroup, function(r) rbind(tp_expand[[r]],tp_expand0[[r]]),simplify = F)
  Ye<-sapply(1:Ngroup, function(r) Ye0[1:ngroup_all[t,r],,r]*tp_expand[[r]],simplify = F)
  Yt<-sapply(1:Ngroup, function(r) Yt0[1:ngroup_all[t,r],,r]*tp_expand[[r]],simplify = F)
  c_Ye<-t(sapply(1:Ngroup, function(r) colSums(Ye[[r]])))#counts of Ye
  c_Yt<-t(sapply(1:Ngroup, function(r) colSums(Yt[[r]])))
  n<-n+ninterim[t,]*trial_process
  Tstat[1,]<-sapply(1:nsimu,function(r) Tstat_fun(y = c_Ye[,r],n = n[,r]))
  Tstat[2,]<-sapply(1:nsimu,function(r) Tstat_fun(y = c_Yt[,r],n = n[,r]))
  gT<-t(sapply(1:2, function(r) exp(a[r]+b[r]*log(Tstat[r,]))))#sigma2 function
  gT[which(gT==Inf)]=10^4
  gT[which(gT==0|gT<10^(-4))]=10^(-4)
  tau<-1/gT
  sigma2_hat[,t]<-apply(gT,1,median)
  for(i in 1:nsimu){
    data_Yt<-combine_vec(sapply(1:Ngroup, function(r) Yt[[r]][,i],simplify = F))
    data_Ye<-combine_vec(sapply(1:Ngroup, function(r) Ye[[r]][,i],simplify = F))
    data<-list(Yt=data_Yt,Ye=data_Ye,n=n[,i],Ngroup=Ngroup,tauE=tau[1,i],tauT=tau[2,i])
    ##mcmc-runjags
    runjagsmodel<-run.jags(model = latent_model, monitor = jags_params,data = data,
                           n.chains = 4,adapt = 2000,burnin = 5000,
                           sample = 5000,summarise = FALSE,thin = 1,method = 'rjparallel',
                           plots=FALSE,silent.jags = T)
    codasamples<-as.mcmc.list(runjagsmodel)
    summary<-summary(codasamples)
    ##extract p mcmc samples
    for(j in 1:Ngroup){
      sample_var<-c(paste('pE[',j,']',sep = ''),paste('pT[',j,']',sep = ''))
      if(j==1){
        pE_mcmc<-as.matrix(codasamples[,sample_var[1]])
        pT_mcmc<-as.matrix(codasamples[,sample_var[2]])
        }
      else{
        pE_mcmc<-cbind(pE_mcmc,as.matrix(codasamples[,sample_var[1]]))
        pT_mcmc<-cbind(pT_mcmc,as.matrix(codasamples[,sample_var[2]]))
        }
      pE_hat_tmp[j,i]<-summary[[1]][sample_var[1],"Mean"]#needed
      pT_hat_tmp[j,i]<-summary[[1]][sample_var[2],"Mean"]#needed
      colnames(pE_mcmc)[j]<-sample_var[1]
      colnames(pT_mcmc)[j]<-sample_var[2]
      }
    
    ##calculate futility and effective probabilities
    prE_futility[,i]<-sapply(1:Ngroup,function(r) mean(pE_mcmc[,r]>p_mid[1]))#needed
    prT_futility[,i]<-sapply(1:Ngroup,function(r) mean(pT_mcmc[,r]<p_mid[2]))#needed
    prE_eff[,i]<-sapply(1:Ngroup,function(r) mean(pE_mcmc[,r]>q0[1]))#needed
    prT_eff[,i]<-sapply(1:Ngroup,function(r) mean(pT_mcmc[,r]<q0[2]))
  }
  pr_futility<-pmin(prE_futility,prT_futility)
  pr_eff<-pmin(prE_eff,prT_eff)
  Pr_futility[ , ,t]<-pr_futility
  
  stopping<-which(pr_futility<c_f & trial_process != 0)
  trial_process[stopping]<-0
  if(t==nstage|sum(trial_process)==0){ #final analysis
    pE_hat_result<-rowMeans(pE_hat_tmp)
    pT_hat_result<-rowMeans(pT_hat_tmp)
    avg_sample<-mean(colSums(n))
    break
  }
}

##save
save(Pr_futility,pr_eff,n,pE_hat_tmp,pT_hat_tmp,file="Rdata/unequal_30.Rdata")

