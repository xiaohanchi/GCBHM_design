# April 18, 2023
# Code for manuscript: A Generalized Calibrated Bayesian Hierarchical Modeling Approach to Basket Trials with Bivariate Endpoints
# BHM design with inv-Gamma prior
# Results are saved in a separate .Rdata file named "bhm_ig_1" 

library(R2jags)
library(rjags)
library(runjags)
library(parallel)
source('../functions.R')
source('../p_settings.R')

##=============================== JAGS CODE ================================##
latent_BHM="
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
  sigma2T<-1/tauT
  sigma2E<-1/tauE
  tauT~dgamma(0.0005,0.000005)
  tauE~dgamma(0.0005,0.000005)
}
"

##===============================Settings===================================##
ngroup0<-c(15,15,15,15)
Ngroup<-4 # #cancer groups/types
nstage<-3 # #trial stages(i.e.,#interim analyses=nstage-1)
p<-p[,,1] # scenario 1
pE<-(p[1,]+p[2,])
pT<-(p[1,]+p[3,])
q1<-c(0.60,0.2) #H_1
q0<-c(0.45,0.3) #H_0
p_mid<-(q1+q0)/2
p_tilde_upper<-solve.level(rho,0.6,0.2) #H_1
p_tilde_lower<-solve.level(rho,0.45,0.3) #H_0
c_f<-0.05

jags_params<-c("muT","muE","pT","pE","sigma2T","sigma2E")# parameters of interest without sigma2


##===============================Simulation===================================##
#initialize
prE_futility<-prT_futility<-prE_eff<-prT_eff<-matrix(0,Ngroup,nsimu)
Pr_futility<-array(NA,dim=c(Ngroup,nsimu,nstage))
pE_hat<-pT_hat<-pE_hat_tmp<-pT_hat_tmp<-matrix(0,Ngroup,nsimu)
trial_process<-matrix(1,Ngroup,nsimu)
tp_expand<-array(NA,dim=c(0,Ngroup,nsimu))
stopping<-integer(0)
n<-matrix(0,Ngroup,nsimu) #initial sample size n0 with **J** values in one column
sigma2T_hat_tmp<-sigma2E_hat_tmp<-matrix(0,nstage,nsimu)
sigma2_hat<-matrix(0,2,nstage)

multi_Y0<-sapply(1:nsimu,function(m) sapply(1:Ngroup, function(l) sapply(1:(nstage*ngroup0[1]), function(r) which(rmultinom(nstage*ngroup0[1],1,p[,l])[,r]==1))),simplify = 'array')
Ye0<-(multi_Y0==1|multi_Y0==2)*1
Yt0<-(multi_Y0==1|multi_Y0==3)*1
for(t in 1:nstage){
  ##generate data set
  tp_expand0<-sapply(1:nsimu, function(r) matrix(rep(trial_process[,r],ngroup0[1]),ngroup0[1],Ngroup,byrow = T),simplify = 'array')#expand trial_process to a ngroup0 x J x nsimu array
  tp_expand<-sapply(1:nsimu, function(r) rbind(tp_expand[,,r],tp_expand0[,,r]),simplify = 'array')
  Ye<-Ye0[1:(t*ngroup0[1]),,]*tp_expand
  Yt<-Yt0[1:(t*ngroup0[1]),,]*tp_expand
  c_Ye<-sapply(1:nsimu,function(r) colSums(Ye[,,r]))#counts of Ye
  c_Yt<-sapply(1:nsimu,function(r) colSums(Yt[,,r]))
  n<-n + ngroup0*trial_process
  for(i in 1:nsimu){
    data<-list(Yt=Yt[,,i],Ye=Ye[,,i],n=n[,i],Ngroup=Ngroup)
    ##mcmc-runjags
    runjagsmodel<-run.jags(model = latent_BHM, monitor = jags_params,data = data,
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
    sigma2T_hat_tmp[t,i]<-summary$statistics["sigma2T","Mean"]
    sigma2E_hat_tmp[t,i]<-summary$statistics["sigma2E","Mean"]
    
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
    sigma2T_hat<-apply(sigma2T_hat_tmp,1,median)
    sigma2E_hat<-apply(sigma2E_hat_tmp,1,median)
    avg_sample<-mean(colSums(n))
    break
  }
}

##save
save(Pr_futility,pr_eff,n,pE_hat_tmp,pT_hat_tmp,file="bhm_ig_1.Rdata")

##final output
print("average number of patients used") 
cat(formatC(c(avg_sample), digits=1, format="f"), sep ="  ", "\n") 
print("estimated response rate (efficacy) by subgroup")
print(formatC(pE_hat_result,digits=2, format="f"),quote = F) 
print("estimated response rate (toxicity) by subgroup")
print(formatC(pT_hat_result,digits=2, format="f"),quote = F) 
print("estimated shrinkage parameter (T) at each interim analysis")
print(formatC(sigma2T_hat, digits=2, format="f"), quote = F) 
print("estimated shrinkage parameter (E) at each interim analysis")
print(formatC(sigma2E_hat, digits=2, format="f"), quote = F) 


