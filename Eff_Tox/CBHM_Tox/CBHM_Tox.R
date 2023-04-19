# April 18, 2023
# Code for manuscript: A Generalized Calibrated Bayesian Hierarchical Modeling Approach to Basket Trials with Multiple Endpoints
# First calibrate a and b using calibration.R and get the calibrated values
# Results are saved in a separate .Rdata file named "var_1" 
# To get the summarized results in the manuscript: run evaluate.R


library(R2jags)
library(rjags)
library(runjags)
library(parallel)
source('../functions.R')
source('../p_settings.R')

##=============================== JAGS CODE ================================##
CBHM_eff_model="
model{
  for(i in 1:Ngroup){ 
    y[i] ~ dbin(p[i],n[i])
    p[i]<-exp(theta[i])/(1+exp(theta[i]))
  }
  for(i in 1:Ngroup){ 
    theta[i]~dnorm(mu,tau)
  }
  mu~dnorm(mu0,0.01)
}
"
CBHM_tox_model="
model{
  for(i in 1:Ngroup){ 
    y[i] ~ dbin(p[i],n[i])
    p[i]<-exp(theta[i])/(1+exp(theta[i]))
  }
  for(i in 1:Ngroup){ 
    theta[i]~dnorm(mu[i],0.01)
  }
}
"
##===============================Settings===================================##
#calibrated a and b
a<- -5.28
b<- 6.05
ngroup0<-c(15,15,15,15)
Ngroup<-4 # cancer groups/types
nstage<-3 # trial stages(i.e.,#interim analyses=nstage-1)
p<-p[,,1]
pE<-(p[1,]+p[2,])
pT<-(p[1,]+p[3,])
pE_upper<-c(0.60,0.60,0.60,0.60)
pE_lower<-c(0.45,0.45,0.45,0.45)
pT_upper<-c(0.20,0.20,0.20,0.20)
pT_lower<-c(0.30,0.30,0.30,0.30)
pE_mid<-(pE_upper+pE_lower)/2
pT_mid<-(pT_upper+pT_lower)/2
c_f<-0.05
nsimu<-5000

muE<-mean(logit_p(pE_lower))
muT<-logit_p(pT_lower)
jags_paramsE<-c("mu","theta","p")# parameters of interest without sigma2
jags_paramsT<-c("theta","p")

##===============================Simulation===================================##
#initialize
pr_futilityE<-pr_futilityT<-pr_eff_tmp<-pr_effE<-pr_effT<-matrix(0,Ngroup,nsimu)
Pr_futility<-array(NA,dim=c(Ngroup,nsimu,nstage))
p_hat_tmpT<-p_hat_tmpE<-sigma2_hat_tmp<-matrix(0,Ngroup,nsimu)
trial_process<-matrix(1,Ngroup,nsimu)
stopping<-integer(0)
yE<-yT<-matrix(0,Ngroup,nsimu)
n<-matrix(0,Ngroup,nsimu) #initial sample size n0
sigma2_hatE<-numeric(nstage)

for(t in 1:nstage){
  ##generate data set
  set.seed(233+t)
  yE<-yE+sapply(1:nsimu,function(r) rbinom(rep(1,Ngroup),ngroup0,pE))*trial_process
  yT<-yT+sapply(1:nsimu,function(r) rbinom(rep(1,Ngroup),ngroup0,pT))*trial_process
  n<-n+ngroup0*trial_process
  TstatE<-sapply(1:nsimu,function(r) Tstat_fun(yE[,r],n[,r]))
  gT_E<-exp(a+b*log(TstatE))#sigma2 function
  gT_E[which(gT_E==Inf)]=10^4
  gT_E[which(gT_E==0|gT_E<10^(-4))]=10^(-4)
  tauE<-1/gT_E
  sigma2_hatE[t]<-median(gT_E)
  for(i in 1:nsimu){
    dataE<-list(y=yE[,i],n=n[,i],Ngroup=Ngroup,tau=tauE[i],mu0=muE)
    runjagsmodel<-run.jags(model = CBHM_eff_model, monitor = jags_paramsE,data = dataE,
                           n.chains = 4,adapt = 2000,burnin = 5000,
                           sample = 5000,summarise = FALSE,thin = 1,method = 'rjparallel',
                           plots=FALSE,silent.jags = T)
    codasamples<-as.mcmc.list(runjagsmodel)
    summary<-summary(codasamples)
    p_mcmc<-matrix(NA,nrow = (runjagsmodel$sample*length(runjagsmodel$mcmc)),ncol = 1)
    colnames<-c()
    ##extract p mcmc samples
    for(j in 1:Ngroup){
      sample_var<-paste('p[',j,']',sep = '')
      if(j==1){p_mcmc<-as.matrix(codasamples[,sample_var])}
      else{p_mcmc<-cbind(p_mcmc,as.matrix(codasamples[,sample_var]))}
      p_hat_tmpE[j,i]<-summary[[1]][sample_var,"Mean"]#needed
      colnames(p_mcmc)[j]<-sample_var}
    ##calculate futility and effective probabilities
    pr_futilityE[,i]<-sapply(1:Ngroup,function(r) mean(p_mcmc[,r]>pE_mid[r]))#needed
    pr_effE[,i]<-sapply(1:Ngroup,function(r) mean(p_mcmc[,r]>pE_lower[r]))#needed
    
    #tox
    dataT<-list(y=yT[,i],n=n[,i],Ngroup=Ngroup,mu=muT)
    runjagsmodel<-run.jags(model = CBHM_tox_model, monitor = jags_paramsT,data = dataT,
                           n.chains = 4,adapt = 2000,burnin = 5000,
                           sample = 5000,summarise = FALSE,thin = 1,method = 'rjparallel',
                           plots=FALSE,silent.jags = T)
    codasamples<-as.mcmc.list(runjagsmodel)
    summary<-summary(codasamples)
    p_mcmc<-matrix(NA,nrow = (runjagsmodel$sample*length(runjagsmodel$mcmc)),ncol = 1)
    colnames<-c()
    ##extract p mcmc samples
    for(j in 1:Ngroup){
      sample_var<-paste('p[',j,']',sep = '')
      if(j==1){p_mcmc<-as.matrix(codasamples[,sample_var])}
      else{p_mcmc<-cbind(p_mcmc,as.matrix(codasamples[,sample_var]))}
      p_hat_tmpT[j,i]<-summary[[1]][sample_var,"Mean"]#needed
      colnames(p_mcmc)[j]<-sample_var}
    ##calculate futility and effective probabilities
    pr_futilityT[,i]<-sapply(1:Ngroup,function(r) mean(p_mcmc[,r]<pT_mid[r]))#needed
    pr_effT[,i]<-sapply(1:Ngroup,function(r) mean(p_mcmc[,r]<pT_lower[r]))#needed
    
  }
  Pr_futility[ , ,t]<-pmin(pr_futilityE,pr_futilityT)
  pr_eff<-pmin(pr_effE,pr_effT)
  stopping<-which(Pr_futility[ , ,t]<c_f & trial_process != 0)
  trial_process[stopping]<-0
  if(t==nstage|sum(trial_process)==0){ #final analysis
    p_hat_resultE<-rowMeans(p_hat_tmpE)
    p_hat_resultT<-rowMeans(p_hat_tmpT)
    avg_sample<-mean(colSums(n))
    break
  }
}

##save
save(Pr_futility,pr_eff,n,p_hat_tmpE,p_hat_tmpT,file="var_1.Rdata")

##final output
print("average number of patients used") 
cat(formatC(c(avg_sample), digits=1, format="f"), sep ="  ", "\n") 
print("estimated response rate (efficacy) by subgroup")
print(formatC(p_hat_resultE,digits=2, format="f"),quote = F) 
print("estimated response rate (toxicity) by subgroup")
print(formatC(p_hat_resultT,digits=2, format="f"),quote = F) 
print("estimated shrinkage parameter (efficacy) at each interim analysis")
cat(formatC(c(sigma2_hatE), digits=2, format="f"), sep ="  ", "\n") 


