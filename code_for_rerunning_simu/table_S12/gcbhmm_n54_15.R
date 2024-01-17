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

source('p_settings_m.R')


##===============================Functions===================================##
Tstat_fun<-function(y,n){
  #input:
  #y: vector with length of K*J, in a single trial
  #n: vector with length of J, total samples of each groups in a single trial
  E<-matrix(0,Ncategory,Ngroup)
  y<-matrix(data = y,nrow = Ncategory,ncol = Ngroup,byrow = F)
  sumy<-rowSums(y)
  sumn<-sum(n)
  E<-t(t(sumy))%*%n/sumn
  if(0 %in% sumy){
    E[which(sumy==0),]<-0.00001*n
  }
  Tstat<-sum((y-E)^2/E)
  return(Tstat)
}


softmax_p<-function(p){
  #p: vector with length K
  p<-as.matrix(p)
  result<-sapply(1:dim(p)[2],function(r) log(p[,r]/p[1,r]))
  if(dim(p)[2]==1){
    result<-as.vector(result)
  }
  return(result)
}

CBHMc_model="
model{
for(i in 1:Ngroup){ 
  y[(K*(i-1)+1):(K*(i-1)+K)] ~ dmulti(p[1:K,i],n[i])
  p[1:K,i]<-exp_v[1:K,i]/(sum(exp_v[1:K,i]))
  exp_v[1,i]<-1
  theta[1,i]<-0
  for(k in 2:K){
    exp_v[k,i]<-exp(theta[k,i])
    theta[k,i]~dnorm(mu[k],tau)
  }
}
for(k in 1:K){
    mu[k]~dnorm(mu0,0.01)
  }
}
"
#n=36
# a=-3.86
# b=11.22
#n=54
a=-1.11
b=8.08
##===============================Simulation===================================##
#ngroup0<-c(12,12,12,12)
ngroup0<-c(18,18,18,18)
Ngroup<-4 # cancer groups/types
Ncategory<-4
nstage<-3 # trial stages(i.e.,#interim analyses=nstage-1)
p<-p[,,15]
q1<-c(0.25,0.5)
q0<-c(0.15,0.3)
bmatrix<-matrix(data = c(1, 0, 0,0, 1,1, 0,0),
                nrow = 2,ncol = 4,byrow = T)#LxK design matrix b
Ncons<-nrow(bmatrix) #number of constraints
ep_type<-1 #1 for 'and' constraints and 2 for 'or' constraints
p_mid<-(q1+q0)/2
p_tilde_upper<-c(0.25,0.25,0.2,0.3)
p_tilde_lower<-c(0.15,0.15,0.3,0.4)
c_f<-0.05
T_factor<-(min(ngroup0)*nstage)^(-1/2)#scaled T factor


theta<-softmax_p(p)
mu<-softmax_p(p_tilde_lower)
mu0<-mean(mu)
jags_params<-c("mu","theta","p")# parameters of interest without sigma2

#initialize
pr_futility<-pr_eff_tmp<-pr_eff<-matrix(0,(Ngroup*Ncons),nsimu) # JxL
Pr_futility<-array(NA,dim=c((Ncons*Ngroup),nsimu,nstage))
p_hat_tmp<-matrix(0,(Ngroup*Ncategory),nsimu)
p_hat<-array(NA,dim = c(2,Ngroup,nsimu))
trial_process0<-trial_process<-matrix(1,Ngroup,nsimu)
stopping<-integer(0)
y<-matrix(0,(Ngroup*Ncategory),nsimu) #collected data with **JxK** values in one column
n<-matrix(0,Ngroup,nsimu) #initial sample size n0 with **J** values in one column
sigma2_hat<-numeric(nstage)

for(t in 1:nstage){
  ##generate data set
  set.seed(233+t)
  tp_expand<-t(sapply(1:(Ngroup*Ncategory), function(r) trial_process[ceiling(r/Ncategory),]))#expand trial_process to a (JxK) x nsimu matrix
  y<-y+sapply(1:nsimu,function(r) sapply(1:Ngroup,function(r) rmultinom(rep(1,Ngroup),ngroup0,p[,r])))*tp_expand
  n<-n+ngroup0*trial_process
  Tstat<-T_factor*sapply(1:nsimu,function(r) Tstat_fun(y = y[,r],n = n[,r]))
  gT<-exp(a+b*log(Tstat))#sigma2 function
  gT[which(gT==Inf)]=10^4
  gT[which(gT==0|gT<10^(-4))]=10^(-4)
  tau<-1/gT
  sigma2_hat[t]<-median(gT)
  for(i in 1:nsimu){
    data<-list(y=y[,i],n=n[,i],Ngroup=Ngroup,K=Ncategory,tau=tau[i],mu0=mu0)
 
    ##mcmc-runjags
    runjagsmodel<-run.jags(model = CBHMc_model, monitor = jags_params,data = data,
                           n.chains = 4,adapt = 2000,burnin = 5000,
                           sample = 5000,summarise = FALSE,thin = 1,method = 'rjparallel',
                           plots=FALSE,silent.jags = T)
    codasamples<-as.mcmc.list(runjagsmodel)
    summary<-summary(codasamples)
    p_mcmc<-matrix(NA,nrow = (runjagsmodel$sample*length(runjagsmodel$mcmc)),ncol = 1)
    colnames<-c()
    ##extract p mcmc samples
    for(k in 1:Ncategory){
      for(j in 1:Ngroup){
        sample_var0<-paste('p[',k,',',j,']',sep = '')
        if(1 %in% bmatrix[,k]){#save mcmc samples used to make go/no-go decisions
          if(is.na(p_mcmc[1,1])){p_mcmc[,1]<-as.matrix(codasamples[,sample_var0])}
          else{p_mcmc<-cbind(p_mcmc,as.matrix(codasamples[,sample_var0]))}
          colnames<-c(colnames,sample_var0)
        }
        p_hat_tmp[Ncategory*(j-1)+k,i]<-summary$statistics[sample_var0,"Mean"]#needed
      }
    }
    p_hat[,,i]<-sapply(1:Ngroup, function(j) sapply(1:2, function(r) p_hat_tmp[4*(j-1)+1,i]+(r-1)*p_hat_tmp[4*(j-1)+2,i]))
    colnames(p_mcmc)<-colnames
    
    ##calculate futility and effective probabilities
    for(j in 1:Ngroup){
      p_index0<-sapply(1:Ncategory, function(r) paste('p[',r,',',j,']',sep = ''))
      for(l in 1:Ncons){
        p_index<-p_index0[which(bmatrix[l,]==1)]
        if(length(p_index)==1&ep_type==1){#rowSums cannot deal with ncol=1
          pr_futility[Ncons*(j-1)+l,i]<-mean(p_mcmc[,p_index]>p_mid[l])
          pr_eff[Ncons*(j-1)+l,i]<-mean(p_mcmc[,p_index]>q0[l])
        }else if(length(p_index)!=1&ep_type==1){
          pr_futility[Ncons*(j-1)+l,i]<-mean(rowSums(p_mcmc[,p_index])>p_mid[l])
          pr_eff[Ncons*(j-1)+l,i]<-mean(rowSums(p_mcmc[,p_index])>q0[l])
        }else if(length(p_index)!=1&ep_type==2){
          if(l==1){#eff
            pr_futility[Ncons*(j-1)+l,i]<-mean(rowSums(p_mcmc[,p_index])>p_mid[l])
            pr_eff[Ncons*(j-1)+l,i]<-mean(rowSums(p_mcmc[,p_index])>q0[l])
          }else if(l==2){#tox
            pr_futility[Ncons*(j-1)+l,i]<-mean(rowSums(p_mcmc[,p_index])<p_mid[l])
            pr_eff[Ncons*(j-1)+l,i]<-mean(rowSums(p_mcmc[,p_index])<q0[l])
          }
        }
      }
    }
    
  }
  Pr_futility[ , ,t]<-pr_futility
  
  ##deal with diff types of endpoints, which must be specified in advance
  if(ep_type==1){#and
    trial_process0<-t(sapply(1:Ngroup,function(r) apply((pr_futility<c_f)[(Ncons*(r-1)+1):(Ncons*(r-1)+Ncons),],2,prod)))
  }else if(ep_type==2){#or
    trial_process0<-t(sapply(1:Ngroup,function(r) apply((pr_futility<c_f)[(Ncons*(r-1)+1):(Ncons*(r-1)+Ncons),],2,sum)))
  }
  stopping<-which(trial_process0 != 0 & trial_process != 0)
  trial_process[stopping]<-0
  if(t==nstage|sum(trial_process)==0){#final analysis
    p_hat_result<-rowMeans(p_hat_tmp)
    avg_sample<-mean(colSums(n))
    break
  }
}

##save
save(Pr_futility,pr_eff,n,p_hat,file="Rdata/gcbhmm_n54_15.Rdata")



