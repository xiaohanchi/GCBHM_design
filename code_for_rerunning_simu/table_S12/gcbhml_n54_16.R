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
#   [1] doParallel_1.0.17 iterators_1.0.14  foreach_1.5.2     runjags_2.2.0-3   rjags_4-14        coda_0.19-4      
# 
# loaded via a namespace (and not attached):
#   [1] compiler_4.1.2    tools_4.1.2       rstudioapi_0.15.0 codetools_0.2-18  grid_4.1.2        lattice_0.20-45  

library(rjags)
library(runjags)
library(parallel)
library(foreach)
library(doParallel)
source('p_settings_l.R')

##===============================Functions===================================##
Tstat_fun<-function(y,n){
  sumy<-sum(y)
  sumn<-sum(n)
  if(sumn==sumy){
    E0=0.00001*n
  }else{
    E0<-n*(sumn-sumy)/sumn
  }
  if(sumy==0){
    E1<-0.00001*n
  }else{
    E1<-n*sumy/sumn
  }
  Tstat<-sum((n-y-E0)^2/E0)+sum((y-E1)^2/E1)
  return(Tstat)
}

solve.level<-function(rho,eff,tox){
  a<-rho*sqrt(eff*(1-eff)*tox*(1-tox))+eff*tox
  b<-eff-a
  c<-tox-a
  d<-1+a-eff-tox
  res<-round(c(a,b,c,d),3)
  return(res)
}

##0705update
marginal<-function(tau1,tau2,theta1,theta2,t){
  M<-1/tau1^2+1/(tau1^2+tau2^2)
  N<-theta1/tau1^2+(t-(theta2-theta1))/(tau1^2+tau2^2)
  L<-theta1^2/tau1^2+(t-(theta2-theta1))^2/(tau1^2+tau2^2)
  logk1<- -1/2*(L-N^2/M)
  logd<- -1/2*log((2*pi*(2*tau1^2+tau2^2)))+
    logk1+pnorm((t-N/M)/sqrt(1/M),log.p = T)-
    pnorm((theta2-theta1)/sqrt(tau1^2+tau2^2),log.p = T)
  d<-exp(logd)
  return(d)
}

int_marginal<-function(r,x){
  m<-marginal(1,1,theta1_mcmc[r,j],theta2_mcmc[r,j],x)
  return(m)
}

##data
trans_y<-function(t,tp_expand,Ngroup){
  Y_1<-1*(multi_Y0==1)[1:(t*ngroup0[1]),,]*tp_expand
  Y_2<-1*(multi_Y0==2)[1:(t*ngroup0[1]),,]*tp_expand
  Y_3<-1*(multi_Y0==3)[1:(t*ngroup0[1]),,]*tp_expand
  Y_4<-1*(multi_Y0==4)[1:(t*ngroup0[1]),,]*tp_expand
  c_Y_1<-sapply(1:nsimu,function(r) colSums(Y_1[,,r]))#counts of Y_1
  c_Y_2<-sapply(1:nsimu,function(r) colSums(Y_2[,,r]))#counts of Y_2
  c_Y_3<-sapply(1:nsimu,function(r) colSums(Y_3[,,r]))#counts of Y_3
  c_Y_4<-sapply(1:nsimu,function(r) colSums(Y_4[,,r]))#counts of Y_4
  
  c_y_tmp<-list()
  for(r in 1:Ngroup){
    c_y_tmp[[r]]<-rbind(c_Y_1[r,],c_Y_2[r,],c_Y_3[r,],c_Y_4[r,])
    if(r==1){c_y<-c_y_tmp[[r]]}
    else{c_y<-rbind(c_y,c_y_tmp[[r]])}
  }
  return(c_y)
}

#parallel
comb <- function(x, ...) {
  lapply(seq_along(x),
         function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
}

latent_model="
model{
  for(j in 1:Ngroup){ 
    for(i in 1:n[j]){
      Y1[i,j]~dbern(step(Z1[i,j])*0.99999+0.0000001)
      Y2[i,j]~dbern(step(Z2[i,j])*0.99999+0.0000001)
      Z2[i,j]<-Zdelta[i,j]+Z1[i,j]
      Z1[i,j]~dnorm(theta1[j],1)
      Zdelta[i,j]~dnorm(delta[j],0.5) T(0,)
    }
    delta[j]<-theta2[j]-theta1[j]
    theta1[j]~dnorm(mu1,tau1)
    theta2[j]~dnorm(mu2,tau2)
  }
  mu1~dnorm(mu10,0.01)
  mu2~dnorm(mu20,0.01)
}
"
#n=36
# a<- c(8.72,4.23) #CR & CR/PR
# b<- c(9.33,4.48) #CR & CR/PR
#n=54
a<- c(7.88,3.98) #CR & CR/PR
b<- c(7.05,3.60) #CR & CR/PR

##===============================Simulation===================================##
#ngroup0<-c(12,12,12,12)
ngroup0<-c(18,18,18,18)
Ngroup<-4 # cancer groups/types
nstage<-3 # trial stages(i.e.,#interim analyses=nstage-1)
p<-p[,,16]
q1<-c(0.25,0.5)
q0<-c(0.15,0.3)
p_mid<-(q1+q0)/2
p_tilde_upper<-c(0.25,0.25,0.5)
p_tilde_lower<-c(0.15,0.15,0.7)
c_f<-0.05
T_factor<-(min(ngroup0)*nstage)^(-1/2)#scaled T factor


hyper0<-c(qnorm(q0[1]),qnorm(q0[2]))
jags_params<-c("theta1","theta2")# parameters of interest without sigma2
num_core<-detectCores(logical = F)

#initialize
pr1_futility<-pr2_futility<-pr_eff_tmp<-pr1_eff<-pr2_eff<-matrix(0,Ngroup,nsimu) # JxL
Pr_futility<-array(NA,dim=c(Ngroup,nsimu,nstage))
p1_hat_tmp<-p2_hat_tmp<-matrix(0,Ngroup,nsimu)
p1_hat<-p2_hat<-array(NA,dim = c(2,Ngroup,nsimu))
trial_process<-matrix(1,Ngroup,nsimu)
tp_expand<-array(NA,dim=c(0,Ngroup,nsimu))
stopping<-integer(0)
n<-matrix(0,Ngroup,nsimu) #initial sample size n0 with **J** values in one column
Tstat<-matrix(0,2,nsimu)
sigma2_hat<-matrix(0,2,nstage)

multi_Y0<-sapply(1:nsimu,function(m) sapply(1:Ngroup, function(l) sapply(1:(nstage*ngroup0[1]), function(r) which(rmultinom(nstage*ngroup0[1],1,p[,l])[,r]==1))),simplify = 'array')
Y10<-(multi_Y0==1)*1
Y20<-(multi_Y0==1|multi_Y0==2)*1

cl <- makeCluster(num_core-1)
registerDoParallel(cl)
for(t in 1:nstage){
  ##generate data set
  tp_expand0<-sapply(1:nsimu, function(r) matrix(rep(trial_process[,r],ngroup0[1]),ngroup0[1],Ngroup,byrow = T),simplify = 'array')#expand trial_process to a ngroup0 x J x nsimu array
  tp_expand<-sapply(1:nsimu, function(r) rbind(tp_expand[,,r],tp_expand0[,,r]),simplify = 'array')
  Y1<-Y10[1:(t*ngroup0[1]),,]*tp_expand
  Y2<-Y20[1:(t*ngroup0[1]),,]*tp_expand
  c_Y1<-sapply(1:nsimu,function(r) colSums(Y1[,,r]))#counts of Y1
  c_Y2<-sapply(1:nsimu,function(r) colSums(Y2[,,r]))
  n<-n+ngroup0*trial_process
  Tstat[1,]<-T_factor*sapply(1:nsimu,function(r) Tstat_fun(y = c_Y1[,r],n = n[,r]))
  Tstat[2,]<-T_factor*sapply(1:nsimu,function(r) Tstat_fun(y = c_Y2[,r],n = n[,r]))
  gT<-t(sapply(1:2, function(r) exp(a[r]+b[r]*log(Tstat[r,]))))#sigma2 function
  gT[which(gT==Inf)]=10^4
  gT[which(gT==0|gT<10^(-4))]=10^(-4)
  tau<-1/gT
  sigma2_hat[,t]<-apply(gT,1,median)
  
  output <- foreach(i=1:nsimu,.packages = c("runjags","coda"),.combine='comb', .multicombine=TRUE,
                    .init=list(list(), list(),list(), list(),list(), list())) %dopar% {
    data<-list(Y2=Y2[,,i],Y1=Y1[,,i],n=n[,i],Ngroup=Ngroup,tau1=tau[1,i],tau2=tau[2,i],
               mu10=hyper0[1],mu20=hyper0[2])
    ##mcmc-runjags
    runjagsmodel<-run.jags(model = latent_model, monitor = jags_params,data = data,
                           n.chains = 4,adapt = 2000,burnin = 5000,
                           sample = 5000,summarise = FALSE,thin = 1,method = 'rjags',
                           plots=FALSE,silent.jags = T)
    codasamples<-as.mcmc.list(runjagsmodel)
    summary<-summary(codasamples)
    
    ##extract mu mcmc samples
    p2_mcmc<-matrix(NA,20000,Ngroup)
    colnames(p2_mcmc)<-c("p2[1]","p2[2]","p2[3]","p2[4]")
    for(j in 1:Ngroup){
      sample_var<-c(paste('theta1[',j,']',sep = ''),paste('theta2[',j,']',sep = ''))
      if(j==1){
        theta1_mcmc<-as.matrix(codasamples[,sample_var[1]])
        theta2_mcmc<-as.matrix(codasamples[,sample_var[2]])
        p1_mcmc<-as.matrix(pnorm(theta1_mcmc[,j]))
      }else{
        theta1_mcmc<-cbind(theta1_mcmc,as.matrix(codasamples[,sample_var[1]]))
        theta2_mcmc<-cbind(theta2_mcmc,as.matrix(codasamples[,sample_var[2]]))
        p1_mcmc<-cbind(p1_mcmc,pnorm(theta1_mcmc[,j]))
      }
      p2_mcmc[,j]<-sapply(1:20000, function(r) {
        f<-function(x){int_marginal(r,x)}
        integrate(f,lower = 0,upper = 100,subdivisions=2000,rel.tol = 1e-10,stop.on.error = FALSE)$value})
      p2_mcmc[which(p2_mcmc[,j]<0),j]<-0
      p2_mcmc[which(p2_mcmc[,j]>1),j]<-1
      p1_hat_tmp[j,i]<-mean(p1_mcmc[,j])
      p2_hat_tmp[j,i]<-mean(p2_mcmc[,j])
      colnames(theta1_mcmc)[j]<-sample_var[1]
      colnames(theta2_mcmc)[j]<-sample_var[2]
      colnames(p1_mcmc)[j]<-paste('p1[',j,']',sep = '')
    }
    ##calculate futility and effective probabilities
    pr1_futility[,i]<-sapply(1:Ngroup,function(r) mean(p1_mcmc[,r]>p_mid[1]))#needed
    pr2_futility[,i]<-sapply(1:Ngroup,function(r) mean(p2_mcmc[,r]>p_mid[2]))#needed
    pr1_eff[,i]<-sapply(1:Ngroup,function(r) mean(p1_mcmc[,r]>q0[1]))#needed
    pr2_eff[,i]<-sapply(1:Ngroup,function(r) mean(p2_mcmc[,r]>q0[2]))
    
    result<-list(p1_hat_tmp[,i],p2_hat_tmp[,i],pr1_futility[,i],pr2_futility[,i],pr1_eff[,i],pr2_eff[,i])
    return(result)
    }
  p1_hat_tmp<-matrix(unlist(output[[1]]),nrow=Ngroup,ncol = nsimu)
  p2_hat_tmp<-matrix(unlist(output[[2]]),nrow=Ngroup,ncol = nsimu)
  pr1_futility<-matrix(unlist(output[[3]]),nrow=Ngroup,ncol = nsimu)
  pr2_futility<-matrix(unlist(output[[4]]),nrow=Ngroup,ncol = nsimu)
  pr1_eff<-matrix(unlist(output[[5]]),nrow=Ngroup,ncol = nsimu)
  pr2_eff<-matrix(unlist(output[[6]]),nrow=Ngroup,ncol = nsimu)
  
  pr_futility<-pmax(pr1_futility,pr2_futility)
  pr_eff<-pmax(pr1_eff,pr2_eff)
  Pr_futility[ , ,t]<-pr_futility
  
  stopping<-which(pr_futility<c_f & trial_process != 0)
  trial_process[stopping]<-0
  if(t==nstage|sum(trial_process)==0){ #final analysis
    p1_hat_result<-rowMeans(p1_hat_tmp)
    p2_hat_result<-rowMeans(p2_hat_tmp)
    avg_sample<-mean(colSums(n))
    break
  }
}
stopCluster(cl)

##save
save(Pr_futility,pr_eff,n,p1_hat_tmp,p2_hat_tmp,file="Rdata/gcbhml_n54_16.Rdata")


