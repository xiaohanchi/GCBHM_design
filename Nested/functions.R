##########################################################################################
# Functions for Nested Efficacy Endpoints                                                #
#                                                                                        #
# Author: Xiaohan Chi                                                                    #
# 10/09/2023                                                                             #
##########################################################################################

softmax_p<-function(p){
  #p: vector with length K
  p<-as.matrix(p)
  result<-sapply(1:dim(p)[2],function(r) log(p[,r]/p[1,r]))
  if(dim(p)[2]==1){
    result<-as.vector(result)
  }
  return(result)
}

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

medianT_fun<-function(n,p){
  #set.seed(2333)
  y<-sapply(1:nsimu_c,function(r) rbinom(rep(1,Ngroup),n,p))
  Tstat<-sapply(1:nsimu_c,function(r) Tstat_fun(y[,r],n))
  medianT<-median(Tstat)
  return(medianT)
}

solve.level<-function(rho,eff,tox){
  a<-rho*sqrt(eff*(1-eff)*tox*(1-tox))+eff*tox
  b<-eff-a
  c<-tox-a
  d<-1+a-eff-tox
  res<-round(c(a,b,c,d),3)
  return(res)
}

##integral
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

#parallel
comb <- function(x, ...) {
  lapply(seq_along(x),
         function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
}
