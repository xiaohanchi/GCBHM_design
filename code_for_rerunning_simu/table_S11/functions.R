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