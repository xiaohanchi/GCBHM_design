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

medianT_fun<-function(n,p){
  #input:
  #n: vector with length of J, total samples of each groups in a single trial
  #p: KxJ matrix
  #set.seed(2333)
  y<-sapply(1:nsimu_c,function(r) sapply(1:Ngroup,function(r) rmultinom(rep(1,Ngroup),n,p[,r])))
  Tstat<-sapply(1:nsimu_c,function(r) Tstat_fun(y[,r],n))
  medianT<-median(Tstat)
  return(medianT)
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