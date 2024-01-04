# October 9, 2023
# Code for manuscript: A Generalized Calibrated Bayesian Hierarchical Modeling Approach to Basket Trials with Bivariate Endpoints
# Calibrate a and b for the following simulation

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


##=============================== Functions ===================================##
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

ngroup0<-c(15,15,15,15)
Ngroup<-4 # #cancer groups/types
Ncategory<-4
nstage<-3
q1<-c(0.25,0.5)
q0<-c(0.15,0.3)
p_tilde_upper<-c(0.25,0.25,0.2,0.3)
p_tilde_lower<-c(0.15,0.15,0.3,0.4)
nsimu_c<-10000
sigma2_c<-c(1,100)#small sigma2 and big sigma2

#step1
p1<-matrix(data=rep(p_tilde_upper,4),
           nrow = Ncategory,ncol = Ngroup,byrow = F)
Hb<-medianT_fun(n=(ngroup0*nstage),p=p1)
#step2
Hb_hetero<-numeric(Ngroup-1)
for(j in 1:(Ngroup-1)){
  p2<-matrix(data = c(rep(p_tilde_upper,j),rep(p_tilde_lower,(Ngroup-j))),
             nrow = Ncategory,ncol = Ngroup,byrow = F)
  Hb_hetero[j]<-medianT_fun(n=(ngroup0*nstage),p=p2)
}
Hb_hetero_min<-min(Hb_hetero)
#step3
b<-(log(sigma2_c[1])-log(sigma2_c[2]))/(log(Hb)-log(Hb_hetero_min))
a<-log(sigma2_c[1])-b*log(Hb)-b*log((nstage*min(ngroup0))^(-1/2))

print("calibrated value of (a, b):")
cat(formatC(c(a,b), digits=2, format="f"), sep ="  ", "\n") 
