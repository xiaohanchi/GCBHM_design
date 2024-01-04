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

source('functions.R')
ngroup0<-c(15,15,15,15)
Ngroup<-4 # #cancer groups/types
nstage<-3
q1<-c(0.25,0.5)
q0<-c(0.15,0.3)
nsimu_c<-10000
sigma2_c<-c(1,100)#small sigma2 and big sigma2

calibration<-function(p1,p0,ngroup0,nstage,Ngroup,sigma2,nsimu){
  #step1
  Hb<-medianT_fun(n=(ngroup0*nstage),p=rep(p1,Ngroup))
  #step2
  Hb_hetero<-numeric(Ngroup-1)
  for(j in 1:(Ngroup-1)){
    p2<-c(rep(p1,j),rep(p0,(Ngroup-j)))
    Hb_hetero[j]<-medianT_fun(n=(ngroup0*nstage),p=p2)
  }
  Hb_hetero_min<-min(Hb_hetero)
  #step3
  b<-(log(sigma2_c[1])-log(sigma2_c[2]))/(log(Hb)-log(Hb_hetero_min))
  a<-log(sigma2_c[1])-b*log(Hb)-b*log((nstage*min(ngroup0))^(-1/2))
  #print results
  print("calibrated value of (a, b):")
  cat(formatC(c(a,b), digits=2, format="f"), sep ="  ", "\n") 
}

print("For CR endpoint: ")
calibration(p1=q1[1],p0=q0[1],ngroup0 = ngroup0,nstage = nstage,Ngroup = Ngroup,
            sigma2 = sigma2_c,nsimu = nsimu_c)
print("For CR/PR endpoint: ")
calibration(p1=q1[2],p0=q0[2],ngroup0 = ngroup0,nstage = nstage,Ngroup = Ngroup,
            sigma2 = sigma2_c,nsimu = nsimu_c)
