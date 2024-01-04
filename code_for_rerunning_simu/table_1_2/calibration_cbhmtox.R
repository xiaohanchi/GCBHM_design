# April 18, 2023
# Code for manuscript: A Generalized Calibrated Bayesian Hierarchical Modeling Approach to Basket Trials with Multiple Endpoints
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
#   [1] runjags_2.2.0-3 R2jags_0.7-1    rjags_4-14      coda_0.19-4    
# 
# loaded via a namespace (and not attached):
#   [1] compiler_4.1.2    tools_4.1.2       abind_1.4-5       rstudioapi_0.15.0 grid_4.1.2        boot_1.3-28.1     lattice_0.20-45  
# [8] R2WinBUGS_2.1-21 


source('functions.R')
ngroup0<-c(15,15,15,15)
Ngroup<-4 # #cancer groups/types
nstage<-3
p_tilde_upper<-solve.level(rho,0.6,0.2)
p_tilde_lower<-solve.level(rho,0.45,0.3)

p1E<-p_tilde_upper[1]+p_tilde_upper[2]
p0E<-p_tilde_lower[1]+p_tilde_lower[2]
p1T<-p_tilde_upper[1]+p_tilde_upper[3]
p0T<-p_tilde_lower[1]+p_tilde_lower[3]

nsimu_c<-10000
sigma2_c<-c(1,100)#small sigma2 and big sigma2

##===========================================================================##
#define calibration function
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
  a<-log(sigma2_c[1])-b*log(Hb)
  #print results
  print("calibrated value of (a, b):")
  cat(formatC(c(a,b), digits=2, format="f"), sep ="  ", "\n") 
  return(round(c(a,b),digits = 2))
}
##===========================================================================##

#Output a and b
res<-calibration(p1=p1E,p0=p0E,ngroup0 = ngroup0,nstage = nstage,Ngroup = Ngroup,
            sigma2 = sigma2_c,nsimu = nsimu_c)

