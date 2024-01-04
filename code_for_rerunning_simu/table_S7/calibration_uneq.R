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
ngroup0<-c(20,20,20,20)
ngroup_all<-matrix(c(20,20,20,20,
                     45,45,45,45),nrow = 2,byrow = T)
ninterim<-c(20,25)#interim sample size

Ngroup<-4 # #cancer groups/types
Ncategory<-4
nstage<-nrow(ngroup_all)
rho=0.3
p_tilde_upper<-solve.level(rho,0.6,0.2)
p_tilde_lower<-solve.level(rho,0.45,0.3)

p1E<-p_tilde_upper[1]+p_tilde_upper[2]
p0E<-p_tilde_lower[1]+p_tilde_lower[2]
p1T<-p_tilde_upper[1]+p_tilde_upper[3]
p0T<-p_tilde_lower[1]+p_tilde_lower[3]

nsimu_c<-10000
sigma2_c<-c(1,100)#small sigma2 and big sigma2

calibration<-function(p1,p0,ngroup_all,nstage,Ngroup,sigma2,nsimu){
  #step1
  Hb<-medianT_fun(n=ngroup_all[nstage,],p=rep(p1,Ngroup))
  #step2
  Hb_hetero<-numeric(Ngroup-1)
  for(j in 1:(Ngroup-1)){
    p2<-c(rep(p1,j),rep(p0,(Ngroup-j)))
    Hb_hetero[j]<-medianT_fun(n=ngroup_all[nstage,],p=p2)
  }
  Hb_hetero_min<-min(Hb_hetero)
  #step3
  b<-(log(sigma2_c[1])-log(sigma2_c[2]))/(log(Hb)-log(Hb_hetero_min))
  a<-log(sigma2_c[1])-b*log(Hb)
  #print results
  print("calibrated value of (a, b):")
  cat(formatC(c(a,b), digits=2, format="f"), sep ="  ", "\n") 
}

print("For efficacy endpoint: ")
calibration(p1=p1E,p0=p0E,ngroup_all = ngroup_all,nstage = nstage,Ngroup = Ngroup,
            sigma2 = sigma2_c,nsimu = nsimu_c)
print("For toxicity endpoint: ")
calibration(p1=p1T,p0=p0T,ngroup_all = ngroup_all,nstage = nstage,Ngroup = Ngroup,
            sigma2 = sigma2_c,nsimu = nsimu_c)

