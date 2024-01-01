# April 18, 2023
# Code for manuscript: A Generalized Calibrated Bayesian Hierarchical Modeling Approach to Basket Trials with Multiple Endpoints
# Calibrate a and b for the following simulation
# Randomness may exist in resulted a and b

source('../functions.R')
ngroup0<-c(15,15,15,15)
Ngroup<-4 # #cancer groups/types
Ncategory<-4
nstage<-3
p_tilde_upper<-solve.level(rho,0.6,0.2)
p_tilde_lower<-solve.level(rho,0.45,0.3)
p1E<-p_tilde_upper[1]+p_tilde_upper[2]
p0E<-p_tilde_lower[1]+p_tilde_lower[2]
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

