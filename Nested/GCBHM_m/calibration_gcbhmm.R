# October 9, 2023
# Code for manuscript: A Generalized Calibrated Bayesian Hierarchical Modeling Approach to Basket Trials with Bivariate Endpoints
# Calibrate a and b for the following simulation


source('functions.R')
ngroup0<-c(15,15,15,15)
Ngroup<-4 # #cancer groups/types
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
