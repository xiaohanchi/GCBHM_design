# October 9, 2023
# Code for manuscript: A Generalized Calibrated Bayesian Hierarchical Modeling Approach to Basket Trials with Bivariate Endpoints
# Code for evaluating the results from BHM.R

ep_type<-1
bmatrix<-matrix(data = c(1, 0, 0,0, 1,1, 0,0),
                nrow = 2,ncol = 4,byrow = T)#LxK design matrix b
Ncons<-nrow(bmatrix) #number of constraints
Ngroup<-4 # cancer groups/types
nstage<-3 # trial stages(i.e.,#interim analyses=nstage-1)
nsimu <- 5000
ngroup0 <- c(15,15,15,15)

### find c_eff
load('var_1.Rdata')
trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
c_eff<-sapply(1:Ngroup,function(r) quantile(apply(pr_eff[(Ncons*(r-1)+1):(Ncons*(r-1)+Ncons),],2,max)*trial_final[r,],.9))

##calculate power
for(i in c(1,5,9,12:16)){
  load(paste0('var_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
  
  if(ep_type==1){#and
    final_eff<-t(sapply(1:Ngroup,function(r) apply(pr_eff[(Ncons*(r-1)+1):(Ncons*(r-1)+Ncons),],2,max)>c_eff[r]))
  }else if(ep_type==2){#or
    final_eff<-t(sapply(1:Ngroup,function(r) apply(pr_eff[(Ncons*(r-1)+1):(Ncons*(r-1)+Ncons),],2,min)>c_eff[r]))
  }
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  
  cat('probability of claiming efficacy by subgroup in scenario',i,':\n')
  cat(formatC(c(power), digits=4, format="f"), sep ="  ", "\n") 
}
