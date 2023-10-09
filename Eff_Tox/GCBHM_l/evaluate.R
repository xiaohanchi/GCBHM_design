# October 9, 2023
# Code for manuscript: A Generalized Calibrated Bayesian Hierarchical Modeling Approach to Basket Trials with Bivariate Endpoints
# Code for evaluating the results from GCBHM_l.R

##load Rdata
Ncons<-nrow(bmatrix) #number of constraints
Ngroup<-4 # cancer groups/types
nstage<-3 # trial stages(i.e.,#interim analyses=nstage-1)
nsimu<-5000
ngroup0<-c(15,15,15,15)
### find c_eff
load('var_1.Rdata')
trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
c_eff<-sapply(1:Ngroup,function(r) quantile(pr_eff[r,]*trial_final[r,],.9))

##calculate power in selected scenarios
for(i in c(1,10,12,37,39,28,30,31,34)){
  #data loaded required: .Rdata from GCBHM_l.R
  load(paste0('var_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
  
  final_eff<-t(sapply(1:Ngroup,function(r) pr_eff[r,]>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  
  cat('probability of claiming efficacy by subgroup in scenario',i,':\n')
  cat(formatC(c(power)*100, digits=1, format="f"), sep ="  ", "\n") 
}
