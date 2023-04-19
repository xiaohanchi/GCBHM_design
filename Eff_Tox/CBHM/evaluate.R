# April 18, 2023
# Code for manuscript: A Generalized Calibrated Bayesian Hierarchical Modeling Approach to Basket Trials with Multiple Endpoints
# Code for evaluating the results from CBHM.R

##load data
#setwd("~basket/simulation/eff&tox/n180rho0.3/CBHM/data")
Ngroup<-4 # cancer groups/types
nstage<-3 # trial stages(i.e.,#interim analyses=nstage-1)
### find c_eff
load('var_1.Rdata')
c_eff<-sapply(1:Ngroup,function(r) quantile(pr_eff[r,],.9))
## set parameters
ngroup0<-c(15,15,15,15)
nsimu<-5000

##calculate power
for(i in 1:39){
  load(paste0('var_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
  final_eff<-t(sapply(1:Ngroup,function(r) pr_eff[r,]>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  cat('probability of claiming efficacy by subgroup in scenario',i,':\n')
  cat(formatC(c(power), digits=4, format="f"), sep ="  ", "\n") 
}
