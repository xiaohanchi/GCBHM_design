#TABLE 1&2

############################################ Settings ################################################
Ngroup <- 4 # cancer groups/types
nstage <- 3 # trial stages(i.e.,#interim analyses=nstage-1)
ngroup0 <- c(15,15,15,15)
nsimu <- 5000
############################################# BHM_IG #################################################
### find c_eff
load('Rdata/bhm_ig_1.Rdata')
trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
c_eff<-sapply(1:Ngroup,function(r) quantile(pr_eff[r,]*trial_final[r,],.9))

##calculate power for selected scenarios
power_ig<-matrix(NA,nrow = 9,ncol = Ngroup)
avg.n_ig<-c()
num=1
for(i in c(1,10,12,37,39,28,30,31,34)){
  load(paste0('Rdata/bhm_ig_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
  final_eff<-t(sapply(1:Ngroup,function(r) pr_eff[r,]>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  power_ig[num,]<-round(c(power*100), digits=1)
  avg.n_ig[num]<-round(mean(colSums(n)),1)
  num<-num+1
}


############################################# BHM_HT #################################################
### find c_eff
load('Rdata/bhm_ht_1.Rdata')
trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
c_eff<-sapply(1:Ngroup,function(r) quantile(pr_eff[r,]*trial_final[r,],.9))

##calculate power for selected scenarios
power_ht<-matrix(NA,nrow = 9,ncol = Ngroup)
avg.n_ht<-c()
num=1
for(i in c(1,10,12,37,39,28,30,31,34)){
  load(paste0('Rdata/bhm_ht_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
  final_eff<-t(sapply(1:Ngroup,function(r) pr_eff[r,]>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  power_ht[num,]<-round(c(power*100), digits=1)
  avg.n_ht[num]<-round(mean(colSums(n)),1)
  num<-num+1
}


