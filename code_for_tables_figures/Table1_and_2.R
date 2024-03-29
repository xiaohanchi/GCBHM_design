#TABLE 1&2

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
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# loaded via a namespace (and not attached):
#   [1] compiler_4.1.2    tools_4.1.2       rstudioapi_0.15.0

############################################ Settings ################################################
Ngroup <- 4 # cancer groups/types
nstage <- 3 # trial stages(i.e.,#interim analyses=nstage-1)
ngroup0 <- c(15,15,15,15)
nsimu <- 5000
############################################# BHM_IG #################################################
### find c_eff
load('Rdata/table1/bhm_ig_1.Rdata')
trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
c_eff<-sapply(1:Ngroup,function(r) quantile(pr_eff[r,]*trial_final[r,],.9))

##calculate power for selected scenarios
power_ig<-matrix(NA,nrow = 9,ncol = Ngroup)
avg.n_ig<-c()
num=1
for(i in c(1,10,12,37,39,28,30,31,34)){
  load(paste0('Rdata/table1/bhm_ig_',i,'.Rdata'))
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
load('Rdata/table1/bhm_ht_1.Rdata')
trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
c_eff<-sapply(1:Ngroup,function(r) quantile(pr_eff[r,]*trial_final[r,],.9))

##calculate power for selected scenarios
power_ht<-matrix(NA,nrow = 9,ncol = Ngroup)
avg.n_ht<-c()
num=1
for(i in c(1,10,12,37,39,28,30,31,34)){
  load(paste0('Rdata/table1/bhm_ht_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
  final_eff<-t(sapply(1:Ngroup,function(r) pr_eff[r,]>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  power_ht[num,]<-round(c(power*100), digits=1)
  avg.n_ht[num]<-round(mean(colSums(n)),1)
  num<-num+1
}



############################################# CBHM #################################################
### find c_eff
load('Rdata/table1/cbhm_1.Rdata')
c_eff<-sapply(1:Ngroup,function(r) quantile(pr_eff[r,],.9))

##calculate power for selected scenarios
power_cbhm<-matrix(NA,nrow = 9,ncol = Ngroup)
avg.n_cbhm<-c()
num=1
for(i in c(1,10,12,37,39,28,30,31,34)){
  load(paste0('Rdata/table1/cbhm_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
  final_eff<-t(sapply(1:Ngroup,function(r) pr_eff[r,]>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  power_cbhm[num,]<-round(c(power*100), digits=1)
  avg.n_cbhm[num]<-round(mean(colSums(n)),1)
  num<-num+1
}


############################################# CBHM_Tox #################################################
### find c_eff
load('Rdata/table1/cbhmtox_1.Rdata')
trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
c_eff<-sapply(1:Ngroup,function(r) quantile(pr_eff[r,]*trial_final[r,],.9))

##calculate power for selected scenarios
power_cbhmt<-matrix(NA,nrow = 9,ncol = Ngroup)
avg.n_cbhmt<-c()
num=1
for(i in c(1,10,12,37,39,28,30,31,34)){
  load(paste0('Rdata/table1/cbhmtox_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
  final_eff<-t(sapply(1:Ngroup,function(r) pr_eff[r,]>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  power_cbhmt[num,]<-round(c(power*100), digits=1)
  avg.n_cbhmt[num]<-round(mean(colSums(n)),1)
  num<-num+1
}

############################################# GCBHM_l #################################################
### find c_eff
load('Rdata/table1/gcbhm_1.Rdata')
trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
c_eff<-sapply(1:Ngroup,function(r) quantile(pr_eff[r,]*trial_final[r,],.9))

##calculate power for selected scenarios
power_gcbhm<-matrix(NA,nrow = 9,ncol = Ngroup)
avg.n_gcbhm<-c()
num=1
for(i in c(1,10,12,37,39,28,30,31,34)){
  load(paste0('Rdata/table1/gcbhm_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
  final_eff<-t(sapply(1:Ngroup,function(r) pr_eff[r,]>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  power_gcbhm[num,]<-round(c(power*100), digits=1)
  avg.n_gcbhm[num]<-round(mean(colSums(n)),1)
  num<-num+1
}

############################################# Make table #################################################

mk_single_sc<-function(scenario){
  Method<-c("BHM_IG","BHM_HT","CBHM_E","CBHM_ET","GCBHM_L")
  pow_mx<-rbind(power_ig[scenario,],power_ht[scenario,],
                power_cbhm[scenario,],power_cbhmt[scenario,],power_gcbhm[scenario,])
  res_mx<-cbind(pow_mx,c(avg.n_ig[scenario],avg.n_ht[scenario],
                         avg.n_cbhm[scenario],avg.n_cbhmt[scenario],avg.n_gcbhm[scenario]))
  res<-data.frame(Scenario=scenario,Method=Method,res_mx)
  colnames(res)<-c("Scenario","Method","sub1","sub2","sub3","sub4","SampleSize")
  return(res)
}

sum_res<-mk_single_sc(scenario = 1)
for(i in 2:9){
  sum_res<-rbind(sum_res,mk_single_sc(scenario = i))
}

write.csv(sum_res,file = "Results/Table1_2.csv")





