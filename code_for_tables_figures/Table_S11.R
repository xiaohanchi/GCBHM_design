#TABLE S11

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
nsimu <- 5000
nstage <- 3
############################################# GCBHM_l: n=36 #################################################
ngroup0<-c(12,12,12,12)
### find c_eff
load('Rdata/tableS11/gcbhml_n36_1.Rdata')
trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
c_eff<-sapply(1:Ngroup,function(r) quantile(pr_eff[r,]*trial_final[r,],.9))

##calculate power for selected scenarios
power_n36<-matrix(NA,nrow = 9,ncol = Ngroup)
avg.n_n36<-c()
num=1
for(i in c(1,10,12,37,39,28,30,31,34)){
  load(paste0('Rdata/tableS11/gcbhml_n36_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
  final_eff<-t(sapply(1:Ngroup,function(r) pr_eff[r,]>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  power_n36[num,]<-round(c(power*100), digits=1)
  avg.n_n36[num]<-round(mean(colSums(n)),1)
  num<-num+1
}



############################################# GCBHM_l: n=45 #################################################
ngroup0 <- c(15,15,15,15)
### find c_eff
load('Rdata/table1/gcbhm_1.Rdata')
trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
c_eff<-sapply(1:Ngroup,function(r) quantile(pr_eff[r,]*trial_final[r,],.9))

##calculate power for selected scenarios
power_n45<-matrix(NA,nrow = 9,ncol = Ngroup)
avg.n_n45<-c()
num=1
for(i in c(1,10,12,37,39,28,30,31,34)){
  load(paste0('Rdata/table1/gcbhm_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
  final_eff<-t(sapply(1:Ngroup,function(r) pr_eff[r,]>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  power_n45[num,]<-round(c(power*100), digits=1)
  avg.n_n45[num]<-round(mean(colSums(n)),1)
  num<-num+1
}

############################################# GCBHM_l: n=54 #################################################
ngroup0<-c(18,18,18,18)
### find c_eff
load('Rdata/tableS11/gcbhml_n54_1.Rdata')
trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
c_eff<-sapply(1:Ngroup,function(r) quantile(pr_eff[r,]*trial_final[r,],.9))

##calculate power for selected scenarios
power_n54<-matrix(NA,nrow = 9,ncol = Ngroup)
avg.n_n54<-c()
num=1
for(i in c(1,10,12,37,39,28,30,31,34)){
  load(paste0('Rdata/tableS11/gcbhml_n54_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
  final_eff<-t(sapply(1:Ngroup,function(r) pr_eff[r,]>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  power_n54[num,]<-round(c(power*100), digits=1)
  avg.n_n54[num]<-round(mean(colSums(n)),1)
  num<-num+1
}


############################################# Make table #################################################

mk_single_sc<-function(scenario){
  method<-c("GCBHM_L","","")
  nj<-c("36","45","54")
  pow_mx<-rbind(power_n36[scenario,],power_n45[scenario,],power_n54[scenario,])
  res_mx<-cbind(pow_mx,c(avg.n_n36[scenario],avg.n_n45[scenario],avg.n_n54[scenario]))
  res<-data.frame(Scenario=scenario,method=method,nj=nj,res_mx)
  colnames(res)<-c("Scenario","Method","nj","sub1","sub2","sub3","sub4","SampleSize")
  return(res)
}

sum_res<-mk_single_sc(scenario = 1)
for(i in 2:9){
  sum_res<-rbind(sum_res,mk_single_sc(scenario = i))
}

write.csv(sum_res,file = "Results/Table_S11.csv")





