#TABLE S7

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
############################################# GCBHM_l: Equal #################################################
nstage <- 3
ngroup0 <- c(15,15,15,15)
### find c_eff
load('Rdata/table1/gcbhm_1.Rdata')
trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
c_eff<-sapply(1:Ngroup,function(r) quantile(pr_eff[r,]*trial_final[r,],.9))

##calculate power for selected scenarios
power_eq<-matrix(NA,nrow = 9,ncol = Ngroup)
avg.n_eq<-c()
num=1
for(i in c(1,10,12,37,39,28,30,31,34)){
  load(paste0('Rdata/table1/gcbhm_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
  final_eff<-t(sapply(1:Ngroup,function(r) pr_eff[r,]>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  power_eq[num,]<-round(c(power*100), digits=1)
  avg.n_eq[num]<-round(mean(colSums(n)),1)
  num<-num+1
}

############################################# GCBHM_l: Unequal #################################################
nstage<-4
ngroup_all<-matrix(c(12,15,15,25,
                     24,30,30,45,
                     36,45,45,45,
                     45,45,45,45),nrow = 4,byrow = T)
### find c_eff
load('Rdata/tableS7/unequal_1.Rdata')
trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup_all[nstage,1]))
c_eff<-sapply(1:Ngroup,function(r) quantile(pr_eff[r,]*trial_final[r,],.9))

##calculate power for selected scenarios
power_uneq<-matrix(NA,nrow = 9,ncol = Ngroup)
avg.n_uneq<-c()
num=1
for(i in c(1,10,12,37,39,28,30,31,34)){
  load(paste0('Rdata/tableS7/unequal_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==ngroup_all[nstage,1])
  final_eff<-t(sapply(1:Ngroup,function(r) pr_eff[r,]>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  power_uneq[num,]<-round(c(power*100), digits=1)
  avg.n_uneq[num]<-round(mean(colSums(n)),1)
  num<-num+1
}


############################################# Make table #################################################

mk_single_sc<-function(scenario){
  AR<-c("Equal","Unequal")
  pow_mx<-rbind(power_eq[scenario,],power_uneq[scenario,])
  res_mx<-cbind(pow_mx,c(avg.n_eq[scenario],avg.n_uneq[scenario]))
  res<-data.frame(Scenario=scenario,AR=AR,res_mx)
  colnames(res)<-c("Scenario","AccrualRate","sub1","sub2","sub3","sub4","SampleSize")
  return(res)
}

sum_res<-mk_single_sc(scenario = 1)
for(i in 2:9){
  sum_res<-rbind(sum_res,mk_single_sc(scenario = i))
}

write.csv(sum_res,file = "Results/Table_S7.csv")





