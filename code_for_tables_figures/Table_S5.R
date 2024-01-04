#TABLE S5

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
bmatrix<-matrix(data = c(1, 0, 0,0, 1,1, 0,0),
                nrow = 2,ncol = 4,byrow = T)#LxK design matrix b
Ncons<-nrow(bmatrix) #number of constraints
Ngroup<-4 # cancer groups/types
nstage<-3 # trial stages(i.e.,#interim analyses=nstage-1)
ngroup0 <- c(15,15,15,15)
nsimu <- 5000
############################################# BHM #################################################
### find c_eff
load('Rdata/tableS5/bhm_1.Rdata')
c_eff<-sapply(1:Ngroup,function(r) quantile(apply(pr_eff[(Ncons*(r-1)+1):(Ncons*(r-1)+Ncons),],2,max),.9))
c_eff<-round(c_eff,digits = 4)

##calculate power for selected scenarios
power_bhm<-matrix(NA,nrow = 8,ncol = Ngroup)
avg.n_bhm<-c()
num=1
for(i in c(1,5,9,12:16)){
  load(paste0('Rdata/tableS5/bhm_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
  final_eff<-t(sapply(1:Ngroup,function(r) apply(pr_eff[(Ncons*(r-1)+1):(Ncons*(r-1)+Ncons),],2,max)>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  power_bhm[num,]<-round(c(power*100), digits=1)
  avg.n_bhm[num]<-round(mean(colSums(n)),1)
  num<-num+1
}



############################################# GCBHM_L #################################################
### find c_eff
load('Rdata/tableS5/gcbhml_1.Rdata')
trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
c_eff<-sapply(1:Ngroup,function(r) quantile(pr_eff[r,]*trial_final[r,],.9))

##calculate power for selected scenarios
power_gcbhml<-matrix(NA,nrow = 8,ncol = Ngroup)
avg.n_gcbhml<-c()
num=1
for(i in c(1,5,9,12:16)){
  load(paste0('Rdata/tableS5/gcbhml_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
  final_eff<-t(sapply(1:Ngroup,function(r) pr_eff[r,]>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  power_gcbhml[num,]<-round(c(power*100), digits=1)
  avg.n_gcbhml[num]<-round(mean(colSums(n)),1)
  num<-num+1
}


############################################# GCBHM_M #################################################
### find c_eff
load('Rdata/tableS5/gcbhmm_1.Rdata')
trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
c_eff<-sapply(1:Ngroup,function(r) 
  quantile(apply(pr_eff[(Ncons*(r-1)+1):(Ncons*(r-1)+Ncons),],2,max)*trial_final[r,],.9))

##calculate power for selected scenarios
power_gcbhmm<-matrix(NA,nrow = 8,ncol = Ngroup)
avg.n_gcbhmm<-c()
num=1
for(i in c(1,5,9,12:16)){
  load(paste0('Rdata/tableS5/gcbhmm_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
  final_eff<-t(sapply(1:Ngroup,function(r) apply(pr_eff[(Ncons*(r-1)+1):(Ncons*(r-1)+Ncons),],2,max)>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  power_gcbhmm[num,]<-round(c(power*100), digits=1)
  avg.n_gcbhmm[num]<-round(mean(colSums(n)),1)
  num<-num+1
}


############################################# Make table #################################################

mk_single_sc<-function(scenario){
  Method<-c("BHM","GCBHM_L","GCBHM_M")
  pow_mx<-rbind(power_bhm[scenario,],power_gcbhml[scenario,],power_gcbhmm[scenario,])
  res_mx<-cbind(pow_mx,c(avg.n_bhm[scenario],avg.n_gcbhml[scenario],avg.n_gcbhmm[scenario]))
  res<-data.frame(Scenario=scenario,Method=Method,res_mx)
  colnames(res)<-c("Scenario","Method","sub1","sub2","sub3","sub4","SampleSize")
  return(res)
}

sum_res<-mk_single_sc(scenario = 1)
for(i in 2:8){
  sum_res<-rbind(sum_res,mk_single_sc(scenario = i))
}

write.csv(sum_res,file = "Results/Table_S5.csv")





