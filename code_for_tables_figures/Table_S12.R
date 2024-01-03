#TABLE S12

############################################ Settings ################################################
Ngroup <- 4 # cancer groups/types
nsimu <- 5000
nstage <- 3
############################################# GCBHM_l: n=36 #################################################
ngroup0<-c(12,12,12,12)
### find c_eff
load('Rdata/tableS12/gcbhml_n36_1.Rdata')
trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
c_eff<-sapply(1:Ngroup,function(r) quantile(pr_eff[r,]*trial_final[r,],.9))

##calculate power for selected scenarios
power_n36l<-matrix(NA,nrow = 8,ncol = Ngroup)
avg.n_n36l<-c()
num=1
for(i in c(1,5,9,12:16)){
  load(paste0('Rdata/tableS12/gcbhml_n36_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
  final_eff<-t(sapply(1:Ngroup,function(r) pr_eff[r,]>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  power_n36l[num,]<-round(c(power*100), digits=1)
  avg.n_n36l[num]<-round(mean(colSums(n)),1)
  num<-num+1
}



############################################# GCBHM_l: n=45 #################################################
ngroup0 <- c(15,15,15,15)
### find c_eff
load('Rdata/tableS5/gcbhml_1.Rdata')
trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
c_eff<-sapply(1:Ngroup,function(r) quantile(pr_eff[r,]*trial_final[r,],.9))

##calculate power for selected scenarios
power_n45l<-matrix(NA,nrow = 8,ncol = Ngroup)
avg.n_n45l<-c()
num=1
for(i in c(1,5,9,12:16)){
  load(paste0('Rdata/tableS5/gcbhml_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
  final_eff<-t(sapply(1:Ngroup,function(r) pr_eff[r,]>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  power_n45l[num,]<-round(c(power*100), digits=1)
  avg.n_n45l[num]<-round(mean(colSums(n)),1)
  num<-num+1
}


############################################# GCBHM_l: n=54 #################################################
ngroup0<-c(18,18,18,18)
### find c_eff
load('Rdata/tableS12/gcbhml_n54_1.Rdata')
trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
c_eff<-sapply(1:Ngroup,function(r) quantile(pr_eff[r,]*trial_final[r,],.9))

##calculate power for selected scenarios
power_n54l<-matrix(NA,nrow = 8,ncol = Ngroup)
avg.n_n54l<-c()
num=1
for(i in c(1,5,9,12:16)){
  load(paste0('Rdata/tableS12/gcbhml_n54_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
  final_eff<-t(sapply(1:Ngroup,function(r) pr_eff[r,]>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  power_n54l[num,]<-round(c(power*100), digits=1)
  avg.n_n54l[num]<-round(mean(colSums(n)),1)
  num<-num+1
}

############################################# GCBHM_m: n=36 #################################################
ngroup0<-c(12,12,12,12)
### find c_eff
load('Rdata/tableS12/gcbhmm_n36_1.Rdata')
trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
c_eff<-sapply(1:Ngroup,function(r) 
  quantile(apply(pr_eff[(Ncons*(r-1)+1):(Ncons*(r-1)+Ncons),],2,max)*trial_final[r,],.9))

##calculate power for selected scenarios
power_n36m<-matrix(NA,nrow = 8,ncol = Ngroup)
avg.n_n36m<-c()
num=1
for(i in c(1,5,9,12:16)){
  load(paste0('Rdata/tableS12/gcbhmm_n36_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
  final_eff<-t(sapply(1:Ngroup,function(r) apply(pr_eff[(Ncons*(r-1)+1):(Ncons*(r-1)+Ncons),],2,max)>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  power_n36m[num,]<-round(c(power*100), digits=1)
  avg.n_n36m[num]<-round(mean(colSums(n)),1)
  num<-num+1
}



############################################# GCBHM_m: n=45 #################################################
ngroup0 <- c(15,15,15,15)
### find c_eff
load('Rdata/tableS5/gcbhmm_1.Rdata')
trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
c_eff<-sapply(1:Ngroup,function(r) 
  quantile(apply(pr_eff[(Ncons*(r-1)+1):(Ncons*(r-1)+Ncons),],2,max)*trial_final[r,],.9))

##calculate power for selected scenarios
power_n45m<-matrix(NA,nrow = 8,ncol = Ngroup)
avg.n_n45m<-c()
num=1
for(i in c(1,5,9,12:16)){
  load(paste0('Rdata/tableS5/gcbhmm_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
  final_eff<-t(sapply(1:Ngroup,function(r) 
    apply(pr_eff[(Ncons*(r-1)+1):(Ncons*(r-1)+Ncons),],2,max)>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  power_n45m[num,]<-round(c(power*100), digits=1)
  avg.n_n45m[num]<-round(mean(colSums(n)),1)
  num<-num+1
}

############################################# GCBHM_m: n=54 #################################################
ngroup0<-c(18,18,18,18)
### find c_eff
load('Rdata/tableS12/gcbhmm_n54_1.Rdata')
trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
c_eff<-sapply(1:Ngroup,function(r) 
  quantile(apply(pr_eff[(Ncons*(r-1)+1):(Ncons*(r-1)+Ncons),],2,max)*trial_final[r,],.9))

##calculate power for selected scenarios
power_n54m<-matrix(NA,nrow = 8,ncol = Ngroup)
avg.n_n54m<-c()
num=1
for(i in c(1,5,9,12:16)){
  load(paste0('Rdata/tableS12/gcbhmm_n54_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
  final_eff<-t(sapply(1:Ngroup,function(r) 
    apply(pr_eff[(Ncons*(r-1)+1):(Ncons*(r-1)+Ncons),],2,max)>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  power_n54m[num,]<-round(c(power*100), digits=1)
  avg.n_n54m[num]<-round(mean(colSums(n)),1)
  num<-num+1
}




############################################# Make table #################################################

mk_single_sc<-function(scenario){
  method<-c("GCBHM_L","","","GCBHM_M","","")
  nj<-c("36","45","54","36","45","54")
  pow_mx<-rbind(power_n36l[scenario,],power_n45l[scenario,],power_n54l[scenario,],
                power_n36m[scenario,],power_n45m[scenario,],power_n54m[scenario,])
  res_mx<-cbind(pow_mx,c(avg.n_n36l[scenario],avg.n_n45l[scenario],avg.n_n54l[scenario],
                         avg.n_n36m[scenario],avg.n_n45m[scenario],avg.n_n54m[scenario]))
  res<-data.frame(Scenario=scenario,method=method,nj=nj,res_mx)
  colnames(res)<-c("Scenario","Method","nj","sub1","sub2","sub3","sub4","SampleSize")
  return(res)
}

sum_res<-mk_single_sc(scenario = 1)
for(i in 2:8){
  sum_res<-rbind(sum_res,mk_single_sc(scenario = i))
}

write.csv(sum_res,file = "Results/Table_S12.csv")





