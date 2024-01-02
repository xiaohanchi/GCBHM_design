#TABLE S9

############################################ Settings ################################################
Ngroup <- 4 # cancer groups/types
nsimu <- 5000

############################################# GCBHM_l: 1 interim #################################################
nstage<-2 
ngroup_all<-matrix(c(20,20,20,20,
                     45,45,45,45),nrow = 2,byrow = T)
### find c_eff
load('Rdata/tableS9/1interim_1.Rdata')
trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup_all[nstage,1]))
c_eff<-sapply(1:Ngroup,function(r) quantile(pr_eff[r,]*trial_final[r,],.9))

##calculate power for selected scenarios
power_1<-matrix(NA,nrow = 9,ncol = Ngroup)
avg.n_1<-c()
num=1
for(i in c(1,10,12,37,39,28,30,31,34)){
  load(paste0('Rdata/tableS9/1interim_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==ngroup_all[nstage,1])
  final_eff<-t(sapply(1:Ngroup,function(r) pr_eff[r,]>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  power_1[num,]<-round(c(power*100), digits=1)
  avg.n_1[num]<-round(mean(colSums(n)),1)
  num<-num+1
}



############################################# GCBHM_l: 2 interim #################################################
nstage <- 3
ngroup0 <- c(15,15,15,15)
### find c_eff
load('Rdata/table1/gcbhm_1.Rdata')
trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
c_eff<-sapply(1:Ngroup,function(r) quantile(pr_eff[r,]*trial_final[r,],.9))

##calculate power for selected scenarios
power_2<-matrix(NA,nrow = 9,ncol = Ngroup)
avg.n_2<-c()
num=1
for(i in c(1,10,12,37,39,28,30,31,34)){
  load(paste0('Rdata/table1/gcbhm_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
  final_eff<-t(sapply(1:Ngroup,function(r) pr_eff[r,]>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  power_2[num,]<-round(c(power*100), digits=1)
  avg.n_2[num]<-round(mean(colSums(n)),1)
  num<-num+1
}

############################################# GCBHM_l: 3 interim #################################################
nstage<-4 
ngroup_all<-matrix(c(12,12,12,12,
                     24,24,24,24,
                     36,36,36,36,
                     45,45,45,45),nrow = 4,byrow = T)
### find c_eff
load('Rdata/tableS9/3interim_1.Rdata')
trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup_all[nstage,1]))
c_eff<-sapply(1:Ngroup,function(r) quantile(pr_eff[r,]*trial_final[r,],.9))

##calculate power for selected scenarios
power_3<-matrix(NA,nrow = 9,ncol = Ngroup)
avg.n_3<-c()
num=1
for(i in c(1,10,12,37,39,28,30,31,34)){
  load(paste0('Rdata/tableS9/3interim_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==ngroup_all[nstage,1])
  final_eff<-t(sapply(1:Ngroup,function(r) pr_eff[r,]>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  power_3[num,]<-round(c(power*100), digits=1)
  avg.n_3[num]<-round(mean(colSums(n)),1)
  num<-num+1
}


############################################# Make table #################################################

mk_single_sc<-function(scenario){
  interim<-c("1","2","3")
  pow_mx<-rbind(power_1[scenario,],power_2[scenario,],power_3[scenario,])
  res_mx<-cbind(pow_mx,c(avg.n_1[scenario],avg.n_2[scenario],avg.n_3[scenario]))
  res<-data.frame(Scenario=scenario,interim=interim,res_mx)
  colnames(res)<-c("Scenario","#Interim","sub1","sub2","sub3","sub4","SampleSize")
  return(res)
}

sum_res<-mk_single_sc(scenario = 1)
for(i in 2:9){
  sum_res<-rbind(sum_res,mk_single_sc(scenario = i))
}

write.csv(sum_res,file = "Results/Table_S9.csv")





