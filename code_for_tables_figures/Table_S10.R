#TABLE S10

############################################ Settings ################################################
bmatrix<-matrix(data = c(1, 0, 0,0, 1,1, 0,0),
                nrow = 2,ncol = 4,byrow = T) 
Ncons<-nrow(bmatrix)  
Ngroup <- 4 # cancer groups/types
nsimu <- 5000

############################################# GCBHM_l: 1 interim #################################################
nstage<-2 
ngroup_all<-matrix(c(20,20,20,20,
                     45,45,45,45),nrow = 2,byrow = T)
### find c_eff
load('Rdata/tableS10/1gcbhml_1.Rdata')
trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup_all[nstage,1]))
c_eff<-sapply(1:Ngroup,function(r) quantile(pr_eff[r,]*trial_final[r,],.9))

##calculate power for selected scenarios
power_l1<-matrix(NA,nrow = 8,ncol = Ngroup)
avg.n_l1<-c()
num=1
for(i in c(1,5,9,12:16)){
  load(paste0('Rdata/tableS10/1gcbhml_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==ngroup_all[nstage,1])
  final_eff<-t(sapply(1:Ngroup,function(r) pr_eff[r,]>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  power_l1[num,]<-round(c(power*100), digits=1)
  avg.n_l1[num]<-round(mean(colSums(n)),1)
  num<-num+1
}



############################################# GCBHM_l: 2 interim #################################################
nstage <- 3
ngroup0 <- c(15,15,15,15)
### find c_eff
load('Rdata/tableS5/gcbhml_1.Rdata')
trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
c_eff<-sapply(1:Ngroup,function(r) quantile(pr_eff[r,]*trial_final[r,],.9))

##calculate power for selected scenarios
power_l2<-matrix(NA,nrow = 8,ncol = Ngroup)
avg.n_l2<-c()
num=1
for(i in c(1,5,9,12:16)){
  load(paste0('Rdata/tableS5/gcbhml_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
  final_eff<-t(sapply(1:Ngroup,function(r) pr_eff[r,]>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  power_l2[num,]<-round(c(power*100), digits=1)
  avg.n_l2[num]<-round(mean(colSums(n)),1)
  num<-num+1
}

############################################# GCBHM_l: 3 interim #################################################
nstage<-4 
ngroup_all<-matrix(c(12,12,12,12,
                     24,24,24,24,
                     36,36,36,36,
                     45,45,45,45),nrow = 4,byrow = T)
### find c_eff
load('Rdata/tableS10/3gcbhml_1.Rdata')
trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup_all[nstage,1]))
c_eff<-sapply(1:Ngroup,function(r) quantile(pr_eff[r,]*trial_final[r,],.9))

##calculate power for selected scenarios
power_l3<-matrix(NA,nrow = 8,ncol = Ngroup)
avg.n_l3<-c()
num=1
for(i in c(1,5,9,12:16)){
  load(paste0('Rdata/tableS10/3gcbhml_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==ngroup_all[nstage,1])
  final_eff<-t(sapply(1:Ngroup,function(r) pr_eff[r,]>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  power_l3[num,]<-round(c(power*100), digits=1)
  avg.n_l3[num]<-round(mean(colSums(n)),1)
  num<-num+1
}


############################################# GCBHM_m: 1 interim #################################################
### find c_eff
load('Rdata/tableS10/1gcbhmm_1.Rdata')
trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup_all[nstage,1]))
c_eff<-sapply(1:Ngroup,function(r) 
  quantile(apply(pr_eff[(Ncons*(r-1)+1):(Ncons*(r-1)+Ncons),],2,max)*trial_final[r,],.9))

##calculate power for selected scenarios
power_m1<-matrix(NA,nrow = 8,ncol = Ngroup)
avg.n_m1<-c()
num=1
for(i in c(1,5,9,12:16)){
  load(paste0('Rdata/tableS10/1gcbhmm_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==ngroup_all[nstage,1])
  final_eff<-t(sapply(1:Ngroup,function(r) 
    apply(pr_eff[(Ncons*(r-1)+1):(Ncons*(r-1)+Ncons),],2,max)>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  power_m1[num,]<-round(c(power*100), digits=1)
  avg.n_m1[num]<-round(mean(colSums(n)),1)
  num<-num+1
}



############################################# GCBHM_m: 2 interim #################################################
nstage <- 3
ngroup0 <- c(15,15,15,15)
### find c_eff
load('Rdata/tableS5/gcbhmm_1.Rdata')
trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
c_eff<-sapply(1:Ngroup,function(r) 
  quantile(apply(pr_eff[(Ncons*(r-1)+1):(Ncons*(r-1)+Ncons),],2,max)*trial_final[r,],.9))

##calculate power for selected scenarios
power_m2<-matrix(NA,nrow = 8,ncol = Ngroup)
avg.n_m2<-c()
num=1
for(i in c(1,5,9,12:16)){
  load(paste0('Rdata/tableS5/gcbhmm_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup0[1]*nstage))
  final_eff<-t(sapply(1:Ngroup,function(r) 
    apply(pr_eff[(Ncons*(r-1)+1):(Ncons*(r-1)+Ncons),],2,max)>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  power_m2[num,]<-round(c(power*100), digits=1)
  avg.n_m2[num]<-round(mean(colSums(n)),1)
  num<-num+1
}
############################################# GCBHM_m: 3 interim #################################################
nstage<-4 
ngroup_all<-matrix(c(12,12,12,12,
                     24,24,24,24,
                     36,36,36,36,
                     45,45,45,45),nrow = 4,byrow = T)
### find c_eff
load('Rdata/tableS10/3gcbhmm_1.Rdata')
trial_final<-matrix(1, Ngroup,nsimu)*(n==(ngroup_all[nstage,1]))
c_eff<-sapply(1:Ngroup,function(r) 
  quantile(apply(pr_eff[(Ncons*(r-1)+1):(Ncons*(r-1)+Ncons),],2,max)*trial_final[r,],.9))

##calculate power for selected scenarios
power_m3<-matrix(NA,nrow = 8,ncol = Ngroup)
avg.n_m3<-c()
num=1
for(i in c(1,5,9,12:16)){
  load(paste0('Rdata/tableS10/3gcbhmm_',i,'.Rdata'))
  trial_final<-matrix(1, Ngroup,nsimu)*(n==ngroup_all[nstage,1])
  final_eff<-t(sapply(1:Ngroup,function(r) 
    apply(pr_eff[(Ncons*(r-1)+1):(Ncons*(r-1)+Ncons),],2,max)>c_eff[r]))
  final_eff<-final_eff*1*trial_final
  power<-rowMeans(final_eff)
  power_m3[num,]<-round(c(power*100), digits=1)
  avg.n_m3[num]<-round(mean(colSums(n)),1)
  num<-num+1
}



############################################# Make table #################################################

mk_single_sc<-function(scenario){
  method<-c("GCBHM_L","","","GCBHM_M","","")
  interim<-c("1","2","3","1","2","3")
  pow_mx<-rbind(power_l1[scenario,],power_l2[scenario,],power_l3[scenario,],
                power_m1[scenario,],power_m2[scenario,],power_m3[scenario,])
  res_mx<-cbind(pow_mx,c(avg.n_l1[scenario],avg.n_l2[scenario],avg.n_l3[scenario],
                         avg.n_m1[scenario],avg.n_m2[scenario],avg.n_m3[scenario]))
  res<-data.frame(Scenario=scenario,method=method,interim=interim,res_mx)
  colnames(res)<-c("Scenario","Method","#Interim","sub1","sub2","sub3","sub4","SampleSize")
  return(res)
}

sum_res<-mk_single_sc(scenario = 1)
for(i in 2:8){
  sum_res<-rbind(sum_res,mk_single_sc(scenario = i))
}

write.csv(sum_res,file = "Results/Table_S10.csv")





