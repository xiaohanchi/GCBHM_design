
mk_single_sc<-function(scenario){
  Method<-c("BHM_IG","","CBHM_E","CBHM_ET","GCBHM_L","")
  sigma<-rbind(sigma_bhm_ep1,sigma_cbhm_ep1,sigma_cbhmtox_ep1,sigma_gcbhml_ep1)
  res<-data.frame(Scenario=scenario,Method=Method,sigma)
  colnames(res)<-c("Scenario","Method","interim1","interim2","interim3")
  rownames(res)<-c()
  return(res)
}
mk_single_sc2<-function(scenario){
  Method<-c("BHM","GCBHM_L","","GCBHM_M")
  sigma<-rbind(sigma_bhm_ep2,sigma_gcbhml_ep2,sigma_gcbhmm_ep2)
  res<-data.frame(Scenario=scenario,Method=Method,sigma)
  colnames(res)<-c("Scenario","Method","interim1","interim2","interim3")
  rownames(res)<-c()
  return(res)
}


setwd("code_for_rerunning_simu/table_S1/eff_tox/")

run_all<-"
sc_array=(1 37 39 30)
for i in ${sc_array[@]}
do
  cp BHM_IG.R BHM_IG_$i.R
  find . -name 'BHM_IG_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
done"
system(run_all)

run_all<-"
sc_array=(1 37 39 30)
for i in ${sc_array[@]}
do
  cp CBHM.R CBHM_$i.R
  find . -name 'CBHM_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
done"
system(run_all)

run_all<-"
sc_array=(1 37 39 30)
for i in ${sc_array[@]}
do
  cp CBHMTox.R CBHMTox_$i.R
  find . -name 'CBHMTox_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
done"
system(run_all)

run_all<-"
sc_array=(1 37 39 30)
for i in ${sc_array[@]}
do
  cp GCBHM_l.R GCBHM_l_$i.R
  find . -name 'GCBHM_l_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
done"
system(run_all)

rho=0.3
scenario=1
source(paste0("BHM_IG_1.R"))
source(paste0("CBHM_1.R"))
source(paste0("CBHMTox_1.R"))
source(paste0("GCBHM_l_1.R"))
sum_res_ep1<-mk_single_sc(scenario = 1)


for(sc in c(37,39,30)){
  if(sc==37){scenario=4}
  else if(sc==39){scenario=5}
  else if(sc==30){scenario=7}
  
  source(paste0("BHM_IG_",sc,".R"))
  source(paste0("CBHM_",sc,".R"))
  source(paste0("CBHMTox_",sc,".R"))
  source(paste0("GCBHM_l_",sc,".R"))
  sum_res_ep1<-rbind(sum_res_ep1,mk_single_sc(scenario = scenario))
}

system("rm BHM_IG_*")
system("rm CBHM_*")
system("rm CBHMTox_*")
system("rm GCBHM_l_*")


setwd("../nested/")
run_all<-"
sc_array=(1 12 15)
for i in ${sc_array[@]}
do
  cp BHM.R BHM_$i.R
  find . -name 'BHM_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
done"
system(run_all)

run_all<-"
sc_array=(1 12 15)
for i in ${sc_array[@]}
do
  cp GCBHM_l.R GCBHM_l_$i.R
  find . -name 'GCBHM_l_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
done"
system(run_all)

run_all<-"
sc_array=(1 12 15)
for i in ${sc_array[@]}
do
  cp GCBHM_m.R GCBHM_m_$i.R
  find . -name 'GCBHM_m_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
done"
system(run_all)

scenario=1
source(paste0("BHM_1.R"))
source(paste0("GCBHM_l_1.R"))
source(paste0("GCBHM_m_1.R"))
sum_res_ep2<-mk_single_sc2(scenario = 1)


for(sc in c(12,15)){
  if(sc==12){scenario=4}
  else if(sc==15){scenario=7}
  
  source(paste0("BHM_",sc,".R"))
  source(paste0("GCBHM_l_",sc,".R"))
  source(paste0("GCBHM_m_",sc,".R"))
  sum_res_ep2<-rbind(sum_res_ep2,mk_single_sc2(scenario = scenario))
}

sum_res<-rbind(sum_res_ep1,sum_res_ep2)

system("rm BHM_*")
system("rm GCBHM_l_*")
system("rm GCBHM_m_*")

setwd("../../../")

write.csv(sum_res,file = "Results/Table_S1.csv")



