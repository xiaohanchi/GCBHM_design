
### GCBHM_L: Equal accrual rate
setwd("code_for_rerunning_simu/table_S5/")
source("calibration_gcbhml.R") 
run_all<-"
sc_array=(1 5 9 12 13 14 15 16)
for i in ${sc_array[@]}
do
  cp GCBHM_l.R GCBHM_l_$i.R
  find . -name 'GCBHM_l_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
  find . -name 'GCBHM_l_'$i'.R' -print0 | xargs -0 perl -pi -e 's/gcbhml_1.Rdata/gcbhml_'$i'.Rdata/g'
done"
system(run_all)
for(i in c(1,5,9,12:16)){
  source(paste0("GCBHM_l_",i,".R"))
}
system("rm GCBHM_l_*")
setwd("../../")
system("cp code_for_rerunning_simu/table_S5/Rdata/gcbhml*Rdata code_for_rerunning_simu/table_S8/Rdata/")

### GCBHM_L: Unequal accrual rate
setwd("code_for_rerunning_simu/table_S8/")
run_all<-"
sc_array=(1 5 9 12 13 14 15 16)
for i in ${sc_array[@]}
do
  cp gcbhml_uneq.R gcbhml_uneq_$i.R
  find . -name 'gcbhml_uneq_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
  find . -name 'gcbhml_uneq_'$i'.R' -print0 | xargs -0 perl -pi -e 's/uneq_gcbhml_1.Rdata/uneq_gcbhml_'$i'.Rdata/g'
done"
system(run_all)
for(i in c(1,5,9,12:16)){
  source(paste0("gcbhml_uneq_",i,".R"))
}
system("rm gcbhml_uneq_*")
setwd("../../")

### GCBHM_M: Equal accrual rate
setwd("code_for_rerunning_simu/table_S5/")
source("calibration_gcbhmm.R") 
run_all<-"
sc_array=(1 5 9 12 13 14 15 16)
for i in ${sc_array[@]}
do
  cp GCBHM_m.R GCBHM_m_$i.R
  find . -name 'GCBHM_m_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
  find . -name 'GCBHM_m_'$i'.R' -print0 | xargs -0 perl -pi -e 's/gcbhmm_1.Rdata/gcbhmm_'$i'.Rdata/g'
done"
system(run_all)
for(i in c(1,5,9,12:16)){
  source(paste0("GCBHM_m_",i,".R"))
}
system("rm GCBHM_m_*")
setwd("../../")
system("cp code_for_rerunning_simu/table_S5/Rdata/gcbhmm*Rdata code_for_rerunning_simu/table_S8/Rdata/")

### GCBHM_M: Unequal accrual rate
setwd("code_for_rerunning_simu/table_S8/")
run_all<-"
sc_array=(1 5 9 12 13 14 15 16)
for i in ${sc_array[@]}
do
  cp gcbhmm_uneq.R gcbhmm_uneq_$i.R
  find . -name 'gcbhmm_uneq_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
  find . -name 'gcbhmm_uneq_'$i'.R' -print0 | xargs -0 perl -pi -e 's/uneq_gcbhmm_1.Rdata/uneq_gcbhmm_'$i'.Rdata/g'
done"
system(run_all)
for(i in c(1,5,9,12:16)){
  source(paste0("gcbhmm_uneq_",i,".R"))
}
system("rm gcbhmm_uneq_*")
setwd("../../")





