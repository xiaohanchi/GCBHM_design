
### Equal accrual rate
rho=0.3
setwd("code_for_rerunning_simu/table_1_2/")
source("calibration_gcbhm.R") 
# run_all<-"
# sc_array=(1 10 12 37 39 28 30 31 34)
# for i in ${sc_array[@]}
# do
#   cp GCBHM_l.R GCBHM_l_$i.R
#   find . -name 'GCBHM_l_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
#   find . -name 'GCBHM_l_'$i'.R' -print0 | xargs -0 perl -pi -e 's/gcbhm_1.Rdata/gcbhm_'$i'.Rdata/g'
# done"
# system(run_all)
for(i in c(1,10,12,37,39,28,30,31,34)){
  source(paste0("GCBHM_l_",i,".R"))
}
#system("rm GCBHM_l_*")
setwd("../../")
system("mv code_for_rerunning_simu/table_1_2/Rdata/gcbhm*Rdata code_for_rerunning_simu/table_S7/Rdata/")

### Unequal accrual rate
setwd("code_for_rerunning_simu/table_S7/")
source("calibration_uneq.R") 
# run_all<-"
# sc_array=(1 10 12 37 39 28 30 31 34)
# for i in ${sc_array[@]}
# do
#   cp gcbhml_uneq.R gcbhml_uneq_$i.R
#   find . -name 'gcbhml_uneq_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
#   find . -name 'gcbhml_uneq_'$i'.R' -print0 | xargs -0 perl -pi -e 's/unequal_1.Rdata/unequal_'$i'.Rdata/g'
# done"
# system(run_all)
for(i in c(1,10,12,37,39,28,30,31,34)){
  source(paste0("gcbhml_uneq_",i,".R"))
}
#system("rm gcbhml_uneq_*")
setwd("../../")
