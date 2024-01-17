### GCBHM_L: #interim=1
setwd("code_for_rerunning_simu/table_S9/")
# run_all<-"
# sc_array=(1 10 12 37 39 28 30 31 34)
# for i in ${sc_array[@]}
# do
#   cp 1gcbhml.R 1gcbhml_$i.R
#   find . -name '1gcbhml_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
#   find . -name '1gcbhml_'$i'.R' -print0 | xargs -0 perl -pi -e 's/1interim_1.Rdata/1interim_'$i'.Rdata/g'
# done"
# system(run_all)
for(i in c(1,10,12,37,39,28,30,31,34)){
  source(paste0("1gcbhml_",i,".R"))
}
#system("rm 1gcbhml_*")
setwd("../../")

### GCBHM_L: #interim=2
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
system("mv code_for_rerunning_simu/table_1_2/Rdata/gcbhm*Rdata code_for_rerunning_simu/table_S9/Rdata/")

### GCBHM_L: #interim=3
setwd("code_for_rerunning_simu/table_S9/")
# run_all<-"
# sc_array=(1 10 12 37 39 28 30 31 34)
# for i in ${sc_array[@]}
# do
#   cp 3gcbhml.R 3gcbhml_$i.R
#   find . -name '3gcbhml_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
#   find . -name '3gcbhml_'$i'.R' -print0 | xargs -0 perl -pi -e 's/3interim_1.Rdata/3interim_'$i'.Rdata/g'
# done"
# system(run_all)
for(i in c(1,10,12,37,39,28,30,31,34)){
  source(paste0("3gcbhml_",i,".R"))
}
#system("rm 3gcbhml_*")
setwd("../../")
