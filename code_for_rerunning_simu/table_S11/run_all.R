### GCBHM_L: n=36
setwd("code_for_rerunning_simu/table_S11/")
# run_all<-"
# sc_array=(1 10 12 37 39 28 30 31 34)
# for i in ${sc_array[@]}
# do
#   cp gcbhml_n36.R gcbhml_n36_$i.R
#   find . -name 'gcbhml_n36_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
#   find . -name 'gcbhml_n36_'$i'.R' -print0 | xargs -0 perl -pi -e 's/gcbhml_n36_1.Rdata/gcbhml_n36_'$i'.Rdata/g'
# done"
# system(run_all)
for(i in c(1,10,12,37,39,28,30,31,34)){
  source(paste0("gcbhml_n36_",i,".R"))
}
#system("rm gcbhml_n36_*")
setwd("../../")

### GCBHM_L: n=45
rho=0.3
setwd("code_for_rerunning_simu/table_1_2/")
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
system("mv code_for_rerunning_simu/table_1_2/Rdata/gcbhm*Rdata code_for_rerunning_simu/table_S11/Rdata/")

### GCBHM_L: n=54
setwd("code_for_rerunning_simu/table_S11/")
# run_all<-"
# sc_array=(1 10 12 37 39 28 30 31 34)
# for i in ${sc_array[@]}
# do
#   cp gcbhml_n54.R gcbhml_n54_$i.R
#   find . -name 'gcbhml_n54_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
#   find . -name 'gcbhml_n54_'$i'.R' -print0 | xargs -0 perl -pi -e 's/gcbhml_n54_1.Rdata/gcbhml_n54_'$i'.Rdata/g'
# done"
# system(run_all)
for(i in c(1,10,12,37,39,28,30,31,34)){
  source(paste0("gcbhml_n54_",i,".R"))
}
#system("rm gcbhml_n54_*")
setwd("../../")
