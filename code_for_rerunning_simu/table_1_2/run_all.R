
### BHM_IG
setwd("code_for_rerunning_simu/table_1_2/")
# run_all<-"
# sc_array=(1 10 12 37 39 28 30 31 34)
# for i in ${sc_array[@]}
# do
#   cp BHM_IG.R BHM_IG_$i.R
#   find . -name 'BHM_IG_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
#   find . -name 'BHM_IG_'$i'.R' -print0 | xargs -0 perl -pi -e 's/bhm_ig_1.Rdata/bhm_ig_'$i'.Rdata/g'
# done"
# system(run_all)
for(i in c(1,10,12,37,39,28,30,31,34)){
  source(paste0("BHM_IG_",i,".R"))
}
#system("rm BHM_IG_*")
setwd("../../")



### BHM_HT
setwd("code_for_rerunning_simu/table_1_2/")
# run_all<-"
# sc_array=(1 10 12 37 39 28 30 31 34)
# for i in ${sc_array[@]}
# do
#   cp BHM_HT.R BHM_HT_$i.R
#   find . -name 'BHM_HT_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
#   find . -name 'BHM_HT_'$i'.R' -print0 | xargs -0 perl -pi -e 's/bhm_ht_1.Rdata/bhm_ht_'$i'.Rdata/g'
# done"
# system(run_all)
for(i in c(1,10,12,37,39,28,30,31,34)){
  source(paste0("BHM_HT_",i,".R"))
}
#system("rm BHM_HT_*")
setwd("../../")



### CBHM
setwd("code_for_rerunning_simu/table_1_2/")
source("calibration_cbhm.R") 
# run_all<-"
# sc_array=(1 10 12 37 39 28 30 31 34)
# for i in ${sc_array[@]}
# do
#   cp CBHM.R CBHM_$i.R
#   find . -name 'CBHM_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
#   find . -name 'CBHM_'$i'.R' -print0 | xargs -0 perl -pi -e 's/cbhm_1.Rdata/cbhm_'$i'.Rdata/g'
# done"
# system(run_all)
for(i in c(1,10,12,37,39,28,30,31,34)){
  source(paste0("CBHM_",i,".R"))
}
#system("rm CBHM_*")
setwd("../../")


### CBHM_Tox
setwd("code_for_rerunning_simu/table_1_2/")
source("calibration_cbhmtox.R") 
# run_all<-"
# sc_array=(1 10 12 37 39 28 30 31 34)
# for i in ${sc_array[@]}
# do
#   cp CBHMTox.R CBHMTox_$i.R
#   find . -name 'CBHMTox_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
#   find . -name 'CBHMTox_'$i'.R' -print0 | xargs -0 perl -pi -e 's/cbhmtox_1.Rdata/cbhmtox_'$i'.Rdata/g'
# done"
# system(run_all)
for(i in c(1,10,12,37,39,28,30,31,34)){
  source(paste0("CBHMTox_",i,".R"))
}
#system("rm CBHMTox_*")
setwd("../../")


### GCBHM_l
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
