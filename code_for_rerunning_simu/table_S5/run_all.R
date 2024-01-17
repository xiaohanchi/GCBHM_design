
### BHM
setwd("code_for_rerunning_simu/table_S5/")
# run_all<-"
# sc_array=(1 5 9 12 13 14 15 16)
# for i in ${sc_array[@]}
# do
#   cp BHM.R BHM_$i.R
#   find . -name 'BHM_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
#   find . -name 'BHM_'$i'.R' -print0 | xargs -0 perl -pi -e 's/bhm_1.Rdata/bhm_'$i'.Rdata/g'
# done"
# system(run_all)
for(i in c(1,5,9,12:16)){
  source(paste0("BHM_",i,".R"))
}
#system("rm BHM_*")
setwd("../../")


### GCBHM_l
setwd("code_for_rerunning_simu/table_S5/")
source("calibration_gcbhml.R") 
# run_all<-"
# sc_array=(1 5 9 12 13 14 15 16)
# for i in ${sc_array[@]}
# do
#   cp GCBHM_l.R GCBHM_l_$i.R
#   find . -name 'GCBHM_l_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
#   find . -name 'GCBHM_l_'$i'.R' -print0 | xargs -0 perl -pi -e 's/gcbhml_1.Rdata/gcbhml_'$i'.Rdata/g'
# done"
# system(run_all)
for(i in c(1,5,9,12:16)){
  source(paste0("GCBHM_l_",i,".R"))
}
#system("rm GCBHM_l_*")
setwd("../../")


### GCBHM_m
setwd("code_for_rerunning_simu/table_S5/")
source("calibration_gcbhmm.R") 
# run_all<-"
# sc_array=(1 5 9 12 13 14 15 16)
# for i in ${sc_array[@]}
# do
#   cp GCBHM_m.R GCBHM_m_$i.R
#   find . -name 'GCBHM_m_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
#   find . -name 'GCBHM_m_'$i'.R' -print0 | xargs -0 perl -pi -e 's/gcbhmm_1.Rdata/gcbhmm_'$i'.Rdata/g'
# done"
# system(run_all)
for(i in c(1,5,9,12:16)){
  source(paste0("GCBHM_m_",i,".R"))
}
#system("rm GCBHM_m_*")
setwd("../../")
