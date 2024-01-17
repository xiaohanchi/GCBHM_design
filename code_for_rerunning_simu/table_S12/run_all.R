### GCBHM_l: n=36
setwd("code_for_rerunning_simu/table_S12/")
# run_all<-"
# sc_array=(1 5 9 12 13 14 15 16)
# for i in ${sc_array[@]}
# do
#   cp gcbhml_n36.R gcbhml_n36_$i.R
#   find . -name 'gcbhml_n36_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
#   find . -name 'gcbhml_n36_'$i'.R' -print0 | xargs -0 perl -pi -e 's/gcbhml_n36_1.Rdata/gcbhml_n36_'$i'.Rdata/g'
# done"
# system(run_all)
for(i in c(1,5,9,12:16)){
  source(paste0("gcbhml_n36_",i,".R"))
}
#system("rm gcbhml_n36_*")
setwd("../../")


### GCBHM_l: n=45
setwd("code_for_rerunning_simu/table_S5/")
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
system("cp code_for_rerunning_simu/table_S5/Rdata/gcbhml*Rdata code_for_rerunning_simu/table_S12/Rdata/")

### GCBHM_l: n=54
setwd("code_for_rerunning_simu/table_S12/")
# run_all<-"
# sc_array=(1 5 9 12 13 14 15 16)
# for i in ${sc_array[@]}
# do
#   cp gcbhml_n54.R gcbhml_n54_$i.R
#   find . -name 'gcbhml_n54_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
#   find . -name 'gcbhml_n54_'$i'.R' -print0 | xargs -0 perl -pi -e 's/gcbhml_n54_1.Rdata/gcbhml_n54_'$i'.Rdata/g'
# done"
# system(run_all)
for(i in c(1,5,9,12:16)){
  source(paste0("gcbhml_n54_",i,".R"))
}
#system("rm gcbhml_n54_*")
setwd("../../")

### GCBHM_m: n=36
setwd("code_for_rerunning_simu/table_S12/")
# run_all<-"
# sc_array=(1 5 9 12 13 14 15 16)
# for i in ${sc_array[@]}
# do
#   cp gcbhmm_n36.R gcbhmm_n36_$i.R
#   find . -name 'gcbhmm_n36_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
#   find . -name 'gcbhmm_n36_'$i'.R' -print0 | xargs -0 perl -pi -e 's/gcbhmm_n36_1.Rdata/gcbhmm_n36_'$i'.Rdata/g'
# done"
# system(run_all)
for(i in c(1,5,9,12:16)){
  source(paste0("gcbhmm_n36_",i,".R"))
}
#system("rm gcbhmm_n36_*")
setwd("../../")

### GCBHM_m: n=45
setwd("code_for_rerunning_simu/table_S5/")
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
system("cp code_for_rerunning_simu/table_S5/Rdata/gcbhmm*Rdata code_for_rerunning_simu/table_S12/Rdata/")

### GCBHM_m: n=54
setwd("code_for_rerunning_simu/table_S12/")
# run_all<-"
# sc_array=(1 5 9 12 13 14 15 16)
# for i in ${sc_array[@]}
# do
#   cp gcbhmm_n54.R gcbhmm_n54_$i.R
#   find . -name 'gcbhmm_n54_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
#   find . -name 'gcbhmm_n54_'$i'.R' -print0 | xargs -0 perl -pi -e 's/gcbhmm_n54_1.Rdata/gcbhmm_n54_'$i'.Rdata/g'
# done"
# system(run_all)
for(i in c(1,5,9,12:16)){
  source(paste0("gcbhmm_n54_",i,".R"))
}
#system("rm gcbhmm_n54_*")
setwd("../../")
