### GCBHM_l: #interim=1
setwd("code_for_rerunning_simu/table_S10/")
# run_all<-"
# sc_array=(1 5 9 12 13 14 15 16)
# for i in ${sc_array[@]}
# do
#   cp 1gcbhml.R 1gcbhml_$i.R
#   find . -name '1gcbhml_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
#   find . -name '1gcbhml_'$i'.R' -print0 | xargs -0 perl -pi -e 's/1gcbhml_1.Rdata/1gcbhml_'$i'.Rdata/g'
# done"
# system(run_all)
for(i in c(1,5,9,12:16)){
  source(paste0("1gcbhml_",i,".R"))
}
#system("rm 1gcbhml_*")
setwd("../../")


### GCBHM_l: #interim=2
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
system("cp code_for_rerunning_simu/table_S5/Rdata/gcbhml*Rdata code_for_rerunning_simu/table_S10/Rdata/")


### GCBHM_l: #interim=3
setwd("code_for_rerunning_simu/table_S10/")
# run_all<-"
# sc_array=(1 5 9 12 13 14 15 16)
# for i in ${sc_array[@]}
# do
#   cp 3gcbhml.R 3gcbhml_$i.R
#   find . -name '3gcbhml_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
#   find . -name '3gcbhml_'$i'.R' -print0 | xargs -0 perl -pi -e 's/3gcbhml_1.Rdata/3gcbhml_'$i'.Rdata/g'
# done"
# system(run_all)
for(i in c(1,5,9,12:16)){
  source(paste0("3gcbhml_",i,".R"))
}
#system("rm 3gcbhml_*")
setwd("../../")

### GCBHM_m: #interim=1
setwd("code_for_rerunning_simu/table_S10/")
# run_all<-"
# sc_array=(1 5 9 12 13 14 15 16)
# for i in ${sc_array[@]}
# do
#   cp 1gcbhmm.R 1gcbhmm_$i.R
#   find . -name '1gcbhmm_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
#   find . -name '1gcbhmm_'$i'.R' -print0 | xargs -0 perl -pi -e 's/1gcbhmm_1.Rdata/1gcbhmm_'$i'.Rdata/g'
# done"
# system(run_all)
for(i in c(1,5,9,12:16)){
  source(paste0("1gcbhmm_",i,".R"))
}
#system("rm 1gcbhmm_*")
setwd("../../")

### GCBHM_m: #interim=2
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
system("cp code_for_rerunning_simu/table_S5/Rdata/gcbhmm*Rdata code_for_rerunning_simu/table_S10/Rdata/")

### GCBHM_m: #interim=3
setwd("code_for_rerunning_simu/table_S10/")
# run_all<-"
# sc_array=(1 5 9 12 13 14 15 16)
# for i in ${sc_array[@]}
# do
#   cp 3gcbhmm.R 3gcbhmm_$i.R
#   find . -name '3gcbhmm_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
#   find . -name '3gcbhmm_'$i'.R' -print0 | xargs -0 perl -pi -e 's/3gcbhmm_1.Rdata/3gcbhmm_'$i'.Rdata/g'
# done"
# system(run_all)
for(i in c(1,5,9,12:16)){
  source(paste0("3gcbhmm_",i,".R"))
}
#system("rm 3gcbhmm_*")
setwd("../../")

