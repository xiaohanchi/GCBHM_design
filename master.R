# Master script

# Set GCBHM_design-main/ as working directory

# Reproduce tables and figures in the manuscript
######################################################################################################
#TABLE 1 & 2
source("code_for_tables_figures/Table1_and_2.R")

#FIGURE 1 & 2
source("code_for_tables_figures/Figure_1_2.R")


# Reproduce tables and figures in the supplementary material
######################################################################################################
#TABLE S1:?

#TABLE S2: gcbhml
source("code_for_tables_figures/Table_S2.R")

#TABLE S3: sample size
source("code_for_tables_figures/Table_S3.R")

#TABLE S4
source("code_for_tables_figures/Table_S4.R")

#TABLE S5
source("code_for_tables_figures/Table_S5.R")

#TABLE S6
source("code_for_tables_figures/Table_S6.R")

#TABLE S7: sample size
source("code_for_tables_figures/Table_S7.R")

#TABLE S8
source("code_for_tables_figures/Table_S8.R")

#TABLE S9: sample size
source("code_for_tables_figures/Table_S9.R")

#TABLE S10

#TABLE S11


#TABLE S12


#FIGURE S1 and S2
source("code_for_tables_figures/Figure_S1_S2.R")

#FIGURE S3 and S4
source("code_for_tables_figures/Figure_S3_S4.R")

#FIGURE S5 and S6
source("code_for_tables_figures/Figure_S5_S6.R")


# Format tables as shown in manuscript and supplementary material
# Name and save ALL tables and figures in the folder results/



# Rerun simulations 
# Customize number of simulations to save running time, default 5000. 
# Remove command lines defining nsimu in ALL related scripts.

nsimu<-10





# Run simulations # Calibrate where necessary

################################################# TABLE 1 & 2 ###############################################
##### generated data also used for Figure 1 and 2, table S2, and Figure S1-S6

rho=0.3
### BHM_IG
# try one script, run:
setwd("code_for_rerunning_simu/table_1_2/")
source("BHM_IG.R")
setwd("../../")

# run all scenarios:
setwd("code_for_rerunning_simu/table_1_2/")
run_all<-"
sc_array=(1 10 12 37 39 28 30 31 34)
for i in ${sc_array[@]}
do
  cp BHM_IG.R BHM_IG_$i.R
  find . -name 'BHM_IG_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
  find . -name 'BHM_IG_'$i'.R' -print0 | xargs -0 perl -pi -e 's/bhm_ig_1.Rdata/bhm_ig_'$i'.Rdata/g'
done"
system(run_all)
for(i in c(1,10,12,37,39,28,30,31,34)){
  source(paste0("BHM_IG_",i,".R"))
}
system("rm BHM_IG_*")
setwd("../../")



### BHM_HT
# try one script, run:
setwd("code_for_rerunning_simu/table_1_2/")
source("BHM_HT.R")
setwd("../../")

# run all scenarios:
setwd("code_for_rerunning_simu/table_1_2/")
run_all<-"
sc_array=(1 10 12 37 39 28 30 31 34)
for i in ${sc_array[@]}
do
  cp BHM_HT.R BHM_HT_$i.R
  find . -name 'BHM_HT_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
  find . -name 'BHM_HT_'$i'.R' -print0 | xargs -0 perl -pi -e 's/bhm_ht_1.Rdata/bhm_ht_'$i'.Rdata/g'
done"
system(run_all)
for(i in c(1,10,12,37,39,28,30,31,34)){
  source(paste0("BHM_HT_",i,".R"))
}
system("rm BHM_HT_*")
setwd("../../")




### CBHM
# try one script, run:
setwd("code_for_rerunning_simu/table_1_2/")
source("CBHM.R")
setwd("../../")

# run all scenarios:
setwd("code_for_rerunning_simu/table_1_2/")
source("calibration_cbhm.R") 
run_all<-"
sc_array=(1 10 12 37 39 28 30 31 34)
for i in ${sc_array[@]}
do
  cp CBHM.R CBHM_$i.R
  find . -name 'CBHM_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
  find . -name 'CBHM_'$i'.R' -print0 | xargs -0 perl -pi -e 's/cbhm_1.Rdata/cbhm_'$i'.Rdata/g'
done"
system(run_all)
for(i in c(1,10,12,37,39,28,30,31,34)){
  source(paste0("CBHM_",i,".R"))
}
system("rm CBHM_*")
setwd("../../")


### CBHM_Tox
# try one script, run:
setwd("code_for_rerunning_simu/table_1_2/")
source("CBHMTox.R")
setwd("../../")

# run all scenarios:
setwd("code_for_rerunning_simu/table_1_2/")
source("calibration_cbhmtox.R") 
run_all<-"
sc_array=(1 10 12 37 39 28 30 31 34)
for i in ${sc_array[@]}
do
  cp CBHMTox.R CBHMTox_$i.R
  find . -name 'CBHMTox_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
  find . -name 'CBHMTox_'$i'.R' -print0 | xargs -0 perl -pi -e 's/cbhmtox_1.Rdata/cbhmtox_'$i'.Rdata/g'
done"
system(run_all)
for(i in c(1,10,12,37,39,28,30,31,34)){
  source(paste0("CBHMTox_",i,".R"))
}
system("rm CBHMTox_*")
setwd("../../")


### GCBHM_l
# try one script, run:
setwd("code_for_rerunning_simu/table_1_2/")
source("GCBHM_l.R")
setwd("../../")

# run all scenarios:
setwd("code_for_rerunning_simu/table_1_2/")
source("calibration_gcbhm.R") 
run_all<-"
sc_array=(1 10 12 37 39 28 30 31 34)
for i in ${sc_array[@]}
do
  cp GCBHM_l.R GCBHM_l_$i.R
  find . -name 'GCBHM_l_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
  find . -name 'GCBHM_l_'$i'.R' -print0 | xargs -0 perl -pi -e 's/gcbhm_1.Rdata/gcbhm_'$i'.Rdata/g'
done"
system(run_all)
for(i in c(1,10,12,37,39,28,30,31,34)){
  source(paste0("GCBHM_l_",i,".R"))
}
system("rm GCBHM_l_*")
setwd("../../")

################################################ TABLE S3 ####################################################
rho=0
### BHM_IG
# try one script, run:
setwd("code_for_rerunning_simu/table_1_2/")
source("BHM_IG.R")
setwd("../../")

# run all scenarios:
setwd("code_for_rerunning_simu/table_1_2/")
run_all<-"
sc_array=(1 10 12 37 39 28 30 31 34)
for i in ${sc_array[@]}
do
  cp BHM_IG.R BHM_IG_$i.R
  find . -name 'BHM_IG_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
  find . -name 'BHM_IG_'$i'.R' -print0 | xargs -0 perl -pi -e 's/bhm_ig_1.Rdata/bhm_ig_'$i'.Rdata/g'
done"
system(run_all)
for(i in c(1,10,12,37,39,28,30,31,34)){
  source(paste0("BHM_IG_",i,".R"))
}
system("rm BHM_IG_*")
setwd("../../")



### BHM_HT
# try one script, run:
setwd("code_for_rerunning_simu/table_1_2/")
source("BHM_HT.R")
setwd("../../")

# run all scenarios:
setwd("code_for_rerunning_simu/table_1_2/")
run_all<-"
sc_array=(1 10 12 37 39 28 30 31 34)
for i in ${sc_array[@]}
do
  cp BHM_HT.R BHM_HT_$i.R
  find . -name 'BHM_HT_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
  find . -name 'BHM_HT_'$i'.R' -print0 | xargs -0 perl -pi -e 's/bhm_ht_1.Rdata/bhm_ht_'$i'.Rdata/g'
done"
system(run_all)
for(i in c(1,10,12,37,39,28,30,31,34)){
  source(paste0("BHM_HT_",i,".R"))
}
system("rm BHM_HT_*")
setwd("../../")




### CBHM
# try one script, run:
setwd("code_for_rerunning_simu/table_1_2/")
source("CBHM.R")
setwd("../../")

# run all scenarios:
setwd("code_for_rerunning_simu/table_1_2/")
source("calibration_cbhm.R") 
run_all<-"
sc_array=(1 10 12 37 39 28 30 31 34)
for i in ${sc_array[@]}
do
  cp CBHM.R CBHM_$i.R
  find . -name 'CBHM_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
  find . -name 'CBHM_'$i'.R' -print0 | xargs -0 perl -pi -e 's/cbhm_1.Rdata/cbhm_'$i'.Rdata/g'
done"
system(run_all)
for(i in c(1,10,12,37,39,28,30,31,34)){
  source(paste0("CBHM_",i,".R"))
}
system("rm CBHM_*")
setwd("../../")


### CBHM_Tox
# try one script, run:
setwd("code_for_rerunning_simu/table_1_2/")
source("CBHMTox.R")
setwd("../../")

# run all scenarios:
setwd("code_for_rerunning_simu/table_1_2/")
source("calibration_cbhmtox.R") 
run_all<-"
sc_array=(1 10 12 37 39 28 30 31 34)
for i in ${sc_array[@]}
do
  cp CBHMTox.R CBHMTox_$i.R
  find . -name 'CBHMTox_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
  find . -name 'CBHMTox_'$i'.R' -print0 | xargs -0 perl -pi -e 's/cbhmtox_1.Rdata/cbhmtox_'$i'.Rdata/g'
done"
system(run_all)
for(i in c(1,10,12,37,39,28,30,31,34)){
  source(paste0("CBHMTox_",i,".R"))
}
system("rm CBHMTox_*")
setwd("../../")


### GCBHM_l
# try one script, run:
setwd("code_for_rerunning_simu/table_1_2/")
source("GCBHM_l.R")
setwd("../../")

# run all scenarios:
setwd("code_for_rerunning_simu/table_1_2/")
source("calibration_gcbhm.R") 
run_all<-"
sc_array=(1 10 12 37 39 28 30 31 34)
for i in ${sc_array[@]}
do
  cp GCBHM_l.R GCBHM_l_$i.R
  find . -name 'GCBHM_l_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
  find . -name 'GCBHM_l_'$i'.R' -print0 | xargs -0 perl -pi -e 's/gcbhm_1.Rdata/gcbhm_'$i'.Rdata/g'
done"
system(run_all)
for(i in c(1,10,12,37,39,28,30,31,34)){
  source(paste0("GCBHM_l_",i,".R"))
}
system("rm GCBHM_l_*")
setwd("../../")

################################################ TABLE S4 ####################################################
rho=0.5
### BHM_IG
# try one script, run:
setwd("code_for_rerunning_simu/table_1_2/")
source("BHM_IG.R")
setwd("../../")

# run all scenarios:
setwd("code_for_rerunning_simu/table_1_2/")
run_all<-"
sc_array=(1 10 12 37 39 28 30 31 34)
for i in ${sc_array[@]}
do
  cp BHM_IG.R BHM_IG_$i.R
  find . -name 'BHM_IG_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
  find . -name 'BHM_IG_'$i'.R' -print0 | xargs -0 perl -pi -e 's/bhm_ig_1.Rdata/bhm_ig_'$i'.Rdata/g'
done"
system(run_all)
for(i in c(1,10,12,37,39,28,30,31,34)){
  source(paste0("BHM_IG_",i,".R"))
}
system("rm BHM_IG_*")
setwd("../../")



### BHM_HT
# try one script, run:
setwd("code_for_rerunning_simu/table_1_2/")
source("BHM_HT.R")
setwd("../../")

# run all scenarios:
setwd("code_for_rerunning_simu/table_1_2/")
run_all<-"
sc_array=(1 10 12 37 39 28 30 31 34)
for i in ${sc_array[@]}
do
  cp BHM_HT.R BHM_HT_$i.R
  find . -name 'BHM_HT_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
  find . -name 'BHM_HT_'$i'.R' -print0 | xargs -0 perl -pi -e 's/bhm_ht_1.Rdata/bhm_ht_'$i'.Rdata/g'
done"
system(run_all)
for(i in c(1,10,12,37,39,28,30,31,34)){
  source(paste0("BHM_HT_",i,".R"))
}
system("rm BHM_HT_*")
setwd("../../")




### CBHM
# try one script, run:
setwd("code_for_rerunning_simu/table_1_2/")
source("CBHM.R")
setwd("../../")

# run all scenarios:
setwd("code_for_rerunning_simu/table_1_2/")
source("calibration_cbhm.R") 
run_all<-"
sc_array=(1 10 12 37 39 28 30 31 34)
for i in ${sc_array[@]}
do
  cp CBHM.R CBHM_$i.R
  find . -name 'CBHM_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
  find . -name 'CBHM_'$i'.R' -print0 | xargs -0 perl -pi -e 's/cbhm_1.Rdata/cbhm_'$i'.Rdata/g'
done"
system(run_all)
for(i in c(1,10,12,37,39,28,30,31,34)){
  source(paste0("CBHM_",i,".R"))
}
system("rm CBHM_*")
setwd("../../")


### CBHM_Tox
# try one script, run:
setwd("code_for_rerunning_simu/table_1_2/")
source("CBHMTox.R")
setwd("../../")

# run all scenarios:
setwd("code_for_rerunning_simu/table_1_2/")
source("calibration_cbhmtox.R") 
run_all<-"
sc_array=(1 10 12 37 39 28 30 31 34)
for i in ${sc_array[@]}
do
  cp CBHMTox.R CBHMTox_$i.R
  find . -name 'CBHMTox_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
  find . -name 'CBHMTox_'$i'.R' -print0 | xargs -0 perl -pi -e 's/cbhmtox_1.Rdata/cbhmtox_'$i'.Rdata/g'
done"
system(run_all)
for(i in c(1,10,12,37,39,28,30,31,34)){
  source(paste0("CBHMTox_",i,".R"))
}
system("rm CBHMTox_*")
setwd("../../")


### GCBHM_l
# try one script, run:
setwd("code_for_rerunning_simu/table_1_2/")
source("GCBHM_l.R")
setwd("../../")

# run all scenarios:
setwd("code_for_rerunning_simu/table_1_2/")
source("calibration_gcbhm.R") 
run_all<-"
sc_array=(1 10 12 37 39 28 30 31 34)
for i in ${sc_array[@]}
do
  cp GCBHM_l.R GCBHM_l_$i.R
  find . -name 'GCBHM_l_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
  find . -name 'GCBHM_l_'$i'.R' -print0 | xargs -0 perl -pi -e 's/gcbhm_1.Rdata/gcbhm_'$i'.Rdata/g'
done"
system(run_all)
for(i in c(1,10,12,37,39,28,30,31,34)){
  source(paste0("GCBHM_l_",i,".R"))
}
system("rm GCBHM_l_*")
setwd("../../")

################################################ TABLE S5 ####################################################
#### generated data also used for table S6
### BHM
# try one script, run:
setwd("code_for_rerunning_simu/table_S5/")
source("BHM.R")
setwd("../../")

# run all scenarios:
setwd("code_for_rerunning_simu/table_S5/")
run_all<-"
sc_array=(1 5 9 12 13 14 15 16)
for i in ${sc_array[@]}
do
  cp BHM.R BHM_$i.R
  find . -name 'BHM_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
  find . -name 'BHM_'$i'.R' -print0 | xargs -0 perl -pi -e 's/bhm_1.Rdata/bhm_'$i'.Rdata/g'
done"
system(run_all)
for(i in c(1,5,9,12:16)){
  source(paste0("BHM_",i,".R"))
}
system("rm BHM_*")
setwd("../../")


### GCBHM_l
# try one script, run:
setwd("code_for_rerunning_simu/table_S5/")
source("GCBHM_l.R")
setwd("../../")

# run all scenarios:
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


### GCBHM_m
# try one script, run:
setwd("code_for_rerunning_simu/table_S5/")
source("GCBHM_m.R")
setwd("../../")

# run all scenarios:
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

















