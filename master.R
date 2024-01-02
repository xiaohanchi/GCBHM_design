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

#TABLE S6

#TABLE S7

#TABLE S8

#TABLE S9

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

############################################# TABLE 1 & 2 #################################################
rho=0.3
### BHM_IG
# try one script, run:
source("Eff_Tox/BHM/BHM_IG.R")

# run all scenarios:
setwd("Eff_Tox/BHM")
run_all<-"
sc_array=(1 10 12 37 39 28 30 31 34)
for i in ${sc_array[@]}
do
  cp BHM_HT.R BHM_HT_$i.R
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
source("Eff_Tox/BHM/BHM_HT.R")

# run all scenarios:
setwd("Eff_Tox/BHM")
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
source("Eff_Tox/CBHM/CBHM.R")

# run all scenarios:
setwd("Eff_Tox/CBHM")
source("calibration.R") 
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
setwd("Eff_Tox/CBHM_Tox")
# try one script, run:
source("CBHM_Tox.R")

# run all scenarios:
source("calibration.R") 
run_all<-"
sc_array=(1 10 12 37 39 28 30 31 34)
for i in ${sc_array[@]}
do
  cp CBHM_Tox.R CBHM_Tox_$i.R
  find . -name 'CBHM_Tox_'$i'.R' -print0 | xargs -0 perl -pi -e \"s/p\\[,,1\\]/p\\[,,$i\\]/g\"
  find . -name 'CBHM_Tox_'$i'.R' -print0 | xargs -0 perl -pi -e 's/cbhmtox_1.Rdata/cbhmtox_'$i'.Rdata/g'
done"
system(run_all)
for(i in c(1,10,12,37,39,28,30,31,34)){
  source(paste0("CBHM_Tox_",i,".R"))
}
system("rm CBHM_Tox_*")
setwd("../../")


### GCBHM_l
setwd("Eff_Tox/GCBHM_l")
# try one script, run:
source("GCBHM_l.R")

# run all scenarios:
source("calibration.R") 
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


source("Nested/BHM/BHM.R")

source("Nested/GCBHM_l/calibration.R")
source("Nested/GCBHM_l/GCBHM_l.R")

source("Nested/GCBHM_m/calibration.R")
source("Nested/GCBHM_m/GCBHM_m.R")


