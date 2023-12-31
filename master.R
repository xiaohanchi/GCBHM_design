# Master script

# Set GCBHM_design-main/ as working directory

# Reproduce tables and figures in the manuscript
# Reproduce tables and figures in the supplementary material

######################################################################################################
#TABLE 1
source("Tables/Table1.R")

source("Eff_Tox/CBHM/evaluate.R")

source("Eff_Tox/CBHM_Tox/evaluate.R")

source("Eff_Tox/GCBHM_l/evaluate.R")

source("Nested/BHM/evaluate.R")

source("Nested/GCBHM_l/evaluate.R")

source("Nested/GCBHM_m/evaluate.R")

# Format tables as shown in manuscript and supplementary material
# Name and save ALL tables and figures in the folder results/



# Rerun simulations 
# Customize number of simulations to save running time, default 5000. 
# Remove command lines defining nsimu in ALL related scripts.

nsimu<-10





# Run simulations # Calibrate where necessary
############################################# BHM_IG #################################################
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
setwd("../")
############################################# BHM_HT #################################################
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
setwd("../")


source("Eff_Tox/BHM/BHM_IG.R")

source("Eff_Tox/CBHM/CBHM.R")

source("Eff_Tox/CBHM_Tox/CBHM_Tox.R")

source("Eff_Tox/GCBHM_l/calibration.R")
source("Eff_Tox/GCBHM_l/GCBHM_l.R")

source("Nested/BHM/BHM.R")

source("Nested/GCBHM_l/calibration.R")
source("Nested/GCBHM_l/GCBHM_l.R")

source("Nested/GCBHM_m/calibration.R")
source("Nested/GCBHM_m/GCBHM_m.R")


