# Master script

# Set GCBHM_design-main/ as working directory

# Reproduce tables and figures in the manuscript
#TABLE 1 & 2
source("code_for_tables_figures/Table1_and_2.R")

#FIGURE 1 & 2
source("code_for_tables_figures/Figure_1_2.R")

# Reproduce tables and figures in the supplementary material
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
source("code_for_tables_figures/Table_S10.R")

#TABLE S11: sample size
source("code_for_tables_figures/Table_S11.R")

#TABLE S12
source("code_for_tables_figures/Table_S12.R")

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
##### generated data is also used for Table S2, Figure 1 and 2, and Figure S1-S6

################ try one script, run:
rho=0.3

### BHM_IG
setwd("code_for_rerunning_simu/table_1_2/")
source("BHM_IG.R")
setwd("../../")

### BHM_HT
setwd("code_for_rerunning_simu/table_1_2/")
source("BHM_HT.R")
setwd("../../")

### CBHM
setwd("code_for_rerunning_simu/table_1_2/")
source("CBHM.R")
setwd("../../")

### CBHM_Tox
setwd("code_for_rerunning_simu/table_1_2/")
source("CBHMTox.R")
setwd("../../")

### GCBHM_l
setwd("code_for_rerunning_simu/table_1_2/")
source("GCBHM_l.R")
setwd("../../")

################ run all scenarios:
rho=0.3
source("code_for_rerunning_simu/table_1_2/run_all.R")
system("mv code_for_rerunning_simu/table_1_2/Rdata/*Rdata code_for_rerunning_simu/table_1_2/Rdata/rho0.3/")

################################################ TABLE S3 ####################################################
################ run all scenarios:
rho=0
source("code_for_rerunning_simu/table_1_2/run_all.R")
system("mv code_for_rerunning_simu/table_1_2/Rdata/*Rdata code_for_rerunning_simu/table_1_2/Rdata/rho0/")

################################################ TABLE S4 ####################################################
################ run all scenarios:
rho=0.5
source("code_for_rerunning_simu/table_1_2/run_all.R")
system("mv code_for_rerunning_simu/table_1_2/Rdata/*Rdata code_for_rerunning_simu/table_1_2/Rdata/rho0.5/")

################################################ TABLE S5 ####################################################
#### generated data is also used for table S6

################ try one script, run:
### BHM
setwd("code_for_rerunning_simu/table_S5/")
source("BHM.R")
setwd("../../")

### GCBHM_l
setwd("code_for_rerunning_simu/table_S5/")
source("GCBHM_l.R")
setwd("../../")

### GCBHM_m
setwd("code_for_rerunning_simu/table_S5/")
source("GCBHM_m.R")
setwd("../../")

################ run all scenarios:
source("code_for_rerunning_simu/table_S5/run_all.R")


################################################ TABLE S7 ####################################################

source("code_for_rerunning_simu/table_S7/run_all.R")

################################################ TABLE S8 ####################################################

source("code_for_rerunning_simu/table_S8/run_all.R")

################################################ TABLE S9 ####################################################

source("code_for_rerunning_simu/table_S9/run_all.R")

################################################ TABLE S10 ####################################################

source("code_for_rerunning_simu/table_S10/run_all.R")

################################################ TABLE S11 ####################################################

source("code_for_rerunning_simu/table_S11/run_all.R")

################################################ TABLE S12 ####################################################




