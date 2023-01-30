##########################################################################################
# Simulation Settings with Nested Efficacy Endpoints                                     #
# A total of 16 settings, 8 (1,5,9,12-16) of which are shown in our paper                #
# Author: Xiaohan Chi                                                                    #
# 01/29/2023                                                                             #
##########################################################################################

###scenarios 
Nscenario=16
Ngroup=4 # cancer groups/types
Ncategory=4
p<-array(NA,dim=c(Ncategory,Ngroup,Nscenario))

### Scenarios when the null hypothesis holds
p[,,1]<-matrix(data = c(0.15, 0.15, 0.15, 0.15,
                        0.15, 0.15, 0.15, 0.15,
                        0.30, 0.30, 0.30, 0.30,
                        0.40, 0.40, 0.40, 0.40),
               nrow = Ncategory,ncol = Ngroup,byrow = T)
p[,,2]<-matrix(data = c(0.15, 0.15, 0.15, 0.15,
                        0.15, 0.15, 0.15, 0.15,
                        0.20, 0.20, 0.20, 0.20,
                        0.50, 0.50, 0.50, 0.50),
               nrow = Ncategory,ncol = Ngroup,byrow = T)
p[,,3]<-matrix(data = c(0.15, 0.15, 0.15, 0.15,
                        0.15, 0.15, 0.15, 0.15,
                        0.20, 0.30, 0.20, 0.30,
                        0.50, 0.40, 0.50, 0.40),
               nrow = Ncategory,ncol = Ngroup,byrow = T)
p[,,4]<-matrix(data = c(0.15, 0.15, 0.15, 0.15,
                        0.15, 0.15, 0.15, 0.15,
                        0.30, 0.20, 0.20, 0.20,
                        0.40, 0.50, 0.50, 0.50),
               nrow = Ncategory,ncol = Ngroup,byrow = T)

### Scenarios when the alternative hypothesis holds
p[,,5]<-matrix(data = c(0.25, 0.25, 0.25, 0.25,
                        0.25, 0.25, 0.25, 0.25,
                        0.20, 0.20, 0.20, 0.20,
                        0.30, 0.30, 0.30, 0.30),
               nrow = Ncategory,ncol = Ngroup,byrow = T)
p[,,6]<-matrix(data = c(0.25, 0.25, 0.25, 0.25,
                        0.25, 0.25, 0.25, 0.25,
                        0.10, 0.10, 0.10, 0.10,
                        0.40, 0.40, 0.40, 0.40),
               nrow = Ncategory,ncol = Ngroup,byrow = T)
p[,,7]<-matrix(data = c(0.25, 0.25, 0.25, 0.25,
                        0.25, 0.25, 0.25, 0.25,
                        0.20, 0.20, 0.10, 0.10,
                        0.30, 0.30, 0.40, 0.40),
               nrow = Ncategory,ncol = Ngroup,byrow = T)
p[,,8]<-matrix(data = c(0.25, 0.25, 0.25, 0.25,
                        0.25, 0.25, 0.25, 0.25,
                        0.20, 0.10, 0.10, 0.10,
                        0.30, 0.40, 0.40, 0.40),
               nrow = Ncategory,ncol = Ngroup,byrow = T)

### Homogeneous Setting (meeting one of constraints)
p[,,9]<-matrix(data = c(0.15, 0.15, 0.15, 0.15,
                        0.35, 0.35, 0.35, 0.35,
                        0.20, 0.20, 0.20, 0.20,
                        0.30, 0.30, 0.30, 0.30),
               nrow = Ncategory,ncol = Ngroup,byrow = T)
### Heterogeneous Setting (meeting one of constraints)
p[,,10]<-matrix(data = c(0.15, 0.25, 0.15, 0.25,
                         0.35, 0.15, 0.35, 0.15,
                         0.20, 0.30, 0.20, 0.30,
                         0.30, 0.30, 0.30, 0.30),
                nrow = Ncategory,ncol = Ngroup,byrow = T)
### Heterogeneous Setting (Complex)
p[,,11]<-matrix(data = c(0.15, 0.15, 0.15, 0.25,
                         0.35, 0.15, 0.25, 0.15,
                         0.20, 0.30, 0.30, 0.30,
                         0.30, 0.40, 0.30, 0.30),
                nrow = Ncategory,ncol = Ngroup,byrow = T)
### Heterogeneous Setting (Complex)
p[,,12]<-matrix(data = c(0.25, 0.25, 0.25, 0.25,
                         0.15, 0.25, 0.35, 0.45,
                         0.20, 0.20, 0.20, 0.20,
                         0.40, 0.30, 0.20, 0.10),
                nrow = Ncategory,ncol = Ngroup,byrow = T)
#H1 vs. H0, heterogeneous
p[,,13]<-matrix(data = c(0.15, 0.15, 0.25, 0.25,
                         0.15, 0.15, 0.25, 0.25,
                         0.30, 0.30, 0.20, 0.20,
                         0.40, 0.40, 0.30, 0.30),
                nrow = Ncategory,ncol = Ngroup,byrow = T)

#H1 vs. H0, heterogeneous
p[,,14]<-matrix(data = c(0.15, 0.25, 0.25, 0.25,
                         0.15, 0.25, 0.25, 0.25,
                         0.30, 0.20, 0.20, 0.20,
                         0.40, 0.30, 0.30, 0.30),
                nrow = Ncategory,ncol = Ngroup,byrow = T)
#H1 vs. H0, heterogeneous
p[,,15]<-matrix(data = c(0.15, 0.15, 0.25, 0.25,
                         0.15, 0.15, 0.35, 0.35,
                         0.30, 0.30, 0.20, 0.20,
                         0.40, 0.40, 0.20, 0.20),
                nrow = Ncategory,ncol = Ngroup,byrow = T)
#H1 vs. H0, heterogeneous
p[,,16]<-matrix(data = c(0.15, 0.15, 0.25, 0.25,
                         0.15, 0.15, 0.15, 0.25,
                         0.30, 0.30, 0.30, 0.20,
                         0.40, 0.40, 0.30, 0.30),
                nrow = Ncategory,ncol = Ngroup,byrow = T)

