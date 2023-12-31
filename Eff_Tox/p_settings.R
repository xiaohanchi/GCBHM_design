##########################################################################################
# Simulation Settings with Efficacy and Toxicity Endpoints                               #
# A total of 39 settings, 9 (1,10,12,37,39,28,31,34) of which are shown in our paper     #
# Author: Xiaohan Chi                                                                    #
# 01/29/2023                                                                             #
##########################################################################################

source('../functions.R')
###scenarios 
Nscenario=39
Ngroup=4 # cancer groups/types
Ncategory=4
rho=0.3
p<-array(NA,dim=c(Ncategory,Ngroup,Nscenario))

### Homogeneous Settings
eff<-rep(seq(0.45,0.60,0.05),3)
tox<-rep(seq(0.3,0.2,-0.05),each=4)
for(i in 1:12){
  level<-solve.level(rho,eff[i],tox[i])
  p[,,i]<-matrix(level,nrow = Ncategory,ncol = Ngroup,byrow = F)
}

### Heterogeneous Settings
eff<-seq(0.45,0.60,0.05)
tox<-seq(0.14,0.32,0.06)
for(i in 13:16){
  p[,,i]<-matrix(c(solve.level(rho,eff[i-12],tox[1]),
                    solve.level(rho,eff[i-12],tox[2]),
                    solve.level(rho,eff[i-12],tox[3]),
                    solve.level(rho,eff[i-12],tox[4])),
                  nrow = Ncategory,ncol = Ngroup,byrow = F)
}
eff<-seq(0.45,0.60,0.05)
tox<-seq(0.22,0.28,0.02)
for(i in 17:20){
  p[,,i]<-matrix(c(solve.level(rho,eff[i-16],tox[1]),
                   solve.level(rho,eff[i-16],tox[2]),
                   solve.level(rho,eff[i-16],tox[3]),
                   solve.level(rho,eff[i-16],tox[4])),
                 nrow = Ncategory,ncol = Ngroup,byrow = F)
}
eff<-seq(0.40,0.64,0.08)
tox<-seq(0.3,0.2,-0.05)
for(i in 21:23){
  p[,,i]<-matrix(c(solve.level(rho,eff[1],tox[i-20]),
                   solve.level(rho,eff[2],tox[i-20]),
                   solve.level(rho,eff[3],tox[i-20]),
                   solve.level(rho,eff[4],tox[i-20])),
                 nrow = Ncategory,ncol = Ngroup,byrow = F)
}
eff<-seq(0.50,0.56,0.02)
tox<-seq(0.3,0.2,-0.05)
for(i in 24:26){
  p[,,i]<-matrix(c(solve.level(rho,eff[1],tox[i-23]),
                   solve.level(rho,eff[2],tox[i-23]),
                   solve.level(rho,eff[3],tox[i-23]),
                   solve.level(rho,eff[4],tox[i-23])),
                 nrow = Ncategory,ncol = Ngroup,byrow = F)
}

##27-36 
p[,,27]<-matrix(c(solve.level(rho,0.45,0.3),
                  solve.level(rho,0.55,0.25),
                  solve.level(rho,0.55,0.25),
                  solve.level(rho,0.55,0.25)),
                nrow = Ncategory,ncol = Ngroup,byrow = F)
p[,,28]<-matrix(c(solve.level(rho,0.45,0.3),
                  solve.level(rho,0.60,0.20),
                  solve.level(rho,0.60,0.20),
                  solve.level(rho,0.60,0.20)),
                nrow = Ncategory,ncol = Ngroup,byrow = F)
p[,,29]<-matrix(c(solve.level(rho,0.45,0.3),
                  solve.level(rho,0.55,0.25),
                  solve.level(rho,0.55,0.25),
                  solve.level(rho,0.60,0.20)),
                nrow = Ncategory,ncol = Ngroup,byrow = F)

p[,,30]<-matrix(c(solve.level(rho,0.45,0.30),
                  solve.level(rho,0.45,0.30),
                  solve.level(rho,0.60,0.20),
                  solve.level(rho,0.60,0.20)),
                nrow = Ncategory,ncol = Ngroup,byrow = F)
p[,,31]<-matrix(c(solve.level(rho,0.45,0.30),
                  solve.level(rho,0.45,0.30),
                  solve.level(rho,0.55,0.25),
                  solve.level(rho,0.60,0.20)),
                nrow = Ncategory,ncol = Ngroup,byrow = F)
p[,,32]<-matrix(c(solve.level(rho,0.45,0.30),
                  solve.level(rho,0.45,0.30),
                  solve.level(rho,0.40,0.35),
                  solve.level(rho,0.55,0.25)),
                nrow = Ncategory,ncol = Ngroup,byrow = F)
p[,,33]<-matrix(c(solve.level(rho,0.45,0.30),
                  solve.level(rho,0.45,0.30),
                  solve.level(rho,0.50,0.25),
                  solve.level(rho,0.55,0.20)),
                nrow = Ncategory,ncol = Ngroup,byrow = F)

p[,,34]<-matrix(c(solve.level(rho,0.45,0.30),
                  solve.level(rho,0.45,0.30),
                  solve.level(rho,0.45,0.30),
                  solve.level(rho,0.60,0.20)),
                nrow = Ncategory,ncol = Ngroup,byrow = F)
p[,,35]<-matrix(c(solve.level(rho,0.45,0.30),
                  solve.level(rho,0.45,0.30),
                  solve.level(rho,0.45,0.30),
                  solve.level(rho,0.55,0.25)),
                nrow = Ncategory,ncol = Ngroup,byrow = F)
p[,,36]<-matrix(c(solve.level(rho,0.45,0.30),
                  solve.level(rho,0.45,0.30),
                  solve.level(rho,0.45,0.30),
                  solve.level(rho,0.50,0.25)),
                nrow = Ncategory,ncol = Ngroup,byrow = F)
p[,,37]<-matrix(c(solve.level(rho,0.50,0.12),
                  solve.level(rho,0.50,0.15),
                  solve.level(rho,0.50,0.20),
                  solve.level(rho,0.50,0.25)),
                nrow = Ncategory,ncol = Ngroup,byrow = F)
p[,,38]<-matrix(c(solve.level(rho,0.55,0.12),
                  solve.level(rho,0.55,0.15),
                  solve.level(rho,0.55,0.20),
                  solve.level(rho,0.55,0.25)),
                nrow = Ncategory,ncol = Ngroup,byrow = F)
p[,,39]<-matrix(c(solve.level(rho,0.50,0.25),
                  solve.level(rho,0.55,0.25),
                  solve.level(rho,0.60,0.25),
                  solve.level(rho,0.65,0.25)),
                nrow = Ncategory,ncol = Ngroup,byrow = F)
