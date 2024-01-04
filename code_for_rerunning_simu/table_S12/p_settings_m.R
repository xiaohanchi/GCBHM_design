###scenarios 
Nscenario=16
Ngroup=4 # cancer groups/types
Ncategory=4
p<-array(NA,dim=c(Ncategory,Ngroup,Nscenario))

#s1:null_1
p[,,1]<-matrix(data = c(0.15, 0.15, 0.15, 0.15,
                        0.15, 0.15, 0.15, 0.15,
                        0.30, 0.30, 0.30, 0.30,
                        0.40, 0.40, 0.40, 0.40),
               nrow = Ncategory,ncol = Ngroup,byrow = T)
#s1:null_2
p[,,2]<-matrix(data = c(0.15, 0.15, 0.15, 0.15,
                        0.15, 0.15, 0.15, 0.15,
                        0.20, 0.20, 0.20, 0.20,
                        0.50, 0.50, 0.50, 0.50),
               nrow = Ncategory,ncol = Ngroup,byrow = T)
#s1:null_3
p[,,3]<-matrix(data = c(0.15, 0.15, 0.15, 0.15,
                        0.15, 0.15, 0.15, 0.15,
                        0.20, 0.30, 0.20, 0.30,
                        0.50, 0.40, 0.50, 0.40),
               nrow = Ncategory,ncol = Ngroup,byrow = T)
#s1:null_4
p[,,4]<-matrix(data = c(0.15, 0.15, 0.15, 0.15,
                        0.15, 0.15, 0.15, 0.15,
                        0.30, 0.20, 0.20, 0.20,
                        0.40, 0.50, 0.50, 0.50),
               nrow = Ncategory,ncol = Ngroup,byrow = T)

#s2:alternative_1
p[,,5]<-matrix(data = c(0.25, 0.25, 0.25, 0.25,
                        0.25, 0.25, 0.25, 0.25,
                        0.20, 0.20, 0.20, 0.20,
                        0.30, 0.30, 0.30, 0.30),
               nrow = Ncategory,ncol = Ngroup,byrow = T)
#s2:alternative_2
p[,,6]<-matrix(data = c(0.25, 0.25, 0.25, 0.25,
                        0.25, 0.25, 0.25, 0.25,
                        0.10, 0.10, 0.10, 0.10,
                        0.40, 0.40, 0.40, 0.40),
               nrow = Ncategory,ncol = Ngroup,byrow = T)
#s2:alternative_3
p[,,7]<-matrix(data = c(0.25, 0.25, 0.25, 0.25,
                        0.25, 0.25, 0.25, 0.25,
                        0.20, 0.20, 0.10, 0.10,
                        0.30, 0.30, 0.40, 0.40),
               nrow = Ncategory,ncol = Ngroup,byrow = T)
#s2:alternative_4
p[,,8]<-matrix(data = c(0.25, 0.25, 0.25, 0.25,
                        0.25, 0.25, 0.25, 0.25,
                        0.20, 0.10, 0.10, 0.10,
                        0.30, 0.40, 0.40, 0.40),
               nrow = Ncategory,ncol = Ngroup,byrow = T)

#s3:homo (meet one of constraints)
p[,,9]<-matrix(data = c(0.15, 0.15, 0.15, 0.15,
                        0.35, 0.35, 0.35, 0.35,
                        0.20, 0.20, 0.20, 0.20,
                        0.30, 0.30, 0.30, 0.30),
               nrow = Ncategory,ncol = Ngroup,byrow = T)
#s4:hetero (meet one of constraints)
p[,,10]<-matrix(data = c(0.15, 0.25, 0.15, 0.25,
                         0.35, 0.15, 0.35, 0.15,
                         0.20, 0.30, 0.20, 0.30,
                         0.30, 0.30, 0.30, 0.30),
                nrow = Ncategory,ncol = Ngroup,byrow = T)
#s5:hetero (complex)
p[,,11]<-matrix(data = c(0.15, 0.15, 0.15, 0.25,
                         0.35, 0.15, 0.25, 0.15,
                         0.20, 0.30, 0.30, 0.30,
                         0.30, 0.40, 0.30, 0.30),
                nrow = Ncategory,ncol = Ngroup,byrow = T)
#s6:hetero (complex)
p[,,12]<-matrix(data = c(0.25, 0.25, 0.25, 0.25,
                         0.15, 0.25, 0.35, 0.45,
                         0.20, 0.20, 0.20, 0.20,
                         0.40, 0.30, 0.20, 0.10),
                nrow = Ncategory,ncol = Ngroup,byrow = T)
#H1 vs. H0, hetero
p[,,13]<-matrix(data = c(0.15, 0.15, 0.25, 0.25,
                         0.15, 0.15, 0.25, 0.25,
                         0.30, 0.30, 0.20, 0.20,
                         0.40, 0.40, 0.30, 0.30),
                nrow = Ncategory,ncol = Ngroup,byrow = T)

#H1 vs. H0, hetero
p[,,14]<-matrix(data = c(0.15, 0.25, 0.25, 0.25,
                         0.15, 0.25, 0.25, 0.25,
                         0.30, 0.20, 0.20, 0.20,
                         0.40, 0.30, 0.30, 0.30),
                nrow = Ncategory,ncol = Ngroup,byrow = T)
#H1 vs. H0, hetero
p[,,15]<-matrix(data = c(0.15, 0.15, 0.25, 0.25,
                         0.15, 0.15, 0.35, 0.35,
                         0.30, 0.30, 0.20, 0.20,
                         0.40, 0.40, 0.20, 0.20),
                nrow = Ncategory,ncol = Ngroup,byrow = T)
#H1 vs. H0, hetero
p[,,16]<-matrix(data = c(0.15, 0.15, 0.25, 0.25,
                         0.15, 0.15, 0.15, 0.25,
                         0.30, 0.30, 0.30, 0.20,
                         0.40, 0.40, 0.30, 0.30),
                nrow = Ncategory,ncol = Ngroup,byrow = T)

# for(i in 1:Nscenario){
#   if(mean(colSums(p[,,i]))!=1){
#     cat("Wrong parameter Settings!i=",i,sep = '')
#   }else if(i==Nscenario&mean(colSums(p[,,i]))==1){
#     cat("Correct parameter Settings!",sep = '')
#   }
# }

