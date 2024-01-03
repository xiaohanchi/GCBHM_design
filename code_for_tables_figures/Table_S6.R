#TABLE S6
rm(list = ls())

library(reshape)
library(dplyr)
############################################ Functions ################################################
convert.list<-function(p,d){
  dims<-dim(p)#used to distinguish different endpoint types saved by diff designs
  if(d==3){
    res<-lapply(1:dims[d], function(r) {
      melt(p[,,r])
    }) %>% do.call('rbind', .)
    res[,1]<-as.factor(res[,1])
    res[,2]<-as.factor(res[,2])
    colnames(res)<-c("Endpoint","Group","p_est")
  }else if(d==2){
    res<-melt(p)[,c(1,3)]
    res[,1]<-as.factor(res[,1])
    colnames(res)<-c("Group","p_est")
  }
  return(res)
}
p_label <- function(m,i){
  p_hat <- substitute(bold(hat(p)[i_label] == m_label),
                      list(m_label = sprintf("%0.2f", m[i]),
                           i_label = ceiling(i/2)))
  as.character(as.expression(p_hat));
}
##rmse
rmse<-function(phat,p,n){
  sqrt(sum((phat-p)^2)/n)
}
############################################# Scenario 1 #################################################
scenario=1
load("Rdata/tableS5/bhm_1.Rdata")
p_hat_bhm<-p_hat
load("Rdata/tableS5/gcbhmm_1.Rdata")
p_hat_gcbhmm<-p_hat
load("Rdata/tableS5/gcbhml_1.Rdata")
p_hat_gcbhl1<-p1_hat_tmp
p_hat_gcbhl2<-p2_hat_tmp

p_convert_bhm<-data.frame(convert.list(p_hat_bhm,3),Design="1BHM")
p_convert_gcbhmm<-data.frame(convert.list(p_hat_gcbhmm,3),Design="3GCBHM_m")
p_convert_gcbhml<-rbind(data.frame(convert.list(p_hat_gcbhl1,2),
                                   Design="2GCBHM_l",
                                   Endpoint=factor("1",levels=c("1","2"))),
                        data.frame(convert.list(p_hat_gcbhl2,2),
                                   Design="2GCBHM_l",
                                   Endpoint=factor("2",levels=c("1","2"))))


p_ep1<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 1),
             dplyr::filter(p_convert_gcbhml,Endpoint == 1),
             dplyr::filter(p_convert_gcbhmm,Endpoint == 1))
pval_ep1<-data.frame(Group=levels(factor(p_ep1$Group)),
                     p_true=c(0.15,0.15,0.15,0.15))#true settings according to considered scenarios
p_ep1new<-merge(p_ep1,pval_ep1,by = "Group")
#bias
summary_ep1 <- p_ep1new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep1$m,3,4),digits = 2)
#se
summary_ep1 <- p_ep1new %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep1$m*10^3,3,4),digits = 2)
#rmse
summary_ep1 <- p_ep1new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep1$m,3,4),digits = 3)

method<-c("BHM","GCBHM_L","GCBHM_M")
response<-"CR"
res_eff<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))


p_ep2<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 2),
             dplyr::filter(p_convert_gcbhml,Endpoint == 2),
             dplyr::filter(p_convert_gcbhmm,Endpoint == 2))
pval_ep2<-data.frame(Group=levels(factor(p_ep2$Group)),
                     p_true=c(0.3,0.3,0.3,0.3))
p_ep2new<-merge(p_ep2,pval_ep2,by = "Group")

#bias
summary_ep2 <- p_ep2new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep2$m,3,4),digits = 2)
#se
summary_ep2 <- p_ep2new %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep2$m*10^3,3,4),digits = 2)
#rmse
summary_ep2 <- p_ep2new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep2$m,3,4),digits = 3)

method<-c("BHM","GCBHM_L","GCBHM_M")
response<-"CR/PR"
res_tox<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))

tabS2<-cbind(Scenario=scenario, rbind(res_eff,res_tox))

############################################# Scenario 2 #################################################
scenario=2
load("Rdata/tableS5/bhm_5.Rdata")
p_hat_bhm<-p_hat
load("Rdata/tableS5/gcbhmm_5.Rdata")
p_hat_gcbhmm<-p_hat
load("Rdata/tableS5/gcbhml_5.Rdata")
p_hat_gcbhl1<-p1_hat_tmp
p_hat_gcbhl2<-p2_hat_tmp

p_convert_bhm<-data.frame(convert.list(p_hat_bhm,3),Design="1BHM")
p_convert_gcbhmm<-data.frame(convert.list(p_hat_gcbhmm,3),Design="3GCBHM_m")
p_convert_gcbhml<-rbind(data.frame(convert.list(p_hat_gcbhl1,2),
                                   Design="2GCBHM_l",
                                   Endpoint=factor("1",levels=c("1","2"))),
                        data.frame(convert.list(p_hat_gcbhl2,2),
                                   Design="2GCBHM_l",
                                   Endpoint=factor("2",levels=c("1","2"))))


p_ep1<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 1),
             dplyr::filter(p_convert_gcbhml,Endpoint == 1),
             dplyr::filter(p_convert_gcbhmm,Endpoint == 1))
pval_ep1<-data.frame(Group=levels(factor(p_ep1$Group)),
                     p_true=c(0.25,0.25,0.25,0.25))#true settings according to considered scenarios
p_ep1new<-merge(p_ep1,pval_ep1,by = "Group")
#bias
summary_ep1 <- p_ep1new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep1$m,3,4),digits = 2)
#se
summary_ep1 <- p_ep1new %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep1$m*10^3,3,4),digits = 2)
#rmse
summary_ep1 <- p_ep1new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep1$m,3,4),digits = 3)

method<-c("BHM","GCBHM_L","GCBHM_M")
response<-"CR"
res_eff<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))


p_ep2<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 2),
             dplyr::filter(p_convert_gcbhml,Endpoint == 2),
             dplyr::filter(p_convert_gcbhmm,Endpoint == 2))
pval_ep2<-data.frame(Group=levels(factor(p_ep2$Group)),
                     p_true=c(0.5,0.5,0.5,0.5))
p_ep2new<-merge(p_ep2,pval_ep2,by = "Group")

#bias
summary_ep2 <- p_ep2new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep2$m,3,4),digits = 2)
#se
summary_ep2 <- p_ep2new %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep2$m*10^3,3,4),digits = 2)
#rmse
summary_ep2 <- p_ep2new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep2$m,3,4),digits = 3)

method<-c("BHM","GCBHM_L","GCBHM_M")
response<-"CR/PR"
res_tox<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))

tabS2<-rbind(tabS2,cbind(Scenario=scenario, rbind(res_eff,res_tox)))

############################################# Scenario 3 #################################################
scenario=3
load("Rdata/tableS5/bhm_9.Rdata")
p_hat_bhm<-p_hat
load("Rdata/tableS5/gcbhmm_9.Rdata")
p_hat_gcbhmm<-p_hat
load("Rdata/tableS5/gcbhml_9.Rdata")
p_hat_gcbhl1<-p1_hat_tmp
p_hat_gcbhl2<-p2_hat_tmp

p_convert_bhm<-data.frame(convert.list(p_hat_bhm,3),Design="1BHM")
p_convert_gcbhmm<-data.frame(convert.list(p_hat_gcbhmm,3),Design="3GCBHM_m")
p_convert_gcbhml<-rbind(data.frame(convert.list(p_hat_gcbhl1,2),
                                   Design="2GCBHM_l",
                                   Endpoint=factor("1",levels=c("1","2"))),
                        data.frame(convert.list(p_hat_gcbhl2,2),
                                   Design="2GCBHM_l",
                                   Endpoint=factor("2",levels=c("1","2"))))


p_ep1<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 1),
             dplyr::filter(p_convert_gcbhml,Endpoint == 1),
             dplyr::filter(p_convert_gcbhmm,Endpoint == 1))
pval_ep1<-data.frame(Group=levels(factor(p_ep1$Group)),
                     p_true=c(0.15,0.15,0.15,0.15))#true settings according to considered scenarios
p_ep1new<-merge(p_ep1,pval_ep1,by = "Group")
#bias
summary_ep1 <- p_ep1new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep1$m,3,4),digits = 2)
#se
summary_ep1 <- p_ep1new %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep1$m*10^3,3,4),digits = 2)
#rmse
summary_ep1 <- p_ep1new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep1$m,3,4),digits = 3)

method<-c("BHM","GCBHM_L","GCBHM_M")
response<-"CR"
res_eff<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))


p_ep2<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 2),
             dplyr::filter(p_convert_gcbhml,Endpoint == 2),
             dplyr::filter(p_convert_gcbhmm,Endpoint == 2))
pval_ep2<-data.frame(Group=levels(factor(p_ep2$Group)),
                     p_true=c(0.5,0.5,0.5,0.5))
p_ep2new<-merge(p_ep2,pval_ep2,by = "Group")

#bias
summary_ep2 <- p_ep2new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep2$m,3,4),digits = 2)
#se
summary_ep2 <- p_ep2new %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep2$m*10^3,3,4),digits = 2)
#rmse
summary_ep2 <- p_ep2new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep2$m,3,4),digits = 3)

method<-c("BHM","GCBHM_L","GCBHM_M")
response<-"CR/PR"
res_tox<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))

tabS2<-rbind(tabS2,cbind(Scenario=scenario, rbind(res_eff,res_tox)))

############################################# Scenario 4 #################################################
scenario=4
load("Rdata/tableS5/bhm_12.Rdata")
p_hat_bhm<-p_hat
load("Rdata/tableS5/gcbhmm_12.Rdata")
p_hat_gcbhmm<-p_hat
load("Rdata/tableS5/gcbhml_12.Rdata")
p_hat_gcbhl1<-p1_hat_tmp
p_hat_gcbhl2<-p2_hat_tmp

p_convert_bhm<-data.frame(convert.list(p_hat_bhm,3),Design="1BHM")
p_convert_gcbhmm<-data.frame(convert.list(p_hat_gcbhmm,3),Design="3GCBHM_m")
p_convert_gcbhml<-rbind(data.frame(convert.list(p_hat_gcbhl1,2),
                                   Design="2GCBHM_l",
                                   Endpoint=factor("1",levels=c("1","2"))),
                        data.frame(convert.list(p_hat_gcbhl2,2),
                                   Design="2GCBHM_l",
                                   Endpoint=factor("2",levels=c("1","2"))))


p_ep1<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 1),
             dplyr::filter(p_convert_gcbhml,Endpoint == 1),
             dplyr::filter(p_convert_gcbhmm,Endpoint == 1))
pval_ep1<-data.frame(Group=levels(factor(p_ep1$Group)),
                     p_true=c(0.25,0.25,0.25,0.25))#true settings according to considered scenarios
p_ep1new<-merge(p_ep1,pval_ep1,by = "Group")
#bias
summary_ep1 <- p_ep1new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep1$m,3,4),digits = 2)
#se
summary_ep1 <- p_ep1new %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep1$m*10^3,3,4),digits = 2)
#rmse
summary_ep1 <- p_ep1new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep1$m,3,4),digits = 3)

method<-c("BHM","GCBHM_L","GCBHM_M")
response<-"CR"
res_eff<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))


p_ep2<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 2),
             dplyr::filter(p_convert_gcbhml,Endpoint == 2),
             dplyr::filter(p_convert_gcbhmm,Endpoint == 2))
pval_ep2<-data.frame(Group=levels(factor(p_ep2$Group)),
                     p_true=c(0.4,0.5,0.6,0.7))
p_ep2new<-merge(p_ep2,pval_ep2,by = "Group")

#bias
summary_ep2 <- p_ep2new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep2$m,3,4),digits = 2)
#se
summary_ep2 <- p_ep2new %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep2$m*10^3,3,4),digits = 2)
#rmse
summary_ep2 <- p_ep2new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep2$m,3,4),digits = 3)

method<-c("BHM","GCBHM_L","GCBHM_M")
response<-"CR/PR"
res_tox<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))

tabS2<-rbind(tabS2,cbind(Scenario=scenario, rbind(res_eff,res_tox)))

############################################# Scenario 5 #################################################
scenario=5
load("Rdata/tableS5/bhm_13.Rdata")
p_hat_bhm<-p_hat
load("Rdata/tableS5/gcbhmm_13.Rdata")
p_hat_gcbhmm<-p_hat
load("Rdata/tableS5/gcbhml_13.Rdata")
p_hat_gcbhl1<-p1_hat_tmp
p_hat_gcbhl2<-p2_hat_tmp

p_convert_bhm<-data.frame(convert.list(p_hat_bhm,3),Design="1BHM")
p_convert_gcbhmm<-data.frame(convert.list(p_hat_gcbhmm,3),Design="3GCBHM_m")
p_convert_gcbhml<-rbind(data.frame(convert.list(p_hat_gcbhl1,2),
                                   Design="2GCBHM_l",
                                   Endpoint=factor("1",levels=c("1","2"))),
                        data.frame(convert.list(p_hat_gcbhl2,2),
                                   Design="2GCBHM_l",
                                   Endpoint=factor("2",levels=c("1","2"))))


p_ep1<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 1),
             dplyr::filter(p_convert_gcbhml,Endpoint == 1),
             dplyr::filter(p_convert_gcbhmm,Endpoint == 1))
pval_ep1<-data.frame(Group=levels(factor(p_ep1$Group)),
                     p_true=c(0.15,0.15,0.25,0.25))#true settings according to considered scenarios
p_ep1new<-merge(p_ep1,pval_ep1,by = "Group")
#bias
summary_ep1 <- p_ep1new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep1$m,3,4),digits = 2)
#se
summary_ep1 <- p_ep1new %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep1$m*10^3,3,4),digits = 2)
#rmse
summary_ep1 <- p_ep1new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep1$m,3,4),digits = 3)

method<-c("BHM","GCBHM_L","GCBHM_M")
response<-"CR"
res_eff<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))


p_ep2<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 2),
             dplyr::filter(p_convert_gcbhml,Endpoint == 2),
             dplyr::filter(p_convert_gcbhmm,Endpoint == 2))
pval_ep2<-data.frame(Group=levels(factor(p_ep2$Group)),
                       p_true=c(0.3,0.3,0.5,0.5))
p_ep2new<-merge(p_ep2,pval_ep2,by = "Group")

#bias
summary_ep2 <- p_ep2new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep2$m,3,4),digits = 2)
#se
summary_ep2 <- p_ep2new %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep2$m*10^3,3,4),digits = 2)
#rmse
summary_ep2 <- p_ep2new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep2$m,3,4),digits = 3)

method<-c("BHM","GCBHM_L","GCBHM_M")
response<-"CR/PR"
res_tox<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))

tabS2<-rbind(tabS2,cbind(Scenario=scenario, rbind(res_eff,res_tox)))

############################################# Scenario 6 #################################################
scenario=6
load("Rdata/tableS5/bhm_14.Rdata")
p_hat_bhm<-p_hat
load("Rdata/tableS5/gcbhmm_14.Rdata")
p_hat_gcbhmm<-p_hat
load("Rdata/tableS5/gcbhml_14.Rdata")
p_hat_gcbhl1<-p1_hat_tmp
p_hat_gcbhl2<-p2_hat_tmp

p_convert_bhm<-data.frame(convert.list(p_hat_bhm,3),Design="1BHM")
p_convert_gcbhmm<-data.frame(convert.list(p_hat_gcbhmm,3),Design="3GCBHM_m")
p_convert_gcbhml<-rbind(data.frame(convert.list(p_hat_gcbhl1,2),
                                   Design="2GCBHM_l",
                                   Endpoint=factor("1",levels=c("1","2"))),
                        data.frame(convert.list(p_hat_gcbhl2,2),
                                   Design="2GCBHM_l",
                                   Endpoint=factor("2",levels=c("1","2"))))


p_ep1<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 1),
             dplyr::filter(p_convert_gcbhml,Endpoint == 1),
             dplyr::filter(p_convert_gcbhmm,Endpoint == 1))
pval_ep1<-data.frame(Group=levels(factor(p_ep1$Group)),
                     p_true=c(0.15,0.25,0.25,0.25))#true settings according to considered scenarios
p_ep1new<-merge(p_ep1,pval_ep1,by = "Group")
#bias
summary_ep1 <- p_ep1new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep1$m,3,4),digits = 2)
#se
summary_ep1 <- p_ep1new %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep1$m*10^3,3,4),digits = 2)
#rmse
summary_ep1 <- p_ep1new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep1$m,3,4),digits = 3)

method<-c("BHM","GCBHM_L","GCBHM_M")
response<-"CR"
res_eff<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))


p_ep2<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 2),
             dplyr::filter(p_convert_gcbhml,Endpoint == 2),
             dplyr::filter(p_convert_gcbhmm,Endpoint == 2))
pval_ep2<-data.frame(Group=levels(factor(p_ep2$Group)),
                     p_true=c(0.3,0.5,0.5,0.5))
p_ep2new<-merge(p_ep2,pval_ep2,by = "Group")

#bias
summary_ep2 <- p_ep2new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep2$m,3,4),digits = 2)
#se
summary_ep2 <- p_ep2new %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep2$m*10^3,3,4),digits = 2)
#rmse
summary_ep2 <- p_ep2new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep2$m,3,4),digits = 3)

method<-c("BHM","GCBHM_L","GCBHM_M")
response<-"CR/PR"
res_tox<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))

tabS2<-rbind(tabS2,cbind(Scenario=scenario, rbind(res_eff,res_tox)))

############################################# Scenario 7 #################################################
scenario=7
load("Rdata/tableS5/bhm_15.Rdata")
p_hat_bhm<-p_hat
load("Rdata/tableS5/gcbhmm_15.Rdata")
p_hat_gcbhmm<-p_hat
load("Rdata/tableS5/gcbhml_15.Rdata")
p_hat_gcbhl1<-p1_hat_tmp
p_hat_gcbhl2<-p2_hat_tmp

p_convert_bhm<-data.frame(convert.list(p_hat_bhm,3),Design="1BHM")
p_convert_gcbhmm<-data.frame(convert.list(p_hat_gcbhmm,3),Design="3GCBHM_m")
p_convert_gcbhml<-rbind(data.frame(convert.list(p_hat_gcbhl1,2),
                                   Design="2GCBHM_l",
                                   Endpoint=factor("1",levels=c("1","2"))),
                        data.frame(convert.list(p_hat_gcbhl2,2),
                                   Design="2GCBHM_l",
                                   Endpoint=factor("2",levels=c("1","2"))))


p_ep1<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 1),
             dplyr::filter(p_convert_gcbhml,Endpoint == 1),
             dplyr::filter(p_convert_gcbhmm,Endpoint == 1))
pval_ep1<-data.frame(Group=levels(factor(p_ep1$Group)),
                     p_true=c(0.15,0.15,0.25,0.25))#true settings according to considered scenarios
p_ep1new<-merge(p_ep1,pval_ep1,by = "Group")
#bias
summary_ep1 <- p_ep1new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep1$m,3,4),digits = 2)
#se
summary_ep1 <- p_ep1new %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep1$m*10^3,3,4),digits = 2)
#rmse
summary_ep1 <- p_ep1new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep1$m,3,4),digits = 3)

method<-c("BHM","GCBHM_L","GCBHM_M")
response<-"CR"
res_eff<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))


p_ep2<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 2),
             dplyr::filter(p_convert_gcbhml,Endpoint == 2),
             dplyr::filter(p_convert_gcbhmm,Endpoint == 2))
pval_ep2<-data.frame(Group=levels(factor(p_ep2$Group)),
                     p_true=c(0.3,0.3,0.6,0.6))
p_ep2new<-merge(p_ep2,pval_ep2,by = "Group")

#bias
summary_ep2 <- p_ep2new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep2$m,3,4),digits = 2)
#se
summary_ep2 <- p_ep2new %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep2$m*10^3,3,4),digits = 2)
#rmse
summary_ep2 <- p_ep2new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep2$m,3,4),digits = 3)

method<-c("BHM","GCBHM_L","GCBHM_M")
response<-"CR/PR"
res_tox<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))

tabS2<-rbind(tabS2,cbind(Scenario=scenario, rbind(res_eff,res_tox)))

############################################# Scenario8 #################################################
scenario=8
load("Rdata/tableS5/bhm_16.Rdata")
p_hat_bhm<-p_hat
load("Rdata/tableS5/gcbhmm_16.Rdata")
p_hat_gcbhmm<-p_hat
load("Rdata/tableS5/gcbhml_16.Rdata")
p_hat_gcbhl1<-p1_hat_tmp
p_hat_gcbhl2<-p2_hat_tmp

p_convert_bhm<-data.frame(convert.list(p_hat_bhm,3),Design="1BHM")
p_convert_gcbhmm<-data.frame(convert.list(p_hat_gcbhmm,3),Design="3GCBHM_m")
p_convert_gcbhml<-rbind(data.frame(convert.list(p_hat_gcbhl1,2),
                                   Design="2GCBHM_l",
                                   Endpoint=factor("1",levels=c("1","2"))),
                        data.frame(convert.list(p_hat_gcbhl2,2),
                                   Design="2GCBHM_l",
                                   Endpoint=factor("2",levels=c("1","2"))))


p_ep1<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 1),
             dplyr::filter(p_convert_gcbhml,Endpoint == 1),
             dplyr::filter(p_convert_gcbhmm,Endpoint == 1))
pval_ep1<-data.frame(Group=levels(factor(p_ep1$Group)),
                     p_true=c(0.15,0.15,0.25,0.25))#true settings according to considered scenarios
p_ep1new<-merge(p_ep1,pval_ep1,by = "Group")
#bias
summary_ep1 <- p_ep1new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep1$m,3,4),digits = 2)
#se
summary_ep1 <- p_ep1new %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep1$m*10^3,3,4),digits = 2)
#rmse
summary_ep1 <- p_ep1new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep1$m,3,4),digits = 3)

method<-c("BHM","GCBHM_L","GCBHM_M")
response<-"CR"
res_eff<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))


p_ep2<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 2),
             dplyr::filter(p_convert_gcbhml,Endpoint == 2),
             dplyr::filter(p_convert_gcbhmm,Endpoint == 2))
pval_ep2<-data.frame(Group=levels(factor(p_ep2$Group)),
                     p_true=c(0.3,0.3,0.4,0.5))
p_ep2new<-merge(p_ep2,pval_ep2,by = "Group")

#bias
summary_ep2 <- p_ep2new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep2$m,3,4),digits = 2)
#se
summary_ep2 <- p_ep2new %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep2$m*10^3,3,4),digits = 2)
#rmse
summary_ep2 <- p_ep2new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep2$m,3,4),digits = 3)

method<-c("BHM","GCBHM_L","GCBHM_M")
response<-"CR/PR"
res_tox<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))

tabS2<-rbind(tabS2,cbind(Scenario=scenario, rbind(res_eff,res_tox)))


colnames(tabS2)<-c("Scenario","Response","Method",
                   "Bias_sub1","Bias_sub2","Bias_sub3","Bias_sub4",
                   "SE_sub1","SE_sub2","SE_sub3","SE_sub4",
                   "RMSE_sub1","RMSE_sub2","RMSE_sub3","RMSE_sub4")
write.csv(tabS2,file = "Results/Table_S6.csv")





