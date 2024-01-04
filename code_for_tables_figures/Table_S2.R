#TABLE S2

# > sessionInfo()
# R version 4.1.2 (2021-11-01)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS 14.0
# 
# Matrix products: default
# LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
#   [1] parallel  stats     grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] doParallel_1.0.17 iterators_1.0.14  foreach_1.5.2     dplyr_1.1.2       reshape_0.8.8     R2jags_0.7-1      runjags_2.2.0-3  
# [8] rjags_4-14        coda_0.19-4      
# 
# loaded via a namespace (and not attached):
#   [1] Rcpp_1.0.10       pillar_1.9.0      compiler_4.1.2    plyr_1.8.6        tools_4.1.2       boot_1.3-28.1     lifecycle_1.0.3  
# [8] tibble_3.2.1      gtable_0.3.1      lattice_0.20-45   pkgconfig_2.0.3   rlang_1.1.1       cli_3.6.0         rstudioapi_0.15.0
# [15] withr_2.5.0       xml2_1.3.3        systemfonts_1.0.4 generics_0.1.1    vctrs_0.6.3       graphics_4.1.2    grid_4.1.2       
# [22] tidyselect_1.2.0  gridtext_0.1.4    glue_1.6.2        R6_2.5.1          textshaping_0.3.6 fansi_1.0.3       farver_2.1.1     
# [29] ggplot2_3.4.3     magrittr_2.0.3    codetools_0.2-18  scales_1.2.1      R2WinBUGS_2.1-21  abind_1.4-5       colorspace_2.0-3 
# [36] ragg_1.2.5        utf8_1.2.2        munsell_0.5.0    


library(reshape)
library(dplyr)

############################################ Functions ################################################
convert.list<-function(p,d){
  dims<-dim(p)
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
p_label <- function(m,i,n){
  p_hat <- substitute(bold(hat(p)[i_label] == m_label),
                      list(m_label = sprintf("%0.2f", m[i]),
                           i_label = ceiling(i/n)))
  as.character(as.expression(p_hat));
}
##rmse
rmse<-function(phat,p,n){
  sqrt(sum((phat-p)^2)/n)
}
############################################# Scenario 1 #################################################
scenario=1
load("Rdata/table1/bhm_ig_1.Rdata")
p_hat_bhmE<-pE_hat_tmp
p_hat_bhmT<-pT_hat_tmp
load("Rdata/table1/cbhm_1.Rdata")
p_hat_cbhmeE<-p_hat_tmpE
load("Rdata/table1/cbhmtox_1.Rdata")
p_hat_cbhme_toxE<-p_hat_tmpE
p_hat_cbhme_toxT<-p_hat_tmpT
load("Rdata/table1/gcbhm_1.Rdata")
p_hat_cbhmlE<-pE_hat_tmp
p_hat_cbhmlT<-pT_hat_tmp

p_convert_bhm<-rbind(data.frame(convert.list(p_hat_bhmE,2),
                                Design="1BHM",
                                Endpoint=factor("1",levels=c("1","2"))),
                     data.frame(convert.list(p_hat_bhmT,2),
                                Design="1BHM",
                                Endpoint=factor("2",levels=c("1","2"))))
p_convert_cbhme<-data.frame(convert.list(p_hat_cbhmeE,2),
                            Design="2CBHMe",
                            Endpoint=factor("1",levels=c("1","2")))
p_convert_cbhmetox<-rbind(data.frame(convert.list(p_hat_cbhme_toxE,2),
                                     Design="3CBHM-Tox",
                                     Endpoint=factor("1",levels=c("1","2"))),
                          data.frame(convert.list(p_hat_cbhme_toxT,2),
                                     Design="3CBHM-Tox",
                                     Endpoint=factor("2",levels=c("1","2"))))
p_convert_cbhml<-rbind(data.frame(convert.list(p_hat_cbhmlE,2),
                                  Design="5CBHM_l",
                                  Endpoint=factor("1",levels=c("1","2"))),
                       data.frame(convert.list(p_hat_cbhmlT,2),
                                  Design="5CBHM_l",
                                  Endpoint=factor("2",levels=c("1","2"))))


p_ep1_1<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 1),
               dplyr::filter(p_convert_cbhme,Endpoint == 1),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 1),
               dplyr::filter(p_convert_cbhml,Endpoint == 1)
)
pval_ep1_1<-data.frame(Group=levels(factor(p_ep1_1$Group)),p_true=c(0.45,0.45,0.45,0.45))
p_ep1_1new<-merge(p_ep1_1,pval_ep1_1,by = "Group")
#bias
summary_ep1_1 <- p_ep1_1new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep1_1$m,4,4),digits = 2)
#se
summary_ep1_1 <- p_ep1_1 %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep1_1$m*10^3,4,4),digits = 2)
#rmse
summary_ep1_1 <- p_ep1_1new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep1_1$m,4,4),digits = 3)

method<-c("BHM_IG","CBHM_E","CBHM_ET","GCBHM_L")
response<-"Efficacy"
res_eff<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))


p_ep2_1<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 2),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 2),
               dplyr::filter(p_convert_cbhml,Endpoint == 2))
pval_ep2_1<-data.frame(Group=levels(factor(p_ep2_1$Group)),p_true=c(0.30,0.30,0.30,0.30))
p_ep2_1new<-merge(p_ep2_1,pval_ep2_1,by = "Group")
#bias
summary_ep2_1 <- p_ep2_1new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep2_1$m,3,4),digits = 2)
#se
summary_ep2_1 <- p_ep2_1 %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep2_1$m*10^3,3,4),digits = 2)

#rmse
summary_ep2_1 <- p_ep2_1new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep2_1$m,3,4),digits = 3)

method<-c("BHM_IG","CBHM_ET","GCBHM_L")
response<-"Toxicity"
res_tox<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))

tabS2<-cbind(Scenario=scenario, rbind(res_eff,res_tox))

############################################# Scenario 2 #################################################
scenario=2
load("Rdata/table1/bhm_ig_10.Rdata")
p_hat_bhmE<-pE_hat_tmp
p_hat_bhmT<-pT_hat_tmp
load("Rdata/table1/cbhm_10.Rdata")
p_hat_cbhmeE<-p_hat_tmpE
load("Rdata/table1/cbhmtox_10.Rdata")
p_hat_cbhme_toxE<-p_hat_tmpE
p_hat_cbhme_toxT<-p_hat_tmpT
load("Rdata/table1/gcbhm_10.Rdata")
p_hat_cbhmlE<-pE_hat_tmp
p_hat_cbhmlT<-pT_hat_tmp

p_convert_bhm<-rbind(data.frame(convert.list(p_hat_bhmE,2),
                                Design="1BHM",
                                Endpoint=factor("1",levels=c("1","2"))),
                     data.frame(convert.list(p_hat_bhmT,2),
                                Design="1BHM",
                                Endpoint=factor("2",levels=c("1","2"))))
p_convert_cbhme<-data.frame(convert.list(p_hat_cbhmeE,2),
                            Design="2CBHMe",
                            Endpoint=factor("1",levels=c("1","2")))
p_convert_cbhmetox<-rbind(data.frame(convert.list(p_hat_cbhme_toxE,2),
                                     Design="3CBHM-Tox",
                                     Endpoint=factor("1",levels=c("1","2"))),
                          data.frame(convert.list(p_hat_cbhme_toxT,2),
                                     Design="3CBHM-Tox",
                                     Endpoint=factor("2",levels=c("1","2"))))
p_convert_cbhml<-rbind(data.frame(convert.list(p_hat_cbhmlE,2),
                                  Design="5CBHM_l",
                                  Endpoint=factor("1",levels=c("1","2"))),
                       data.frame(convert.list(p_hat_cbhmlT,2),
                                  Design="5CBHM_l",
                                  Endpoint=factor("2",levels=c("1","2"))))


p_ep1_1<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 1),
               dplyr::filter(p_convert_cbhme,Endpoint == 1),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 1),
               dplyr::filter(p_convert_cbhml,Endpoint == 1)
)
pval_ep1_1<-data.frame(Group=levels(factor(p_ep1_1$Group)),p_true=c(0.5,0.5,0.5,0.5))
p_ep1_1new<-merge(p_ep1_1,pval_ep1_1,by = "Group")
#bias
summary_ep1_1 <- p_ep1_1new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep1_1$m,4,4),digits = 2)
#se
summary_ep1_1 <- p_ep1_1 %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep1_1$m*10^3,4,4),digits = 2)
#rmse
summary_ep1_1 <- p_ep1_1new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep1_1$m,4,4),digits = 3)

method<-c("BHM_IG","CBHM_E","CBHM_ET","GCBHM_L")
response<-"Efficacy"
res_eff<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))


p_ep2_1<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 2),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 2),
               dplyr::filter(p_convert_cbhml,Endpoint == 2))
pval_ep2_1<-data.frame(Group=levels(factor(p_ep2_1$Group)),p_true=c(0.20,0.20,0.20,0.20))
p_ep2_1new<-merge(p_ep2_1,pval_ep2_1,by = "Group")
#bias
summary_ep2_1 <- p_ep2_1new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep2_1$m,3,4),digits = 2)
#se
summary_ep2_1 <- p_ep2_1 %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep2_1$m*10^3,3,4),digits = 2)

#rmse
summary_ep2_1 <- p_ep2_1new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep2_1$m,3,4),digits = 3)

method<-c("BHM_IG","CBHM_ET","GCBHM_L")
response<-"Toxicity"
res_tox<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))

tabS2<-rbind(tabS2,cbind(Scenario=scenario, rbind(res_eff,res_tox)))

############################################# Scenario 3 #################################################
scenario=3
load("Rdata/table1/bhm_ig_12.Rdata")
p_hat_bhmE<-pE_hat_tmp
p_hat_bhmT<-pT_hat_tmp
load("Rdata/table1/cbhm_12.Rdata")
p_hat_cbhmeE<-p_hat_tmpE
load("Rdata/table1/cbhmtox_12.Rdata")
p_hat_cbhme_toxE<-p_hat_tmpE
p_hat_cbhme_toxT<-p_hat_tmpT
load("Rdata/table1/gcbhm_12.Rdata")
p_hat_cbhmlE<-pE_hat_tmp
p_hat_cbhmlT<-pT_hat_tmp

p_convert_bhm<-rbind(data.frame(convert.list(p_hat_bhmE,2),
                                Design="1BHM",
                                Endpoint=factor("1",levels=c("1","2"))),
                     data.frame(convert.list(p_hat_bhmT,2),
                                Design="1BHM",
                                Endpoint=factor("2",levels=c("1","2"))))
p_convert_cbhme<-data.frame(convert.list(p_hat_cbhmeE,2),
                            Design="2CBHMe",
                            Endpoint=factor("1",levels=c("1","2")))
p_convert_cbhmetox<-rbind(data.frame(convert.list(p_hat_cbhme_toxE,2),
                                     Design="3CBHM-Tox",
                                     Endpoint=factor("1",levels=c("1","2"))),
                          data.frame(convert.list(p_hat_cbhme_toxT,2),
                                     Design="3CBHM-Tox",
                                     Endpoint=factor("2",levels=c("1","2"))))
p_convert_cbhml<-rbind(data.frame(convert.list(p_hat_cbhmlE,2),
                                  Design="5CBHM_l",
                                  Endpoint=factor("1",levels=c("1","2"))),
                       data.frame(convert.list(p_hat_cbhmlT,2),
                                  Design="5CBHM_l",
                                  Endpoint=factor("2",levels=c("1","2"))))


p_ep1_1<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 1),
               dplyr::filter(p_convert_cbhme,Endpoint == 1),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 1),
               dplyr::filter(p_convert_cbhml,Endpoint == 1)
)
pval_ep1_1<-data.frame(Group=levels(factor(p_ep1_1$Group)),p_true=c(0.6,0.6,0.6,0.6))
p_ep1_1new<-merge(p_ep1_1,pval_ep1_1,by = "Group")
#bias
summary_ep1_1 <- p_ep1_1new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep1_1$m,4,4),digits = 2)
#se
summary_ep1_1 <- p_ep1_1 %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep1_1$m*10^3,4,4),digits = 2)
#rmse
summary_ep1_1 <- p_ep1_1new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep1_1$m,4,4),digits = 3)

method<-c("BHM_IG","CBHM_E","CBHM_ET","GCBHM_L")
response<-"Efficacy"
res_eff<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))


p_ep2_1<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 2),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 2),
               dplyr::filter(p_convert_cbhml,Endpoint == 2))
pval_ep2_1<-data.frame(Group=levels(factor(p_ep2_1$Group)),p_true=c(0.20,0.20,0.20,0.20))
p_ep2_1new<-merge(p_ep2_1,pval_ep2_1,by = "Group")
#bias
summary_ep2_1 <- p_ep2_1new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep2_1$m,3,4),digits = 2)
#se
summary_ep2_1 <- p_ep2_1 %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep2_1$m*10^3,3,4),digits = 2)

#rmse
summary_ep2_1 <- p_ep2_1new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep2_1$m,3,4),digits = 3)

method<-c("BHM_IG","CBHM_ET","GCBHM_L")
response<-"Toxicity"
res_tox<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))

tabS2<-rbind(tabS2,cbind(Scenario=scenario, rbind(res_eff,res_tox)))

############################################# Scenario 4 #################################################
scenario=4
load("Rdata/table1/bhm_ig_37.Rdata")
p_hat_bhmE<-pE_hat_tmp
p_hat_bhmT<-pT_hat_tmp
load("Rdata/table1/cbhm_37.Rdata")
p_hat_cbhmeE<-p_hat_tmpE
load("Rdata/table1/cbhmtox_37.Rdata")
p_hat_cbhme_toxE<-p_hat_tmpE
p_hat_cbhme_toxT<-p_hat_tmpT
load("Rdata/table1/gcbhm_37.Rdata")
p_hat_cbhmlE<-pE_hat_tmp
p_hat_cbhmlT<-pT_hat_tmp

p_convert_bhm<-rbind(data.frame(convert.list(p_hat_bhmE,2),
                                Design="1BHM",
                                Endpoint=factor("1",levels=c("1","2"))),
                     data.frame(convert.list(p_hat_bhmT,2),
                                Design="1BHM",
                                Endpoint=factor("2",levels=c("1","2"))))
p_convert_cbhme<-data.frame(convert.list(p_hat_cbhmeE,2),
                            Design="2CBHMe",
                            Endpoint=factor("1",levels=c("1","2")))
p_convert_cbhmetox<-rbind(data.frame(convert.list(p_hat_cbhme_toxE,2),
                                     Design="3CBHM-Tox",
                                     Endpoint=factor("1",levels=c("1","2"))),
                          data.frame(convert.list(p_hat_cbhme_toxT,2),
                                     Design="3CBHM-Tox",
                                     Endpoint=factor("2",levels=c("1","2"))))
p_convert_cbhml<-rbind(data.frame(convert.list(p_hat_cbhmlE,2),
                                  Design="5CBHM_l",
                                  Endpoint=factor("1",levels=c("1","2"))),
                       data.frame(convert.list(p_hat_cbhmlT,2),
                                  Design="5CBHM_l",
                                  Endpoint=factor("2",levels=c("1","2"))))


p_ep1_1<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 1),
               dplyr::filter(p_convert_cbhme,Endpoint == 1),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 1),
               dplyr::filter(p_convert_cbhml,Endpoint == 1)
)
pval_ep1_1<-data.frame(Group=levels(factor(p_ep1_1$Group)),p_true=c(0.5,0.5,0.5,0.5))
p_ep1_1new<-merge(p_ep1_1,pval_ep1_1,by = "Group")
#bias
summary_ep1_1 <- p_ep1_1new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep1_1$m,4,4),digits = 2)
#se
summary_ep1_1 <- p_ep1_1 %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep1_1$m*10^3,4,4),digits = 2)
#rmse
summary_ep1_1 <- p_ep1_1new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep1_1$m,4,4),digits = 3)

method<-c("BHM_IG","CBHM_E","CBHM_ET","GCBHM_L")
response<-"Efficacy"
res_eff<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))


p_ep2_1<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 2),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 2),
               dplyr::filter(p_convert_cbhml,Endpoint == 2))
pval_ep2_1<-data.frame(Group=levels(factor(p_ep2_1$Group)),p_true=c(0.12,0.15,0.20,0.25))
p_ep2_1new<-merge(p_ep2_1,pval_ep2_1,by = "Group")
#bias
summary_ep2_1 <- p_ep2_1new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep2_1$m,3,4),digits = 2)
#se
summary_ep2_1 <- p_ep2_1 %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep2_1$m*10^3,3,4),digits = 2)

#rmse
summary_ep2_1 <- p_ep2_1new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep2_1$m,3,4),digits = 3)

method<-c("BHM_IG","CBHM_ET","GCBHM_L")
response<-"Toxicity"
res_tox<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))

tabS2<-rbind(tabS2,cbind(Scenario=scenario, rbind(res_eff,res_tox)))

############################################# Scenario 5 #################################################
scenario=5
load("Rdata/table1/bhm_ig_39.Rdata")
p_hat_bhmE<-pE_hat_tmp
p_hat_bhmT<-pT_hat_tmp
load("Rdata/table1/cbhm_39.Rdata")
p_hat_cbhmeE<-p_hat_tmpE
load("Rdata/table1/cbhmtox_39.Rdata")
p_hat_cbhme_toxE<-p_hat_tmpE
p_hat_cbhme_toxT<-p_hat_tmpT
load("Rdata/table1/gcbhm_39.Rdata")
p_hat_cbhmlE<-pE_hat_tmp
p_hat_cbhmlT<-pT_hat_tmp

p_convert_bhm<-rbind(data.frame(convert.list(p_hat_bhmE,2),
                                Design="1BHM",
                                Endpoint=factor("1",levels=c("1","2"))),
                     data.frame(convert.list(p_hat_bhmT,2),
                                Design="1BHM",
                                Endpoint=factor("2",levels=c("1","2"))))
p_convert_cbhme<-data.frame(convert.list(p_hat_cbhmeE,2),
                            Design="2CBHMe",
                            Endpoint=factor("1",levels=c("1","2")))
p_convert_cbhmetox<-rbind(data.frame(convert.list(p_hat_cbhme_toxE,2),
                                     Design="3CBHM-Tox",
                                     Endpoint=factor("1",levels=c("1","2"))),
                          data.frame(convert.list(p_hat_cbhme_toxT,2),
                                     Design="3CBHM-Tox",
                                     Endpoint=factor("2",levels=c("1","2"))))
p_convert_cbhml<-rbind(data.frame(convert.list(p_hat_cbhmlE,2),
                                  Design="5CBHM_l",
                                  Endpoint=factor("1",levels=c("1","2"))),
                       data.frame(convert.list(p_hat_cbhmlT,2),
                                  Design="5CBHM_l",
                                  Endpoint=factor("2",levels=c("1","2"))))


p_ep1_1<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 1),
               dplyr::filter(p_convert_cbhme,Endpoint == 1),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 1),
               dplyr::filter(p_convert_cbhml,Endpoint == 1)
)
pval_ep1_1<-data.frame(Group=levels(factor(p_ep1_1$Group)),p_true=c(0.5,0.55,0.60,0.65))
p_ep1_1new<-merge(p_ep1_1,pval_ep1_1,by = "Group")
#bias
summary_ep1_1 <- p_ep1_1new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep1_1$m,4,4),digits = 2)
#se
summary_ep1_1 <- p_ep1_1 %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep1_1$m*10^3,4,4),digits = 2)
#rmse
summary_ep1_1 <- p_ep1_1new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep1_1$m,4,4),digits = 3)

method<-c("BHM_IG","CBHM_E","CBHM_ET","GCBHM_L")
response<-"Efficacy"
res_eff<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))


p_ep2_1<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 2),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 2),
               dplyr::filter(p_convert_cbhml,Endpoint == 2))
pval_ep2_1<-data.frame(Group=levels(factor(p_ep2_1$Group)),p_true=c(0.25,0.25,0.25,0.25))
p_ep2_1new<-merge(p_ep2_1,pval_ep2_1,by = "Group")
#bias
summary_ep2_1 <- p_ep2_1new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep2_1$m,3,4),digits = 2)
#se
summary_ep2_1 <- p_ep2_1 %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep2_1$m*10^3,3,4),digits = 2)

#rmse
summary_ep2_1 <- p_ep2_1new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep2_1$m,3,4),digits = 3)

method<-c("BHM_IG","CBHM_ET","GCBHM_L")
response<-"Toxicity"
res_tox<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))

tabS2<-rbind(tabS2,cbind(Scenario=scenario, rbind(res_eff,res_tox)))

############################################# Scenario 6 #################################################
scenario=6
load("Rdata/table1/bhm_ig_28.Rdata")
p_hat_bhmE<-pE_hat_tmp
p_hat_bhmT<-pT_hat_tmp
load("Rdata/table1/cbhm_28.Rdata")
p_hat_cbhmeE<-p_hat_tmpE
load("Rdata/table1/cbhmtox_28.Rdata")
p_hat_cbhme_toxE<-p_hat_tmpE
p_hat_cbhme_toxT<-p_hat_tmpT
load("Rdata/table1/gcbhm_28.Rdata")
p_hat_cbhmlE<-pE_hat_tmp
p_hat_cbhmlT<-pT_hat_tmp

p_convert_bhm<-rbind(data.frame(convert.list(p_hat_bhmE,2),
                                Design="1BHM",
                                Endpoint=factor("1",levels=c("1","2"))),
                     data.frame(convert.list(p_hat_bhmT,2),
                                Design="1BHM",
                                Endpoint=factor("2",levels=c("1","2"))))
p_convert_cbhme<-data.frame(convert.list(p_hat_cbhmeE,2),
                            Design="2CBHMe",
                            Endpoint=factor("1",levels=c("1","2")))
p_convert_cbhmetox<-rbind(data.frame(convert.list(p_hat_cbhme_toxE,2),
                                     Design="3CBHM-Tox",
                                     Endpoint=factor("1",levels=c("1","2"))),
                          data.frame(convert.list(p_hat_cbhme_toxT,2),
                                     Design="3CBHM-Tox",
                                     Endpoint=factor("2",levels=c("1","2"))))
p_convert_cbhml<-rbind(data.frame(convert.list(p_hat_cbhmlE,2),
                                  Design="5CBHM_l",
                                  Endpoint=factor("1",levels=c("1","2"))),
                       data.frame(convert.list(p_hat_cbhmlT,2),
                                  Design="5CBHM_l",
                                  Endpoint=factor("2",levels=c("1","2"))))


p_ep1_1<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 1),
               dplyr::filter(p_convert_cbhme,Endpoint == 1),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 1),
               dplyr::filter(p_convert_cbhml,Endpoint == 1)
)
pval_ep1_1<-data.frame(Group=levels(factor(p_ep1_1$Group)),p_true=c(0.45,0.60,0.60,0.60))
p_ep1_1new<-merge(p_ep1_1,pval_ep1_1,by = "Group")
#bias
summary_ep1_1 <- p_ep1_1new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep1_1$m,4,4),digits = 2)
#se
summary_ep1_1 <- p_ep1_1 %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep1_1$m*10^3,4,4),digits = 2)
#rmse
summary_ep1_1 <- p_ep1_1new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep1_1$m,4,4),digits = 3)

method<-c("BHM_IG","CBHM_E","CBHM_ET","GCBHM_L")
response<-"Efficacy"
res_eff<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))


p_ep2_1<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 2),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 2),
               dplyr::filter(p_convert_cbhml,Endpoint == 2))
pval_ep2_1<-data.frame(Group=levels(factor(p_ep2_1$Group)),p_true=c(0.30,0.20,0.20,0.20))
p_ep2_1new<-merge(p_ep2_1,pval_ep2_1,by = "Group")
#bias
summary_ep2_1 <- p_ep2_1new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep2_1$m,3,4),digits = 2)
#se
summary_ep2_1 <- p_ep2_1 %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep2_1$m*10^3,3,4),digits = 2)

#rmse
summary_ep2_1 <- p_ep2_1new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep2_1$m,3,4),digits = 3)

method<-c("BHM_IG","CBHM_ET","GCBHM_L")
response<-"Toxicity"
res_tox<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))

tabS2<-rbind(tabS2,cbind(Scenario=scenario, rbind(res_eff,res_tox)))

############################################# Scenario 7 #################################################
scenario=7
load("Rdata/table1/bhm_ig_30.Rdata")
p_hat_bhmE<-pE_hat_tmp
p_hat_bhmT<-pT_hat_tmp
load("Rdata/table1/cbhm_30.Rdata")
p_hat_cbhmeE<-p_hat_tmpE
load("Rdata/table1/cbhmtox_30.Rdata")
p_hat_cbhme_toxE<-p_hat_tmpE
p_hat_cbhme_toxT<-p_hat_tmpT
load("Rdata/table1/gcbhm_30.Rdata")
p_hat_cbhmlE<-pE_hat_tmp
p_hat_cbhmlT<-pT_hat_tmp

p_convert_bhm<-rbind(data.frame(convert.list(p_hat_bhmE,2),
                                Design="1BHM",
                                Endpoint=factor("1",levels=c("1","2"))),
                     data.frame(convert.list(p_hat_bhmT,2),
                                Design="1BHM",
                                Endpoint=factor("2",levels=c("1","2"))))
p_convert_cbhme<-data.frame(convert.list(p_hat_cbhmeE,2),
                            Design="2CBHMe",
                            Endpoint=factor("1",levels=c("1","2")))
p_convert_cbhmetox<-rbind(data.frame(convert.list(p_hat_cbhme_toxE,2),
                                     Design="3CBHM-Tox",
                                     Endpoint=factor("1",levels=c("1","2"))),
                          data.frame(convert.list(p_hat_cbhme_toxT,2),
                                     Design="3CBHM-Tox",
                                     Endpoint=factor("2",levels=c("1","2"))))
p_convert_cbhml<-rbind(data.frame(convert.list(p_hat_cbhmlE,2),
                                  Design="5CBHM_l",
                                  Endpoint=factor("1",levels=c("1","2"))),
                       data.frame(convert.list(p_hat_cbhmlT,2),
                                  Design="5CBHM_l",
                                  Endpoint=factor("2",levels=c("1","2"))))


p_ep1_1<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 1),
               dplyr::filter(p_convert_cbhme,Endpoint == 1),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 1),
               dplyr::filter(p_convert_cbhml,Endpoint == 1)
)
pval_ep1_1<-data.frame(Group=levels(factor(p_ep1_1$Group)),p_true=c(0.45,0.45,0.60,0.60))
p_ep1_1new<-merge(p_ep1_1,pval_ep1_1,by = "Group")
#bias
summary_ep1_1 <- p_ep1_1new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep1_1$m,4,4),digits = 2)
#se
summary_ep1_1 <- p_ep1_1 %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep1_1$m*10^3,4,4),digits = 2)
#rmse
summary_ep1_1 <- p_ep1_1new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep1_1$m,4,4),digits = 3)

method<-c("BHM_IG","CBHM_E","CBHM_ET","GCBHM_L")
response<-"Efficacy"
res_eff<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))


p_ep2_1<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 2),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 2),
               dplyr::filter(p_convert_cbhml,Endpoint == 2))
pval_ep2_1<-data.frame(Group=levels(factor(p_ep2_1$Group)),p_true=c(0.30,0.30,0.20,0.20))
p_ep2_1new<-merge(p_ep2_1,pval_ep2_1,by = "Group")
#bias
summary_ep2_1 <- p_ep2_1new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep2_1$m,3,4),digits = 2)
#se
summary_ep2_1 <- p_ep2_1 %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep2_1$m*10^3,3,4),digits = 2)

#rmse
summary_ep2_1 <- p_ep2_1new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep2_1$m,3,4),digits = 3)

method<-c("BHM_IG","CBHM_ET","GCBHM_L")
response<-"Toxicity"
res_tox<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))

tabS2<-rbind(tabS2,cbind(Scenario=scenario, rbind(res_eff,res_tox)))

############################################# Scenario8 #################################################
scenario=8
load("Rdata/table1/bhm_ig_31.Rdata")
p_hat_bhmE<-pE_hat_tmp
p_hat_bhmT<-pT_hat_tmp
load("Rdata/table1/cbhm_31.Rdata")
p_hat_cbhmeE<-p_hat_tmpE
load("Rdata/table1/cbhmtox_31.Rdata")
p_hat_cbhme_toxE<-p_hat_tmpE
p_hat_cbhme_toxT<-p_hat_tmpT
load("Rdata/table1/gcbhm_31.Rdata")
p_hat_cbhmlE<-pE_hat_tmp
p_hat_cbhmlT<-pT_hat_tmp

p_convert_bhm<-rbind(data.frame(convert.list(p_hat_bhmE,2),
                                Design="1BHM",
                                Endpoint=factor("1",levels=c("1","2"))),
                     data.frame(convert.list(p_hat_bhmT,2),
                                Design="1BHM",
                                Endpoint=factor("2",levels=c("1","2"))))
p_convert_cbhme<-data.frame(convert.list(p_hat_cbhmeE,2),
                            Design="2CBHMe",
                            Endpoint=factor("1",levels=c("1","2")))
p_convert_cbhmetox<-rbind(data.frame(convert.list(p_hat_cbhme_toxE,2),
                                     Design="3CBHM-Tox",
                                     Endpoint=factor("1",levels=c("1","2"))),
                          data.frame(convert.list(p_hat_cbhme_toxT,2),
                                     Design="3CBHM-Tox",
                                     Endpoint=factor("2",levels=c("1","2"))))
p_convert_cbhml<-rbind(data.frame(convert.list(p_hat_cbhmlE,2),
                                  Design="5CBHM_l",
                                  Endpoint=factor("1",levels=c("1","2"))),
                       data.frame(convert.list(p_hat_cbhmlT,2),
                                  Design="5CBHM_l",
                                  Endpoint=factor("2",levels=c("1","2"))))


p_ep1_1<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 1),
               dplyr::filter(p_convert_cbhme,Endpoint == 1),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 1),
               dplyr::filter(p_convert_cbhml,Endpoint == 1)
)
pval_ep1_1<-data.frame(Group=levels(factor(p_ep1_1$Group)),p_true=c(0.45,0.45,0.55,0.60))
p_ep1_1new<-merge(p_ep1_1,pval_ep1_1,by = "Group")
#bias
summary_ep1_1 <- p_ep1_1new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep1_1$m,4,4),digits = 2)
#se
summary_ep1_1 <- p_ep1_1 %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep1_1$m*10^3,4,4),digits = 2)
#rmse
summary_ep1_1 <- p_ep1_1new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep1_1$m,4,4),digits = 3)

method<-c("BHM_IG","CBHM_E","CBHM_ET","GCBHM_L")
response<-"Efficacy"
res_eff<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))


p_ep2_1<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 2),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 2),
               dplyr::filter(p_convert_cbhml,Endpoint == 2))
pval_ep2_1<-data.frame(Group=levels(factor(p_ep2_1$Group)),p_true=c(0.30,0.30,0.25,0.20))
p_ep2_1new<-merge(p_ep2_1,pval_ep2_1,by = "Group")
#bias
summary_ep2_1 <- p_ep2_1new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep2_1$m,3,4),digits = 2)
#se
summary_ep2_1 <- p_ep2_1 %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep2_1$m*10^3,3,4),digits = 2)

#rmse
summary_ep2_1 <- p_ep2_1new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep2_1$m,3,4),digits = 3)

method<-c("BHM_IG","CBHM_ET","GCBHM_L")
response<-"Toxicity"
res_tox<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))

tabS2<-rbind(tabS2,cbind(Scenario=scenario, rbind(res_eff,res_tox)))

############################################# Scenario 9 #################################################
scenario=9
load("Rdata/table1/bhm_ig_34.Rdata")
p_hat_bhmE<-pE_hat_tmp
p_hat_bhmT<-pT_hat_tmp
load("Rdata/table1/cbhm_34.Rdata")
p_hat_cbhmeE<-p_hat_tmpE
load("Rdata/table1/cbhmtox_34.Rdata")
p_hat_cbhme_toxE<-p_hat_tmpE
p_hat_cbhme_toxT<-p_hat_tmpT
load("Rdata/table1/gcbhm_34.Rdata")
p_hat_cbhmlE<-pE_hat_tmp
p_hat_cbhmlT<-pT_hat_tmp

p_convert_bhm<-rbind(data.frame(convert.list(p_hat_bhmE,2),
                                Design="1BHM",
                                Endpoint=factor("1",levels=c("1","2"))),
                     data.frame(convert.list(p_hat_bhmT,2),
                                Design="1BHM",
                                Endpoint=factor("2",levels=c("1","2"))))
p_convert_cbhme<-data.frame(convert.list(p_hat_cbhmeE,2),
                            Design="2CBHMe",
                            Endpoint=factor("1",levels=c("1","2")))
p_convert_cbhmetox<-rbind(data.frame(convert.list(p_hat_cbhme_toxE,2),
                                     Design="3CBHM-Tox",
                                     Endpoint=factor("1",levels=c("1","2"))),
                          data.frame(convert.list(p_hat_cbhme_toxT,2),
                                     Design="3CBHM-Tox",
                                     Endpoint=factor("2",levels=c("1","2"))))
p_convert_cbhml<-rbind(data.frame(convert.list(p_hat_cbhmlE,2),
                                  Design="5CBHM_l",
                                  Endpoint=factor("1",levels=c("1","2"))),
                       data.frame(convert.list(p_hat_cbhmlT,2),
                                  Design="5CBHM_l",
                                  Endpoint=factor("2",levels=c("1","2"))))


p_ep1_1<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 1),
               dplyr::filter(p_convert_cbhme,Endpoint == 1),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 1),
               dplyr::filter(p_convert_cbhml,Endpoint == 1)
)
pval_ep1_1<-data.frame(Group=levels(factor(p_ep1_1$Group)),p_true=c(0.45,0.45,0.45,0.60))
p_ep1_1new<-merge(p_ep1_1,pval_ep1_1,by = "Group")
#bias
summary_ep1_1 <- p_ep1_1new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep1_1$m,4,4),digits = 2)
#se
summary_ep1_1 <- p_ep1_1 %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep1_1$m*10^3,4,4),digits = 2)
#rmse
summary_ep1_1 <- p_ep1_1new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep1_1$m,4,4),digits = 3)

method<-c("BHM_IG","CBHM_E","CBHM_ET","GCBHM_L")
response<-"Efficacy"
res_eff<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))


p_ep2_1<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 2),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 2),
               dplyr::filter(p_convert_cbhml,Endpoint == 2))
pval_ep2_1<-data.frame(Group=levels(factor(p_ep2_1$Group)),p_true=c(0.30,0.30,0.30,0.20))
p_ep2_1new<-merge(p_ep2_1,pval_ep2_1,by = "Group")
#bias
summary_ep2_1 <- p_ep2_1new %>% group_by(Group, Design) %>% summarize(m=mean(p_est)-mean(p_true))
bias<-round(matrix(summary_ep2_1$m,3,4),digits = 2)
#se
summary_ep2_1 <- p_ep2_1 %>% group_by(Group, Design) %>% summarize(m=sd(p_est)/sqrt(length(n[1,])))
se<-round(matrix(summary_ep2_1$m*10^3,3,4),digits = 2)

#rmse
summary_ep2_1 <- p_ep2_1new %>% group_by(Group, Design) %>% summarize(m=rmse(p_est,p_true,length(n[1,])))
rmse_val<-round(matrix(summary_ep2_1$m,3,4),digits = 3)

method<-c("BHM_IG","CBHM_ET","GCBHM_L")
response<-"Toxicity"
res_tox<-data.frame(Response=response,Method=method,cbind(bias,se,rmse_val))

tabS2<-rbind(tabS2,cbind(Scenario=scenario, rbind(res_eff,res_tox)))


colnames(tabS2)<-c("Scenario","Response","Method",
                   "Bias_sub1","Bias_sub2","Bias_sub3","Bias_sub4",
                   "SE_sub1","SE_sub2","SE_sub3","SE_sub4",
                   "RMSE_sub1","RMSE_sub2","RMSE_sub3","RMSE_sub4")
write.csv(tabS2,file = "Results/Table_S2.csv")





