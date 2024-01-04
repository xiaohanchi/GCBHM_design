#FIGURE S3&S4

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
#   [1] parallel  stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] doParallel_1.0.17  iterators_1.0.14   foreach_1.5.2      patchwork_1.1.1    ggtext_0.1.1       ggplot2_3.4.3     
# [7] RColorBrewer_1.1-3 dplyr_1.1.2        reshape_0.8.8      R2jags_0.7-1       runjags_2.2.0-3    rjags_4-14        
# [13] coda_0.19-4       
# 
# loaded via a namespace (and not attached):
#   [1] Rcpp_1.0.10       pillar_1.9.0      compiler_4.1.2    plyr_1.8.6        tools_4.1.2       boot_1.3-28.1     lifecycle_1.0.3  
# [8] tibble_3.2.1      gtable_0.3.1      lattice_0.20-45   pkgconfig_2.0.3   rlang_1.1.1       cli_3.6.0         rstudioapi_0.15.0
# [15] withr_2.5.0       xml2_1.3.3        systemfonts_1.0.4 generics_0.1.1    vctrs_0.6.3       grid_4.1.2        tidyselect_1.2.0 
# [22] gridtext_0.1.4    glue_1.6.2        R6_2.5.1          textshaping_0.3.6 fansi_1.0.3       farver_2.1.1      magrittr_2.0.3   
# [29] codetools_0.2-18  scales_1.2.1      R2WinBUGS_2.1-21  abind_1.4-5       colorspace_2.0-3  ragg_1.2.5        utf8_1.2.2       
# [36] munsell_0.5.0

library(reshape)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(ggtext)
library(patchwork)
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

############################################ Scenario 1 ################################################
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
               dplyr::filter(p_convert_cbhml,Endpoint == 1))
p11<-ggplot(dplyr::filter(p_ep1_1,Group==3), aes(x=factor(Design),y=100*(p_est-0.45))) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "red")+
  geom_boxplot(fill = '#A4A4A4', color = "black")+
  scale_x_discrete(
    name='Design',
    labels=c(bquote(BHM[IG]),bquote(CBHM[E]),bquote(CBHM[ET]),bquote(GCBHM[L])),
    position='bottom'
  )+
  scale_y_continuous(limits = c(-40,40),breaks=seq(-40,40,20))+
  xlab("Design")+
  ylab(bquote(paste('Bias (x',10^-2,')')))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),axis.title.x = element_blank())

p_ep2_1<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 2),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 2),
               dplyr::filter(p_convert_cbhml,Endpoint == 2))
p21<-ggplot(dplyr::filter(p_ep2_1,Group==3), aes(x=factor(Design),y=100*(p_est-0.3))) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "red")+
  geom_boxplot(fill = '#A4A4A4', color = "black")+
  scale_x_discrete(
    name='Design',
    labels=c(bquote(BHM[IG]),bquote(CBHM[ET]),bquote(GCBHM[L])),
    position='bottom'
  )+
  scale_y_continuous(limits = c(-30,40),breaks=seq(-20,40,20))+
  xlab("Design")+
  ylab(bquote(paste('Bias (x',10^-2,')')))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),axis.title.x = element_blank())


############################################ Scenario 2 ################################################
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

p_ep1_2<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 1),
               dplyr::filter(p_convert_cbhme,Endpoint == 1),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 1),
               dplyr::filter(p_convert_cbhml,Endpoint == 1))
p12<-ggplot(dplyr::filter(p_ep1_2,Group==3), aes(x=factor(Design),y=100*(p_est-0.5))) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "red")+
  geom_boxplot(fill = '#A4A4A4', color = "black")+
  scale_x_discrete(
    name='Design',
    labels=c(bquote(BHM[IG]),bquote(CBHM[E]),bquote(CBHM[ET]),bquote(GCBHM[L])),
    position='bottom'
  )+
  scale_y_continuous(limits = c(-40,40),breaks=seq(-40,40,20))+
  xlab("Design")+
  ylab(bquote(paste('Bias (x',10^-2,')')))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),axis.title.x = element_blank())

p_ep2_2<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 2),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 2),
               dplyr::filter(p_convert_cbhml,Endpoint == 2))
p22<-ggplot(dplyr::filter(p_ep2_2,Group==3), aes(x=factor(Design),y=100*(p_est-0.2))) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "red")+
  geom_boxplot(fill = '#A4A4A4', color = "black")+
  scale_x_discrete(
    name='Design',
    labels=c(bquote(BHM[IG]),bquote(CBHM[ET]),bquote(GCBHM[L])),
    position='bottom'
  )+
  scale_y_continuous(limits = c(-30,40),breaks=seq(-20,40,20))+
  xlab("Design")+
  ylab(bquote(paste('Bias (x',10^-2,')')))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),axis.title.x = element_blank())

############################################ Scenario 3 ################################################
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

p_ep1_3<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 1),
               dplyr::filter(p_convert_cbhme,Endpoint == 1),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 1),
               dplyr::filter(p_convert_cbhml,Endpoint == 1))
p13<-ggplot(dplyr::filter(p_ep1_3,Group==3), aes(x=factor(Design),y=100*(p_est-0.6))) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "red")+
  geom_boxplot(fill = '#A4A4A4', color = "black")+
  scale_x_discrete(
    name='Design',
    labels=c(bquote(BHM[IG]),bquote(CBHM[E]),bquote(CBHM[ET]),bquote(GCBHM[L])),
    position='bottom'
  )+
  scale_y_continuous(limits = c(-40,40),breaks=seq(-40,40,20))+
  xlab("Design")+
  ylab(bquote(paste('Bias (x',10^-2,')')))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),axis.title.x = element_blank())

p_ep2_3<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 2),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 2),
               dplyr::filter(p_convert_cbhml,Endpoint == 2))
p23<-ggplot(dplyr::filter(p_ep2_3,Group==3), aes(x=factor(Design),y=100*(p_est-0.2))) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "red")+
  geom_boxplot(fill = '#A4A4A4', color = "black")+
  scale_x_discrete(
    name='Design',
    labels=c(bquote(BHM[IG]),bquote(CBHM[ET]),bquote(GCBHM[L])),
    position='bottom'
  )+
  scale_y_continuous(limits = c(-30,40),breaks=seq(-20,40,20))+
  xlab("Design")+
  ylab(bquote(paste('Bias (x',10^-2,')')))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),axis.title.x = element_blank())

############################################ Scenario 4 ################################################
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

p_ep1_4<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 1),
               dplyr::filter(p_convert_cbhme,Endpoint == 1),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 1),
               dplyr::filter(p_convert_cbhml,Endpoint == 1))
p14<-ggplot(dplyr::filter(p_ep1_4,Group==3), aes(x=factor(Design),y=100*(p_est-0.5))) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "red")+
  geom_boxplot(fill = '#A4A4A4', color = "black")+
  scale_x_discrete(
    name='Design',
    labels=c(bquote(BHM[IG]),bquote(CBHM[E]),bquote(CBHM[ET]),bquote(GCBHM[L])),
    position='bottom'
  )+
  scale_y_continuous(limits = c(-40,40),breaks=seq(-40,40,20))+
  xlab("Design")+
  ylab(bquote(paste('Bias (x',10^-2,')')))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),axis.title.x = element_blank())

p_ep2_4<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 2),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 2),
               dplyr::filter(p_convert_cbhml,Endpoint == 2))
p24<-ggplot(dplyr::filter(p_ep2_4,Group==3), aes(x=factor(Design),y=100*(p_est-0.2))) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "red")+
  geom_boxplot(fill = '#A4A4A4', color = "black")+
  scale_x_discrete(
    name='Design',
    labels=c(bquote(BHM[IG]),bquote(CBHM[ET]),bquote(GCBHM[L])),
    position='bottom'
  )+
  scale_y_continuous(limits = c(-30,40),breaks=seq(-20,40,20))+
  xlab("Design")+
  ylab(bquote(paste('Bias (x',10^-2,')')))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),axis.title.x = element_blank())


############################################ Scenario 5 ################################################
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

p_ep1_5<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 1),
               dplyr::filter(p_convert_cbhme,Endpoint == 1),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 1),
               dplyr::filter(p_convert_cbhml,Endpoint == 1))
p15<-ggplot(dplyr::filter(p_ep1_5,Group==3), aes(x=factor(Design),y=100*(p_est-0.6))) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "red")+
  geom_boxplot(fill = '#A4A4A4', color = "black")+
  scale_x_discrete(
    name='Design',
    labels=c(bquote(BHM[IG]),bquote(CBHM[E]),bquote(CBHM[ET]),bquote(GCBHM[L])),
    position='bottom'
  )+
  scale_y_continuous(limits = c(-40,40),breaks=seq(-40,40,20))+
  xlab("Design")+
  ylab(bquote(paste('Bias (x',10^-2,')')))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),axis.title.x = element_blank())

p_ep2_5<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 2),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 2),
               dplyr::filter(p_convert_cbhml,Endpoint == 2))
p25<-ggplot(dplyr::filter(p_ep2_5,Group==3), aes(x=factor(Design),y=100*(p_est-0.25))) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "red")+
  geom_boxplot(fill = '#A4A4A4', color = "black")+
  scale_x_discrete(
    name='Design',
    labels=c(bquote(BHM[IG]),bquote(CBHM[ET]),bquote(GCBHM[L])),
    position='bottom'
  )+
  scale_y_continuous(limits = c(-30,40),breaks=seq(-20,40,20))+
  xlab("Design")+
  ylab(bquote(paste('Bias (x',10^-2,')')))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),axis.title.x = element_blank())


############################################ Scenario 6 ################################################
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

p_ep1_6<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 1),
               dplyr::filter(p_convert_cbhme,Endpoint == 1),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 1),
               dplyr::filter(p_convert_cbhml,Endpoint == 1))
p16<-ggplot(dplyr::filter(p_ep1_6,Group==3), aes(x=factor(Design),y=100*(p_est-0.60))) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "red")+
  geom_boxplot(fill = '#A4A4A4', color = "black")+
  scale_x_discrete(
    name='Design',
    labels=c(bquote(BHM[IG]),bquote(CBHM[E]),bquote(CBHM[ET]),bquote(GCBHM[L])),
    position='bottom'
  )+
  scale_y_continuous(limits = c(-40,40),breaks=seq(-40,40,20))+
  xlab("Design")+
  ylab(bquote(paste('Bias (x',10^-2,')')))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),axis.title.x = element_blank())

p_ep2_6<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 2),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 2),
               dplyr::filter(p_convert_cbhml,Endpoint == 2))
p26<-ggplot(dplyr::filter(p_ep2_6,Group==3), aes(x=factor(Design),y=100*(p_est-0.2))) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "red")+
  geom_boxplot(fill = '#A4A4A4', color = "black")+
  scale_x_discrete(
    name='Design',
    labels=c(bquote(BHM[IG]),bquote(CBHM[ET]),bquote(GCBHM[L])),
    position='bottom'
  )+
  scale_y_continuous(limits = c(-30,40),breaks=seq(-20,40,20))+
  xlab("Design")+
  ylab(bquote(paste('Bias (x',10^-2,')')))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),axis.title.x = element_blank())


############################################ Scenario 7 ################################################
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

p_ep1_7<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 1),
               dplyr::filter(p_convert_cbhme,Endpoint == 1),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 1),
               dplyr::filter(p_convert_cbhml,Endpoint == 1))
p17<-ggplot(dplyr::filter(p_ep1_7,Group==3), aes(x=factor(Design),y=100*(p_est-0.6))) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "red")+
  geom_boxplot(fill = '#A4A4A4', color = "black")+
  scale_x_discrete(
    name='Design',
    labels=c(bquote(BHM[IG]),bquote(CBHM[E]),bquote(CBHM[ET]),bquote(GCBHM[L])),
    position='bottom'
  )+
  scale_y_continuous(limits = c(-40,40),breaks=seq(-40,40,20))+
  xlab("Design")+
  ylab(bquote(paste('Bias (x',10^-2,')')))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),axis.title.x = element_blank())

p_ep2_7<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 2),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 2),
               dplyr::filter(p_convert_cbhml,Endpoint == 2))
p27<-ggplot(dplyr::filter(p_ep2_7,Group==3), aes(x=factor(Design),y=100*(p_est-0.2))) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "red")+
  geom_boxplot(fill = '#A4A4A4', color = "black")+
  scale_x_discrete(
    name='Design',
    labels=c(bquote(BHM[IG]),bquote(CBHM[ET]),bquote(GCBHM[L])),
    position='bottom'
  )+
  scale_y_continuous(limits = c(-30,40),breaks=seq(-20,40,20))+
  xlab("Design")+
  ylab(bquote(paste('Bias (x',10^-2,')')))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),axis.title.x = element_blank())


############################################ Scenario 8 ################################################
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

p_ep1_8<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 1),
               dplyr::filter(p_convert_cbhme,Endpoint == 1),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 1),
               dplyr::filter(p_convert_cbhml,Endpoint == 1))
p18<-ggplot(dplyr::filter(p_ep1_8,Group==3), aes(x=factor(Design),y=100*(p_est-0.55))) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "red")+
  geom_boxplot(fill = '#A4A4A4', color = "black")+
  scale_x_discrete(
    name='Design',
    labels=c(bquote(BHM[IG]),bquote(CBHM[E]),bquote(CBHM[ET]),bquote(GCBHM[L])),
    position='bottom'
  )+
  scale_y_continuous(limits = c(-40,40),breaks=seq(-40,40,20))+
  xlab("Design")+
  ylab(bquote(paste('Bias (x',10^-2,')')))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),axis.title.x = element_blank())

p_ep2_8<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 2),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 2),
               dplyr::filter(p_convert_cbhml,Endpoint == 2))
p28<-ggplot(dplyr::filter(p_ep2_8,Group==3), aes(x=factor(Design),y=100*(p_est-0.25))) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "red")+
  geom_boxplot(fill = '#A4A4A4', color = "black")+
  scale_x_discrete(
    name='Design',
    labels=c(bquote(BHM[IG]),bquote(CBHM[ET]),bquote(GCBHM[L])),
    position='bottom'
  )+
  scale_y_continuous(limits = c(-30,40),breaks=seq(-20,40,20))+
  xlab("Design")+
  ylab(bquote(paste('Bias (x',10^-2,')')))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),axis.title.x = element_blank())


############################################ Scenario 9 ################################################
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

p_ep1_9<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 1),
               dplyr::filter(p_convert_cbhme,Endpoint == 1),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 1),
               dplyr::filter(p_convert_cbhml,Endpoint == 1))
p19<-ggplot(dplyr::filter(p_ep1_9,Group==3), aes(x=factor(Design),y=100*(p_est-0.45))) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "red")+
  geom_boxplot(fill = '#A4A4A4', color = "black")+
  scale_x_discrete(
    name='Design',
    labels=c(bquote(BHM[IG]),bquote(CBHM[E]),bquote(CBHM[ET]),bquote(GCBHM[L])),
    position='bottom'
  )+
  scale_y_continuous(limits = c(-40,40),breaks=seq(-40,40,20))+
  xlab("Design")+
  ylab(bquote(paste('Bias (x',10^-2,')')))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),axis.title.x = element_blank())

p_ep2_9<-rbind(dplyr::filter(p_convert_bhm,Endpoint == 2),
               dplyr::filter(p_convert_cbhmetox,Endpoint == 2),
               dplyr::filter(p_convert_cbhml,Endpoint == 2))
p29<-ggplot(dplyr::filter(p_ep2_9,Group==3), aes(x=factor(Design),y=100*(p_est-0.3))) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "red")+
  geom_boxplot(fill = '#A4A4A4', color = "black")+
  scale_x_discrete(
    name='Design',
    labels=c(bquote(BHM[IG]),bquote(CBHM[ET]),bquote(GCBHM[L])),
    position='bottom'
  )+
  scale_y_continuous(limits = c(-30,40),breaks=seq(-20,40,20))+
  xlab("Design")+
  ylab(bquote(paste('Bias (x',10^-2,')')))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),axis.title.x = element_blank())

############################################ Final ################################################

p_final1<-((p11+p12+p13)/(p14+p15+p16)/(p17+p18+p19))+
  plot_annotation(tag_levels = '1', tag_prefix = '(',tag_suffix = ')')&
  theme(plot.tag = element_text(color=brewer.pal(7,"Greys")[6]))

ggsave(p_final1, file='Results/Figure_S3.pdf', width=10, height=10,dpi = 800)



p_final2<-((p21+p22+p23)/(p24+p25+p26)/(p27+p28+p29))+
  plot_annotation(tag_levels = '1', tag_prefix = '(',tag_suffix = ')')&
  theme(plot.tag = element_text(color=brewer.pal(7,"Greys")[6]))

ggsave(p_final2, file='Results/Figure_S4.pdf', width=10, height=10,dpi = 800)
