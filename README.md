# GCBHM_design
This repository contains R code for implementing the generalized calibrated Bayesian hierarchical (GCBHM) design.

### Repository Contents

* code_for_rerunning_simu/: R code necessary for rerunning the simulations presented in the manuscript.
* code_for_tables_figures/: R code for reproducing all the tables and figures featured in the main paper and supplementary materials.
* Rdata/: This directory stores saved intermediate results in Rdata files from our simulations. These files are used for reproducing the tables and figures presented in the paper.
* Results/ ALL tables and figures presented in  paper.
* master.R: This file serves as a summary, providing an overview of the code needed to reproduce the results.

### Session Information
```R
> sessionInfo()
R version 4.1.2 (2021-11-01)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS 14.0

Matrix products: default
LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] patchwork_1.1.1    ggtext_0.1.1       ggplot2_3.4.3      RColorBrewer_1.1-3 dplyr_1.1.2       
 [6] reshape_0.8.8      doParallel_1.0.17  iterators_1.0.14   foreach_1.5.2      runjags_2.2.0-3   
[11] R2jags_0.7-1       rjags_4-14         coda_0.19-4       

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.10       pillar_1.9.0      compiler_4.1.2    plyr_1.8.6        tools_4.1.2      
 [6] boot_1.3-28.1     lifecycle_1.0.3   tibble_3.2.1      gtable_0.3.1      lattice_0.20-45  
[11] pkgconfig_2.0.3   rlang_1.1.1       cli_3.6.0         rstudioapi_0.15.0 xml2_1.3.3       
[16] withr_2.5.0       generics_0.1.1    vctrs_0.6.3       gridtext_0.1.4    grid_4.1.2       
[21] tidyselect_1.2.0  glue_1.6.2        R6_2.5.1          fansi_1.0.3       magrittr_2.0.3   
[26] scales_1.2.1      codetools_0.2-18  R2WinBUGS_2.1-21  abind_1.4-5       colorspace_2.0-3 
[31] utf8_1.2.2        munsell_0.5.0 

```
### Authors and Reference
Chi, X., Yuan, Y., Yu, Z., and Lin, R.<sup>\*</sup> (2024). A Generalized Calibrated Bayesian Hierarchical Modeling Approach to Basket Trials with Multiple Endpoints. Biometrical Journal.


