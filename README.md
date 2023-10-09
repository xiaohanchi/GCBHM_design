# GCBHM_design
This repository contains R code for implementing the generalized calibrated Bayesian hierarchical (GCBHM) design.

### Repository Contents

* Eff_Tox/ code for implementing simulations with "efficacy and toxicity endpoints" decribed in Section 6.1
    * BHM/ the latent BHM design, which uses the latent variable model with a noninformative prior for $\sigma^2_r$
    * CBHM/ the CBHM design using a single efficacy endpoint
    * CBHM_Tox/ the CBHM-Tox design, which evaluates the efficacy endpoint using the CBHM model and monitors the toxicity endpoint independently in each subtype without information borrowing
    * GCBHM_l/ the proposed GCBHM$_l$ design
* Nested/ code for implementing simulations with "nested efficacy endpoints" decribed in Section 6.2\
    * BHM/ the BHM design, which uses the Bayesian hierarchical multi-nomial model with a noninformative prior for $\sigma^2$
    * GCBHM_m/ the proposed GCBHM_m design
    * GCBHM_l/ the proposed GCBHM_l design
### Authors and Reference
Chi, X., Yuan, Y., Yu, Z., and Lin, R.<sup>\*</sup> (2023+). A Generalized Calibrated Bayesian Hierarchical Modeling Approach to Basket Trials with Multiple Endpoints. 


