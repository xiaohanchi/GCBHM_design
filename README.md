# GCBHM_design
This repository contains R codes for implementing the generalized calibrated Bayesian hierarchical design.

### Repository Contents

* Eff_Tox/ codes for implementing simulations with "efficacy and toxicity endpoints" decribed in Section 6.1
    * BHM/ the latent BHM design, which uses the latent variable model with a noninformative prior for $\sigma^2_r$
    * CBHM/ the CBHM design using a single efficacy endpoint
    * CBHM_Tox/ the CBHM-Tox design, which evaluates the efficacy endpoint using the CBHM model and monitors the toxicity endpoint independently in each subtype without information borrowing
    * GCBHM_l/ our proposed GCBHM_l design
* Nested/ codes for implementing simulations with "nested efficacy endpoints" decribed in Section 6.2
### Authors and Reference
Chi, X., Yuan, Y., Lin, R.<sup>\*</sup>, and Yu, Z. (2022+). A Generalized Calibrated Bayesian Hierarchical Modeling Approach to Basket Trials with Multiple Endpoints. 


