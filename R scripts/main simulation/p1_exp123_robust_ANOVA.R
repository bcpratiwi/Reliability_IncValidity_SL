# this script applies robust ANOVA procedures on the simulation results
# the package containing the robust methods is WRS2 package

library(tidyverse)
library(WRS2)
library(rstatix)

################################################################################
######################### OLS in- vs out-of-sample IV ##########################
################################################################################

load("RESULTS_ols_long.Rdata") # contains RESULTS_ols_long

RESULTS_ols_wide <- RESULTS_ols_long %>%
                      pivot_wider(names_from=sample, values_from=IV_MSE) %>%
                      mutate(ave_IV_MSE = (train+test)/2, diff_IV_MSE=train-test)


## Interaction Effect Approach with Ncal (Main Effects are included) -------------------------------------------------------------------------
mixedAnova1_N_sample <- bwtrim(IV_MSE ~ N*sample, id = ndesign, data
                               = select(RESULTS_ols_long, sample, ndesign, IV_MSE, N))

## Interaction Effects (Main Effects are included)
# rho2 vs R2 -------------------------------------------------------------------
mixedAnova1_rho2_R2 <- t2way(ave_IV_MSE ~ rho2*R2,
                             data = select(RESULTS_ols_wide, ave_IV_MSE, rho2, R2))


# rho2 ratio -------------------------------------------------------------------
mixedAnova1_rho2_ratio <- t2way(ave_IV_MSE ~ rho2*ratio, 
                                data = select(RESULTS_ols_wide, ave_IV_MSE, rho2, ratio))

# rho1 r12 ---------------------------------------------------------------------
mixedAnova1_rho1_r12 <- t2way(ave_IV_MSE ~ rho1*r12,  
                              data = select(RESULTS_ols_wide, ave_IV_MSE, rho1, r12))

# rho1 R2 ----------------------------------------------------------------------
mixedAnova1_rho1_R2 <- t2way(ave_IV_MSE ~ rho1*R2,  
                             data = select(RESULTS_ols_wide, ave_IV_MSE, rho1, R2))

################################################################################
#################### Part II: Out-of-sample IV 3 Methods #######################
################################################################################

load("RESULTS.Rdata") # object contain RESULTS of IV from OLS, ridge, annd SIMEX
RESULTS <- RESULTS %>%
              mutate(rho2=as.factor(rho2), rho1=as.factor(rho1), R2=as.factor(R2),
                     N=as.factor(N), r12=as.factor(r12))

## rho2 and methods -------------------------------------
mixedAnova2_rho2_methods <- bwtrim(IV_MSE ~ .*., id = ndesign, 
                                   data = select(RESULTS, Estimates, IV_MSE, rho2, ndesign))

## rho1 and methods -------------------------------------
mixedAnova2_rho1_methods <- bwtrim(IV_MSE ~ .*., id = ndesign, 
                                   data = select(RESULTS, Estimates, IV_MSE, rho1, ndesign))

## N and methods -------------------------------------------------------------
mixedAnova2_N_methods <- bwtrim(IV_MSE ~ .*., id = ndesign, 
                                data = select(RESULTS, Estimates, IV_MSE, N, ndesign))

save(list=ls(pattern="mixedAnova"), file="robustMixedANOVAs.Rdata")
