* Encoding: UTF-8.

* analysis on RESULTS_ols_wide.sav in- and out- of sample IV from ols

DATASET ACTIVATE DataSet1.
GLM IVMSEtest IVMSEtrain BY rho1 rho2 ratio N r12 R2
  /WSFACTOR=type 2 Polynomial 
  /MEASURE=sample 
  /METHOD=SSTYPE(3)
  /PRINT=ETASQ HOMOGENEITY LOF
  /SAVE=RESID ZRESID SRESID
  /CRITERIA=ALPHA(.05)
  /DESIGN= rho1 rho2 ratio N r12 R2 rho1*rho2 ratio*rho1 N*rho1 r12*rho1 R2*rho1 ratio*rho2 N*rho2 
    r12*rho2 R2*rho2 N*ratio r12*ratio R2*ratio N*r12 N*R2 R2*r12.

* analysis on RESULTS_ols_wide.sav  out- of sample IV from ols, ridge, and simex 
 
DATASET ACTIVATE DataSet2.
GLM ridge least_squares simex BY rho1 rho2 ratio N r12 R2
  /WSFACTOR=method 3 Polynomial 
  /MEASURE=Estimates 
  /METHOD=SSTYPE(3)
  /SAVE=RESID ZRESID SRESID
  /PRINT=ETASQ HOMOGENEITY LOF
  /CRITERIA=ALPHA(.05)
  /DESIGN= rho1 rho2 ratio N r12 R2 rho1*rho2 ratio*rho1 N*rho1 r12*rho1 R2*rho1 ratio*rho2 N*rho2 
    r12*rho2 R2*rho2 N*ratio r12*ratio R2*ratio N*r12 N*R2 R2*r12.

