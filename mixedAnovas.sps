* Encoding: UTF-8.

* use RESULTS_train_test_wide.sav

DATASET ACTIVATE DataSet1.
USE ALL.
COMPUTE filter_$=(PE = 3).
VARIABLE LABELS filter_$ 'PE = 3 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

GLM value.train value.test BY rho1 rho2 ratio N r12 R2
  /WSFACTOR=sample 2 Polynomial 
  /MEASURE=type 
  /METHOD=SSTYPE(3)
  /PRINT=ETASQ 
  /CRITERIA=ALPHA(.05)
  /DESIGN= rho1 rho2 ratio N r12 R2 rho1*rho2 ratio*rho1 N*rho1 r12*rho1 R2*rho1 ratio*rho2 N*rho2 
    r12*rho2 R2*rho2 N*ratio r12*ratio R2*ratio N*r12 N*R2 R2*r12.


* use RESULTS_test_wide.sav

GLM ridge least_squares simex BY r12 R2 ratio rho1 rho2 N
  /WSFACTOR=Estimates 3 Polynomial 
  /METHOD=SSTYPE(3)
  /PRINT=ETASQ 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN= Estimates
  /DESIGN= r12 R2 ratio rho1 rho2 N R2*r12 r12*ratio r12*rho1 r12*rho2 N*r12 R2*ratio R2*rho1 
    R2*rho2 N*R2 ratio*rho1 ratio*rho2 N*ratio rho1*rho2 N*rho1 N*rho2.

