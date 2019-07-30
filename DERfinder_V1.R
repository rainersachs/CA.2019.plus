#Newer DERs to try out
source('Datatable.R')
source('DERfinder_function_V1.R')

#Our IDERs (Individual Dose Effect Relations). Applicable to the 1-ion components of a mixed simulated GCR beam 
#Modifying NTE1 and NTE2 by insisting they be twice continuously differentiable and monotonic increasing. Double check NTE1, NTE2, Our model
library("forecast")

#================================ DER MODELS ==================================#

#PW to RKS: Please stick to these two names for now: NEW1, NEW2. 
#           Don't change the function names yet, but only what is in the function.
# Available parameter names are kap0, sig0, eta0 and eta1

# func_NEW2 = function(d, L,Z.b, kap0, sig0, eta0){ # using 5th power to improve a bit
#      P=(1-exp(-Z.b/kap0))^5
#      sigma=sig0*P+(eta0*L/6.24)*(1-P)
#      return =(BG_CA+ (sigma*6.24*(d/L)*(1 - exp(-1024*d/L))*(1-exp(-1024*d/L))))
# }
# This calibrates if starting value of eta0 is chosen as 100 instead of 10
# It has summary,AIC, and CV by fold that is slightly better than the one right below.
# The CV fold improvement holds for almost every (or every?) one of the 17 folds
# ========= Available parameter names are kap0, sig0, eta0 and eta1   ====== #
func_NEW2 = function(d, L,Z.b, kap0, sig0, eta0){
  P=(1-exp(-Z.b/kap0))^2
  sigma=sig0*P+(eta0*L/6.24)*(1-P)
  return =(BG_CA+ (sigma*6.24*(d/L)*(1 - exp(-1024*d/L))*(1-exp(-1024*d/L))))
} # kap0 in numerator and starting value 1/2000 gives the same result
func_NEW2 = function(d, L,Z.b, kap0, sig0, eta0){
     P=(1-exp(-Z.b/kap0))^4
     sigma=sig0*P+(eta0*L/6.24)*(1-P)
     return =(BG_CA+ (sigma*6.24*(d/L)*(1 - exp(-1024*d/L))*(1-exp(-1024*d/L))))
    } # this is much better than above as regards summary, AIC, CV by fold, IEA behavior, and ribbon behavior.

###################### New candidate model 1: NEW1 

# func_2para=function(d, L, eta0, sig0){
#   return(BG_CA + sig0*6.24*d/L*(1-exp(-1024*d/L)) + eta0*(1-exp(-10^5*d)))
# } # the simple old NTE-also model
# func_NEW2 = function(d, L,Z.b, kap0, sig0, eta0){
#   P=(1-exp(-Z.b/kap0))^4
#   sigma=sig0*P+(eta0*L/6.24)*(1-P)
#   return =(BG_CA+ (sigma*6.24*(d/L)*(1 - exp(-1024*d/L))*(1-exp(-1024*d/L))))
# } # to be canniblized for NEW1
#kap0 604.38518  ; sig0   5.04177  ;    eta0   0.03860; 
# ========= Available parameter names are kap0, sig0, eta0 and eta1   ====== #
# func_NEW1=function(d, L, Z.b, kap0,sig0, eta1){
#     P=(1-exp(-Z.b/kap0))^4
#     sigma=sig0*P+(.0386*L/6.24)*(1-P)
#     return =(BG_CA+ (sigma*6.24*(d/L)*(1 - exp(-1024*d/L))*(1-exp(-1024*d/L)) +
#                eta1*(1-exp(-10^5*d))))
# }
func_NEW1=function(d, L, Z.b, kap0,sig0,eta0, eta1){
  P=(1-exp(-Z.b/kap0))^4
  sigma=sig0*P+(eta0*L/6.24)*(1-P)
  return =(BG_CA+ (sigma*6.24*(d/L)*(1 - exp(-1024*d/L))*(1-exp(-1024*d/L)) +
                     eta1*(1-exp(-10^5*d))))
}
#================================ MODEL CALIBRATION ==================================#
# nlsLM requires a starting value for the parameters to be calibrated. 
result = calibration_HZE("NEW1", StartValues = list(kap0= 600,sig0 =5,eta0 =.04, eta1 = .045))
parameters_NEW1 = result[[1]]
sig_NEW1 = result[[2]]
model_NEW1 = result[[3]]
print(summary(model_NEW1))

result = calibration_HZE("NEW2", StartValues = list(kap0 = 2000, sig0 = 100, eta0=.5))
# result = calibration_HZE("NEW2", StartValues = list(kap0 = 2000, sig0 = 10, eta0=.5))
parameters_NEW2 = result[[1]]
sig_NEW2 = result[[2]]
model_NEW2 = result[[3]]

print(summary(model_NEW2) )
# 
#================================ MODEL COMPARISON ==================================#
HZE_models = c("NEW1", "NEW2","4para","3para","2para","2paraTE")
print("AIC, BIC, CV_function(NEW1, by_fold = T), and CV_function(NEW2, by_fold = T)")
print(sapply(HZE_models, AIC_function))
print(sapply(HZE_models, BIC_function))
#sapply(HZE_models, CV_function) #Please look at CV results by_fold. Removing O350 makes nls not not converge, but the other ions work well.
print(CV_function("NEW1", by_fold = T))
print(CV_function("NEW2", by_fold = T))
