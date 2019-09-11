source('Datatable.R')
source('DERfinder_function.R')

#Our IDERs (Individual Dose Effect Relations). Applicable to the 1-ion components of a mixed simulated GCR beam 
#Modifying NTE1 and NTE2 by insisting they be twice continuously differentiable and monotonic increasing. Double check NTE1, NTE2, Our model
library("forecast")

#================================ DER MODELS ==================================#

#PW to RKS: Please stick to these two names for now: NEW1, NEW2. 
#           Don't change the function names yet, but only what is in the function.

###New candidate model 1: NEW1

func_NEW1 = function(d, L, Z.b, kap0, sig0, eta0){
  P=(1-exp(-Z.b/kap0))^4
  sigma=sig0*P+(eta0*L/6.24)*(1-P)
  return =(BG_CA+ (sigma*6.24*(d/L)*(1 - exp(-1024*d/L))**2))
}

###New candidate model 2: NEW2. 
func_NEW2 = function(d, L, Z.b, kap0, sig0, eta0){
  P=(1-exp(-Z.b/kap0))^2
  sigma=sig0*P+(eta0*L/6.24)*(1-P)
  return =(BG_CA+ (sigma*6.24*(d/L)*(1 - exp(-1024*d/L))*(1-exp(-1024*d/L))))
}

#================================ MODEL CALIBRATION ==================================#
# nlsLM requires a starting value for the parameters to be calibrated. 

#Calibration for NEW1 model
result = calibration_HZE("NEW1", StartValues = list(sig0 = 10))
parameters_NEW1 = result[[1]]
sig_NEW1= result[[2]]
model_NEW1 = result[[3]]

summary(model_NEW1)
#Calibration for NEW2 model
result = calibration_HZE("NEW2")
parameters_NEW2 = result[[1]]
sig_NEW2 = result[[2]]
model_NEW2 = result[[3]]

summary(model_NEW2)
#================================ MODEL COMPARISON ==================================#
HZE_models = c(
  #"NEW1",  #Note that NEW1 is a crazy bad model right now and it will break CV since it is not close to a good model, which is why I am excluding it.
  "NEW2","4para","3para","2para","2paraTE")
sapply(HZE_models, AIC_function)
sapply(HZE_models, BIC_function)
sapply(HZE_models, CV_function) #Please look at CV results by_fold. Removing O350 makes nls not not converge, but the other ions work well.
CV_function("4para", by_fold = T)

