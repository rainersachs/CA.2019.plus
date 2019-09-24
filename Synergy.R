source('Datatable.R')
source('Synergy_functions.R')

#Our IDERs (Individual Dose Effect Relations). Applicable to the 1-ion components of a mixed simulated GCR beam 
#Modifying NTE1 and NTE2 by insisting they be twice continuously differentiable and monotonic increasing. Double check NTE1, NTE2, Our model
library("forecast")

SwiftLight_data <- filter(main_df, Z <= 2 & d>0)
HZE_data = main_df %>% filter(Z > 3 & d > 0)

#================================ DER MODELS ==================================#

#PW to RKS: Please stick to these two names for now: NEW1, NEW2. 
#           Don't change the function names yet, but only what is in the function.

###New candidate model 1: NEW1, NTE model with 4 parameters
func_NEW1=function(d, L, Z.b, kap0,sig0,eta0, eta1){
  P=(1-exp(-Z.b/kap0))^4
  sigma=sig0*P+(eta0*L/6.24)*(1-P)
  return(BG_CA+ (sigma*6.24*(d/L)*(1 - exp(-1024*d/L))**2 +
                   eta1*(1-exp(-10^5*d))))
}

###New candidate model 2: NEW2, TE model with 3 parameters
func_NEW2 = function(d, L, Z.b, kap0, sig0, eta0){
  P=(1-exp(-Z.b/kap0))^4
  sigma=sig0*P+(eta0*L/6.24)*(1-P)
  return(BG_CA+ (sigma*6.24*(d/L)*(1 - exp(-1024*d/L))**2))
}
  



#4para 
func_4para = function(d, L, Z.b, eta0, eta1, sig0, kap0) {
  P = (1-exp(-Z.b/kap0))^2
  sig = sig0*P + 0.041/6.24*L*(1-P) # 0.041 +- 0.0051 comes from 16Cacao
  eta = eta0*L*exp(-eta1*L)
  return(BG_CA + sig*6.24*d/L*(1-exp(-1024*d/L)) + eta*(1-exp(-10^5*d)))
} 

#3para 
func_3para = function(d, L, eta0, eta1, sig0){
  eta = eta0*L*exp(-eta1*L)
  return(BG_CA + 6.24*sig0*d/L*(1 - exp(-1024*d/L)) + eta*(1-exp(-10^5*d)))
}

#2para 
func_2para=function(d, L, eta0, sig0){
  return(BG_CA + sig0*6.24*d/L*(1-exp(-1024*d/L)) + eta0*(1-exp(-10^5*d)))
}

#2paraTE 
func_2paraTE = function(d, L, Z.b, kap0, sig0){
  P = (1 - exp(-Z.b/kap0))^2
  sigma = sig0*P + (0.041 * L/6.24)*(1-P)
  return(BG_CA + (sigma*6.24*(d/L))*(1 - exp(-1024*(d/L))))
}

#Swift Light Ion 2 parameter 
func_sli_exp2 <- function(a, C, d) {
  return(BG_CA + C*(exp(a*d)-1))
}

#Swift Light Ion 3 paramter 
func_sli_exp3 <- function(a, b, C, d) {
  return(BG_CA + C*(exp(a*d+b*d^2)-1))
}

func_sli_lin1 <- function(d, a){
  return(BG_CA + a*d)
}

HZE_models = c("NEW1", "NEW2","4para","3para","2para","2paraTE")

#================================ MODEL CALIBRATION ==================================#
# nlsLM requires a starting value for the parameters to be calibrated. 

#Calibration for NEW1 model
result = calibration_HZE("NEW1", StartValues = list(kap0 = 600, sig0 = 5, eta0 = 0.05, eta1 = 0.01))
parameters_NEW1 = result[[1]]
sig_NEW1= result[[2]]
model_NEW1 = result[[3]]
summary(model_NEW1, correlation = T)
vcov(model_NEW1) #This is the same as sig_NEW1

#Calibration for NEW2 model
result = calibration_HZE("NEW2", StartValues = list(kap0 = 2000, sig0 = 5, eta0=.05))
parameters_NEW2 = result[[1]]
sig_NEW2 = result[[2]]
model_NEW2 = result[[3]]

#Calibration for 2-parameter model
result = calibration_HZE("2para")
parameters_2para = result[[1]]
sig_2para = result[[2]]
model_2para = result[[3]]

#Calibration for 3-parameter model
result = calibration_HZE("3para")
parameters_3para = result[[1]]
sig_3para = result[[2]]
model_3para = result[[3]]

#Calibration for 4-parameter model
result = calibration_HZE("4para")
parameters_4para = result[[1]]
sig_4para= result[[2]]
model_4para = result[[3]]

#Calibration for 2-parameter TE Only Model
result = calibration_HZE("2paraTE")
parameters_2paraTE = result[[1]]
sig_2paraTE = result[[2]]
model_2paraTE = result[[3]]


#Calibration for SLI Linear 1 parameter
model_sli_lin1 <- lm(I(CA - BG_CA) ~ 0 + d, data = SwiftLight_data, weights = (1/(SwiftLight_data$error)^2))
ccoef <- coef(model_sli_lin1)
parameters_sli_lin1 <- data.frame(value = as.numeric(ccoef), model = "sli_lin1", parameter = "a")
sig_sli_lin1 <- vcov(model_sli_lin1)

#Calibration for SLI Linear 2 parameter
model_sli_lin2 <- lm(I(CA - BG_CA) ~ 0 + d + I(d^2), data = SwiftLight_data, weights = (1/(SwiftLight_data$error)^2))
ccoef <- coef(model_sli_lin2)
parameters_sli_lin2 <- data.frame(value = as.numeric(ccoef), model = "sli_lin2", parameter = names(ccoef))
sig_sli_lin2 <- vcov(model_sli_lin2)


Data_parameter = rbind(parameters_2para, 
                       parameters_3para, 
                       parameters_4para, 
                       parameters_2paraTE, 
                       parameters_NEW1,
                       parameters_NEW2,
                       parameters_sli_lin1)

#================================ MODEL COMPARISON ==================================#

sapply(HZE_models, AIC_function)
sapply(HZE_models, BIC_function)
sapply(HZE_models, CV_function) #Please look at CV results by_fold. Removing O350 makes nls not not converge, but the other ions work well.
CV_function("4para", by_fold = T)

calibration_HZE("4para", data = HZE_data %>% filter(ion != "O350"))


#=========== BASELINE NO-SYNERGY/ANTAGONISM MIXTURE DER FUNCTIONS =============#
#=============================== SEA and IEA ==================================#

IDER = function(d, L = NULL, Z.b = NULL, ions = NULL, r = NULL, parameters = Data_parameter, model = "4para", data = main_df) {
  #Main function that outputs the Individual dose-effect relationship. If "ions" are specified, take the average of them instead. 
  #Right now this function requires that the calibrated parameters to be defined in the exact names as above. Need to work on this part.
  
  name = model #To avoid "model" being both a string (model name) and a column name
  parameters = parameters %>% filter((model == name) | (parameter == "a")) #Only the parameters we want

  if (is.null(ions)){
    if (L < 5){ #Then it's SLI
      a = (parameters %>% filter(parameter == "a"))$value
      return(func_sli_lin1(d = d, a = a) - BG_CA)
    }
    if (model == "4para"){ #Parameters are kap0, sig0, eta0, eta1
      kap0 = (parameters %>% filter(parameter == "kap0"))$value
      sig0 = (parameters %>% filter(parameter == "sig0"))$value
      eta0 = (parameters %>% filter(parameter == "eta0"))$value
      eta1 = (parameters %>% filter(parameter == "eta1"))$value
      return(func_4para(d = d, L = L, Z.b = Z.b, eta0 = eta0, eta1 = eta1, sig0 = sig0, kap0 = kap0) - BG_CA)

    }
    if (model == "3para"){ #Parameters are sig0, eta0, eta1
      sig0 = (parameters %>% filter(parameter == "sig0"))$value
      eta0 = (parameters %>% filter(parameter == "eta0"))$value
      eta1 = (parameters %>% filter(parameter == "eta1"))$value
      return(func_3para(d = d, L = L, eta0 = eta0, eta1 = eta1, sig0 = sig0) - BG_CA)
    }
    if (model == "2para"){ #Parameters are sig0, eta0
      sig0 = (parameters %>% filter(parameter == "sig0"))$value
      eta0 = (parameters %>% filter(parameter == "eta0"))$value
      return(func_2para(d = d, L = L, eta0 = eta0, sig0 = sig0) - BG_CA)
    }
    if (model == "2paraTE"){ #Parameters are kap0, sig0
      kap0 = (parameters %>% filter(parameter == "kap0"))$value
      sig0 = (parameters %>% filter(parameter == "sig0"))$value
      return(func_2paraTE(d = d, L = L, Z.b = Z.b, kap0 = kap0, sig0 = sig0) - BG_CA)
    }
    if (model == "NEW1"){
      kap0 = (parameters %>% filter(parameter == "kap0"))$value
      sig0 = (parameters %>% filter(parameter == "sig0"))$value
      eta0 = (parameters %>% filter(parameter == "eta0"))$value
      eta1 = (parameters %>% filter(parameter == "eta1"))$value
      
      return(func_NEW1(d = d, L = L, Z.b = Z.b, kap0 = kap0, sig0 = sig0, eta0 = eta0, eta1 = eta1) - BG_CA)
    }
    if (model == "NEW2"){
      kap0 = (parameters %>% filter(parameter == "kap0"))$value
      sig0 = (parameters %>% filter(parameter == "sig0"))$value
      eta0 = (parameters %>% filter(parameter == "eta0"))$value
      return(func_NEW2(d = d, L = L, Z.b = Z.b, kap0 = kap0, sig0 = sig0, eta0 = eta0) - BG_CA)
    }
    else{stop("Model Not Recognized")}
  }
  
  
  ########### SEA 
  if (is.null(r)){#Default r is average
    r = 1/length(ions)
  }
  info_table = data %>% group_by(ion, L, Z.b) %>% summarise()
  info_table = suppressWarnings(left_join(data.frame(ions), info_table, by = c("ions" = "ion")))
  output = 0
  for (i in 1: nrow(info_table)){
    output = output + IDER(d*r, L = info_table$L[i], Z.b = info_table$Z.b[i], model = model, parameters = parameters)
  }
  return(output)
  stop("Model Not Recognized")
}


#Mixture/IEA
##Derivatives are in analytical formats for the original models. For new models, the numerical formats are used.
MIXDER_function = function(r, L, Z.b, d = seq(0, 0.2, by = 0.001), parameters = Data_parameter, model = "4para") {
  if (round(sum(r), 3) != 1){stop("r does not add up to one")}
  name = model
  parameters = parameters %>% filter((model == name) | parameter == "a")
  if (model == "4para"){
    kap0 = (parameters %>% filter(parameter == "kap0"))$value
    sig0 = (parameters %>% filter(parameter == "sig0"))$value
    eta0 = (parameters %>% filter(parameter == "eta0"))$value
    eta1 = (parameters %>% filter(parameter == "eta1"))$value
    a = (parameters %>% filter(parameter == "a"))$value
    dE=function(yini,State,Pars){
      eta0 = eta0; eta1 = eta1; sig0 = sig0; kap0 = kap0; a = a
      with(as.list(c(State, Pars)), {
        P = numeric(length = length(L))
        sig = numeric(length = length(L))
        etaa = numeric(length = length(L))
        u = numeric(length = length(L))
        dE = numeric(length = length(L))
        
        for (i in 1:length(L)) {
          if (L[i] > 5){ #If the ion is HZE
            P[i] = (1-exp(-Z.b[i]/kap0))^2
            sig[i] = sig0*P[i] + 0.041/6.24*L[i]*(1-P[i])
            etaa[i] = eta0*L[i]*exp(-eta1*L[i])
            #print(c(P = P[i], sig = sig[i], eta = etaa[i]))
            left = 0.001
            uni = try(uniroot(function(x) func_4para(x, L[i], Z.b[i], eta0, eta1, sig0, kap0) - BG_CA - I, interval = c(left, 1), maxiter = 10000, extendInt = "yes",tol = 10^-10))
            if (class(uni) == "try-error"){
              print(i)
              uni = uniroot(function(x) func_4para(x, L[i], Z.b[i], eta0, eta1, sig0, kap0) - BG_CA - I, interval = c(10^-10, left), 
                            tol = 10^-10)
              print(uni$root)
            }
            u[i] = uni$root
            dE[i] = (sig[i]*6.24/L[i]*exp(-1024*u[i]/L[i])*(exp(1024*u[i]/L[i]) + 1024*u[i]/L[i] - 1) + etaa[i]*10^5*exp(-10^5*u[i]))
          }
          else {
            dE[i] = a
          }
        }
        dI = as.numeric(r %*% dE)
        return(list(c(dI)))
      })
    }
  }
  
  else if (model == "3para"){
    sig0 = (parameters %>% filter(parameter == "sig0"))$value
    eta0 = (parameters %>% filter(parameter == "eta0"))$value
    eta1 = (parameters %>% filter(parameter == "eta1"))$value
    dE=function(yini,State,Pars){
      eta0 = eta0; eta1 = eta1; sig0 = sig0
      with(as.list(c(State, Pars)), {
        etaa = vector(length = length(L))
        u = vector(length = length(L))
        for (i in 1:length(L)) {
          etaa[i] = eta0*L[i]*exp(-eta1*L[i])
          u[i] = uniroot(function(d) sig0*6.24*d/L[i]*(1-exp(-1024*d/L[i])) + etaa[i]*(1-exp(-10^5*d)) - I, lower = 0, upper = 1, extendInt = "yes", tol = 10^-10)$root
        }
        dI = vector(length = length(L))
        for (i in 1:length(L)) {
          dI[i] = r[i]*(sig0*6.24/L[i]*exp(-1024*u[i]/L[i])*(exp(1024*u[i]/L[i]) + 1024*u[i]/L[i] - 1) + etaa[i]*10^5*exp(-10^5*u[i]))
        }
        dI = sum(dI)
        return(list(c(dI)))
      })
    }
  }
  else if (model == "2para"){
    sig0 = (parameters %>% filter(parameter == "sig0"))$value
    eta0 = (parameters %>% filter(parameter == "eta0"))$value
    a = (parameters %>% filter(parameter == "a"))$value
    
    dE = function(yini, State, Pars){
      sig0 = sig0; eta0 = eta0; a = a
      with(as.list(c(State, Pars)), {
        u = numeric(length = length(L))
        dE = numeric(length = length(L))
        for (i in 1:length(L)) {
          if (L[i] > 5){#If the ion is HZE
            u[i] = uniroot(function(x) func_2para(x, L[i], eta0, sig0) - BG_CA - I, lower = 0, upper = 1, extendInt = "yes", tol = 10^-10)$root
            dE[i] = sig0*6.24/L[i]*(1 - exp(-1024*u[i]/L[i]) + 1024*u[i]/L[i] * exp(-1024*u[i]/L[i])) + eta0*(10^5*exp(-10^5 * u[i]))
          }
          else{
            dE[i] = a
          }
        }
        dI = as.numeric(r %*% dE)
        return(list(c(dI)))
      })
    }
  }
  
  else if (model == "2paraTE"){
    kap = (parameters %>% filter(parameter == "kap0"))$value
    sig0 = (parameters %>% filter(parameter == "sig0"))$value
    a = (parameters %>% filter(parameter == "a"))$value
    
    dE=function(yini,State,Pars){
      kap = kap; sig0 = sig0; a = a
      with(as.list(c(State, Pars)), {
        P = numeric(length = length(L))
        sig = numeric(length = length(L))
        u = numeric(length = length(L))
        dE = numeric(length = length(L))
        for (i in 1:length(L)) {
          if (L[i] > 5){ #If the ion is HZE
            P[i] = (1-exp(-Z.b[i]/kap))^2
            sig[i] = sig0*P[i] + 0.041/6.24*L[i]*(1-P[i])
            u[i] = uniroot(function(d) sig[i]*6.24*d/L[i]*(1-exp(-1024*d/L[i])) - I, lower = 0, upper = 1, extendInt = "yes", tol = 10^-10)$root
            dE[i] = (sig[i]*6.24/L[i])*(1 - exp(-1024*u[i]/L[i])) + (1024/L[i]*exp(-1024*u[i]/L[i]))*(sig[i]*6.24*u[i]/L[i])
          }
          else{
            dE[i] = a
          }
        }
        dI = as.numeric(r %*% dE)
        return(list(c(dI)))
      })
    }
  }
  else if (model == "NEW1"){
    kap0 = (parameters %>% filter(parameter == "kap0"))$value
    sig0 = (parameters %>% filter(parameter == "sig0"))$value
    eta0 = (parameters %>% filter(parameter == "eta0"))$value
    eta1 = (parameters %>% filter(parameter == "eta1"))$value
    
    a = (parameters %>% filter(parameter == "a"))$value
    dE=function(yini,State,Pars){
      eta0 = eta0; sig0 = sig0; eta1 = eta1; kap0 = kap0; a = a
      with(as.list(c(State, Pars)), {
        P = numeric(length = length(L))
        sigma = numeric(length = length(L))
        u = numeric(length = length(L))
        dE = numeric(length = length(L))
        
        for (i in 1:length(L)) {
          if (L[i] > 5){ #If the ion is HZE
            P[i] = (1-exp(-Z.b[i]/kap0))^4
            sigma[i] = sig0*P[i] + (eta0*L[i]/6.24)*(1-P[i])
            left = 0.001
            uni = try(uniroot(function(x) func_NEW1(x, L[i], Z.b[i], kap0, sig0, eta0, eta1) - BG_CA - I, interval = c(left, 1), maxiter = 10000, extendInt = "yes",tol = 10^-10))
            if (class(uni) == "try-error"){
              print(i)
              uni = uniroot(function(x) func_NEW1(x, L[i], Z.b[i], kap0, sig0, eta0, eta1) - BG_CA - I, interval = c(10^-10, left), 
                            tol = 10^-10)
              print(uni$root)
            }
            u[i] = uni$root
            dE[i] = (sigma[i]*6.24/L[i])*
              (1-exp(-1024 * u[i]/L[i]))*
              ((1-exp(-1024 * u[i]/L[i])) + 2048 * u[i]/L[i]*exp(-1024*u[i]/L[i])) +
              eta1*exp(-10^5*u[i])*10^5
          }
          else {
            dE[i] = a
          }
        }
        dI = as.numeric(r %*% dE)
        return(list(c(dI)))
      })
    }
  }
  
  else if (model == "NEW2"){
    kap0 = (parameters %>% filter(parameter == "kap0"))$value
    sig0 = (parameters %>% filter(parameter == "sig0"))$value
    eta0 = (parameters %>% filter(parameter == "eta0"))$value
    a = (parameters %>% filter(parameter == "a"))$value
    dE=function(yini,State,Pars){
      eta0 = eta0; sig0 = sig0; kap0 = kap0; a = a
      with(as.list(c(State, Pars)), {
        P = numeric(length = length(L))
        sigma = numeric(length = length(L))
        u = numeric(length = length(L))
        dE = numeric(length = length(L))
        
        for (i in 1:length(L)) {
          if (L[i] > 5){ #If the ion is HZE
            P[i] = (1-exp(-Z.b[i]/kap0))^4
            sigma[i] = sig0*P[i] + (eta0*L[i]/6.24)*(1-P[i])
            left = 0.001
            uni = try(uniroot(function(x) func_NEW2(x, L[i], Z.b[i], kap0, sig0, eta0) - BG_CA - I, interval = c(left, 1), maxiter = 10000, extendInt = "yes",tol = 10^-10))
            if (class(uni) == "try-error"){
              print(i)
              uni = uniroot(function(x) func_NEW2(x, L[i], Z.b[i], kap0, sig0, eta0) - BG_CA - I, interval = c(10^-10, left), 
                            tol = 10^-10)
              print(uni$root)
            }
            u[i] = uni$root
            dE[i] = (sigma[i]*6.24/L[i])*
              (1-exp(-1024 * u[i]/L[i]))*
              ((1-exp(-1024 * u[i]/L[i])) + 2048 * u[i]/L[i]*exp(-1024*u[i]/L[i]))
          }
          else {
            dE[i] = a
          }
        }
        dI = as.numeric(r %*% dE)
        return(list(c(dI)))
      })
    }
  }
  else{
    stop("Model Not Recognized")
  }
  pars = NULL; yini = c(I= 0); d = d
  out = ode(yini,times = d, dE, pars)
  return(out)
}
