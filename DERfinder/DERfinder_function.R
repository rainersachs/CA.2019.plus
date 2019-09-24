SwiftLight_data <- filter(main_df, Z <= 2 & d>0)
HZE_data = main_df %>% filter(Z > 3 & d > 0)
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

start_values = list( 
  #Potential parameters for HZE
  eta0 = 0.001,
  eta1 = 0.01,
  sig0 = 100,
  kap0 = 500, 
  #Potential parameters for SwiftLight
  a = -0.5, b = -10, C = -1
)

calibration_HZE = function(model_name, data = HZE_data, StartValues = start_values, return_model = F){
  #A function that returns the calibrated model for HZE and only takes in a model name (as string)
  #StartValues is a list with starting guesses assigned to each parameter.
  
  func_name = paste("func_", model_name, sep ="") #This is the name of the function corresponding to the model
  func = eval(parse(text = func_name)) #This is the actual function
  arguments = formalArgs(func)
  start = StartValues[names(StartValues) %in% arguments] #Getting the start values for the corresponding parameters
  formula = paste("CA ~ ", func_name, "(", paste(arguments, collapse = ", "), ")", sep = "")
  model = nlsLM(formula = formula, data = data, start = start, weights = (1/(data$error)^2))
  coefs = coef(model)
  parameters_df = data.frame(value = as.numeric(coefs), model = model_name, parameter = names(coefs))
  if(return_model){
    return(model)
  }
  else{
    return(list(
      parameters = parameters_df,
      sigma = vcov(model),
      model = model))
  }
}

# calibration_SLI = function(model_type, npara, data = SwiftLight_data, return_model = F){
#   if (grepl("linear", tolower(model_type))){ #If this is a linear model, then we do not need to use nls anymore.
#     if (npara == 1){
#       model = lm(I(CA - BG_CA) ~ 0 + d, data = data, weights = 1/error^2)
#     }
#     else if (npara == 2){
#       model = lm(I(CA - BG_CA) ~ d, data = data, weights = 1/error^2)
#     }
#   }
#   else if (grepl("exp", tolower(model_type))){
#     
#   }
# }

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
model_sli_linear1 <- lm(I(CA - BG_CA) ~ 0 + d, data = SwiftLight_data, weights = (1/(SwiftLight_data$error)^2))
ccoef <- coef(model_sli_linear1)
parameters_sli_lin1 <- data.frame(value = as.numeric(ccoef), model = "sli_lin1", parameter = names(ccoef))
sig_lin1 <- vcov(model_sli_linear1)

#Calibration for SLI Linear 2 parameter
model_sli_linear2 <- lm(I(CA - BG_CA) ~ 0 + d + I(d^2), data = SwiftLight_data, weights = (1/(SwiftLight_data$error)^2))
ccoef <- coef(model_sli_linear2)
parameters_sli_lin2 <- data.frame(value = as.numeric(ccoef), model = "sli_lin2", parameter = names(ccoef))
sig_lin2 <- vcov(model_sli_linear2)

#================================ MODEL COMPARISON ==================================#

AIC_function = function(model_name) {
  model = calibration_HZE(model_name = model_name, return_model = T)
  RS = resid(model)**2 %>% as.numeric()
  n = length(RS)
  k = model %>% coef %>% length
  WRSS = sum((1/HZE_data$error^2)*RS)
  return(n + n*log(2*pi) + n*log(WRSS/n) + 2*(k+1))
}
BIC_function = function(model_name) {
  model = calibration_HZE(model_name = model_name, return_model = T)
  RS = resid(model)**2 %>% as.numeric()
  n = length(RS)
  k = model %>% coef %>% length
  WRSS = sum((1/HZE_data$error^2)*RS)
  return(n + n*log(2*pi) + n*log(WRSS/n) + log(n)*(k+1))
}

CV_function = function(model_name, data = HZE_data, by = "label", by_fold = F){
  #Leave-one-ion-out Cross Validation. The train set is one type of ion. Outputs a weighted sum of weighted MSE
  #by_fold = T gives Weighted MSE by each fold (each ion) with the test size (number of rows in the data belonging to that ion)
  if (tolower(by) %in% c("label", "labels")){
    folds = unique(data$label)
    by = "label"
  }
  else if(tolower(by) %in% c("ion","ions")){
    folds = unique(data$ion)
    by = "ion"
  }
  WMSE = numeric(length(folds))
  n = numeric(length(folds))
  for (i in 1:length(folds)){
    train_index = which(folds[i] == data[, by])
    train = data[-train_index,]
    test = data[train_index,]
    n[i] = nrow(test)
    model = calibration_HZE(model_name, data = train, return_model = T)
    prediction = predict(model, test)
    RS = (test$CA - prediction)**2
    WMSE[i] = mean((1/test$error^2)*RS)
  }
  if(by_fold){return(data.frame(ion = folds, WMSE = WMSE, n = n))}
  else{return(sum(n*WMSE/nrow(data)))}
}
