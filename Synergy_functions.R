start_values = list( 
  #Potential parameters for HZE
  eta0 = 0.001,
  eta1 = 0.01,
  sig0 = 5,
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

calibration_SLI = function(model_type, npara, data = SwiftLight_data, return_model = F){
  if (grepl("linear", tolower(model_type))){ #If this is a linear model, then we do not need to use nls anymore.
    if (npara == 1){
      model = lm(I(CA - BG_CA) ~ 0 + d, data = data, weights = 1/error^2)
    }
    else if (npara == 2){
      model = lm(I(CA - BG_CA) ~ d, data = data, weights = 1/error^2)
    }
  }
  else if (grepl("exp", tolower(model_type))){
    
  }
}

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
