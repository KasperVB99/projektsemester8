fit_and_predict = function(defined_workflow, split_data){
  suppressPackageStartupMessages(library(tidymodels))
    
  fit = fit(
    defined_workflow,
    data = split_data$training
  )
  
  is_predict = predict(
    fit,
    new_data = split_data$training
  ) %>% 
    dplyr::mutate(symbol = split_data$training$symbol,
                  timestamp = split_data$training$timestamp)
  
  oos_predict = predict(
    fit,
    new_data = split_data$testing
  ) %>% 
    dplyr::mutate(symbol = split_data$testing$symbol,
                  timestamp = split_data$testing$timestamp)
  
  fitted_and_predicted = list(fit = fit,
                              is_predict = is_predict,
                              oos_predict = oos_predict)
  
  return(fitted_and_predicted)
}