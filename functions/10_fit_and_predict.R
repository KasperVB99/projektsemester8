fit_and_predict = function(defined_workflows, split_data, model = "linear_reg"){
  library(tidymodels)
  
  if (model == "linear_reg"){
    workflow = defined_workflows$linear_reg_workflow
    
    fit = fit(workflow, data = split_data$training)
    is_predict = predict(fit, new_data = split_data$training)
    oos_predict = predict(fit, new_data = split_data$testing)
  }
  
  fitted_and_predicted = list(fit = fit,
                              is_predict = is_predict,
                              oos_predict = oos_predict)
  
  return(fitted_and_predicted)

}