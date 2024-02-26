define_workflows = function(preprocessed_data, specified_models){
  linear_reg_model = specified_models$model_spec_linear_reg
  
  linear_reg_recipe = preprocessed_data$linear_reg_recipe
  
  linear_reg_workflow = workflows::workflow() %>% 
    workflows::add_model(linear_reg_model) %>% 
    workflows::add_recipe(linear_reg_recipe)
  
  workflows_list = list(linear_reg_workflow = linear_reg_workflow)
  
  return(workflows_list)
}