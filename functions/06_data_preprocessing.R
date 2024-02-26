data_preprocessing = function(optimized_portfolio_weights, split_data){
  outcome_variables = colnames(optimized_portfolio_weights)[-1]
  
  linear_reg_recipe = recipes::recipe(~ ., split_data$training) %>% 
    recipes::update_role(tidyselect::all_of(outcome_variables), new_role = "outcome") %>% 
    recipes::step_date(timestamp, 
                       features = c("dow", "month"),
                       keep_original_cols = FALSE) %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())
  
  recipes = list(linear_reg_recipe = linear_reg_recipe)
}