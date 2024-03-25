data_preprocessing = function(optimized_portfolio_weights, split_data){
  
  lin_reg_recipe = recipes::recipe(weight ~., split_data$training) %>% 
    recipes::step_date(
      timestamp,
      features = c("month"),
      keep_original_cols = FALSE
    ) %>% 
    recipes::step_dummy(
      recipes::all_nominal_predictors()
      ) %>% 
    recipes::step_normalize(
      recipes::all_numeric_predictors(), - dplyr::contains(c("symbol", "timestamp"))
      )
  
  recipes = list(lin_reg_recipe = lin_reg_recipe)
  
  return(recipes)
}
