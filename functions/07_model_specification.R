model_specification = function(){
  model_spec_linear_reg = parsnip::linear_reg(
    mode = "regression",
    engine = "glmnet",
    penalty = parsnip::tune()
  )
  
  models = list(model_spec_linear_reg = model_spec_linear_reg)
  
  return(models)
}