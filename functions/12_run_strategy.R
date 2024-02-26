run_strategy = function(fitted_and_predicted, split_data, period = "oos"){
  
  raw_data$cleaned_raw_data
  
  is_dates = split_data$training %>% 
    dplyr::select(timestamp)
  
  oos_dates = split_data$testing
  
  fitted_and_predicted = linear_reg_fitted_and_predicted
  
  is_predicted = fitted_and_predicted$is_predict %>% 
    dplyr::bind_cols(is_dates) %>% 
    dplyr::select(timestamp, dplyr::everything())
    
  oos_predicted = fitted_and_predicted$oos_predict
  
  suggested_weights = is_predicted %>% 
    tidyr::pivot_longer(cols = -timestamp, names_to = "symbol", values_to = "weight") %>% 
    dplyr::group_by(timestamp) %>% 
    dplyr::mutate(weight = dplyr::if_else(weight < 0, 0, weight),
                  weight = dplyr::if_else(weight > 1, 1, weight),
                  weight = weight / sum(weight)) %>%
    tidyr::pivot_wider(values_from = weight, names_from = symbol) %>% 
    dplyr::ungroup() %>% 
    dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~ mean(.x, na.rm = TRUE)))
  
  
  
}