get_exogenous_variables = function(date_start, date_end){
  t_bill_rate = tidyquant::tq_get("TB3MS",
                    get = "economic.data",
                    from = date_start,
                    to = date_end) %>% 
    dplyr::select(timestamp = date, rf_rate = price) %>% 
    dplyr::mutate(rf_rate = rf_rate / 12)
  
  exogenous_list = list(t_bill_rate = t_bill_rate) %>% 
    purrr::reduce(dplyr::full_join)
  
  return(exogenous_list)
}
