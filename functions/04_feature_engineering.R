feature_engineering = function(cleaned_raw_data, optimized_portfolio_weights){

  exogenous_variables = get_exogenous_variables(date_start, date_end)
  
  longer_portfolio_weights = optimized_portfolio_weights %>% 
    tidyr::pivot_longer(-timestamp, names_to = "symbol", values_to = "weight")
    
  features = cleaned_raw_data %>% 
    dplyr::filter(timestamp >= "2013-06-01") %>% 
    dplyr::group_by(year = lubridate::year(timestamp), month = lubridate::month(timestamp), symbol) %>%
    dplyr::mutate(eighth_or_next = as.integer((lubridate::day(timestamp) >= 8 & dplyr::row_number() >= 1)),
                  invest_day = dplyr::if_else(eighth_or_next == 1 & cumsum(eighth_or_next) == 1, 1, 0),
                  calc_day = dplyr::lead(invest_day)) %>% 
    dplyr::filter(calc_day == 1) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(timestamp = lubridate::floor_date(timestamp, unit = "months")) %>% 
    dplyr::left_join(exogenous_variables) %>% 
    dplyr::select(symbol, timestamp, close, rf_rate) %>% 
    dplyr::group_by(symbol) %>% 
    dplyr::mutate(monthly_returns = log(close / dplyr::lag(close, 1)),
                  excees_returns = monthly_returns - rf_rate,
                  rolling_sum_1_quart = slider::slide_index_dbl(.x = excees_returns, 
                                                            .i = timestamp,
                                                            .f = ~sum(.x, na.rm = TRUE),
                                                            .before = months(6),
                                                            .after = -months(0),
                                                            complete = TRUE),
                  std_dev_1_quart = slider::slide_index_dbl(.x = excees_returns, 
                                                            .i = timestamp,
                                                            .f = ~sd(.x, na.rm = TRUE),
                                                            .before = months(6),
                                                            .after = -months(0),
                                                            complete = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-close) %>% 
    dplyr::full_join(longer_portfolio_weights) %>% 
    tidyr::drop_na() %>% 
    dplyr::arrange(timestamp)
  
    return(features)
}
