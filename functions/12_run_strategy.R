run_strategy = function(fitted_and_predicted, split_data, period = "oos"){
  
  market_prices = raw_data$cleaned_raw_data %>% 
    dplyr::filter(timestamp >= "2014-01-07") %>% 
    tidyr::drop_na() %>%   # der skal laves en mere elegant løsning end dette på et tidspunkt
    dplyr::group_by(year = lubridate::year(timestamp), month = lubridate::month(timestamp), symbol) %>%
    dplyr::mutate(eighth_or_next = as.integer((lubridate::day(timestamp) >= 8 & dplyr::row_number() >= 1)),
                  invest_day = dplyr::if_else(eighth_or_next == 1 & cumsum(eighth_or_next) == 1, 1, 0),
                  invest_day = dplyr::if_else(dplyr::lead(invest_day) == 1, -1, invest_day)) %>%
    dplyr::ungroup() %>% 
    dplyr::select(-eighth_or_next) %>% 
    dplyr::filter(invest_day != 0) %>% 
    dplyr::group_by(month, year, symbol) %>% 
    dplyr::mutate(market_price = dplyr::lag(close),
                  buy_price = open,
                  timestamp = lubridate::floor_date(timestamp, unit = "months")) %>% 
    dplyr::filter(invest_day == 1) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(symbol, timestamp, market_price, buy_price)

  
  is_dates = split_data$training %>% 
    dplyr::select(timestamp)
  
  oos_dates = split_data$testing
  
  fitted_and_predicted = linear_reg_fitted_and_predicted
  
  is_predicted = fitted_and_predicted$is_predict %>% 
    dplyr::bind_cols(is_dates) %>% 
    dplyr::select(timestamp, dplyr::starts_with(".pred"))
    
  oos_predicted = fitted_and_predicted$oos_predict %>% 
    dplyr::bind_cols(oos_dates) %>% 
    dplyr::select(timestamp, dplyr::starts_with(".pred"))
  
  suggested_weights = oos_predicted %>% 
    tidyr::pivot_longer(cols = -timestamp, names_to = "symbol", values_to = "target_weight") %>% 
    dplyr::mutate(symbol = gsub("^\\.pred_", "", symbol)) %>% 
    dplyr::inner_join(market_prices) %>% 
    dplyr::group_by(timestamp) %>% 
    dplyr::mutate(target_weight = pmax(0, target_weight),
                  target_weight = target_weight / sum(target_weight)) %>% 
    dplyr::group_split()

  final_portfolio_list = list()
  previous_result = list(NULL)
  
  for (i in 1:length(suggested_weights)){
    
    final_portfolio_list[[i]] = reallocate_portfolio(suggested_weights[[i]], previous_result[[i]])
    previous_result[[(i+1)]] = final_portfolio_list[[i]]
  }
  
  final_portfolio = final_portfolio_list %>% 
    purrr::reduce(dplyr::bind_rows)
  
  return(final_portfolio)
  
}
