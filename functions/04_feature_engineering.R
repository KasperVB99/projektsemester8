feature_engineering = function(cleaned_raw_data, optimized_portfolio_weights){

  features = cleaned_raw_data %>% 
    dplyr::filter(timestamp >= "2014-01-08") %>% 
    dplyr::group_by(year = lubridate::year(timestamp), month = lubridate::month(timestamp), symbol) %>%
    dplyr::mutate(eighth_or_next = as.integer((lubridate::day(timestamp) >= 8 & dplyr::row_number() >= 1)),
                  invest_day = dplyr::if_else(eighth_or_next == 1 & cumsum(eighth_or_next) == 1, 1, 0)) %>% 
    dplyr::filter(invest_day == 1) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(timestamp = lubridate::floor_date(timestamp, unit = "months")) %>% 
    dplyr::select(symbol, timestamp, close) %>% 
    dplyr::group_by(symbol) %>% 
    dplyr::mutate(monthly_returns = log(close/ dplyr::lag(close)),
                  rolling_sum_1_quart = slider::slide_index_dbl(.x = monthly_returns, 
                                                            .i = timestamp,
                                                            .f = ~sum(.x, na.rm = TRUE),
                                                            .before = months(3),
                                                            .after = -months(1),
                                                            complete = TRUE),
                  rolling_sum_half_year = slider::slide_index_dbl(.x = monthly_returns, 
                                                                .i = timestamp,
                                                                .f = ~sum(.x, na.rm = TRUE),
                                                                .before = months(6),
                                                                .after = -months(1),
                                                                complete = TRUE)) %>% 
    dplyr::select(-close) %>% 
    tidyr::pivot_wider(names_from = "symbol", values_from = c(monthly_returns, rolling_sum_1_quart, rolling_sum_half_year)) %>% 
    dplyr::full_join(optimized_portfolio_weights) %>% 
    tidyr::drop_na()
  
    return(features)
}