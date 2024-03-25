run_strategy = function(fitted_and_predicted, split_data, raw_data, period = "is", reallocate_on){
  
  if (period == "is"){
    dates = split_data$training %>% 
      dplyr::select(timestamp)
  } else if (period == "oos") {
    dates = split_data$testing %>% 
      dplyr::select(timestamp)
  } else stop("period has to be either \"is\" for in-sample or \"oos\" for out-of-sample. Use \"is\" for cross-validation")
  
  if (!reallocate_on %in% c("monthly", "quarterly", "semi-annually", "annually", "never")){
    stop("reallocate_on has to be one off \"monthly\", \"quarterly\", \"semi-annually\", \"annually\",
         \"never\". Please check your spelling.")
  }
  
  market_prices = raw_data$cleaned_raw_data %>% 
    dplyr::filter(timestamp >= "2015-01-07") %>% 
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
    dplyr::mutate(market_price = dplyr::if_else(is.na(market_price) == TRUE, buy_price, market_price)) %>% 
    dplyr::select(symbol, timestamp, market_price, buy_price)

  suggested_weights = weights %>% 
    dplyr::rename(target_weight = .pred) %>% 
    dplyr::inner_join(market_prices) %>% 
    dplyr::group_by(timestamp) %>% 
    dplyr::mutate(target_weight = pmax(0, target_weight),
                  target_weight = target_weight / sum(target_weight)) %>% 
    dplyr::group_split()

  if (reallocate_on == "monthly"){
    allocation_periods = seq(0, length(suggested_weights), by = 1)
  } else if (reallocate_on == "quarterly"){
    allocation_periods = seq(0, length(suggested_weights), by = 3)
  } else if (reallocate_on == "semi_annually"){
    allocation_periods = seq(0, length(suggested_weights), by = 6)
  } else if (reallocate_on == "annually"){
    allocation_periods = seq(0, length(suggested_weights), by = 12)
  } else if (reallocate_on == "biannually"){
    allocation_periods = seq(0, length(suggested_weights), by = 24)
  } else if (reallocate_on == "never"){
    allocation_periods = 0
  }
  
  final_portfolio_list = list()
  previous_result = list(NULL)
  
  for (i in 1:length(suggested_weights)){
    if (i %in% allocation_periods){
      final_portfolio_list[[i]] = reallocate_portfolio(suggested_weights[[i]], previous_result[[i]],
                                                       allow_selling = TRUE)
      previous_result[[(i+1)]] = final_portfolio_list[[i]]
    } else {
      final_portfolio_list[[i]] = reallocate_portfolio(suggested_weights[[i]], previous_result[[i]],
                                                       allow_selling = FALSE)
      previous_result[[(i+1)]] = final_portfolio_list[[i]]
    }
  }
  
  final_portfolio = final_portfolio_list %>% 
    purrr::reduce(dplyr::bind_rows)
  
  return(final_portfolio)
  
}
