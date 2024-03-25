calculate_PnL = function(final_portfolio, cv = FALSE, model = "Lin. Reg."){
  
  pnl_calculation = final_portfolio %>% 
    dplyr::select(timestamp, new_portfolio_weights, symbol, market_price, buy_price) %>% 
    dplyr::group_by(symbol) %>% 
    dplyr::mutate(returns = tidyr::replace_na(log(market_price / dplyr::lag(buy_price)), 0)) %>% 
    dplyr::group_by(symbol) %>% 
    dplyr::mutate(portfolio_returns = dplyr::lag(new_portfolio_weights) * returns * 100) %>% 
    dplyr::group_by(timestamp) %>% 
    dplyr::summarise(portfolio_returns = sum(portfolio_returns)) %>% 
    dplyr::ungroup() %>% 
    tidyr::drop_na() %>% 
    dplyr::mutate(cum_pnl = cumsum(portfolio_returns),
                  symbol = model) %>% 
    dplyr::group_by(symbol) %>% 
    dplyr::select(timestamp, symbol, portfolio_returns, cum_pnl)
  
  avg_weight = final_portfolio %>% 
    dplyr::mutate(weight_dist = sqrt((target_weight - new_portfolio_weights)^2) / new_portfolio_weights) %>% 
    dplyr::group_by(symbol) %>% 
    dplyr::summarise(avg_weight = mean(target_weight),
                     avg_actual_weight = mean(new_portfolio_weights),
                     avg_dist = mean(weight_dist, na.rm = TRUE))
  
  sp500_returns = final_portfolio %>% 
    dplyr::filter(symbol == "SXR8.DE") %>% 
    dplyr::mutate(returns = tidyr::replace_na(log(market_price / dplyr::lag(buy_price)) * 100, 0)) %>% 
    dplyr::mutate(cum_pnl = cumsum(returns),
                  symbol = "sp500") %>% 
    dplyr::select(timestamp, sp500_returns = returns)
  
  plot_tbl = dplyr::inner_join(pnl_calculation, sp500_returns) %>% 
    tidyr::pivot_longer(c(portfolio_returns, sp500_returns), names_to = "model", values_to = "returns") %>% 
    dplyr::group_by(model) %>% 
    dplyr::mutate(cum_pnl = cumsum(returns)) %>% 
    ggplot2::ggplot(ggplot2::aes(x = timestamp, y = cum_pnl, col = model)) +
    ggplot2::geom_line()
  
  metrics = pnl_calculation %>% 
    dplyr::inner_join(sp500_returns) %>% 
    dplyr::mutate(excess = portfolio_returns - sp500_returns) %>% 
    dplyr::summarise(pnl = sum(portfolio_returns),
                     sd = sd(portfolio_returns),
                     sharpe = (mean(portfolio_returns) * 12) / sd(portfolio_returns) / sqrt(12),
                     info_ratio = (mean(excess) * 12) / sd(excess) / sqrt(12),
                     mean_return = mean(portfolio_returns))
  
  pnl_list = list(
    metrics = metrics,
    plot = plot_tbl
  )
  
  if (cv == TRUE){
    only_sharpe = metrics %>% 
      dplyr::select(info_ratio)
    
    return(only_sharpe)
  } else {
    return(pnl_list)
    
  }
  
}
