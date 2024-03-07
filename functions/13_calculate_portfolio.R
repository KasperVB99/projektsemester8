calculate_portfolio = function(final_portfolio){
  
  pnl_calculation = final_portfolio %>% 
    dplyr::select(timestamp, new_portfolio_weights, symbol, market_price, buy_price) %>% 
    dplyr::group_by(symbol) %>% 
    dplyr::mutate(returns = tidyr::replace_na(log(market_price / dplyr::lag(buy_price)), 0)) %>% 
    dplyr::group_by(timestamp) %>% 
    dplyr::summarise(portfolio_returns = sum(new_portfolio_weights * returns)) %>% 
    dplyr::ungroup() %>% 
    tidyr::drop_na() %>% 
    dplyr::mutate(cum_pnl = cumsum(portfolio_returns),
                  symbol = "Lin. reg. portfolio") %>% 
    dplyr::select(timestamp, symbol, portfolio_returns, cum_pnl)
  
  sp500_returns = final_portfolio %>% 
    dplyr::filter(symbol == "SXR8.DE") %>% 
    dplyr::mutate(returns = tidyr::replace_na(log(market_price / dplyr::lag(buy_price)), 0)) %>% 
    dplyr::mutate(cum_pnl = cumsum(returns)) %>% 
    dplyr::select(timestamp, symbol, cum_pnl)
    
  hej = dplyr::bind_rows(pnl_calculation, sp500_returns)
  
  metrics = pnl_calculation %>% 
    dplyr::summarise(pnl = sum(portfolio_returns),
                     sd = sd(portfolio_returns),
                     sharpe = pnl/sd)
  
  ggplot2::ggplot(hej, ggplot2::aes(x = timestamp, y = cum_pnl, group = symbol, col = symbol)) +
    ggplot2::geom_line() +
    ggthemes::theme_economist() +
    ggplot2::labs(title = "PnL of strategy") +
    ggplot2::ylab("Cumulative PnL") +
    ggplot2::xlab("")
  
}