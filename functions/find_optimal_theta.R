find_optimal_theta = function(theta,
                              data,
                              allow_short_selling = FALSE){
  
  get_sharpe = function(theta,
                        data,
                        allow_short_selling = FALSE){
    weights = compute_simple_portfolio_weights(theta = theta,
                                             data = data)
  
    sharpe = weights %>% 
      dplyr::select(timestamp, symbol, weights, excees_returns) %>% 
      dplyr::group_by(timestamp) %>% 
      dplyr::summarise(portfolio_returns = weighted.mean(excees_returns * weights)) %>% 
      dplyr::ungroup() %>% 
      dplyr::summarise(sharpe = mean(portfolio_returns, na.rm = TRUE) / sd(portfolio_returns, na.rm = TRUE),
                       utility = mean((1 + portfolio_returns)^(1 - 5) / (1 - 5)),
                       pnl = sum(portfolio_returns)) %>% 
      dplyr::pull(sharpe)
    
    return(-sharpe)
  }
  
  optimal_theta = optim(
    par = theta,
    fn = get_sharpe,
    data = data,
    allow_short_selling = allow_short_selling,
    method = "Nelder-Mead"
  )
  
  return(optimal_theta)
}
