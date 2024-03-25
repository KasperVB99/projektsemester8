compute_simple_portfolio_weights = function(theta,
                                            data,
                                            allow_short_selling = FALSE) {
  portfolio_weights = data %>%
    dplyr::group_by(timestamp) %>% 
    dplyr::bind_cols(
      tilt_weights = data %>%
        dplyr::select(excees_returns, rolling_sum_1_quart, std_dev_1_quart) %>%
        dplyr::transmute(dplyr::across(dplyr::everything(), ~ scale(.))) %>% 
        as.matrix() %*% theta %>% as.numeric()
    ) %>%
    dplyr::mutate(
      benchmark_weights = 1 / dplyr::n_distinct(symbol),
      weights = tilt_weights,
      # Short-sell constraint
      weights = pmax(0, weights),
      # Weights sum up to 1
      weights = weights / sum(weights),
      weights = dplyr::if_else(is.nan(weights), benchmark_weights, weights)
    ) %>%
    dplyr::ungroup()
  
  return(portfolio_weights)
}
