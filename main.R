#-------------------------------------------------------------------------------

targets::tar_make_future(workers = 8)

#-------------------------------------------------------------------------------

targets::tar_prune()

#-------------------------------------------------------------------------------

targets::tar_load_everything()
targets::tar_load_globals()

#-------------------------------------------------------------------------------

hej = lin_reg_recipe %>% 
  recipes::prep() %>% 
  recipes::bake(new_data = NULL)

split_data$resamples$resamples$splits[[15]][3]

hej = broom::tidy(linear_reg_fitted_and_predicted$fit$fit$fit) %>% 
  dplyr::select(response, term, p.value) %>% 
  dplyr::mutate(p.value = round(p.value, 3))


model_tuning(split_data = split_data, raw_data = raw_data,
             defined_workflow$linear_reg_workflow, model = "lin_reg")

engineered_features %>%
  dplyr::bind_cols(
    weights = engineered_features %>%
      dplyr::select(excees_returns, rolling_sum_1_quart, std_dev_1_quart) %>%
      as.matrix() %*% optimal_theta %>% as.numeric())

theta = rep(1, 3)

optimal_theta = find_optimal_theta(theta, split_data$training, allow_short_selling = FALSE)

weights = compute_simple_portfolio_weights(optimal_theta$par, split_data$training) %>% 
  dplyr::select(.pred = weights, symbol, timestamp) %>% 
  run_strategy(., split_data, raw_data, period = "is")

weights %>% 
  dplyr::select(timestamp, symbol, weights, excees_returns) %>% 
  dplyr::group_by(timestamp) %>% 
  dplyr::summarise(portfolio_returns = weighted.mean(excees_returns * weights)) %>% 
  dplyr::ungroup() %>% 
  dplyr::summarise(sharpe = mean(portfolio_returns, na.rm = TRUE) / sd(portfolio_returns, na.rm = TRUE),
                   utility = mean((1 + portfolio_returns)^(1 - 5) / (1 - 5)),
                   pnl = sum(portfolio_returns))

optimal_theta$convergence

pnl_calculation = weights %>% 
  dplyr::select(timestamp, symbol, weights, excees_returns) %>% 
  dplyr::group_by(timestamp) %>% 
  dplyr::summarise(portfolio_returns = weighted.mean(excees_returns * weights)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(cum_pnl = cumsum(portfolio_returns)) %>% 
  dplyr::select(timestamp, portfolio_returns, cum_pnl)

sp500_returns = weights %>% 
  dplyr::filter(symbol == "SXR8.DE") %>% 
  dplyr::mutate(cum_pnl = cumsum(monthly_returns),
                symbol = "sp500") %>% 
  dplyr::select(timestamp, sp500_returns = monthly_returns)

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

