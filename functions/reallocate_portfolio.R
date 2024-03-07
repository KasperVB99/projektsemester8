reallocate_portfolio = function(suggested_weights, previous_result){
  
  # Define the parameters
  if(is.null(previous_result) == TRUE){
    budget = 10000
  } else{
    budget = 10000 + previous_result$money_leftover[1]
  }
  
  market_prices = suggested_weights$market_price
  buy_prices = suggested_weights$buy_price
  target_allocation_weights <- suggested_weights$target_weight
  
  # Define the optimization variable
  x = CVXR::Variable(length(target_allocation_weights), integer=TRUE)
  if(is.null(previous_result) == TRUE){
    current_positions = rep(0, nrow(suggested_weights))
  } else{
    current_positions = previous_result$new_position
  }
  
  # Define expressions for asset class values and actual allocation weights
  asset_class_values = x * buy_prices + current_positions * market_prices
  total_value = sum(current_positions * market_prices) + budget
  actual_allocation_weights = asset_class_values / total_value
  
  # Define the portfolio drift as the objective
  portfolio_drift = sum(abs(actual_allocation_weights - target_allocation_weights)) / 2
  
  # Objective function
  objective = CVXR::Minimize(portfolio_drift)
  
  # Total cost constraint
  total_cost = sum(x * buy_prices)
  
  # Define the constraints
  constraints = list(x >= 0,
                      budget >= total_cost,
                      (budget - total_cost) <= min(market_prices) - 1e-2)
  
  # Solve the problem
  problem = CVXR::Problem(objective, constraints)
  result = CVXR::solve(problem)

  buy_assets = result[[1]]
  
  new_positions = buy_assets + current_positions
  new_portfolio_weights = (new_positions) * buy_prices / sum((new_positions) * buy_prices)
  money_used = buy_assets * buy_prices
  
  output = suggested_weights %>% 
    dplyr::mutate(new_position = new_positions[,1],
                  new_portfolio_weights = new_position * buy_price / (sum(new_position * buy_price)),
                  money_leftover = budget - sum(money_used))
  
  return(output)
}
