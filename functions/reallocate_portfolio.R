reallocate_portfolio = function(){
  library(CVXR)
  
  # Define the parameters
  budget <- 10000
  market_prices <- c(250, 250)
  target_allocation_weights <- c(0.25, 0.75)
  
  # Define the optimization variable
  x <- Variable(length(target_allocation_weights), integer=TRUE)
  positions = c(20, 80)
  
  # Define expressions for asset class values and actual allocation weights
  asset_class_values <- x * market_prices + positions * market_prices
  total_value <- sum(positions * market_prices) + budget
  actual_allocation_weights <- asset_class_values / total_value
  
  # Define the portfolio drift as the objective
  portfolio_drift <- sum(abs(actual_allocation_weights - target_allocation_weights)) / 2
  
  # Objective function
  objective <- Minimize(portfolio_drift)
  
  # Total cost constraint
  total_cost <- sum(x * market_prices)
  
  # Define the constraints
  constraints <- list(x >= 0,
                      budget >= total_cost,
                      (budget - total_cost) <= min(market_prices) - 1e-2)
  
  # Solve the problem
  problem <- Problem(objective, constraints)
  result <- solve(problem)
  
  result[[1]]
  
  # Extract the positions
  positions <- round(value(x))
  
  # Print positions
  print(positions)
  
  # Calculate and print the rounded allocation weights
  allocation_weights <- round(positions * market_prices / sum(positions * market_prices), 2)
  print(allocation_weights)
  
}