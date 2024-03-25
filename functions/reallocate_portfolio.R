reallocate_portfolio = function(suggested_weights, previous_result, allow_selling = FALSE){
  minimum_transaction_fee = 12
  transaction_fee = 0.0015
  minimum_sell_price_for_percent = minimum_transaction_fee/transaction_fee
  exchange_fee = 0.0025
  total_transaction_fee = transaction_fee + exchange_fee
  
  suggested_weights = suggested_weights[[1]]
  
  # Definerer vores budget alt efter om vi er i første periode eller ej:
  if(is.null(previous_result)){
    budget = 10000
  } else{
    budget = 10000 + previous_result$money_leftover[1]
  }
  
  # Definerer de priser vi kender inden køb, de priser vi køber til og vores målvægte til hvert aktiv.
  market_prices = suggested_weights$market_price
  buy_prices = suggested_weights$buy_price
  target_allocation_weights = suggested_weights$target_weight
  
  # Definerer den variabel, jeg gerne vil kende størrelsen på (hvor meget der skal købes/sælges af hvert aktiv):
  x = CVXR::Variable(length(target_allocation_weights), integer=TRUE)
  
  # Definerer mine nuværende positioner, som er 0, hvis det er første periode.
  if(is.null(previous_result) == TRUE){
    current_positions = rep(0, nrow(suggested_weights))
  } else{
    current_positions = previous_result$new_position
  }
  
  # Definerer vægten af hvert aktiv, den totale værdi af porteføljen, og de faktiske vægte:
  asset_class_values = x * buy_prices + current_positions * market_prices
  total_value = sum(current_positions * market_prices) + budget
  actual_allocation_weights = asset_class_values / total_value
  
  # Definerer portfolio drift som den variabel, jeg gerne vil minimere størrelsen af:
  portfolio_drift = sum(abs(actual_allocation_weights - target_allocation_weights)) / 2
  
  # Definerer, at portfolio drift skal minimeres
  objective = CVXR::Minimize(portfolio_drift)
  
  # Udregner mine totale omkostninger ved at omlægge porteføljen
  sell_amounts = CVXR::pos(-x)
  sell_values = sell_amounts * buy_prices # Ganger med -1 for at få positive værdier.
  total_transaction_costs = sell_values * total_transaction_fee
  total_cost = sum(x * buy_prices)
  
  
  # Definerer mine constrains; omkostninger mindre end budget, og money left over skal være mindre end den billigste ETF.
  if (allow_selling == FALSE){
    constraints = list(x >= 0,
                       budget >= total_cost,
                       (budget - total_cost) <= min(market_prices) - 1e-2)
  } else if (allow_selling == TRUE){
    constraints = list((x + current_positions) >= 0,
                       (budget - total_cost - total_transaction_costs) >= 0,
                       (budget - total_cost) <= min(market_prices) - 1e-2)
  }
  
  # Solver problemet:
  problem = CVXR::Problem(objective, constraints)
  result = CVXR::solve(problem)

  buy_assets = result[[1]]
  
  new_positions = buy_assets + current_positions
  new_portfolio_weights = (new_positions) * buy_prices / sum((new_positions) * buy_prices)
  money_used = buy_assets * buy_prices 
  
  output = suggested_weights %>% 
    dplyr::mutate(positions_bought = buy_assets[,1],
                  new_position = new_positions[,1],
                  new_portfolio_weights = new_position * buy_price / (sum(new_position * buy_price)),
                  transactions_costs = pmax(money_used[,1]*-1, 0) * (total_transaction_fee),
                  money_leftover = budget - sum(money_used) - sum(transactions_costs))
  
  return(output)
}
