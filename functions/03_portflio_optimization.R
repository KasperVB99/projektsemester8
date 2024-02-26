portfolio_optimization = function(cleaned_raw_data){
  
  # Laver en matrix med log returns for hver dag i datasættet:
  return_matrix = cleaned_raw_data %>% 
    dplyr::arrange(timestamp) %>% 
    dplyr::select(timestamp, close, symbol) %>% 
    dplyr::group_by(symbol) %>% 
    dplyr::distinct(timestamp, .keep_all = TRUE) %>% 
    dplyr::mutate(close = log(close/ dplyr::lag(close))) %>% 
    tidyr::pivot_wider(names_from = symbol, values_from = close)
  
  # Sørger for, at returns er grupperet sammen på den korrekte måde, dvs. at der bliver investeret d. 8. eller første hverdag herefter.
  returns_grouped = return_matrix %>% 
    dplyr::filter(timestamp >= "2014-01-08") %>% 
    tidyr::drop_na() %>%   # der skal laves en mere elegant løsning end dette på et tidspunkt
    dplyr::group_by(year = lubridate::year(timestamp), month = lubridate::month(timestamp)) %>%
    dplyr::mutate(eighth_or_next = as.integer((lubridate::day(timestamp) >= 8 & dplyr::row_number() >= 1)),
                  invest_day = dplyr::if_else(eighth_or_next == 1 & cumsum(eighth_or_next) == 1, 1, 0)) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(invest_day = cumsum(invest_day)) %>% 
    dplyr::select(-year, -month, -eighth_or_next)
  
  # Trækker månedstal ud for hver optimeringsgruppe - bruges senere til at lave endeligt datasæt.
  months = returns_grouped %>% 
    dplyr::select(timestamp, invest_day) %>% 
    dplyr::group_by(invest_day) %>% 
    dplyr::slice(1) %>% 
    dplyr::summarise(month = lubridate::floor_date(timestamp, unit = "months")) %>% 
    dplyr::select(month) %>% 
    dplyr::ungroup()
  
  result_list = list()
  
  # Rullende portefølheoptimering. Udføres ikke på de to sidste rækker af returns metricen, da disse ikke har et helt kvartals data.
  result_list <- foreach(i = 1:(dplyr::n_distinct(returns_grouped$invest_day) - 2), .packages = c("magrittr", "PortfolioAnalytics")) %dopar% {
    returns <- returns_grouped %>%
      dplyr::filter(invest_day >= i, 
                    invest_day < i + 3) %>%
      dplyr::select(-invest_day) %>%
      xts::as.xts()
    
    port_spec <- PortfolioAnalytics::portfolio.spec(assets = colnames(returns)) %>% 
      PortfolioAnalytics::add.constraint(type = "full_investment") %>% 
      PortfolioAnalytics::add.constraint(type = "long_only") %>% 
      PortfolioAnalytics::add.objective(type = "return", name = "mean") %>% 
      PortfolioAnalytics::add.objective(type = "risk", name = "StdDev")
    
    optimize.portfolio(R = returns, portfolio = port_spec, optimize_method = "ROI", maxSR = TRUE)$weights
  }

  # Laver det endelige datasæt med de optimale vægte for hvert aktiv på en given investeringsmåned.
  results = months %>% 
    dplyr::slice(1:length(result_list)) %>% 
    dplyr::bind_cols(purrr::reduce(result_list, dplyr::bind_rows)) %>% 
    dplyr::mutate_if(is.numeric, ~ round(., 2))
    
  
  return(results)
}