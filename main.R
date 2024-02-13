library(magrittr)
library(PortfolioAnalytics)
#-------------------------------------------------------------------------------

targets::tar_make()

#-------------------------------------------------------------------------------

targets::tar_load_everything()
targets::tar_load_globals()

hej = get_data("nordnet_etf")

raw_data %>% dplyr::filter(symbol == "EUNW.DE")



rf = runif(126, min = 0, max = 0.1)

data = raw_data %>% 
  dplyr::mutate(timestamp = lubridate::as_date(timestamp)) %>% 
  dplyr::filter(timestamp >= "2023-06-01",
                timestamp <= "2023-12-31") %>% 
  dplyr::arrange(timestamp) %>% 
  dplyr::select(timestamp, close, symbol) %>% 
  dplyr::group_by(symbol) %>% 
  dplyr::distinct(timestamp, .keep_all = TRUE) %>% 
  dplyr::mutate(close = log(close/ dplyr::lag(close))) %>% 
  tidyr::pivot_wider(names_from = symbol, values_from = close) %>% 
  tidyr::drop_na()

port_spec = PortfolioAnalytics::portfolio.spec(assets = colnames(data[,-1])) %>% 
  PortfolioAnalytics::add.constraint(type = "full_investment") %>% 
  PortfolioAnalytics::add.constraint(type = "long_only") %>% 
  PortfolioAnalytics::add.objective(type = "risk", name = "mean") %>% 
  PortfolioAnalytics::add.objective(type = "risk", name = "StdDev")


resultat = optimize.portfolio(R = hej, portfolio = port_spec, optimize_method = "ROI")


hej = xts::as.xts(data)
set.portfolio.moments_v1

cor(data[,-1])
