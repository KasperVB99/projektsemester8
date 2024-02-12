library(magrittr)
library(PortfolioAnalytics)
#-------------------------------------------------------------------------------

targets::tar_make()

#-------------------------------------------------------------------------------

targets::tar_load_everything()
targets::tar_load_globals()







rf = runif(126, min = 0, max = 0.1)

data = raw_data %>% 
  dplyr::filter(date >= "2018-01-01",
                date <= "2017-12-31") %>% 
  dplyr::bind_cols(rf) %>% 
  dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ log(. / dplyr::lag(.)) - (`...7`/365.25))) %>% 
  dplyr::select(-`...7`) %>% 
  tidyr::drop_na()

port_spec = PortfolioAnalytics::portfolio.spec(assets = colnames(data[,-1])) %>% 
  PortfolioAnalytics::add.constraint(type = "full_investment") %>% 
  PortfolioAnalytics::add.constraint(type = "long_only") %>% 
  PortfolioAnalytics::add.objective(type = "risk", name = "mean") %>% 
  PortfolioAnalytics::add.objective(type = "risk", name = "StdDev")


resultat = optimize.portfolio(R = data, portfolio = port_spec, optimize_method = "ROI", 
                                          maxSR = TRUE, trace = TRUE)


hej = xts::as.xts(data)
set.portfolio.moments_v1

cor(data[,-1])
