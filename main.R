#-------------------------------------------------------------------------------

targets::tar_make()

#-------------------------------------------------------------------------------

targets::tar_load_everything()
targets::tar_load_globals()  

#-------------------------------------------------------------------------------



hej = raw_data %>% 
  dplyr::arrange(timestamp) %>% 
  dplyr::select(timestamp, close, symbol) %>% 
  dplyr::group_by(symbol) %>% 
  dplyr::distinct(timestamp, .keep_all = TRUE) %>% 
  dplyr::mutate(close = log(close/ dplyr::lag(close))) %>% 
  tidyr::pivot_wider(names_from = symbol, values_from = close) %>% 
  dplyr::filter(timestamp >= "2013-01-01")

hej_split = rsample::initial_time_split(hej, prop = 0.8)

rsample::testing(hej_split)

rf = runif(126, min = 0, max = 0.1)

data = raw_data %>% 
  dplyr::mutate(timestamp = lubridate::as_date(timestamp)) %>% 
  dplyr::filter(symbol %in% symbols$symbol) %>% 
  dplyr::arrange(timestamp) %>% 
  dplyr::select(timestamp, close, symbol) %>% 
  dplyr::group_by(symbol) %>% 
  dplyr::distinct(timestamp, .keep_all = TRUE) %>% 
  dplyr::mutate(close = log(close/ dplyr::lag(close))) %>% 
  tidyr::pivot_wider(names_from = symbol, values_from = close) %>% 
  tidyr::drop_na() %>% 
  dplyr::filter(timestamp < "2022-01-01") %>% 
  dplyr::select(-IUSA.DE, -DBXW.DE, -EUNL.DE, -IUSQ.DE, -EQQQ.DE, -LYMS.DE, -IQQQ.DE,
                -XDUK.DE, -XDJP.DE, -DBXD.DE, -EUN0.DE, -XDN0.DE, -LYM9.DE)
  
  cor_matrix = cor(data[,-1])

colnames(cor_matrix)

high_cor_pairs <- which(abs(cor_matrix) > 0.80 & cor_matrix < 1, arr.ind = TRUE)

data.frame(symbol1 = rownames(high_cor_pairs),
           symbol2 = colnames(cor_matrix)[high_cor_pairs[, "col"]],
           correlation = cor_matrix[high_cor_pairs]) %>% 
  dplyr::arrange(desc(correlation)) %>% 
  dplyr::distinct(correlation, .keep_all = TRUE)

port_spec = PortfolioAnalytics::portfolio.spec(assets = colnames(data[,-1])) %>% 
  PortfolioAnalytics::add.constraint(type = "full_investment") %>% 
  PortfolioAnalytics::add.constraint(type = "long_only") %>% 
  PortfolioAnalytics::add.objective(type = "return", name = "mean") %>% 
  PortfolioAnalytics::add.objective(type = "risk", name = "StdDev")


resultat = optimize.portfolio(R = data, portfolio = port_spec, optimize_method = "ROI")
