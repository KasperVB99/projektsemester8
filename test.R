library(magrittr)

nordnet_etf = get_data("nordnet_etf")

symbols = nordnet_etf$Ticker

symbols[47:158]

symbols_manglende = nordnet_etf$Ticker[33:158]

all_etf = tidyquant::tq_get(symbols_manglende, get = "alphavantager",
                                       av_fun = "TIME_SERIES_DAILY",
                                       outputsize = "full",
                                       show_col_types = FALSE,
                                       from = date_start,
                                       to = date_end)

den_her = all_etf %>% 
  dplyr::mutate(timestamp = format(as.POSIXct(timestamp)))

conn = DBI::dbConnect(RSQLite::SQLite(), "data/raw_data.sqlite")

DBI::dbWriteTable(conn, "raw_etf_data", den_her, 
                  extended_types = TRUE, append = TRUE)

DBI::dbDisconnect(conn)

hola = get_data("raw_etf_data") %>% 
  dplyr::arrange(timestamp) %>% 
  dplyr::select(timestamp, close, symbol) %>% 
  dplyr::group_by(symbol) %>% 
  dplyr::distinct(timestamp, .keep_all = TRUE) %>% 
  dplyr::mutate(close = log(close/ dplyr::lag(close))) %>% 
  tidyr::pivot_wider(names_from = symbol, values_from = close) %>% 
  tidyr::drop_na()


hejsa = get_data("raw_etf_data") %>% 
  dplyr::group_by(timestamp, symbol) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::distinct(symbol)

DataExplorer::plot_correlation(hola)


hej = get_data("raw_etf_data")
