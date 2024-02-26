library(magrittr)

symbols = get_data("nordnet_etf") %>% 
  dplyr::rename(symbol = Ticker) %>% 
  dplyr::full_join(raw_data) %>% 
  dplyr::arrange(timestamp) %>% 
  dplyr::group_by(symbol) %>% 
  dplyr::filter(`Antal ejere` >= 1000,
                min(timestamp) < "2014-01-01") %>% 
  dplyr::distinct(symbol)

hej = raw_data %>% 
  dplyr::filter(symbol == "EUNW.DE")


  ggplot2::ggplot(hej, ggplot2::aes(x = lubridate::as_datetime(timestamp), y = close, group = symbol)) +
  ggplot2::geom_line()

hej = nordnet_etf$symbol

hej = get_data("nordnet_etf")

all_etf = tidyquant::tq_get(symbols, get = "alphavantager",
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
  dplyr::mutate(close = log(close/ dplyr::lag(close))) %>% 
  dplyr::summarise(returns = sum(close, na.rm = TRUE)) %>% 
  dplyr::arrange(desc(returns))


hejsa = get_data("raw_etf_data") %>% 
  dplyr::group_by(timestamp, symbol) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::distinct(symbol)

DataExplorer::plot_correlation(hola)


hej = get_data("raw_etf_data")
>>>>>>> fa8035461e6b3b1d23cb6a03f28ca0cb38fcb4ee
