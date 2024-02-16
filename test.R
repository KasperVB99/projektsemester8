nordnet_etf <- readr::read_csv("data/nordnet_etf.csv")

library(magrittr)

hej = nordnet_etf %>% 
  dplyr::filter(Rating == 5) %>% 
  dplyr::arrange(desc(`Antal ejere`)) %>% 
  dplyr::slice_head(n = 25)
  dplyr::select(Ticker)

  
DBI::dbConnect(
  RSQLite::SQLite(),
  "data/raw_data.sqlite",
  extended_types = TRUE
)