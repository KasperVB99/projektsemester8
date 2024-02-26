data_loading = function(date_start, date_end){

  # Trækker det rå data ud af databasen:
  raw_data = get_data("raw_etf_data") %>% 
    dplyr::mutate(timestamp = lubridate::date(lubridate::as_date(timestamp)))
  
  # Finder alle symboler, som har mindst 1000 ejere og data siden mindst 1. januar 2014:
  symbols = get_data("nordnet_etf") %>% 
    dplyr::rename(symbol = Ticker) %>% 
    dplyr::full_join(raw_data) %>% 
    dplyr::arrange(timestamp) %>% 
    dplyr::group_by(symbol) %>% 
    dplyr::filter(`Antal ejere` >= 1000,
                  min(timestamp) < "2014-01-01") %>% 
    dplyr::distinct(symbol)
  
  # Medtager kun de ETF'er, som har mindst 1000 ejere og data siden mindst 1. januar 2014. Derudover må ingen ETF'er have korrelation på over
  # 0,8 i træningsperioden (denne øvelse er lavet i et seperat script)
  cleaned_raw_data = raw_data %>% 
    dplyr::filter(symbol %in% symbols$symbol &
                  !symbol %in% c("IUSA.DE", "DBXW.DE", "EUNL.DE", "IUSQ.DE", "EQQQ.DE", "LYMS.DE", "IQQQ.DE",
                              "XDUK.DE", "XDJP.DE", "DBXD.DE", "EUN0.DE", "XDN0.DE", "LYM9.DE"))
  
  data_list = list(raw_data = raw_data,
                   cleaned_raw_data = cleaned_raw_data)
  
  return(data_list)
}
