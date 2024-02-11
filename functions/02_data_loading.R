data_loading = function(date_start, date_end){
  world_index = tidyquant::tq_get("EUNL.DE", get = "alphavantager",
                                  av_fun = "TIME_SERIES_DAILY",
                                  outputsize = "full",
                                  show_col_types = FALSE,
                                  from = date_start,
                                  to = date_end) %>% 
    dplyr::select(date = timestamp, world_index = close)
  
  sp500_index = tidyquant::tq_get("SXR8.DE", get = "alphavantager",
                                  av_fun = "TIME_SERIES_DAILY",
                                  outputsize = "full",
                                  show_col_types = FALSE,
                                  from = date_start,
                                  to = date_end) %>% 
    dplyr::select(date = timestamp, sp500_index = close)
  
  em_index = tidyquant::tq_get("IS3N.DE", get = "alphavantager",
                               av_fun = "TIME_SERIES_DAILY",
                               outputsize = "full",
                               show_col_types = FALSE,
                               from = date_start,
                               to = date_end) %>% 
    dplyr::select(date = timestamp, em_index = close)
  
  clean_energy_index = tidyquant::tq_get("IQQH.DE", get = "alphavantager",
                                         av_fun = "TIME_SERIES_DAILY",
                                         outputsize = "full",
                                         show_col_types = FALSE,
                                         from = date_start,
                                         to = date_end) %>% 
    dplyr::select(date = timestamp, clean_energy_index = close)
  
  
  data_list = list(world_index, sp500_index, em_index,
                   clean_energy_index)
  
  data = data_list %>% 
    purrr::reduce(dplyr::full_join) %>% 
    dplyr::arrange(date) %>% 
    dplyr::filter(date >= "2016-01-01",
                  date <= "2023-12-31")
  
  return(data)
}
