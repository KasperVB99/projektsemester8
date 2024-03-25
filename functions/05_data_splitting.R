data_splitting = function(engineered_features){
  
  training = engineered_features %>% 
    dplyr::filter(timestamp < "2022-01-01")
  
  testing = engineered_features %>% 
    dplyr::filter(timestamp >= "2022-01-01")
  
  resamples = combinatorial_purged_cv(training, splits = 6, test_size = 2)
  
  
  initial_split_list = list(training = training,
                            testing = testing,
                            resamples = resamples)
  
  return(initial_split_list)
}