data_splitting = function(engineered_features){
  initial_split = rsample::initial_time_split(engineered_features, prop = 0.8)
  
  training = rsample::training(initial_split)
  testing = rsample::testing(initial_split)
  resamples = combinatorial_purged_cv(training, splits = 6, test_size = 2)
  
  
  initial_split_list = list(training = training,
                            testing = testing,
                            resamples = resamples)
  
  return(initial_split_list)
}