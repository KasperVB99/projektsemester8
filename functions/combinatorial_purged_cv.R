combinatorial_purged_cv = function(dataset, splits, test_size){

  testing_combinations = combn(splits, test_size)
  n = ncol(testing_combinations)
  n_paths = (test_size / splits) * choose(splits, test_size)
  
  groups = dataset %>% 
    dplyr::mutate(group = dplyr::ntile(dplyr::row_number(), splits))
  
  testing_list = list()
  training_list = list()
  
  for (i in 1:n){
    testing_list[[i]] = groups %>% 
      dplyr::filter(group %in% testing_combinations[,i]) %>% 
      dplyr::mutate(group = paste0("G",group, "_S",i))
    
    training_list[[i]] = groups %>% 
      dplyr::filter(!group %in% testing_combinations[,i]) %>% 
      dplyr::mutate(group = paste0("G",group, "_S",i))
  }
  
  
  split_list = list()
  
  for (i in seq_along(training_list)) {
    analysis_indices <- which(dataset$timestamp %in% training_list[[i]]$timestamp)
    assessment_indices <- which(dataset$timestamp %in% training_list[[i]]$timestamp)
    split_list[[i]] = list(analysis = analysis_indices,
                           assessment = assessment_indices)
  }
  
  splits = lapply(split_list, rsample::make_splits, data = dataset)
  
  resamples = rsample::manual_rset(splits, paste("Split_", seq(1, 15), sep=""))
  
  
  
  paths = list()
  
  for (i in 1:n_paths){
    if(i == 1){
      paths[[i]] = testing_list %>% 
        purrr::reduce(dplyr::bind_rows) %>% 
        dplyr::distinct(timestamp, .keep_all = TRUE)
    } else {
      paths[[i]] = testing_list %>% 
        purrr::reduce(dplyr::bind_rows) %>% 
        dplyr::anti_join(purrr::reduce(paths, dplyr::bind_rows)) %>% 
        dplyr::distinct(timestamp, .keep_all = TRUE)
    }
    
  }
  
  paths = paths %>% 
    purrr::map(~dplyr::distinct(.x, group))
  
  return_list = list(resamples = resamples,
                     training = training_list,
                     testing = testing_list,
                     paths = paths)
  
  return(return_list)
  
}
