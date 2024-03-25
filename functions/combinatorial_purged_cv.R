combinatorial_purged_cv = function(dataset, splits, test_size){
  
  testing_combinations = combn(splits, test_size)
  n = ncol(testing_combinations)
  n_paths = (test_size / splits) * choose(splits, test_size)
  
  groups = dataset %>% 
    dplyr::mutate(row_number = dplyr::row_number(),
                  group = dplyr::ntile(row_number, splits))
  
  testing_list = list()
  training_list = list()
  
  for (i in 1:n){
    testing_list[[i]] = groups %>% 
      dplyr::filter(group %in% testing_combinations[,i]) %>% 
      dplyr::mutate(group = paste0("Group",group, "_Split",sprintf("%02d", i)))
    
    training_list[[i]] = groups %>% 
      dplyr::filter(!group %in% testing_combinations[,i]) %>% 
      dplyr::mutate(group = paste0("Group",group, "_Split",sprintf("%02d", i)))
  }
  
  split_list = list()
  
  for (i in seq_along(training_list)) {
    analysis_indices <- which(dataset$timestamp %in% training_list[[i]]$timestamp)
    assessment_indices <- which(dataset$timestamp %in% testing_list[[i]]$timestamp)
    split_list[[i]] = list(analysis = analysis_indices,
                           assessment = assessment_indices)
  }
  
  splits = purrr::map(split_list, ~rsample::make_splits(., data = dataset))
  
  resamples = rsample::manual_rset(splits, as.factor(paste("Split", sprintf("%02d", seq(1, 15)), sep="")))
  
  paths = list()
  
  for (i in 1:n_paths){
    if(i == 1){
      paths[[i]] = testing_list %>% 
        purrr::reduce(dplyr::bind_rows) %>% 
        dplyr::distinct(row_number, timestamp, .keep_all = TRUE)
    } else {
      paths[[i]] = testing_list %>% 
        purrr::reduce(dplyr::bind_rows) %>% 
        dplyr::anti_join(purrr::reduce(paths, dplyr::bind_rows)) %>% 
        dplyr::distinct(row_number, timestamp, .keep_all = TRUE)
    }
    
  }
  
  row_number_group = groups %>% 
    dplyr::mutate(group = group,
                     row_number = dplyr::row_number()) %>% 
    dplyr::select(group, row_number)
  
  
  paths = paths %>% 
    purrr::imap(~dplyr::mutate(.x, path = .y)) %>% 
    purrr::reduce(dplyr::bind_rows)
  
  
  return_list = list(resamples = resamples,
                     training = training_list,
                     testing = testing_list,
                     paths = paths,
                     row_number_group = row_number_group)
  
  return(return_list)
  
}
