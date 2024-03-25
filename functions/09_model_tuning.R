model_tuning = function(split_data, raw_data, defined_workflow, model = "lin_reg"){
  resamples = split_data$resamples$resamples
  control_grid = tune::control_grid(save_pred = TRUE)
  
  if (model == "lin_reg"){
    grid = dials::grid_regular(dials::penalty(),
                                       levels = 1)
  }
  
  grid_results = tune::tune_grid(
    defined_workflow,
    resamples = resamples,
    grid = grid,
    control = control_grid
  )
    

  tune_options = grid_results$.metrics[[1]] %>% 
    dplyr::select(penalty, .config) %>% 
    dplyr::distinct()
  
  rows = split_data$resamples$row_number_group
  
  testing_grid = split_data$resamples$row_number_group
  
  hej = grid_results$.predictions %>% 
    furrr::future_map(., ~dplyr::rename(., row_number = .row)) %>% 
    furrr::future_map(., ~dplyr::select(., -weight)) %>%
    furrr::future_map2(., .y = split_data$resamples$testing, .f = dplyr::inner_join) %>% 
    furrr::future_map(., ~dplyr::select(., timestamp, .pred,, .config, symbol, group, row_number)) %>% 
    purrr::reduce(dplyr::bind_rows) %>% 
    dplyr::arrange(timestamp, group, row_number) %>% 
    dplyr::left_join(split_data$resamples$paths) %>% 
    dplyr::group_by(.config, path) %>%
    dplyr::group_split()
  
  best_model = tibble::tibble(
    final_portfolio = furrr::future_map(hej, ~run_strategy(., split_data = split_data, 
                                                           period = "is",
                                                           raw_data = raw_data))) %>% 
    dplyr::mutate(.config = furrr::future_map(final_portfolio, ~dplyr::distinct(., .config))) %>% 
    tidyr::unnest(.config) %>% 
    dplyr::mutate(information_ratio = furrr::future_map(final_portfolio, ~calculate_PnL(., cv = TRUE, model = "lin reg"))) %>% 
    tidyr::unnest(information_ratio) %>% 
    dplyr::inner_join(tune_options) %>% 
    dplyr::arrange(desc(info_ratio)) %>% 
    dplyr::slice(1) %>% 
    dplyr::select(penalty, .config)
  
  finalized_workflow = tune::finalize_workflow(
    defined_workflow,
    best_model
  )

  tuned_model = list(finalized_workflow = finalized_workflow,
                      grid_results = grid_results)
  
  return(tuned_model)
}
