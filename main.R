#-------------------------------------------------------------------------------

targets::tar_make()

#-------------------------------------------------------------------------------

targets::tar_prune()

#-------------------------------------------------------------------------------

targets::tar_load_everything()
targets::tar_load_globals()

#-------------------------------------------------------------------------------

hej = broom::tidy(linear_reg_fitted_and_predicted$fit$fit$fit) %>% 
  dplyr::select(response, term, p.value) %>% 
  dplyr::mutate(p.value = round(p.value, 3))
