library(magrittr)
#-------------------------------------------------------------------------------

targets::tar_make_future(workers = 5)

#-------------------------------------------------------------------------------

targets::tar_load_everything()



data_loading(date_start, date_end)
