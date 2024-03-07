date_start = "2013-01-01"
date_end = "2023-12-31"

tidyquant::av_api_key("ZSPW2044ZOTNC74Y")

no_cores <- parallel::detectCores() # Save one core for system processes
doParallel::registerDoParallel(no_cores)
