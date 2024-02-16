library(magrittr)
library(PortfolioAnalytics)
R.utils::sourceDirectory("./functions", modifiedOnly = FALSE)


list(
  targets::tar_target(
    raw_data,
    data_loading(
      date_start = date_start,
      date_end = date_end
    ),
    format = "feather"
  )
)

