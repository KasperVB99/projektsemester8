library(magrittr)
library(PortfolioAnalytics)
R.utils::sourceDirectory("./functions", modifiedOnly = FALSE)


list(
  targets::tar_target(
    raw_data,
    data_loading(
      date_start = date_start,
      date_end = date_end
    )
  ),
  targets::tar_target(
    optimized_portfolio_weights,
    portfolio_optimization(
      cleaned_raw_data = raw_data$cleaned_raw_data
    )
  )
)

