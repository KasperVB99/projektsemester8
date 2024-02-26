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
  ),
  targets::tar_target(
    engineered_features,
    feature_engineering(
      cleaned_raw_data = raw_data$cleaned_raw_data,
      optimized_portfolio_weights = optimized_portfolio_weights
    )
  ),
  targets::tar_target(
    split_data,
    data_splitting(
      engineered_features = engineered_features
    )
  ),
  targets::tar_target(
    preprocessed_data,
    data_preprocessing(
      optimized_portfolio_weights = optimized_portfolio_weights,
      split_data = split_data
    )
  ),
   targets::tar_target(
     specified_models,
     model_specification(
     )
   ),
  targets::tar_target(
    defined_workflows,
    define_workflows(
      preprocessed_data = preprocessed_data,
      specified_models = specified_models
    )
  ),
  targets::tar_target(
    linear_reg_fitted_and_predicted,
    fit_and_predict(
      defined_workflows = defined_workflows,
      split_data = split_data,
      model = "linear_reg"
    )
  )
)

