# Simple MMM model (starter): linear regression per division
# Later we can swap this for Bayesian + adstock + saturation

suppressPackageStartupMessages({
  library(dplyr)
})

fit_mmm_lm <- function(df_division) {
  stopifnot("sales" %in% names(df_division))

  # Candidate predictors: all numeric columns except sales
  predictors <- names(df_division)[sapply(df_division, is.numeric)]
  predictors <- setdiff(predictors, c("sales"))

  # Guard
  if (length(predictors) == 0) stop("No numeric predictors found.")

  # Avoid perfect leakage if overall_views is derived
  f <- as.formula(paste("sales ~", paste(predictors, collapse = " + ")))
  model <- lm(f, data = df_division)

  list(
    model = model,
    predictors = predictors,
    baseline_pred = predict(model, newdata = df_division),
    baseline_actual = df_division$sales
  )
}
