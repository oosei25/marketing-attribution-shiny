# Scenario simulation utilities for MMM-lite
# - Applies user-provided multipliers to channel variables
# - Predicts baseline vs scenario sales
# - Clamps predictions to >= 0 for interpretability

suppressPackageStartupMessages({
  library(dplyr)
})

apply_multipliers <- function(df_division, multipliers, protect_cols = c("sales", "week")) {
  df2 <- df_division
  if (is.null(multipliers) || length(multipliers) == 0) return(df2)

  for (nm in names(multipliers)) {
    if (nm %in% names(df2) && !(nm %in% protect_cols)) {
      df2[[nm]] <- df2[[nm]] * as.numeric(multipliers[[nm]])
    }
  }
  df2
}

#' Simulate MMM baseline vs scenario
#' @param df_division data.frame for one division (must contain week + sales + channel columns)
#' @param fit fitted model OR list(model = <lm>)
#' @param multipliers named numeric vector (one per channel column)
#' @return list(out = data.frame(week, baseline, scenario, lift), summary = data.frame totals)
simulate_mmm <- function(df_division, fit, multipliers) {
  stopifnot(is.data.frame(df_division))
  if (!("week" %in% names(df_division))) {
    # allow common alternatives
    if ("calendar_week" %in% names(df_division)) df_division$week <- df_division$calendar_week
    if ("Calendar_Week" %in% names(df_division)) df_division$week <- df_division$Calendar_Week
  }
  stopifnot("week" %in% names(df_division))

  model <- fit
  if (is.list(fit) && "model" %in% names(fit)) model <- fit$model

  # Baseline prediction
  pred_base <- as.numeric(predict(model, newdata = df_division))
  pred_base <- pmax(0, pred_base)

  # Scenario prediction (apply multipliers to channels)
  df_new <- apply_multipliers(df_division, multipliers)
  pred_new <- as.numeric(predict(model, newdata = df_new))
  pred_new <- pmax(0, pred_new)

  lift <- pred_new - pred_base

  out <- data.frame(
    week = df_division$week,
    baseline = pred_base,
    scenario = pred_new,
    lift = lift
  )

  summary <- data.frame(
    baseline_total = sum(pred_base, na.rm = TRUE),
    scenario_total = sum(pred_new, na.rm = TRUE),
    lift_total = sum(lift, na.rm = TRUE)
  )

  list(out = out, summary = summary)
}
