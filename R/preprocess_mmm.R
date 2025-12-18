# Preprocess MMM dataset (weekly media exposures / sales)
# Input: df with normalized names including division, calendar_week, sales
# Output: cleaned df with calendar_week as Date and numeric channels

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
})

preprocess_mmm <- function(df) {
  stopifnot(all(c("division","calendar_week","sales") %in% names(df)))

  out <- df %>%
    mutate(
      division = as.character(division),
      week = suppressWarnings(mdy(calendar_week))
    )

  if (all(is.na(out$week))) {
    out <- out %>% mutate(week = suppressWarnings(ymd(calendar_week)))
  }

  # Coerce numerics for all non-id columns (except week/division)
  num_cols <- setdiff(names(out), c("division","calendar_week","week"))
  for (c in num_cols) {
    out[[c]] <- suppressWarnings(as.numeric(out[[c]]))
  }

  out %>%
    filter(!is.na(week)) %>%
    arrange(division, week)
}
