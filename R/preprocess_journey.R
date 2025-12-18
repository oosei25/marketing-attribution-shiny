# Preprocess journey data for Markov attribution
# Input: df with standardized column names: user_id, timestamp, channel, (optional) campaign, conversion
# Output: list(path_states, paths_string, touches_clean)

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(tidyr)
})

parse_conversion <- function(x) {
  if (is.logical(x)) return(x)
  x2 <- tolower(trimws(as.character(x)))
  x2 %in% c("1", "true", "yes", "y", "converted", "conversion")
}

# Build one path per user:
# Start -> channels (time-ordered) -> terminal (Conversion/Null)
preprocess_journey <- function(df,
                              lookback_days = NULL,
                              remove_consecutive_duplicates = TRUE,
                              remove_consecutive_dupes = remove_consecutive_duplicates,
                              ...) {
  # Backwards-compatible alias: some callers use remove_consecutive_dupes
  remove_consecutive_duplicates <- isTRUE(remove_consecutive_dupes)


  stopifnot(is.data.frame(df))
  needed <- c("user_id", "timestamp", "channel", "conversion")
  missing <- setdiff(needed, names(df))
  if (length(missing) > 0) stop("preprocess_journey: missing columns: ", paste(missing, collapse = ", "))

  touches <- df %>%
    transmute(
      user_id = as.character(user_id),
      timestamp = as.character(timestamp),
      channel = str_squish(as.character(channel)),
      campaign = if ("campaign" %in% names(df)) str_squish(as.character(df$campaign)) else NA_character_,
      conversion_raw = df$conversion
    ) %>%
    mutate(
      touch_time = suppressWarnings(ymd_hms(timestamp, tz = "UTC"))
    )

  # Fallback if datetime parse fails (date-only)
  if (all(is.na(touches$touch_time))) {
    touches <- touches %>%
      mutate(touch_time = suppressWarnings(ymd(timestamp, tz = "UTC")))
  }

  # Final fallback: try as.POSIXct
  if (all(is.na(touches$touch_time))) {
    touches <- touches %>%
      mutate(touch_time = suppressWarnings(as.POSIXct(timestamp, tz = "UTC")))
  }

  # If we still can't parse times, create a sequence per user based on row order
  if (all(is.na(touches$touch_time))) {
    touches <- touches %>%
      group_by(user_id) %>%
      mutate(touch_time = as.POSIXct("2025-01-01", tz = "UTC") + row_number()) %>%
      ungroup()
  }

  touches <- touches %>%
    mutate(converted = parse_conversion(conversion_raw)) %>%
    select(-conversion_raw)

  # Sort
  touches <- touches %>% arrange(user_id, touch_time)

  # Determine per-user terminal state
  user_term <- touches %>%
    group_by(user_id) %>%
    summarise(
      terminal_state = ifelse(any(converted), "Conversion", "Null"),
      end_time = ifelse(any(converted), max(touch_time), max(touch_time)),
      .groups = "drop"
    )

  touches <- touches %>%
    left_join(user_term, by = "user_id")

  # Optional lookback window: keep touches within N days of end_time
  if (!is.null(lookback_days) && is.finite(as.numeric(lookback_days)) && as.numeric(lookback_days) > 0) {
    lb <- as.numeric(lookback_days)
    touches <- touches %>%
      filter(touch_time >= (end_time - days(lb)))
  }

  # Remove consecutive duplicate channels per user (helps Markov stability)
  if (isTRUE(remove_consecutive_duplicates)) {
    touches <- touches %>%
      group_by(user_id) %>%
      mutate(prev_channel = lag(channel)) %>%
      filter(is.na(prev_channel) | channel != prev_channel) %>%
      select(-prev_channel) %>%
      ungroup()
  }

  # Build path_states long table
  path_states <- touches %>%
    group_by(user_id) %>%
    summarise(
      states = list(channel),
      terminal_state = dplyr::first(terminal_state),
      .groups = "drop"
    ) %>%
    mutate(
      states = Map(function(st, term) c("Start", st, term), states, terminal_state)
    ) %>%
    select(user_id, terminal_state, states) %>%
    tidyr::unnest_longer(states, values_to = "state") %>%
    group_by(user_id) %>%
    mutate(step = row_number()) %>%
    ungroup()

  # Build path strings for display
  paths_string <- path_states %>%
    group_by(user_id, terminal_state) %>%
    summarise(path = paste(state, collapse = " > "), .groups = "drop")

  list(
    path_states = path_states,
    paths_string = paths_string,
    touches_clean = touches
  )
}
