# Validate uploaded datasets and standardize column names
# Returns list(type, df, mapping, errors)

normalize_names <- function(x) {
  x2 <- tolower(trimws(x))
  x2 <- gsub("[^a-z0-9]+", "_", x2)
  x2
}

validate_dataset <- function(df) {
  errors <- character(0)
  if (is.null(df) || nrow(df) == 0) {
    return(list(ok = FALSE, type = NA_character_, df = df, mapping = list(), errors = c("Empty dataset.")))
  }

  # Build a normalized-name map
  orig_names <- names(df)
  norm_names <- normalize_names(orig_names)
  name_map <- setNames(orig_names, norm_names)

  has_cols <- function(required_norm) all(required_norm %in% norm_names)

  # Journey / MTA dataset (minimum)
  journey_req <- c("user_id", "timestamp", "channel", "conversion")
  mmm_req     <- c("division", "calendar_week", "sales")

  if (has_cols(journey_req)) {
    type <- "journey"
  } else if (has_cols(mmm_req)) {
    type <- "mmm"
  } else {
    type <- NA_character_
    errors <- c(errors,
      paste0("Could not detect dataset type. Need either journey columns: ",
             paste(journey_req, collapse = ", "),
             " OR MMM columns: ",
             paste(mmm_req, collapse = ", "))
    )
  }

  # Standardize names for downstream code
  std <- df
  names(std) <- norm_names

  # Light type checks
  if (!is.na(type) && type == "journey") {
    # Timestamp parse check
    if (!is.character(std$timestamp) && !inherits(std$timestamp, "POSIXct")) {
      errors <- c(errors, "timestamp must be a datetime-like column (string OK).")
    }
    if (!is.character(std$channel)) {
      errors <- c(errors, "channel must be text.")
    }
    if (!is.character(std$conversion) && !is.logical(std$conversion)) {
      errors <- c(errors, "conversion must be Yes/No (string) or TRUE/FALSE (logical).")
    }
  }

  if (!is.na(type) && type == "mmm") {
    if (!is.character(std$calendar_week) && !inherits(std$calendar_week, "Date")) {
      errors <- c(errors, "calendar_week must be a date-like column (string OK).")
    }
    if (!("sales" %in% names(std))) errors <- c(errors, "sales column is required.")
  }

  list(
    ok = length(errors) == 0 && !is.na(type),
    type = type,
    df = std,
    mapping = as.list(name_map),
    errors = errors
  )
}
