# Markov Removal-Effect Attribution (robust credits + signed effects)
#
# Outputs BOTH:
# - removal_effect (can be negative)
# - credit_pct (normalized credit across positive effects only)
#
# If all effects are <= 0 (or effectively 0), credits become 0 and so we
# use removal_pct_points for a signed bar chart.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

.fit_transition_matrix <- function(path_states_long) {
  stopifnot(all(c("user_id","step","state") %in% names(path_states_long)))

  counts <- path_states_long %>%
    arrange(user_id, step) %>%
    group_by(user_id) %>%
    mutate(next_state = lead(state)) %>%
    ungroup() %>%
    filter(!is.na(next_state)) %>%
    count(state, next_state, name = "n")

  # all states in either column
  states <- sort(unique(c(counts$state, counts$next_state)))

  P <- matrix(0, nrow = length(states), ncol = length(states),
              dimnames = list(states, states))

  for (i in seq_len(nrow(counts))) {
    P[counts$state[i], counts$next_state[i]] <- counts$n[i]
  }

  # row-normalize
  rs <- rowSums(P)
  P <- sweep(P, 1, ifelse(rs == 0, 1, rs), "/")

  list(P = P, counts = counts, states = states)
}

.absorb_prob <- function(P, start = "Start", absorb = "Conversion") {
  # Probability of eventually hitting absorb starting from start.
  # Solve absorbing Markov chain via fundamental matrix on transient states.
  st <- rownames(P)
  if (!(start %in% st) || !(absorb %in% st)) return(NA_real_)

  absorbing <- intersect(c("Conversion","Null"), st)
  transient <- setdiff(st, absorbing)

  if (!(start %in% transient)) return(NA_real_)
  if (!(absorb %in% absorbing)) return(NA_real_)

  Q <- P[transient, transient, drop = FALSE]
  R <- P[transient, absorbing, drop = FALSE]

  # N = (I - Q)^(-1)
  I <- diag(nrow(Q))
  inv <- tryCatch(solve(I - Q), error = function(e) NULL)
  if (is.null(inv)) return(NA_real_)

  B <- inv %*% R
  # B[row=start, col=absorb]
  as.numeric(B[which(transient == start), which(absorbing == absorb)])
}

.remove_channel_renorm <- function(P, channel) {
  st <- rownames(P)
  if (!(channel %in% st)) return(P)

  P2 <- P

  # zero transitions INTO the channel, then renormalize each row
  P2[, channel] <- 0
  rs <- rowSums(P2)
  P2 <- sweep(P2, 1, ifelse(rs == 0, 1, rs), "/")

  # drop the channel state entirely
  keep <- setdiff(st, channel)
  P2[keep, keep, drop = FALSE]
}

#' Markov attribution
#' @param path_states_long data.frame(user_id, step, state)
#' @return list(base_cvr, attribution, transition_matrix, counts)
markov_attribution <- function(path_states_long) {
  fit <- .fit_transition_matrix(path_states_long)
  P <- fit$P
  counts <- fit$counts

  base_cvr <- .absorb_prob(P, start = "Start", absorb = "Conversion")

  channels <- setdiff(rownames(P), c("Start", "Conversion", "Null"))

  removal <- lapply(channels, function(ch) {
    P2 <- .remove_channel_renorm(P, ch)
    cvr2 <- .absorb_prob(P2, start = "Start", absorb = "Conversion")
    data.frame(
      channel = ch,
      base_cvr = base_cvr,
      cvr_removed = cvr2,
      removal_effect = base_cvr - cvr2
    )
  }) %>% bind_rows()

  # Robust credit normalization on positive effects only
  eps <- 1e-12
  pos_eff <- pmax(removal$removal_effect, 0)
  total_eff <- sum(pos_eff, na.rm = TRUE)

  credit <- if (is.finite(total_eff) && total_eff > eps) pos_eff / total_eff else rep(0, nrow(removal))

  removal <- removal %>%
    mutate(
      credit = credit,
      removal_pct_points = 100 * removal_effect,
      credit_pct = 100 * credit
    ) %>%
    arrange(desc(credit_pct))

  list(
    base_cvr = base_cvr,
    attribution = removal,
    transition_matrix = P,
    counts = counts
  )
}
