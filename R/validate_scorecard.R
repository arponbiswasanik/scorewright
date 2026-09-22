#' Discrimination and gains validation for a scorecard
#'
#' Computes AUC, Gini, the Kolmogorov-Smirnov statistic with its
#' location, and a decile gains/lift table for a risk score against
#' a binary outcome.
#'
#' @details
#' Orientation: HIGHER score = higher predicted risk. A points
#' score (higher = safer) should be passed negated. AUC uses the
#' Mann-Whitney rank formulation with average ranks for ties; Gini
#' = 2*AUC - 1. KS is computed at the observation level: the
#' maximum separation of the cumulative bad and good distributions
#' when observations are ordered by descending score; ks_at reports
#' the population fraction at which it occurs. The gains table bins
#' the score into equal-count bins, ordered from riskiest (bin 1)
#' to safest, with per-bin bad rates, cumulative bad capture
#' (share of all bads), per-row KS, and lift (bin bad rate over
#' the overall bad rate).
#'
#' @param score Numeric risk score, no NA.
#' @param y Binary outcome coded 0 (good) / 1 (bad), no NA.
#' @param n_bins Number of gains-table bins (integer >= 2).
#'
#' @return A list: auc, gini, ks, ks_at, n, n_bad, n_good, and
#'   gains (data.frame: bin, n, n_bad, n_good, bad_rate, cum_n,
#'   cum_bad, cum_good, cum_bad_share, cum_good_share, ks, lift).
#'
#' @references Thomas, L. C., Edelman, D. B., and Crook, J. N.
#'   (2017). Credit Scoring and Its Applications, 2nd ed. SIAM.
#'   Siddiqi, N. (2006). Credit Risk Scorecards. Wiley, ch. 6.
validate_scorecard <- function(score, y, n_bins = 10) {
  
  if (!is.numeric(score) || anyNA(score)) {
    stop("`score` must be numeric without NA.", call. = FALSE)
  }
  if (!is.numeric(y)) {
    stop("`y` must be a numeric vector coded 0 (good) and 1 (bad).",
         call. = FALSE)
  }
  if (length(score) != length(y)) {
    stop("`score` and `y` must have the same length.", call. = FALSE)
  }
  if (anyNA(y)) stop("`y` must not contain NA.", call. = FALSE)
  if (!all(y %in% c(0, 1))) {
    stop("`y` must be coded strictly as 0 and 1.", call. = FALSE)
  }
  if (length(unique(y)) < 2) {
    stop("`y` must contain both classes.", call. = FALSE)
  }
  if (length(n_bins) != 1 || !is.numeric(n_bins) ||
      n_bins < 2 || n_bins != floor(n_bins)) {
    stop("`n_bins` must be an integer of at least 2.", call. = FALSE)
  }
  
  n <- length(score)
  npos <- sum(y)
  nneg <- n - npos
  
  ## Mann-Whitney AUC (average ranks for ties)
  r <- rank(score)
  auc <- (sum(r[y == 1]) - npos * (npos + 1) / 2) / (npos * nneg)
  gini <- 2 * auc - 1
  
  ## KS at observation level (descending score)
  ord <- order(score, decreasing = TRUE)
  ys <- y[ord]
  d <- cumsum(ys) / npos - cumsum(1 - ys) / nneg
  k <- which.max(d)
  ks <- d[k]
  ks_at <- k / n
  
  ## Gains table: equal-count bins, ordered riskiest first
  brks <- unique(quantile(score, probs = seq(0, 1, length.out = n_bins + 1)))
  grp <- cut(score, breaks = brks, include.lowest = TRUE)
  n_t  <- as.integer(table(grp))
  nb_t <- as.integer(table(grp[y == 1]))
  ng_t <- as.integer(table(grp[y == 0]))
  lvls <- levels(grp)
  
  rev_rows <- rev(seq_along(n_t))
  n_b <- n_t[rev_rows]
  nb <- nb_t[rev_rows]
  ng <- ng_t[rev_rows]
  
  bad_rate <- nb / n_b
  cum_bad_share <- cumsum(nb) / npos
  cum_good_share <- cumsum(ng) / nneg
  
  gains <- data.frame(
    bin            = lvls[rev_rows],
    n              = n_b,
    n_bad          = nb,
    n_good         = ng,
    bad_rate       = bad_rate,
    cum_n          = cumsum(n_b),
    cum_bad        = cumsum(nb),
    cum_good       = cumsum(ng),
    cum_bad_share  = cum_bad_share,
    cum_good_share = cum_good_share,
    ks             = cum_bad_share - cum_good_share,
    lift           = bad_rate / (npos / n),
    row.names      = NULL,
    stringsAsFactors = FALSE
  )
  
  list(auc = auc, gini = gini, ks = ks, ks_at = ks_at,
       gains = gains, n = n, n_bad = npos, n_good = nneg)
}