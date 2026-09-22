#' Cutoff strategy analysis for a points scorecard
#'
#' Computes approval statistics at a cutoff, the approval/bad-rate
#' trade-off curve across all candidate cutoffs, and swap sets
#' between two cutoffs, supporting data-driven cutoff policy
#' selection.
#'
#' @details
#' Orientation follows the points scale: higher score = safer, and
#' approval is score >= cutoff (ties approved). Swap sets quantify
#' policy moves: tightening from A to B declines loans previously
#' approved (n_out, with good/bad split); loosening approves loans
#' previously declined (n_in). Threshold policies are nested, so
#' swaps are one-directional. The good/bad split of swaps is the
#' core policy quantity: a tightening that removes mostly goods
#' destroys volume without reducing risk.
#'
#' bad_rejected_share is the share of all bads declined by the
#' cutoff (bads sit at low scores; approval is score >= cutoff).
#'
#' All statistics are computed on the provided sample and are
#' in-sample by construction; out-of-time evaluation applies the
#' same functions to a holdout period.
#'
#' @param score Numeric points score, no NA; higher = safer.
#' @param y Binary outcome coded 0 (good) / 1 (bad), no NA.
#' @param cutoff Approval threshold; approve score >= cutoff.
#' @param from,to Cutoffs to compare for the swap set.
#'
#' @return cutoff_analysis: list with n, n_approved,
#'   approval_rate, n_bad_approved, bad_rate_approved,
#'   overall_bad_rate, good_approval_rate, bad_approval_rate,
#'   bad_rejected_share. cutoff_curve: data.frame (cutoff,
#'   n_approved, approval_rate, bad_rate_approved,
#'   good_approval_rate, bad_rejected_share), one row per distinct
#'   score, ascending. cutoff_swap: list with n_out, n_out_good,
#'   n_out_bad, n_in, n_in_good, n_in_bad, swap_events.
#'
#' @references Siddiqi, N. (2006). Credit Risk Scorecards. Wiley,
#'   ch. 7.
cutoff_analysis <- function(score, y, cutoff) {
  
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
  if (length(cutoff) != 1 || !is.numeric(cutoff) ||
      !is.finite(cutoff)) {
    stop("`cutoff` must be a single finite number.", call. = FALSE)
  }
  
  n <- length(score)
  npos <- sum(y)
  nneg <- n - npos
  
  appr <- score >= cutoff
  n_appr <- sum(appr)
  if (n_appr == 0) {
    stop("`cutoff` approves no observations; use a lower cutoff.",
         call. = FALSE)
  }
  
  n_bad_appr <- sum(y[appr] == 1)
  n_good_appr <- n_appr - n_bad_appr
  
  list(
    n = n,
    n_approved = n_appr,
    approval_rate = n_appr / n,
    n_bad_approved = n_bad_appr,
    bad_rate_approved = n_bad_appr / n_appr,
    overall_bad_rate = npos / n,
    good_approval_rate = n_good_appr / nneg,
    bad_approval_rate = n_bad_appr / npos,
    bad_rejected_share = (npos - n_bad_appr) / npos
  )
}

#' @rdname cutoff_analysis
cutoff_curve <- function(score, y) {
  
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
  
  cuts <- sort(unique(score))
  n <- length(score)
  npos <- sum(y)
  nneg <- n - npos
  
  do.call(rbind, lapply(cuts, function(cc) {
    appr <- score >= cc
    n_appr <- sum(appr)
    n_bad_appr <- sum(y[appr] == 1)
    n_good_appr <- n_appr - n_bad_appr
    data.frame(
      cutoff = cc,
      n_approved = n_appr,
      approval_rate = n_appr / n,
      bad_rate_approved = n_bad_appr / n_appr,
      good_approval_rate = n_good_appr / nneg,
      bad_rejected_share = (npos - n_bad_appr) / npos,
      row.names = NULL
    )
  }))
}

#' @rdname cutoff_analysis
cutoff_swap <- function(score, y, from, to) {
  
  for (arg in c("from", "to")) {
    v <- get(arg)
    if (length(v) != 1 || !is.numeric(v) || !is.finite(v)) {
      stop("`", arg, "` must be a single finite number.", call. = FALSE)
    }
  }
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
  
  a_from <- score >= from
  a_to   <- score >= to
  
  out <- a_from & !a_to      # approved before, declined now
  inn <- !a_from & a_to      # declined before, approved now
  
  list(
    n_out = sum(out),
    n_out_good = sum(out & y == 0),
    n_out_bad = sum(out & y == 1),
    n_in = sum(inn),
    n_in_good = sum(inn & y == 0),
    n_in_bad = sum(inn & y == 1),
    swap_events = sum(out) + sum(inn)
  )
}