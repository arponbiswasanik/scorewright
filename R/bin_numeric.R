#' Monotonic coarse classing for a numeric characteristic
#'
#' Bins a numeric characteristic into coarse classes under scorecard
#' constraints, merging adjacent bins greedily by minimum
#' information-value loss until all constraints hold.
#'
#' @details
#' Constraints enforced on the ordered bins (Siddiqi 2006, ch. 4):
#' minimum population share per bin (of non-missing observations),
#' minimum bad count per bin, estimability (no zero-event bins), a
#' maximum bin count, and weak monotonicity of the WOE sequence.
#' The Missing bin is exempt from floors and monotonicity but is
#' counted in the bad and good totals.
#'
#' Orientation is chosen by the two-run rule: the greedy merge is
#' run under weakly non-increasing and weakly non-decreasing WOE
#' separately, and the result with higher final IV is kept (ties
#' resolve to non-increasing). This replaces rank-correlation
#' heuristics, which a non-monotone middle bin can mislead.
#'
#' Bins with equal conditional bad rates merge at exactly zero IV
#' loss (additivity of contributions at constant rate), so
#' tie-breaking merges are information-free.
#'
#' Bins are left-closed: \code{[a, b)}, with the final bin
#' \code{[a, b]}. Values outside explicit \code{breaks} become NA
#' and join the Missing bin.
#'
#' @param x Numeric characteristic.
#' @param y Binary outcome coded 0 (good) / 1 (bad), no NA.
#' @param breaks Optional numeric cut points (at least 3, finite,
#'   strictly increasing). Default: unique deciles of observed x.
#' @param min_bin_pop Minimum population share per ordered bin.
#' @param min_bad Minimum bad count per ordered bin.
#' @param max_bins Maximum number of ordered bins.
#'
#' @return A data.frame as produced by \code{\link{woe_table}} on
#'   the final bins, with attributes \code{breaks} (final cut
#'   points) and \code{direction} ("decreasing" or "increasing").
#'
#' @references Siddiqi, N. (2006). Credit Risk Scorecards. Wiley,
#'   ch. 4.
bin_numeric <- function(x, y, breaks = NULL,
                        min_bin_pop = 0.05, min_bad = 25, max_bins = 5) {
  
  ## ---- input validation ---------------------------------------------
  
  if (is.factor(x)) {
    stop("`x` is a factor; use woe_table() for categorical ",
         "characteristics.", call. = FALSE)
  }
  if (!is.numeric(x)) {
    stop("`x` must be numeric; categorical characteristics should ",
         "use woe_table().", call. = FALSE)
  }
  if (!is.numeric(y)) {
    stop("`y` must be a numeric vector coded 0 (good) and 1 (bad).",
         call. = FALSE)
  }
  if (length(x) != length(y)) {
    stop("`x` and `y` must have the same length.", call. = FALSE)
  }
  if (anyNA(y)) stop("`y` must not contain NA.", call. = FALSE)
  if (!all(y %in% c(0, 1))) {
    stop("`y` must be coded strictly as 0 and 1.", call. = FALSE)
  }
  if (length(unique(y)) < 2) {
    stop("`y` must contain both classes.", call. = FALSE)
  }
  if (!(min_bin_pop > 0 && min_bin_pop < 1)) {
    stop("`min_bin_pop` must be strictly between 0 and 1.",
         call. = FALSE)
  }
  if (min_bad < 1) stop("`min_bad` must be at least 1.", call. = FALSE)
  if (max_bins < 2) stop("`max_bins` must be at least 2.", call. = FALSE)
  if (all(is.na(x))) stop("`x` is entirely NA.", call. = FALSE)
  
  ## ---- breaks ---------------------------------------------------------
  
  if (is.null(breaks)) {
    br <- unique(quantile(x, probs = seq(0, 1, 0.1), na.rm = TRUE))
    if (length(br) < 3) {
      stop("`x` yields fewer than three unique decile values; treat ",
           "the characteristic as categorical and use woe_table().",
           call. = FALSE)
    }
    breaks <- unname(as.numeric(br))
  } else {
    if (!is.numeric(breaks) || anyNA(breaks) ||
        !all(is.finite(breaks)) || length(breaks) < 3 ||
        is.unsorted(breaks, strictly = TRUE)) {
      stop("`breaks` must be finite, strictly increasing, and of ",
           "length >= 3.", call. = FALSE)
    }
    breaks <- unname(as.numeric(breaks))
  }
  
  ## ---- initial fine binning (left-closed) ------------------------------
  
  f <- cut(x, breaks = breaks, right = FALSE, include.lowest = TRUE,
           dig.lab = 10)
  ord_n  <- as.integer(table(f))
  ord_nb <- as.integer(table(f[y == 1]))
  ord_ng <- as.integer(table(f[y == 0]))
  n_non_na <- sum(ord_n)
  if (n_non_na == 0) {
    stop("No non-NA observations of `x` fall within `breaks`.",
         call. = FALSE)
  }
  
  has_missing <- anyNA(f)
  m_nb <- if (has_missing) sum(y[is.na(f)] == 1) else 0L
  m_ng <- if (has_missing) sum(y[is.na(f)] == 0) else 0L
  B <- sum(ord_nb) + m_nb
  G <- sum(ord_ng) + m_ng
  
  contrib <- function(nb, ng) {
    (ng / G - nb / B) * log((ng / G) / (nb / B))
  }
  
  ## ---- greedy merge under one orientation -------------------------------
  
  run_dir <- function(direction) {
    nb <- ord_nb; ng <- ord_ng; edges <- breaks
    repeat {
      k <- length(nb)
      n <- nb + ng
      estim <- nb > 0 & ng > 0
      mono <- if (!all(estim)) {
        FALSE
      } else {
        w <- log(ng / nb)
        if (direction == "dec") all(diff(w) <= 1e-12)
        else all(diff(w) >= -1e-12)
      }
      viol <- !all(estim) || any(nb < min_bad) ||
        any(n < min_bin_pop * n_non_na) || k > max_bins || !mono
      if (!viol) break
      if (k == 1L) return(list(status = "unsat", iv = NA_real_))
      
      ## loss per adjacent pair; non-estimable pairs take priority
      losses <- vapply(seq_len(k - 1L), function(i) {
        before <- if (estim[i] && estim[i + 1L]) {
          contrib(nb[i], ng[i]) + contrib(nb[i + 1L], ng[i + 1L])
        } else NA_real_
        after <- if (nb[i] + nb[i + 1L] > 0 && ng[i] + ng[i + 1L] > 0) {
          contrib(nb[i] + nb[i + 1L], ng[i] + ng[i + 1L])
        } else NA_real_
        if (is.na(before) || is.na(after)) -1 else before - after
      }, numeric(1))
      
      j <- which.min(losses)               # ties resolve leftmost
      nb[j] <- nb[j] + nb[j + 1L]
      ng[j] <- ng[j] + ng[j + 1L]
      nb <- nb[-(j + 1L)]
      ng <- ng[-(j + 1L)]
      edges <- edges[-(j + 1L)]
    }
    iv <- sum((ng / G - nb / B) * log((ng / G) / (nb / B)))
    status <- if (k == 1L && !has_missing) "collapsed" else "ok"
    list(status = status, iv = iv, nb = nb, ng = ng,
         edges = edges, dir = direction)
  }
  
  ## ---- two-run rule ------------------------------------------------------
  
  res_dec <- run_dir("dec")
  res_inc <- run_dir("inc")
  
  if (res_dec$status == "unsat" && res_inc$status == "unsat") {
    stop("Constraints cannot be satisfied at any bin count; relax ",
         "min_bin_pop and/or min_bad.", call. = FALSE)
  }
  pick <- if (res_dec$status == "unsat") {
    res_inc
  } else if (res_inc$status == "unsat") {
    res_dec
  } else if (res_dec$iv >= res_inc$iv) {    # tie -> non-increasing
    res_dec
  } else {
    res_inc
  }
  
  if (pick$status == "collapsed") {
    stop("Binning collapsed to a single bin; no monotone structure ",
         "under the given constraints.", call. = FALSE)
  }
  
  ## ---- assemble output on the final break set -----------------------------
  
  f_final <- cut(x, breaks = pick$edges, right = FALSE,
                 include.lowest = TRUE, dig.lab = 10)
  out <- woe_table(f_final, y)
  attr(out, "breaks") <- pick$edges
  attr(out, "direction") <-
    if (pick$dir == "dec") "decreasing" else "increasing"
  out
}