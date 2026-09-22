#' Population and characteristic stability index
#'
#' Computes the population stability index (PSI) between a
#' reference (expected) and a current (actual) distribution,
#' binned either by explicit breaks, by equal-count quantile bins
#' of the reference, or by factor levels. Applied to a risk score
#' it is PSI; applied to a model characteristic on its deployed
#' bins it is the characteristic stability index (CSI).
#'
#' @details
#' PSI = sum over bins of (p_expected - p_actual) *
#' ln(p_expected / p_actual). Interpretation thresholds follow the
#' industry convention (documented in Siddiqi, 2017, Intelligent
#' Credit Scoring): below 0.10 stable; 0.10-0.25 moderate shift;
#' above 0.25 significant shift warranting investigation. These
#' are materiality thresholds, not hypothesis tests: at volume a
#' statistically significant change can remain monitoring-green,
#' and PSI is deliberately insensitive to sample size beyond the
#' bin proportions themselves.
#'
#' Bins are defined by the reference. Numeric binning uses
#' left-closed intervals with open outer bins (-Inf, +Inf), so
#' actual values outside the reference range are absorbed by the
#' end bins rather than dropped. NA observations in either side
#' form an explicit Missing bin; a bin with zero proportion in
#' either distribution makes PSI undefined and errors with
#' guidance (reduce bins / handle new categories or missingness
#' explicitly). Categorical actual values unseen in the reference
#' error, since PSI is undefined for categories absent from the
#' reference.
#'
#' @param expected Numeric or factor reference sample.
#' @param actual Numeric or factor current sample; lengths may
#'   differ (reference n vs. monitoring-period n).
#' @param breaks Optional interior cut points (finite, strictly
#'   increasing). Default: equal-count quantile bins of
#'   \code{expected}.
#' @param n_bins Number of quantile bins when \code{breaks} is
#'   NULL (integer >= 2).
#'
#' @return A data.frame with columns bin, n_exp, p_exp, n_act,
#'   p_act, psi_contribution, and attributes psi (total), status
#'   ("stable"/"moderate"/"significant"), thresholds.
#'
#' @references Siddiqi, N. (2017). Intelligent Credit Scoring.
#'   Wiley.
psi_table <- function(expected, actual, breaks = NULL, n_bins = 10) {
  
  if (is.character(expected)) expected <- factor(expected)
  if (is.character(actual)) actual <- factor(actual)
  
  if (length(expected) == 0 || length(actual) == 0) {
    stop("`expected` and `actual` must be non-empty.", call. = FALSE)
  }
  
  if (is.factor(expected) || is.factor(actual)) {
    if (!is.factor(expected) || !is.factor(actual)) {
      stop("`expected` and `actual` must both be factors.", call. = FALSE)
    }
    bins <- levels(expected)
    unseen <- setdiff(unique(as.character(actual[!is.na(actual)])), bins)
    if (length(unseen) > 0) {
      stop("Actual contains level(s) unseen in the reference: ",
           paste(unseen, collapse = ", "),
           "; PSI is undefined for categories absent from the ",
           "reference.", call. = FALSE)
    }
    n_exp_o <- as.integer(table(expected))
    n_act_o <- as.integer(table(factor(actual, levels = bins)))
  } else {
    if (!is.numeric(expected) || !is.numeric(actual)) {
      stop("`expected` and `actual` must be numeric or factors.",
           call. = FALSE)
    }
    if (is.null(breaks)) {
      if (length(n_bins) != 1 || !is.numeric(n_bins) ||
          n_bins < 2 || n_bins != floor(n_bins)) {
        stop("`n_bins` must be an integer of at least 2.", call. = FALSE)
      }
      q <- unique(quantile(expected,
                           probs = seq(0, 1, length.out = n_bins + 1),
                           na.rm = TRUE))
      if (length(q) < 3) {
        stop("`expected` yields fewer than three unique quantile ",
             "values; provide explicit `breaks` or reduce `n_bins`.",
             call. = FALSE)
      }
      breaks <- unname(q[-c(1, length(q))])
    } else {
      if (!is.numeric(breaks) || anyNA(breaks) ||
          !all(is.finite(breaks)) || length(breaks) < 1 ||
          is.unsorted(breaks, strictly = TRUE)) {
        stop("`breaks` must be finite, strictly increasing interior ",
             "cut points.", call. = FALSE)
      }
    }
    edges <- c(-Inf, as.numeric(breaks), Inf)
    f_exp <- cut(expected, edges, right = FALSE)
    bins <- levels(f_exp)
    n_exp_o <- as.integer(table(f_exp))
    n_act_o <- as.integer(table(cut(actual, edges, right = FALSE)))
  }
  
  m_exp <- sum(is.na(expected))
  m_act <- sum(is.na(actual))
  if (m_exp + m_act > 0) {
    bins <- c(bins, "Missing")
    n_exp_o <- c(n_exp_o, m_exp)
    n_act_o <- c(n_act_o, m_act)
  }
  
  N_exp <- sum(n_exp_o)
  N_act <- sum(n_act_o)
  if (N_exp == 0 || N_act == 0) {
    stop("`expected` and `actual` must each contain non-NA ",
         "observations.", call. = FALSE)
  }
  p_exp <- n_exp_o / N_exp
  p_act <- n_act_o / N_act
  
  zero <- which(p_exp == 0 | p_act == 0)
  if (length(zero) > 0) {
    stop("Bin(s) ", paste0("'", bins[zero], "'", collapse = ", "),
         " have zero proportion in expected or actual; PSI is ",
         "undefined. Reduce the number of bins, merge sparse bins, ",
         "or handle new categories/missingness explicitly.",
         call. = FALSE)
  }
  
  psi_c <- (p_exp - p_act) * log(p_exp / p_act)
  psi <- sum(psi_c)
  status <- if (psi < 0.10) "stable" else if (psi < 0.25) "moderate"
  else "significant"
  
  out <- data.frame(
    bin = bins, n_exp = n_exp_o, p_exp = p_exp,
    n_act = n_act_o, p_act = p_act, psi_contribution = psi_c,
    stringsAsFactors = FALSE, row.names = NULL
  )
  attr(out, "psi") <- psi
  attr(out, "status") <- status
  attr(out, "thresholds") <- c(0.10, 0.25)
  out
}