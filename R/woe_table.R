#' Weight-of-evidence and information-value table for a binned
#' characteristic
#'
#' Computes bin-level counts, outcome distributions, weights of
#' evidence (WOE), and information-value (IV) contributions for a
#' categorical (already binned) characteristic against a binary
#' outcome.
#'
#' @details
#' Sign convention follows Siddiqi (2006, ch. 4):
#' \deqn{WOE_i = \ln(D_{good,i} / D_{bad,i})}
#' where \code{D_good} and \code{D_bad} are each bin's share of all
#' goods and all bads. Positive WOE therefore marks a good-heavy
#' (safer) bin; negative WOE marks a bad-heavy (riskier) bin. The
#' convention is not universal in the literature -- some sources
#' and packages invert it -- so this implementation pins it
#' explicitly. IV is convention-free: inverting the WOE sign also
#' inverts the distribution difference, leaving every IV
#' contribution unchanged.
#'
#' Missing values of \code{x} form an explicit \code{Missing} bin,
#' reflecting the standard scorecard practice of treating missing
#' as its own informative category rather than imputing or
#' dropping. The label \code{Missing} is reserved and must not
#' already appear as a level of \code{x}.
#'
#' Bins containing zero bad or zero good observations are not
#' estimable. The function errors with guidance to merge such bins
#' with adjacent ones; any zero-cell correction policy is expected
#' to be applied explicitly at the binning step, not silently here.
#'
#' IV interpretation bands (Siddiqi, 2006): below 0.02 unpredictable;
#' 0.02-0.1 weak; 0.1-0.3 medium; 0.3-0.5 strong; above 0.5
#' suspicious, warranting review of the characteristic.
#'
#' @param x A binned characteristic: factor or character. Numeric
#'   characteristics must be binned first; passing a numeric vector
#'   is an error.
#' @param y Binary outcome, coded 0 (good) and 1 (bad), without NA,
#'   same length as \code{x}.
#'
#' @return A data.frame with one row per bin, in factor-level order
#'   with the \code{Missing} bin (when present) appended last, and
#'   columns: \code{bin}, \code{n}, \code{n_bad}, \code{n_good},
#'   \code{dist_bad}, \code{dist_good}, \code{woe},
#'   \code{iv_contribution}. Total IV is
#'   \code{sum(iv_contribution)}.
#'
#' @references Siddiqi, N. (2006). Credit Risk Scorecards:
#'   Developing and Implementing Intelligent Credit Scoring Systems.
#'   Wiley, ch. 4.
#'
#' @examples
#' bin <- c(rep("A", 34), rep("B", 42), rep("C", 20), rep(NA, 24))
#' y <- c(rep(1, 4), rep(0, 30), rep(1, 8), rep(0, 34),
#'        rep(1, 7), rep(0, 13), rep(1, 5), rep(0, 19))
#' woe_table(bin, y)
woe_table <- function(x, y) {
  
  ## -- input validation ------------------------------------------------
  
  if (length(x) != length(y)) {
    stop("`x` and `y` must have the same length.", call. = FALSE)
  }
  if (!is.numeric(y)) {
    stop("`y` must be a numeric vector coded 0 (good) and 1 (bad).",
         call. = FALSE)
  }
  if (anyNA(y)) {
    stop("`y` must not contain NA.", call. = FALSE)
  }
  if (!all(y %in% c(0, 1))) {
    stop("`y` must be coded strictly as 0 and 1.", call. = FALSE)
  }
  if (is.factor(x)) {
    x <- droplevels(x)
    bins <- levels(x)
  } else if (is.character(x)) {
    bins <- sort(unique(x[!is.na(x)]))
  } else {
    stop("`x` must be a factor or character vector; ",
         "numeric characteristics must be binned first.",
         call. = FALSE)
  }
  
  ## -- missing-bin construction ----------------------------------------
  
  x_chr <- as.character(x)
  has_missing <- anyNA(x_chr)
  if (has_missing) {
    if ("Missing" %in% bins) {
      stop("Label 'Missing' is reserved for NA observations but ",
           "already appears as a level of `x`.", call. = FALSE)
    }
    x_chr[is.na(x_chr)] <- "Missing"
    bins <- c(bins, "Missing")
  }
  x_bin <- factor(x_chr, levels = bins)
  
  ## -- bin counts ------------------------------------------------------
  
  n_all  <- as.integer(table(x_bin))
  n_bad  <- as.integer(table(x_bin[y == 1]))
  n_good <- as.integer(table(x_bin[y == 0]))
  
  ## -- estimability: zero-event bins are refused, not patched ----------
  
  if (any(n_bad == 0)) {
    stop("Bin(s) ", paste0("'", bins[n_bad == 0], "'", collapse = ", "),
         " contain zero bad observations; merge with an adjacent bin ",
         "before computing WOE, or apply an explicit zero-cell ",
         "correction policy at the binning step.", call. = FALSE)
  }
  if (any(n_good == 0)) {
    stop("Bin(s) ", paste0("'", bins[n_good == 0], "'", collapse = ", "),
         " contain zero good observations; merge with an adjacent bin ",
         "before computing WOE, or apply an explicit zero-cell ",
         "correction policy at the binning step.", call. = FALSE)
  }
  
  ## -- WOE and IV (Siddiqi 2006, ch. 4) ---------------------------------
  
  B <- sum(n_bad)
  G <- sum(n_good)
  
  dist_bad  <- n_bad / B
  dist_good <- n_good / G
  woe <- log(dist_good / dist_bad)
  iv_contribution <- (dist_good - dist_bad) * woe
  
  data.frame(
    bin            = bins,
    n              = n_all,
    n_bad          = n_bad,
    n_good         = n_good,
    dist_bad       = dist_bad,
    dist_good      = dist_good,
    woe            = woe,
    iv_contribution = iv_contribution,
    stringsAsFactors = FALSE,
    row.names      = NULL
  )
}