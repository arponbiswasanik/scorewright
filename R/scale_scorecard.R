#' Points-to-score scaling for a WOE logistic scorecard
#'
#' Maps fitted WOE-logistic coefficients onto a fixed score scale
#' using the standard points-to-score transformation (Siddiqi 2006,
#' ch. 5).
#'
#' @details
#' The score scale is defined by Score = Offset + Factor *
#' ln(odds), where odds are good:bad, Factor = PDO / ln(2) and
#' Offset = base_score - Factor * ln(base_odds). PDO is the points
#' required to double the odds.
#'
#' Points are distributed across characteristics so that an
#' applicant's total points equal the score exactly:
#' points_{j,b} = -(WOE_{j,b} * beta_j + beta_0 / n) * Factor +
#' Offset / n, with n the number of characteristics and beta_0 the
#' intercept of the bad = 1 logistic regression on WOE-transformed
#' inputs. With this package's WOE convention (positive = good),
#' negative coefficients yield positive points for safe bins.
#'
#' Points are returned at full precision; rounding to integers is
#' a presentation-layer decision.
#'
#' @param betas Named numeric vector of logistic coefficients,
#'   including the intercept as "(Intercept)".
#' @param woe_tables Named list of data.frames with columns bin
#'   and woe, as returned by woe_table() and bin_numeric(). Names
#'   must match the non-intercept names of betas exactly.
#' @param base_score Score at base_odds.
#' @param base_odds Good:bad odds at base_score.
#' @param pdo Points to double the odds.
#'
#' @return A data.frame with columns characteristic, bin, woe,
#'   points, and attributes factor, offset, base_score, base_odds,
#'   pdo, n_characteristics.
#'
#' @references Siddiqi, N. (2006). Credit Risk Scorecards. Wiley,
#'   ch. 5.
#'
#' @examples
#' wt1 <- data.frame(bin = c("A", "B"), woe = c(0.5, -0.5))
#' wt2 <- data.frame(bin = c("L", "M", "H"), woe = c(1, 0, -1))
#' betas <- c("(Intercept)" = -2, "char1" = -0.8, "char2" = -1.2)
#' scale_scorecard(betas, list(char1 = wt1, char2 = wt2))
scale_scorecard <- function(betas, woe_tables, base_score = 600,
                            base_odds = 30, pdo = 40) {
  
  if (!is.numeric(betas) || length(betas) == 0 || is.null(names(betas)) ||
      anyNA(betas) || !all(is.finite(betas))) {
    stop("`betas` must be a finite named numeric vector.", call. = FALSE)
  }
  if (!"(Intercept)" %in% names(betas)) {
    stop("`betas` must include the model intercept named '(Intercept)'.",
         call. = FALSE)
  }
  if (!is.list(woe_tables) || is.null(names(woe_tables)) ||
      any(!nzchar(names(woe_tables)))) {
    stop("`woe_tables` must be a named list.", call. = FALSE)
  }
  beta0 <- unname(betas["(Intercept)"])
  coefs <- betas[setdiff(names(betas), "(Intercept)")]
  if (!setequal(names(coefs), names(woe_tables))) {
    stop("`woe_tables` names must match the non-intercept names of ",
         "`betas` exactly.", call. = FALSE)
  }
  if (length(base_score) != 1 || !is.numeric(base_score) ||
      !is.finite(base_score)) {
    stop("`base_score` must be a single finite number.", call. = FALSE)
  }
  if (length(base_odds) != 1 || !is.numeric(base_odds) ||
      !is.finite(base_odds) || base_odds <= 0) {
    stop("`base_odds` must be a positive finite number.", call. = FALSE)
  }
  if (length(pdo) != 1 || !is.numeric(pdo) || !is.finite(pdo) || pdo <= 0) {
    stop("`pdo` must be a positive finite number.", call. = FALSE)
  }
  
  factor <- pdo / log(2)
  offset <- base_score - factor * log(base_odds)
  n <- length(coefs)
  
  out <- do.call(rbind, lapply(names(coefs), function(nm) {
    wt <- woe_tables[[nm]]
    if (!is.data.frame(wt) ||
        !all(c("bin", "woe") %in% names(wt)) || nrow(wt) == 0) {
      stop("woe_tables[['", nm, "']] must be a data.frame with columns ",
           "'bin' and 'woe'.", call. = FALSE)
    }
    data.frame(
      characteristic = nm,
      bin            = wt$bin,
      woe            = wt$woe,
      points         = -((wt$woe * coefs[[nm]]) + beta0 / n) * factor +
        offset / n,
      stringsAsFactors = FALSE,
      row.names      = NULL
    )
  }))
  
  attr(out, "factor") <- factor
  attr(out, "offset") <- offset
  attr(out, "base_score") <- base_score
  attr(out, "base_odds") <- base_odds
  attr(out, "pdo") <- pdo
  attr(out, "n_characteristics") <- n
  out
}