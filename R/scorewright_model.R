#' Fit the scorecard and assemble all downstream results
#'
#' Runs the full pipeline once: binning, WOE transformation,
#' logistic estimation, points-to-score scaling, validation, and
#' monitoring metrics. Deterministic given the packaged data.
#'
#' @return A list with elements: coefs, woe_tables, points_table,
#'   scores, y, validation, psi_monthly, csi_matrix, cutoff_curve,
#'   and the scaling constants.
#'
#' @keywords internal
scorewright_model <- function() {
  
  dat <- sme_dev_sample
  y <- dat$ever_90dpd_12m
  
  num_chars <- c("bureau_score", "dscr", "business_age", "ltv",
                 "current_ratio")
  cat_chars <- c("sector", "tenor_months", "loan_purpose",
                 "collateral_type", "check_returns_12m")
  dat$check_returns_12m <- factor(pmin(dat$check_returns_12m, 2),
                                  levels = c(0, 1, 2))
  
  woe_tables <- list()
  woe_cols <- list()
  for (ch in num_chars) {
    b <- bin_numeric(dat[[ch]], y)
    f <- cut(dat[[ch]], breaks = attr(b, "breaks"), right = FALSE,
             include.lowest = TRUE, dig.lab = 10)
    f <- ifelse(is.na(f), "Missing", as.character(f))
    woe_tables[[ch]] <- b
    woe_cols[[ch]] <- unname(setNames(b$woe, b$bin)[f])
  }
  for (ch in cat_chars) {
    wt <- woe_table(dat[[ch]], y)
    woe_tables[[ch]] <- wt
    woe_cols[[ch]] <- unname(setNames(wt$woe, wt$bin)[
      as.character(dat[[ch]])])
  }
  
  design <- as.data.frame(woe_cols)
  fit <- glm(y ~ ., family = binomial(), data = design)
  coefs <- coef(fit)
  
  sc <- scale_scorecard(coefs, woe_tables,
                        base_score = 600, base_odds = 30, pdo = 40)
  F <- attr(sc, "factor")
  O <- attr(sc, "offset")
  
  bin_of <- function(ch) {
    if (ch %in% num_chars) {
      fc <- cut(dat[[ch]], breaks = attr(woe_tables[[ch]], "breaks"),
                right = FALSE, include.lowest = TRUE, dig.lab = 10)
      ifelse(is.na(fc), "Missing", as.character(fc))
    } else as.character(dat[[ch]])
  }
  scores <- rowSums(sapply(names(woe_cols), function(ch) {
    pts <- setNames(sc$points[sc$characteristic == ch],
                    sc$bin[sc$characteristic == ch])
    unname(pts[bin_of(ch)])
  }))
  
  validation <- validate_scorecard(-scores, y)
  
  ## Monitoring: score the snapshots with the frozen scorecard
  snaps <- sme_scoring_snaps
  mon <- format(snaps$app_month, "%Y-%m")
  months_txt <- sort(unique(mon))
  
  score_rows <- function(df) {
    lp <- rep(unname(coefs["(Intercept)"]), nrow(df))
    for (ch in names(woe_tables)) {
      wt <- woe_tables[[ch]]
      if (ch %in% num_chars) {
        br <- attr(wt, "breaks")
        interior <- br[-c(1, length(br))]
        f <- cut(df[[ch]], c(-Inf, interior, Inf), right = FALSE)
        idx <- as.integer(f)
        w <- rep(NA_real_, nrow(df))
        ok <- !is.na(idx)
        w[ok] <- wt$woe[idx[ok]]
        if (any(!ok)) w[!ok] <- wt$woe[wt$bin == "Missing"]
      } else {
        w <- unname(setNames(wt$woe, as.character(wt$bin))[
          as.character(df[[ch]])])
      }
      lp <- lp + unname(coefs[[ch]]) * w
    }
    O - F * lp
  }
  score_cur <- score_rows(snaps)
  
  psi_monthly <- vapply(months_txt, function(m) {
    attr(psi_table(scores, score_cur[mon == m]), "psi")
  }, numeric(1))
  
  all_chars <- c(num_chars, cat_chars, "branch", "annual_sales",
                 "loan_amount", "employees", "rm_tenure", "rate_pct")
  csi <- matrix(NA_real_, length(all_chars), length(months_txt),
                dimnames = list(all_chars, months_txt))
  for (ch in all_chars) for (m in months_txt) {
    exp_x <- dat[[ch]]
    act_x <- snaps[[ch]][mon == m]
    if (ch %in% num_chars) {
      br <- attr(woe_tables[[ch]], "breaks")
      csi[ch, m] <- attr(psi_table(exp_x, act_x,
                                   breaks = br[-c(1, length(br))]), "psi")
    } else if (ch == "check_returns_12m") {
      csi[ch, m] <- attr(psi_table(
        factor(pmin(exp_x, 2), levels = c(0, 1, 2)),
        factor(pmin(act_x, 2), levels = c(0, 1, 2))), "psi")
    } else {
      csi[ch, m] <- attr(psi_table(exp_x, act_x), "psi")
    }
  }
  
  list(
    coefs = coefs,
    woe_tables = woe_tables,
    points_table = sc,
    scores = scores,
    y = y,
    validation = validation,
    psi_monthly = psi_monthly,
    csi_matrix = csi,
    cutoff_curve = cutoff_curve(scores, y),
    factor = F,
    offset = O
  )
}