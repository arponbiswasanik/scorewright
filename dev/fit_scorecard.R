# dev/fit_scorecard.R
# Fit the full WOE-logistic scorecard on the development sample,
# scale it, and verify: exact points reconstruction (two routes),
# score distribution vs the 600 @ 30:1 / PDO 40 anchor, negative
# coefficient signs, in-sample discrimination, and agreement with
# the oracle's true PD (the DGP recovery check).
devtools::load_all()

dat <- sme_dev_sample
y   <- dat$ever_90dpd_12m

## Characteristic selection: designed signal tiers only; nulls
## (branch, rate_pct, annual_sales, rm_tenure, employees,
## loan_amount) are excluded.
num_chars <- c("bureau_score", "dscr", "business_age", "ltv",
               "current_ratio")
cat_chars <- c("sector", "tenor_months", "loan_purpose",
               "collateral_type", "check_returns_12m")

## Domain-guided classing for cheque returns (0 / 1 / 2+)
dat$check_returns_12m <- factor(pmin(dat$check_returns_12m, 2),
                                levels = c(0, 1, 2))

woe_tables <- list()
woe_cols   <- list()

for (ch in num_chars) {
  b <- bin_numeric(dat[[ch]], y)
  f <- cut(dat[[ch]], breaks = attr(b, "breaks"), right = FALSE,
           include.lowest = TRUE, dig.lab = 10)
  f <- ifelse(is.na(f), "Missing", as.character(f))
  woe_tables[[ch]] <- b
  woe_cols[[ch]]   <- unname(setNames(b$woe, b$bin)[f])
}

for (ch in cat_chars) {
  wt <- woe_table(dat[[ch]], y)
  woe_tables[[ch]] <- wt
  woe_cols[[ch]]   <- unname(setNames(wt$woe, wt$bin)[
    as.character(dat[[ch]])])
}

design <- as.data.frame(woe_cols)

fit   <- glm(y ~ ., family = binomial(), data = design)
coefs <- coef(fit)
cat("---- coefficients ----\n")
print(round(coefs, 4))

stopifnot(all(coefs[-1] < 0))  # positive WOE = good => negative betas

sc <- scale_scorecard(coefs, woe_tables,
                      base_score = 600, base_odds = 30, pdo = 40)
F <- attr(sc, "factor"); O <- attr(sc, "offset")

## Exact reconstruction: points-sum route vs linear-predictor route
bin_of <- function(ch) {
  if (ch %in% num_chars) {
    fc <- cut(dat[[ch]], breaks = attr(woe_tables[[ch]], "breaks"),
              right = FALSE, include.lowest = TRUE, dig.lab = 10)
    ifelse(is.na(fc), "Missing", as.character(fc))
  } else as.character(dat[[ch]])
}
score_pts <- rowSums(sapply(names(woe_cols), function(ch) {
  pts <- setNames(sc$points[sc$characteristic == ch],
                  sc$bin[sc$characteristic == ch])
  unname(pts[bin_of(ch)])
}))
score_lp <- O - F * fit$linear.predictors
cat("max |points-route - lp-route|:",
    max(abs(score_pts - score_lp)), "\n")
stopifnot(max(abs(score_pts - score_lp)) < 1e-8)

## Score distribution
cat("---- score distribution ----\n")
print(round(quantile(score_pts, c(0, .01, .25, .5, .75, .99, 1)), 1))
cat("mean score:", round(mean(score_pts), 1), "\n")

## Bad rate by score decile (must be non-increasing)
br <- unique(quantile(score_pts, seq(0, 1, 0.1)))
dec <- cut(score_pts, breaks = br, include.lowest = TRUE)
cat("---- bad rate by score decile ----\n")
print(round(tapply(y, dec, mean), 4))
stopifnot(all(diff(tapply(y, dec, mean)) <= 0.005))

## In-sample discrimination
r <- rank(fit$fitted.values)
npos <- sum(y); nneg <- length(y) - npos
auc <- (sum(r[y == 1]) - npos * (npos + 1) / 2) / (npos * nneg)
cat("in-sample AUC:", round(auc, 4), "\n")

## Oracle recovery: fitted PD vs the true PD, and the oracle ceiling
gt <- readRDS("inst/testdata/sme_ground_truth.rds")
cat("cor(fitted PD, true PD):",
    round(cor(fit$fitted.values, gt$dev$pd_true), 4), "\n")
r0 <- rank(gt$dev$pd_true)
auc0 <- (sum(r0[y == 1]) - npos * (npos + 1) / 2) / (npos * nneg)
cat("oracle-ceiling AUC (true PD vs outcome):", round(auc0, 4), "\n")