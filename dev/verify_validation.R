# dev/verify_validation.R
source("dev/fit_scorecard.R")   # re-runs deterministic fit; defines fit, score_pts, y

v_pd  <- validate_scorecard(fit$fitted.values, y)   # PD: higher = riskier
v_pts <- validate_scorecard(-score_pts, y)          # points: higher = safer
cat("Gini (PD):", round(v_pd$gini, 4), "| KS:", round(v_pd$ks, 4),
    "at", round(v_pd$ks_at, 2), "of population\n")
cat("Gini (-points):", round(v_pts$gini, 4), "\n")

gt <- readRDS("inst/testdata/sme_ground_truth.rds")
v_or <- validate_scorecard(gt$dev$pd_true, y)
cat("Oracle-ceiling Gini:", round(v_or$gini, 4),
    "| fitted/ceiling:", round(v_pd$gini / v_or$gini, 3), "\n")

cat("Top-decile bad capture:", round(v_pd$gains$cum_bad_share[1], 3),
    "| lift:", round(v_pd$gains$lift[1], 2), "\n")
print(head(v_pd$gains, 3))

stopifnot(
  abs(v_pd$gini - v_pts$gini) < 1e-8,      # orientation consistency
  v_pd$gini / v_or$gini > 0.90,            # near-ceiling discrimination
  v_pd$gains$cum_bad_share[1] > 0.30       # top decile captures >= 30% of bads
)