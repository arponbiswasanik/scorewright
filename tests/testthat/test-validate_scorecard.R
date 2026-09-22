# tests/testthat/test-validate_scorecard.R
# Worked example: 10 obs, 4 bad, 6 good; HIGHER score = riskier.
# score = c(95,90,85,80,75,70,65,60,55,50)
# y     = c( 1, 1, 0, 0, 1, 1, 0, 0, 0, 0)
# Hand computation: bad ranks (ascending) 5,6,9,10, sum 30;
# AUC = (30-10)/24; Gini = 2*AUC-1; KS = 4/4 - 2/6 at 60%.

score <- c(95, 90, 85, 80, 75, 70, 65, 60, 55, 50)
y     <- c(1, 1, 0, 0, 1, 1, 0, 0, 0, 0)

test_that("AUC and Gini reproduce the worked example", {
  v <- validate_scorecard(score, y)
  expect_equal(v$auc, 20/24, tolerance = 1e-12)
  expect_equal(v$gini, 2 * 20/24 - 1, tolerance = 1e-12)
  expect_equal(v$n, 10L)
  expect_equal(v$n_bad, 4L)
  expect_equal(v$n_good, 6L)
})

test_that("KS statistic and location", {
  v <- validate_scorecard(score, y)
  expect_equal(v$ks, 1 - 2/6, tolerance = 1e-12)
  expect_equal(v$ks_at, 0.6, tolerance = 1e-12)
})

test_that("gains table: counts, capture, lift (n_bins = 5)", {
  v <- validate_scorecard(score, y, n_bins = 5)
  g <- v$gains
  expect_equal(nrow(g), 5L)
  expect_equal(sum(g$n), 10L)
  expect_equal(sum(g$n_bad), 4L)
  expect_equal(g$n[1], 2L)
  expect_equal(g$n_bad[1], 2L)
  expect_equal(g$bad_rate[1], 1.0)
  expect_equal(g$cum_bad_share[1], 0.5)
  expect_equal(g$lift[1], 1.0 / 0.4)
  expect_equal(g$cum_bad_share[3], 1.0)
})

test_that("orientation: reversing score complements AUC", {
  expect_equal(validate_scorecard(score, y)$auc +
                 validate_scorecard(-score, y)$auc, 1.0,
               tolerance = 1e-12)
})

test_that("ties use average ranks (Mann-Whitney)", {
  expect_equal(validate_scorecard(c(1, 1, 2), c(0, 1, 1))$auc, 0.75)
})

test_that("input validation", {
  expect_error(validate_scorecard(score, c(0, 1, 2, 0, 0, 0, 0, 0, 0, 0)), "0 and 1")
  expect_error(validate_scorecard(score, c(0, 1, NA, 0, 0, 0, 0, 0, 0, 0)), "NA")
  expect_error(validate_scorecard(score, c(0, 1)), "length")
  expect_error(validate_scorecard(score, rep(0, 10)), "classes")
  expect_error(validate_scorecard(score, y, n_bins = 1), "n_bins")
})