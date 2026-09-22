# tests/testthat/test-scale_scorecard.R
# Worked example: base 600 at odds 30:1, PDO 40. Two-characteristic
# model: b0 = -2, b1 = -0.8 (bins A/B, WOE +0.5/-0.5),
# b2 = -1.2 (bins L/M/H, WOE +1/0/-1). Expected values are literals
# from an independently computed, two-route-verified derivation
# (points-sum vs direct score formula).

wt1 <- data.frame(bin = c("A", "B"), woe = c(0.5, -0.5))
wt2 <- data.frame(bin = c("L", "M", "H"), woe = c(1.0, 0.0, -1.0))
betas <- c("(Intercept)" = -2.0, "char1" = -0.8, "char2" = -1.2)

test_that("scaling constants are derived correctly", {
  sc <- scale_scorecard(betas, list(char1 = wt1, char2 = wt2))
  expect_equal(attr(sc, "factor"), 40 / log(2), tolerance = 1e-9)
  expect_equal(attr(sc, "offset"), 600 - (40 / log(2)) * log(30),
               tolerance = 1e-9)
  expect_equal(attr(sc, "base_score"), 600)
  expect_equal(attr(sc, "base_odds"), 30)
  expect_equal(attr(sc, "pdo"), 40)
  expect_equal(attr(sc, "n_characteristics"), 2L)
})

test_that("points reproduce the worked example", {
  sc <- scale_scorecard(betas, list(char1 = wt1, char2 = wt2))
  expect_equal(sc$characteristic, rep(c("char1", "char2"), c(2, 3)))
  expect_equal(sc$bin, c("A", "B", "L", "M", "H"))
  expect_equal(sc$woe, c(0.5, -0.5, 1, 0, -1))
  expect_equal(sc$points,
               c(282.65311038, 236.48686907,
                 328.81935169, 259.56998972, 190.32062776),
               tolerance = 1e-5)
})

test_that("sum of points reconstructs the score exactly", {
  sc <- scale_scorecard(betas, list(char1 = wt1, char2 = wt2))
  F <- attr(sc, "factor"); O <- attr(sc, "offset")
  # applicant A+L: ln(odds) = -(-2 - 0.4 - 1.2) = 3.6
  expect_equal(sum(sc$points[sc$bin %in% c("A", "L")]),
               O + F * 3.6, tolerance = 1e-9)
  # applicant B+H: ln(odds) = 0.4
  expect_equal(sum(sc$points[sc$bin %in% c("B", "H")]),
               O + F * 0.4, tolerance = 1e-9)
})

test_that("PDO property: doubling odds adds exactly pdo points", {
  sc <- scale_scorecard(betas, list(char1 = wt1, char2 = wt2))
  F <- attr(sc, "factor"); O <- attr(sc, "offset")
  expect_equal(O + F * log(30), 600, tolerance = 1e-9)
  expect_equal(O + F * log(60), 640, tolerance = 1e-9)
  expect_equal(O + F * log(120), 680, tolerance = 1e-9)
})

test_that("betas and woe_tables names are validated", {
  expect_error(
    scale_scorecard(c(char1 = -0.8, char2 = -1.2),
                    list(char1 = wt1, char2 = wt2)),
    "Intercept")
  expect_error(
    scale_scorecard(c("(Intercept)" = -2, char1 = -0.8, extra = -1),
                    list(char1 = wt1, char2 = wt2)),
    "match")
  expect_error(
    scale_scorecard(c("(Intercept)" = -2, char1 = -0.8),
                    list(char1 = wt1, char2 = wt2)),
    "match")
})

test_that("scaling parameters are validated", {
  expect_error(scale_scorecard(betas, list(char1 = wt1, char2 = wt2),
                               base_odds = -1), "base_odds")
  expect_error(scale_scorecard(betas, list(char1 = wt1, char2 = wt2),
                               pdo = 0), "pdo")
})