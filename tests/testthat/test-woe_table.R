# tests/testthat/test-woe_table.R
#
# Ground truth: hand-computed worked example under the Siddiqi (2006)
# sign convention -- WOE = ln(dist_good / dist_bad), so positive WOE
# marks the safer (good-heavy) bin. Expected values below are LITERALS
# from an independent calculator verification performed before this
# file was written; they are not expressions of the implementation's
# own formulas.
#
# Worked example: 120 loans, 24 bad, 96 good. Bins A, B, C, plus an
# explicit Missing bin (missing-as-own-bin is core methodology).
# Total IV = 0.213353 (medium predictive power, 0.1-0.3 band).

## Shared fixture ------------------------------------------------------

bin <- c(rep("A", 34), rep("B", 42), rep("C", 20), rep(NA_character_, 24))
y   <- c(
  rep(1, 4), rep(0, 30),   # A:       4 bad, 30 good
  rep(1, 8), rep(0, 34),   # B:       8 bad, 34 good
  rep(1, 7), rep(0, 13),   # C:       7 bad, 13 good
  rep(1, 5), rep(0, 19)    # Missing: 5 bad, 19 good
)
x_f <- factor(bin, levels = c("A", "B", "C"))

## Core reproduction ----------------------------------------------------

test_that("woe_table reproduces the hand-verified worked example", {
  wt <- woe_table(x_f, y)
  
  expect_s3_class(wt, "data.frame")
  expect_equal(nrow(wt), 4L)
  expect_equal(wt$bin, c("A", "B", "C", "Missing"))
  expect_equal(wt$n,      c(34, 42, 20, 24))
  expect_equal(wt$n_bad,  c(4, 8, 7, 5))
  expect_equal(wt$n_good, c(30, 34, 13, 19))
  expect_equal(wt$dist_bad,  c(4/24, 8/24, 7/24, 5/24))
  expect_equal(wt$dist_good, c(30/96, 34/96, 13/96, 19/96))
  expect_equal(
    wt$woe,
    c(0.6286087, 0.0606246, -0.7672552, -0.0512933),
    tolerance = 1e-6
  )
  expect_equal(
    wt$iv_contribution,
    c(0.0916721, 0.0012630, 0.1198836, 0.0005343),
    tolerance = 1e-6
  )
  expect_equal(sum(wt$iv_contribution), 0.213353, tolerance = 1e-6)
})

test_that("sign convention: positive WOE marks the safer bin", {
  wt <- woe_table(x_f, y)
  expect_gt(wt$woe[wt$bin == "A"], 0)   # safest bin
  expect_lt(wt$woe[wt$bin == "C"], 0)   # riskiest bin
})

## Missing-bin behavior --------------------------------------------------

test_that("NA observations form an explicit Missing bin", {
  wt <- woe_table(x_f, y)
  expect_true("Missing" %in% wt$bin)
  expect_equal(wt$n[wt$bin == "Missing"], 24L)
})

test_that("no Missing row when x has no NA values", {
  x2 <- factor(c("A", "A", "A", "B", "B", "B"), levels = c("A", "B"))
  y2 <- c(1, 0, 0, 1, 1, 0)
  wt <- woe_table(x2, y2)
  expect_equal(wt$bin, c("A", "B"))
  expect_false("Missing" %in% wt$bin)
})

## Pre-registered failure modes -------------------------------------------

test_that("zero-event bins error with guidance to merge", {
  xz <- factor(c("A", "A", "B", "B", "B"), levels = c("A", "B"))
  yz <- c(0, 0, 1, 0, 0)              # bin A: zero bad
  expect_error(woe_table(xz, yz), "zero bad")
  
  xg <- factor(c("A", "A", "B", "B"), levels = c("A", "B"))
  yg <- c(1, 1, 1, 0)                 # bin B: zero good
  expect_error(woe_table(xg, yg), "zero good")
})

test_that("y must be binary 0/1 without NA", {
  xb <- factor(c("A", "A", "B", "B"), levels = c("A", "B"))
  expect_error(woe_table(xb, c(0, 1, 2, 0)), "0 and 1")
  expect_error(woe_table(xb, c(0, 1, NA, 0)), "NA")
})

test_that("x and y must have matching lengths", {
  xb <- factor(c("A", "A", "B", "B"), levels = c("A", "B"))
  expect_error(woe_table(xb, c(0, 1)), "length")
})

test_that("x must be categorical; numerics must be binned first", {
  expect_error(woe_table(c(1.2, 2.3, 1.2, 2.3), c(0, 1, 0, 1)),
               "factor|character")
})

## Robustness --------------------------------------------------------------

test_that("character input is accepted and matches factor input", {
  wt_f <- woe_table(x_f, y)
  wt_c <- woe_table(bin, y)
  expect_equal(wt_f, wt_c)
})

test_that("unused factor levels are dropped", {
  xu <- factor(c("A", "A", "B", "B"), levels = c("A", "B", "D"))
  yu <- c(1, 0, 1, 0)
  wt <- woe_table(xu, yu)
  expect_equal(wt$bin, c("A", "B"))
})