# tests/testthat/test-bin_numeric.R
#
# Ground truth: six hand-traced worked examples from the Step 11
# spec, independently calculator-verified before this file was
# written. Two author arithmetic slips were caught by the
# verification battery and corrected before entering this file
# (Example 4 Missing-bin contribution / total IV; Example 6 total
# IV via the full-precision summing rule); see DESIGN.md.
# Expected values below are LITERALS from independent
# verification, not expressions of the implementation's formulas.
#
# Coverage map:
#   Example 1: population floor triggers a zero-loss merge
#   Example 2: monotonicity enforcement via the two-run rule
#   Example 3: zero-event bin merges as priority
#   Example 4: Missing bin exempt from floors, counted in B and G
#   Example 5: label flip negates WOE, preserves IV, flips direction
#   Example 6: default decile breaks
#   plus: error paths and structural invariants

## Shared fixtures ------------------------------------------------------

x1 <- c(rep(5, 14), rep(15, 12), rep(25, 10), rep(35, 12))
y1 <- c(rep(1, 2), rep(0, 12), rep(1, 3), rep(0, 9),
        rep(1, 5), rep(0, 5), rep(1, 6), rep(0, 6))

x2 <- c(rep(5, 15), rep(15, 15), rep(25, 15))
y2 <- c(rep(1, 5), rep(0, 10), rep(1, 2), rep(0, 13),
        rep(1, 8), rep(0, 7))

x3 <- c(rep(5, 10), rep(15, 10), rep(25, 10), rep(35, 10))
y3 <- c(rep(0, 10), rep(1, 2), rep(0, 8), rep(1, 4), rep(0, 6),
        rep(1, 4), rep(0, 6))

x4 <- c(x1, rep(NA_real_, 6))
y4 <- c(y1, 1, 1, 1, 1, 0, 0)

x5 <- x1
y5 <- 1 - y1

x6 <- c(rep(5, 10), rep(15, 10))
y6 <- c(rep(1, 6), rep(0, 4), rep(1, 2), rep(0, 8))

## The six worked examples ------------------------------------------------

test_that("Example 1: population floor triggers zero-loss merge", {
  b <- bin_numeric(x1, y1, breaks = c(0, 10, 20, 30, 40),
                   min_bin_pop = 0.25, min_bad = 2, max_bins = 5)
  
  expect_s3_class(b, "data.frame")
  expect_equal(nrow(b), 3L)
  expect_equal(b$bin, c("[0,10)", "[10,20)", "[20,40]"))
  expect_equal(b$n, c(14, 12, 22))
  expect_equal(b$n_bad, c(2, 3, 11))
  expect_equal(b$n_good, c(12, 9, 11))
  expect_equal(b$dist_bad, c(0.125, 0.1875, 0.6875))
  expect_equal(b$dist_good, c(0.375, 0.28125, 0.34375))
  expect_equal(b$woe, c(1.0986123, 0.4054651, -0.6931472),
               tolerance = 1e-6)
  expect_equal(b$iv_contribution,
               c(0.2746531, 0.0380124, 0.2382693), tolerance = 1e-6)
  expect_equal(sum(b$iv_contribution), 0.5509348, tolerance = 1e-6)
  expect_equal(attr(b, "breaks"), c(0, 10, 20, 40))
  expect_equal(attr(b, "direction"), "decreasing")
})

test_that("Example 2: monotonicity enforcement via two-run rule", {
  b <- bin_numeric(x2, y2, breaks = c(0, 10, 20, 30),
                   min_bin_pop = 0.25, min_bad = 2)
  
  expect_equal(nrow(b), 2L)
  expect_equal(b$bin, c("[0,20)", "[20,30]"))
  expect_equal(b$n, c(30, 15))
  expect_equal(b$n_bad, c(7, 8))
  expect_equal(b$n_good, c(23, 7))
  expect_equal(b$dist_bad, c(7/15, 8/15))
  expect_equal(b$dist_good, c(23/30, 7/30))
  expect_equal(b$woe, c(0.4964369, -0.8266786), tolerance = 1e-6)
  expect_equal(b$iv_contribution, c(0.1489311, 0.2480036),
               tolerance = 1e-6)
  expect_equal(sum(b$iv_contribution), 0.3969346, tolerance = 1e-6)
  expect_equal(attr(b, "breaks"), c(0, 20, 30))
  expect_equal(attr(b, "direction"), "decreasing")
})

test_that("Example 3: zero-event bin merges as priority", {
  b <- bin_numeric(x3, y3, breaks = c(0, 10, 20, 30, 40),
                   min_bin_pop = 0.2, min_bad = 2)
  
  expect_equal(nrow(b), 3L)
  expect_equal(b$bin, c("[0,20)", "[20,30)", "[30,40]"))
  expect_equal(b$n, c(20, 10, 10))
  expect_equal(b$n_bad, c(2, 4, 4))
  expect_equal(b$n_good, c(18, 6, 6))
  expect_equal(b$dist_bad, c(0.2, 0.4, 0.4))
  expect_equal(b$dist_good, c(0.6, 0.2, 0.2))
  expect_equal(b$woe, c(1.0986123, -0.6931472, -0.6931472),
               tolerance = 1e-6)
  expect_equal(b$iv_contribution,
               c(0.4394449, 0.1386294, 0.1386294), tolerance = 1e-6)
  expect_equal(sum(b$iv_contribution), 0.7167038, tolerance = 1e-6)
  expect_equal(attr(b, "breaks"), c(0, 20, 30, 40))
  expect_equal(attr(b, "direction"), "decreasing")
})

test_that("Example 4: Missing bin exempt from floors, counted in B and G", {
  b <- bin_numeric(x4, y4, breaks = c(0, 10, 20, 30, 40),
                   min_bin_pop = 0.25, min_bad = 2, max_bins = 5)
  
  expect_equal(nrow(b), 4L)
  expect_equal(b$bin, c("[0,10)", "[10,20)", "[20,40]", "Missing"))
  expect_equal(b$n, c(14, 12, 22, 6))
  expect_equal(b$n_bad, c(2, 3, 11, 4))
  expect_equal(b$n_good, c(12, 9, 11, 2))
  expect_equal(b$dist_bad, c(0.10, 0.15, 0.55, 0.20))
  expect_equal(b$dist_good, c(12/34, 9/34, 11/34, 2/34))
  expect_equal(b$woe,
               c(1.2611312, 0.5679840, -0.5306283, -1.2237754),
               tolerance = 1e-6)
  expect_equal(b$iv_contribution,
               c(0.3189920, 0.0651511, 0.1201717, 0.1727683),
               tolerance = 1e-6)
  expect_equal(sum(b$iv_contribution), 0.6770831, tolerance = 1e-6)
  expect_equal(attr(b, "breaks"), c(0, 10, 20, 40))
  expect_equal(attr(b, "direction"), "decreasing")
})

test_that("Example 5: label flip negates WOE, preserves IV, flips direction", {
  b <- bin_numeric(x5, y5, breaks = c(0, 10, 20, 30, 40),
                   min_bin_pop = 0.25, min_bad = 2, max_bins = 5)
  
  expect_equal(b$bin, c("[0,10)", "[10,20)", "[20,40]"))
  expect_equal(b$n, c(14, 12, 22))
  expect_equal(b$woe, c(-1.0986123, -0.4054651, 0.6931472),
               tolerance = 1e-6)
  expect_equal(b$iv_contribution,
               c(0.2746531, 0.0380124, 0.2382693), tolerance = 1e-6)
  expect_equal(sum(b$iv_contribution), 0.5509348, tolerance = 1e-6)
  expect_equal(attr(b, "breaks"), c(0, 10, 20, 40))
  expect_equal(attr(b, "direction"), "increasing")
})

test_that("Example 6: default decile breaks orient and bin correctly", {
  # min_bad overridden because the production default (25) assumes
  # development-sample scale; this fixture holds 8 bads total.
  b <- bin_numeric(x6, y6, min_bad = 2)
  
  expect_equal(nrow(b), 2L)
  expect_equal(b$bin, c("[5,10)", "[10,15]"))
  expect_equal(b$n, c(10, 10))
  expect_equal(b$n_bad, c(6, 2))
  expect_equal(b$n_good, c(4, 8))
  expect_equal(b$woe, c(-0.8109302, 0.9808293), tolerance = 1e-6)
  expect_equal(b$iv_contribution, c(0.3378876, 0.4086789),
               tolerance = 1e-6)
  expect_equal(sum(b$iv_contribution), 0.7465664, tolerance = 1e-6)
  expect_equal(attr(b, "breaks"), c(5, 10, 15))
  expect_equal(attr(b, "direction"), "increasing")
})

## Error paths --------------------------------------------------------------

test_that("x must be numeric; factors point to woe_table", {
  expect_error(bin_numeric(factor(c("A", "B")), c(1, 0)), "woe_table")
  expect_error(bin_numeric(c("A", "B"), c(1, 0)), "numeric|woe_table")
})

test_that("y must be binary 0/1, no NA, both classes present", {
  x <- c(1, 2, 3, 4)
  expect_error(bin_numeric(x, c(0, 1, 2, 0), breaks = c(0, 2, 5)),
               "0 and 1")
  expect_error(bin_numeric(x, c(0, 1, NA, 0), breaks = c(0, 2, 5)),
               "NA")
  expect_error(bin_numeric(x, c(0, 0, 0, 0), breaks = c(0, 2, 5)),
               "classes")
})

test_that("x and y must have matching lengths", {
  expect_error(bin_numeric(c(1, 2, 3), c(0, 1), breaks = c(0, 2, 5)),
               "length")
})

test_that("breaks must be finite, strictly increasing, length >= 3", {
  x <- c(1, 2, 3, 4); y <- c(1, 0, 1, 0)
  expect_error(bin_numeric(x, y, breaks = c(10, 0, 5)), "breaks")
  expect_error(bin_numeric(x, y, breaks = c(0, 5)), "breaks")
  expect_error(bin_numeric(x, y, breaks = c(0, Inf, 5)), "breaks")
})

test_that("constraint parameters are validated", {
  x <- c(1, 2, 3, 4); y <- c(1, 0, 1, 0)
  expect_error(bin_numeric(x, y, breaks = c(0, 2, 5), min_bin_pop = 0),
               "min_bin_pop")
  expect_error(bin_numeric(x, y, breaks = c(0, 2, 5), min_bin_pop = 1),
               "min_bin_pop")
  expect_error(bin_numeric(x, y, breaks = c(0, 2, 5), min_bad = 0),
               "min_bad")
  expect_error(bin_numeric(x, y, breaks = c(0, 2, 5), max_bins = 1),
               "max_bins")
})

test_that("all-NA x errors", {
  expect_error(
    bin_numeric(rep(NA_real_, 4), c(1, 0, 1, 0), breaks = c(0, 2, 5)),
    "NA|missing"
  )
})

test_that("default breaks require at least three unique deciles", {
  expect_error(bin_numeric(rep(5, 20), rep(c(1, 0), 10)),
               "categorical|woe_table")
})

test_that("collapse to one bin errors with guidance", {
  x <- c(rep(5, 10), rep(15, 10))
  y <- c(rep(1, 5), rep(0, 5), rep(1, 5), rep(0, 5))
  expect_error(
    bin_numeric(x, y, breaks = c(0, 10, 20), min_bin_pop = 0.6,
                min_bad = 2),
    "collapsed"
  )
})

test_that("unsatisfiable constraints error with relaxation guidance", {
  x <- c(rep(5, 10), rep(15, 10))
  y <- c(rep(1, 4), rep(0, 6), rep(0, 10))
  expect_error(
    bin_numeric(x, y, breaks = c(0, 10, 20), min_bad = 10),
    "relax|satisf"
  )
})

## Structural invariants ------------------------------------------------------

test_that("out-of-range values under explicit breaks become Missing", {
  x <- c(rep(5, 10), rep(15, 10), rep(25, 10))
  y <- c(rep(1, 2), rep(0, 8), rep(1, 4), rep(0, 6),
         rep(1, 6), rep(0, 4))
  b <- bin_numeric(x, y, breaks = c(0, 10, 20), min_bad = 2)
  expect_true("Missing" %in% b$bin)
  expect_equal(b$n[b$bin == "Missing"], 10L)
  expect_equal(b$n_bad[b$bin == "Missing"], 6L)
})

test_that("returned WOE sequence is weakly monotone in stated direction", {
  b1 <- bin_numeric(x1, y1, breaks = c(0, 10, 20, 30, 40),
                    min_bin_pop = 0.25, min_bad = 2, max_bins = 5)
  w <- b1$woe[b1$bin != "Missing"]
  expect_true(all(diff(w) <= 0))
  
  b6 <- bin_numeric(x6, y6, min_bad = 2)
  w6 <- b6$woe[b6$bin != "Missing"]
  expect_true(all(diff(w6) >= 0))
})

test_that("final bins agree with woe_table on the final breaks", {
  b <- bin_numeric(x1, y1, breaks = c(0, 10, 20, 30, 40),
                   min_bin_pop = 0.25, min_bad = 2, max_bins = 5)
  f <- cut(x1, breaks = attr(b, "breaks"), include.lowest = TRUE,
           dig.lab = 10)
  wt <- woe_table(f, y1)
  expect_equal(wt$woe, b$woe[b$bin != "Missing"])
  expect_equal(wt$iv_contribution, b$iv_contribution[b$bin != "Missing"])
})