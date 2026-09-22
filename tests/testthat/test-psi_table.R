# tests/testthat/test-psi_table.R
# Worked examples independently calculator-verified.
# PSI = sum((p_exp - p_act) * ln(p_exp / p_act)); thresholds
# 0.10 / 0.25 (industry convention, documented in roxygen).
# Tolerance note: expected values are compared at 1e-6 because
# the implementation sums per-bin contributions in bin order,
# while the calculator battery used closed-form expressions --
# floating-point summation order differs at the ~1e-8 level.

test_that("numeric example with explicit breaks", {
  exp <- rep(1:5, each = 5)
  act <- c(rep(1, 2), rep(2, 3), rep(3, 5), rep(4, 7), rep(5, 3))
  p <- psi_table(exp, act, breaks = c(1.5, 2.5, 3.5, 4.5))
  expect_equal(p$n_exp, c(5, 5, 5, 5, 5))
  expect_equal(p$n_act, c(2, 3, 5, 7, 3))
  expect_equal(p$p_exp, rep(0.2, 5))
  expect_equal(p$p_act, c(0.10, 0.15, 0.25, 0.35, 0.15))
  expect_equal(p$psi_contribution,
               c(0.06931472, 0.01438410, 0.01115718,
                 0.08394237, 0.01438410), tolerance = 1e-6)
  expect_equal(attr(p, "psi"), 0.19318247, tolerance = 1e-6)
  expect_equal(attr(p, "status"), "moderate")
})

test_that("identical distributions give exactly zero", {
  p <- psi_table(1:20, 1:20)
  expect_equal(attr(p, "psi"), 0)
  expect_equal(attr(p, "status"), "stable")
})

test_that("categorical path", {
  e <- factor(c("A", "A", "A", "B", "B"), levels = c("A", "B"))
  a <- factor(c("A", "A", "B", "B", "B"), levels = c("A", "B"))
  p <- psi_table(e, a)
  expect_equal(p$p_exp, c(0.6, 0.4))
  expect_equal(p$p_act, c(0.4, 0.6))
  # PSI = 0.6->0.4 and 0.4->0.6 swap: both bins contribute
  # 0.08109302; total is 0.16218604 (moderate, just above 0.10)
  expect_equal(attr(p, "psi"), 0.16218604, tolerance = 1e-6)
  expect_equal(attr(p, "status"), "moderate")
})

test_that("NA observations form a Missing bin, ordered bins first", {
  e <- c(rep(1, 3), rep(2, 3), NA, NA)
  a <- c(rep(1, 2), rep(2, 2), rep(NA_real_, 4))
  p <- psi_table(e, a, breaks = c(1.5))
  expect_equal(p$bin[3], "Missing")
  expect_equal(p$p_exp, c(0.375, 0.375, 0.25))
  expect_equal(p$p_act, c(0.25, 0.25, 0.50))
  # tolerance 1e-6: sum order differs from the battery's closed-form
  # expression (2*0.125*log(1.5) + 0.25*log(2)); both are 0.2746531
  expect_equal(attr(p, "psi"), 0.2746531, tolerance = 1e-6)
  expect_equal(attr(p, "status"), "significant")
})

test_that("outer bins are open: out-of-range actual is absorbed, not Missing", {
  p <- psi_table(1:10, c(-5, 100), breaks = c(5.5))
  expect_false("Missing" %in% p$bin)
  expect_equal(p$n_act, c(1, 1))
  expect_equal(attr(p, "psi"), 0)
})

test_that("zero-proportion bins error with guidance", {
  exp <- rep(1:5, each = 5)
  act <- c(rep(1, 10), rep(5, 10))
  expect_error(psi_table(exp, act, breaks = c(1.5, 2.5, 3.5, 4.5)), "zero")
})

test_that("unseen factor levels in actual error", {
  e <- factor(c("A", "A", "B", "B"))
  a <- factor(c("A", "C"), levels = c("A", "C"))
  expect_error(psi_table(e, a), "unseen")
})

test_that("input validation", {
  expect_error(psi_table(numeric(0), 1:5), "empty")
  expect_error(psi_table(1:5, factor(letters[1:5])), "both")
  expect_error(psi_table(1:10, 1:10, breaks = c(3, 1)), "breaks")
  expect_error(psi_table(1:10, 1:10, breaks = c(NA, 2)), "breaks")
  expect_error(psi_table(1:10, 1:10, n_bins = 1), "n_bins")
})