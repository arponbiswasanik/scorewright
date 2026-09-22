# tests/testthat/test-cutoff_analysis.R
# Worked example: 9 applicants, points score (higher = safer).
# scores: 540(b) 560(g) 570(g) 590(g) 610(b) 620(g) 650(g) 680(g) 700(g)
# At cutoff 600 (approve score >= 600): approved = {610,620,650,680,700}
# = 5 loans, 1 bad. Threshold policies are nested -- swaps are
# one-directional (tightening removes, loosening adds).
# bad_rejected_share: share of ALL bads declined by the cutoff.
# Renamed from bad_capture_above after the live-fire revealed the
# name asserted the opposite of the formula; the original symmetric
# example could not distinguish the two readings -- asymmetric
# assertions now pin the semantics.

score <- c(540, 560, 570, 590, 610, 620, 650, 680, 700)
y     <- c(  1,   0,   0,   0,   1,   0,   0,   0,   0)

test_that("cutoff statistics reproduce the worked example", {
  ca <- cutoff_analysis(score, y, cutoff = 600)
  expect_equal(ca$n, 9L)
  expect_equal(ca$n_approved, 5L)
  expect_equal(ca$approval_rate, 5/9, tolerance = 1e-12)
  expect_equal(ca$n_bad_approved, 1L)
  expect_equal(ca$bad_rate_approved, 1/5, tolerance = 1e-12)
  expect_equal(ca$overall_bad_rate, 2/9, tolerance = 1e-12)
  expect_equal(ca$good_approval_rate, 4/7, tolerance = 1e-12)
  expect_equal(ca$bad_approval_rate, 1/2, tolerance = 1e-12)
  expect_equal(ca$bad_rejected_share, 1/2, tolerance = 1e-12)
})

test_that("swap set: threshold swaps are one-directional", {
  # tightening 600 -> 660 removes {610(b), 620(g), 650(g)}, adds none
  sw <- cutoff_swap(score, y, from = 600, to = 660)
  expect_equal(sw$n_out, 3L)
  expect_equal(sw$n_out_good, 2L)
  expect_equal(sw$n_out_bad, 1L)
  expect_equal(sw$n_in, 0L)
  expect_equal(sw$n_in_good, 0L)
  expect_equal(sw$n_in_bad, 0L)
  expect_equal(sw$swap_events, 3L)
  
  # loosening 660 -> 600 adds the same three back, removes none
  sw2 <- cutoff_swap(score, y, from = 660, to = 600)
  expect_equal(sw2$n_in, 3L)
  expect_equal(sw2$n_in_good, 2L)
  expect_equal(sw2$n_in_bad, 1L)
  expect_equal(sw2$n_out, 0L)
  expect_equal(sw2$swap_events, 3L)
})

test_that("curve table covers all distinct cutoffs", {
  ct <- cutoff_curve(score, y)
  expect_equal(nrow(ct), 9L)           # one row per distinct score
  expect_equal(ct$cutoff[1], 540)      # ascending
  expect_equal(ct$cutoff[9], 700)
  expect_equal(ct$approval_rate, (9:1)/9, tolerance = 1e-12)
  # at the tightest cutoff (approve only >= 700): bad rate 0
  expect_equal(ct$bad_rate_approved[9], 0)
  expect_equal(ct$bad_rejected_share[9], 1.0, tolerance = 1e-12)
})

test_that("orientation: higher score = safer (approved = score >= cutoff)", {
  ca <- cutoff_analysis(score, y, cutoff = 680)
  expect_equal(ca$n_approved, 2L)      # 680, 700
  expect_equal(ca$n_bad_approved, 0L)
  expect_equal(ca$bad_rejected_share, 1.0, tolerance = 1e-12)  # both bads below
  ca2 <- cutoff_analysis(score, y, cutoff = 540)
  expect_equal(ca2$bad_rejected_share, 0, tolerance = 1e-12)   # all approved
})

test_that("ties at the cutoff are approved (>= convention)", {
  ca <- cutoff_analysis(score, y, cutoff = 610)
  expect_equal(ca$n_approved, 5L)      # 610, 620, 650, 680, 700
  expect_equal(ca$n_bad_approved, 1L)  # 610
})

test_that("input validation", {
  expect_error(cutoff_analysis(score, c(0, 1, 2, 0, 0, 0, 0, 0, 0)), "0 and 1")
  expect_error(cutoff_analysis(score, c(0, 1, NA, 0, 0, 0, 0, 0, 0)), "NA")
  expect_error(cutoff_analysis(score, c(0, 1)), "length")
  expect_error(cutoff_analysis(score, rep(0, 9)), "classes")
  expect_error(cutoff_analysis(score, y, cutoff = NA), "cutoff")
  expect_error(cutoff_swap(score, y, from = 600, to = NA), "from|to")
})