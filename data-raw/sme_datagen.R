# data-raw/sme_datagen.R
# ---------------------------------------------------------------
# Synthetic SME lending data generator for scorewright
#
# Purpose
#   Produces two reproducible datasets with known ground truth:
#   - sme_dev_sample: accepted SME loans with fully observed
#     12-month outcomes (development sample)
#   - sme_scoring_snaps: post-deployment monthly application
#     snapshots with injected population drift (monitoring)
#
# Design
#   True PD per loan follows a logistic ground truth with fixed,
#   documented coefficients. Downstream verification is therefore
#   possible: IV screening must rank characteristics consistently
#   with the injected signal strengths, and PSI/CSI must detect
#   the drift injected from snapshot 4 onward.
#
# Reproducibility
#   Fixed seed; regenerating produces identical datasets.
# ---------------------------------------------------------------

library(dplyr)

set.seed(20260119)  # fixed seed: regeneration is byte-identical

## -- 1. Volume and timeline -------------------------------------

n_loans   <- 5000
n_cohorts <- 24

# Dev sample cohorts: Jan 2023 - Dec 2024. All 12-month outcome
# windows close by Dec 2025, i.e. fully mature before the
# monitoring period (2026 snapshots) begins.
cohort_months <- seq(
  from = as.Date("2023-01-01"),
  by   = "month",
  length.out = n_cohorts
)

## -- 2. Loan identifiers and origination cohorts ------------------

# Cohort volumes are not uniform: a growing lender disburses more
# each month. Weights rise linearly (~60% growth over two years),
# creating the cohort-size imbalance that real vintage analysis
# must handle.
sme_dev_sample <- tibble(
  loan_id   = sprintf("SW-%06d", seq_len(n_loans)),
  app_month = sample(cohort_months, n_loans, replace = TRUE,
                     prob = seq(1, 1.6, length.out = n_cohorts))
) |>
  arrange(app_month, loan_id)

## -- Sanity checks (self-checking generation) ----------------------

stopifnot(
  nrow(sme_dev_sample) == n_loans,
  !anyDuplicated(sme_dev_sample$loan_id),
  length(unique(sme_dev_sample$app_month)) == n_cohorts
)