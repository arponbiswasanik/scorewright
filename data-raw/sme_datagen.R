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

## -- 3. Firm characteristics --------------------------------------
## Generation order encodes the dependency structure:
##   sector -> business_age -> employees -> annual_sales
##           -> check_returns_12m -> bureau_score
##   business_age -> rm_tenure (capped at both firm age and 15y,
##                   the lender's own operating history)
##   sector -> loan_purpose
## Missingness is NOT applied here; it is carved out in a later
## step with documented mechanisms, keeping full ground truth
## available for verification.

sector <- sample(
  c("Trading", "Manufacturing", "Services", "Agro-processing",
    "Construction", "Transport & Logistics"),
  n_loans, replace = TRUE,
  prob = c(0.35, 0.15, 0.20, 0.10, 0.10, 0.10)
) |>
  factor(levels = c("Trading", "Manufacturing", "Services",
                    "Agro-processing", "Construction",
                    "Transport & Logistics"))
# Trading dominates, mirroring the Bangladeshi SME application mix.

## business_age: right-skewed (gamma), most firms 3-25 years.
business_age <- round(rgamma(n_loans, shape = 2.2, rate = 0.15), 1)
business_age <- pmax(1, pmin(40, business_age))

## employees: sector-scaled lognormal. Manufacturing runs larger
## crews; trading and transport firms run small teams.
emp_mult <- c(Trading = 0.7, Manufacturing = 2.2, Services = 0.8,
              `Agro-processing` = 1.1, Construction = 1.3,
              `Transport & Logistics` = 0.6)
employees <- round(exp(rnorm(n_loans, log(12) + log(emp_mult[sector]), 0.65)))
employees <- as.integer(pmax(2, pmin(200, employees)))

## annual_sales: turnover per employee varies by sector; lognormal
## noise on top. Sales therefore correlate with headcount by
## construction (deliberate: realistic multicollinearity).
spe <- c(Trading = 4.0e6, Manufacturing = 2.5e6, Services = 1.5e6,
         `Agro-processing` = 1.8e6, Construction = 3.0e6,
         `Transport & Logistics` = 2.0e6)
annual_sales <- exp(rnorm(n_loans, log(employees * spe[sector]), 0.55))
annual_sales <- pmax(1e6, pmin(2e8, annual_sales))
annual_sales <- round(annual_sales / 1e4) * 1e4  # declared in round BDT 10k

## check_returns_12m: sparse Poisson counts; cheque-intensive
## trading business defaults to more dishonoured cheques.
ret_lambda <- c(Trading = 0.55, Manufacturing = 0.30, Services = 0.25,
                `Agro-processing` = 0.30, Construction = 0.40,
                `Transport & Logistics` = 0.35)
check_returns_12m <- rpois(n_loans, ret_lambda[sector])

## bureau_score: conditional on firm age (+) and dishonoured
## cheques (-). These correlations mean bureau score carries
## information about other observables, as in real bureau data.
bureau_score <- 645 +
  1.2 * (business_age - 15) -
  28 * check_returns_12m +
  rnorm(n_loans, 0, 70)
bureau_score <- as.integer(pmax(300, pmin(900, round(bureau_score))))

## rm_tenure: fraction of firm life banked with this lender,
## capped at 15 years (lender's operating history).
rm_tenure <- round(pmin(business_age, 15) * runif(n_loans, 0.1, 0.85), 1)

## loan_purpose: conditional on sector through a probability table.
purpose_levels <- c("Working Capital", "Machinery Purchase",
                    "Business Expansion", "Trade Finance")
purpose_prob <- matrix(
  c(0.45, 0.10, 0.20, 0.25,   # Trading
    0.35, 0.40, 0.20, 0.05,   # Manufacturing
    0.30, 0.20, 0.35, 0.15,   # Services
    0.40, 0.30, 0.25, 0.05,   # Agro-processing
    0.30, 0.35, 0.25, 0.10,   # Construction
    0.30, 0.45, 0.20, 0.05),  # Transport & Logistics
  nrow = 6, byrow = TRUE,
  dimnames = list(levels(sector), purpose_levels)
)
loan_purpose <- rep(NA_character_, n_loans)
for (s in levels(sector)) {
  idx <- which(sector == s)
  loan_purpose[idx] <- sample(purpose_levels, length(idx),
                              replace = TRUE, prob = purpose_prob[s, ])
}
loan_purpose <- factor(loan_purpose, levels = purpose_levels)

## Attach to the development sample -------------------------------

sme_dev_sample <- sme_dev_sample |>
  mutate(
    sector           = sector,
    business_age     = business_age,
    employees        = employees,
    annual_sales     = annual_sales,
    check_returns_12m = check_returns_12m,
    bureau_score     = bureau_score,
    rm_tenure        = rm_tenure,
    loan_purpose     = loan_purpose
  )

## Sanity checks (self-checking generation) ------------------------

stopifnot(
  !anyNA(sme_dev_sample),
  all(business_age >= 1 & business_age <= 40),
  all(employees >= 2 & employees <= 200),
  all(annual_sales >= 1e6 & annual_sales <= 2e8),
  all(check_returns_12m >= 0),
  all(bureau_score >= 300 & bureau_score <= 900),
  all(rm_tenure >= 0),
  all(rm_tenure <= business_age),   # cannot bank longer than the firm exists
  all(rowSums(purpose_prob) == 1)   # probability table well-formed
)




