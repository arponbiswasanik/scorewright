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


## -- 4. Branch -----------------------------------------------------

branch <- sample(
  c("Dhaka", "Chattogram", "Khulna", "Sylhet", "Rajshahi",
    "Barishal", "Rangpur", "Bogura"),
  n_loans, replace = TRUE,
  prob = c(0.30, 0.16, 0.10, 0.09, 0.09, 0.08, 0.09, 0.09)
) |>
  factor(levels = c("Dhaka", "Chattogram", "Khulna", "Sylhet",
                    "Rajshahi", "Barishal", "Rangpur", "Bogura"))
# Designed null: independent of risk and of every other field.
# IV screening must rank branch near zero -- a verification anchor.
# Dhaka dominates, mirroring a real branch network's concentration.

## -- 5. Loan sizing, tenor, collateral ------------------------------

# Loan sizing: facilities are sized as a fraction of turnover;
# capex purposes command larger fractions than working capital.
amt_frac <- c("Working Capital"    = 0.11,
              "Machinery Purchase" = 0.22,
              "Business Expansion" = 0.18,
              "Trade Finance"      = 0.10)[loan_purpose]

loan_amount <- annual_sales * amt_frac *
  exp(rnorm(n_loans, 0, 0.45))          # officer discretion
loan_amount <- pmax(5e5, pmin(2e7, loan_amount))
loan_amount <- round(loan_amount / 5e4) * 5e4    # BDT 50k tickets

# Tenor follows purpose: trade finance revolves at 12 months;
# machinery capex amortises over 36-60.
tenor_levels <- c(12, 24, 36, 48, 60)
tenor_prob <- matrix(
  c(0.70, 0.25, 0.05, 0.00, 0.00,    # Working Capital
    0.05, 0.15, 0.40, 0.30, 0.10,    # Machinery Purchase
    0.20, 0.35, 0.30, 0.10, 0.05,    # Business Expansion
    0.85, 0.15, 0.00, 0.00, 0.00),   # Trade Finance
  nrow = 4, byrow = TRUE,
  dimnames = list(levels(loan_purpose), tenor_levels)
)
tenor_num <- rep(NA_integer_, n_loans)
for (p in levels(loan_purpose)) {
  idx <- which(loan_purpose == p)
  tenor_num[idx] <- sample(tenor_levels, length(idx),
                           replace = TRUE, prob = tenor_prob[p, ])
}

# Collateral follows purpose; big tickets upgrade to property.
collat_levels <- c("Residential Property", "Commercial Property",
                   "Machinery", "Inventory", "FDR")
collat_purpose <- matrix(
  c(0.20, 0.10, 0.15, 0.35, 0.20,    # Working Capital
    0.15, 0.10, 0.55, 0.10, 0.10,    # Machinery Purchase
    0.35, 0.25, 0.15, 0.10, 0.15,    # Business Expansion
    0.10, 0.05, 0.05, 0.50, 0.30),   # Trade Finance
  nrow = 4, byrow = TRUE,
  dimnames = list(levels(loan_purpose), collat_levels)
)
collateral_type <- rep(NA_character_, n_loans)
for (p in levels(loan_purpose)) {
  idx <- which(loan_purpose == p)
  collateral_type[idx] <- sample(collat_levels, length(idx),
                                 replace = TRUE, prob = collat_purpose[p, ])
}
big <- which(loan_amount > 1e7 & collateral_type %in% c("FDR", "Inventory"))
collateral_type[big] <- sample(c("Commercial Property", "Residential Property"),
                               length(big), replace = TRUE, prob = c(0.6, 0.4))

# Small clean facilities for long-standing clients: unsecured,
# producing STRUCTURAL missingness in ltv (undefined without
# collateral). Distinct from the informative missingness applied
# later to bureau score and financial ratios.
clean_cand <- which(
  loan_purpose %in% c("Working Capital", "Trade Finance") &
    loan_amount <= 3e6 & rm_tenure > 4
)
keep_clean <- sample(c(TRUE, FALSE), length(clean_cand),
                     replace = TRUE, prob = c(0.45, 0.55))
clean_idx <- clean_cand[keep_clean]
collateral_type[clean_idx] <- "Unsecured"
collateral_type <- factor(collateral_type,
                          levels = c(collat_levels, "Unsecured"))

## -- 6. LTV and pricing ---------------------------------------------

# LTV: within the advance-rate policy band per collateral class.
ltv_range <- cbind(lo = c(`Residential Property` = 0.40,
                          `Commercial Property`  = 0.50,
                          `Machinery`            = 0.50,
                          `Inventory`            = 0.40,
                          `FDR`                  = 0.70),
                   hi = c(`Residential Property` = 0.70,
                          `Commercial Property`  = 0.75,
                          `Machinery`            = 0.80,
                          `Inventory`            = 0.70,
                          `FDR`                  = 0.90))
ltv <- rep(NA_real_, n_loans)
sec_idx <- which(collateral_type != "Unsecured")
u <- runif(length(sec_idx))
ltv[sec_idx] <- ltv_range[collateral_type[sec_idx], "lo"] +
  u * (ltv_range[collateral_type[sec_idx], "hi"] -
         ltv_range[collateral_type[sec_idx], "lo"])
ltv <- round(ltv, 3)

# Pricing: tenor premium, collateral discount, ticket-size loading.
# Deliberately NOT a function of bureau score: pricing must remain
# a weak standalone predictor in the ground truth.
rate_tenor <- c("12" = 0, "24" = 0.6, "36" = 1.1,
                "48" = 1.5, "60" = 1.8)[as.character(tenor_num)]
rate_collat <- c(`Residential Property` = -1.2,
                 `Commercial Property`  = -0.8,
                 `Machinery`            = -0.3,
                 `Inventory`            =  0.3,
                 `FDR`                  = -2.0,
                 `Unsecured`            =  1.8)[collateral_type]
rate_size <- ifelse(loan_amount < 1e6, 0.8,
                    ifelse(loan_amount > 1e7, -0.5, 0))
rate_pct <- round(12 + rate_tenor + rate_collat + rate_size +
                    rnorm(n_loans, 0, 0.8), 1)
rate_pct <- pmax(9, pmin(18, rate_pct))

## -- 7. Financial ratios ---------------------------------------------

# DSCR generated mechanically from the deal, as a credit officer
# computes it: (sector margin x turnover) / annual debt service.
sector_margin <- c(Trading = 0.13, Manufacturing = 0.20, Services = 0.25,
                   `Agro-processing` = 0.17, Construction = 0.15,
                   `Transport & Logistics` = 0.22)[sector]
annual_debt_service <- loan_amount / tenor_num * 12
dscr <- (annual_sales * sector_margin) / annual_debt_service
dscr <- round(dscr * exp(rnorm(n_loans, 0, 0.35)), 2)
dscr <- pmax(0.2, pmin(6, dscr))

# Acceptance screening (compensating factors): the lender requires
# declared coverage >= 1.0 UNLESS collateral is liquid (FDR or
# property). Weak-collateral deals computing below the floor were
# never disbursed and are absent from the development sample: for
# those collateral classes the observed DSCR distribution is the
# acceptance-conditional (truncated) one. Weak coverage survives
# only where collateral compensates. This deliberately encodes the
# accepted-applications selection that reject inference exists to
# address; see DESIGN.md.
strong_collat <- collateral_type %in%
  c("FDR", "Residential Property", "Commercial Property")
repeat {
  redo <- which(!strong_collat & dscr < 1.0)
  if (length(redo) == 0) break
  dscr[redo] <- round((annual_sales[redo] * sector_margin[redo]) /
                        annual_debt_service[redo] *
                        exp(rnorm(length(redo), 0, 0.35)), 2)
  dscr[redo] <- pmin(6, dscr[redo])
}
stopifnot(
  all(dscr[!strong_collat] >= 1.0),   # floor holds outside liquid collateral
  all(dscr > 0 & dscr <= 6)
)

# Current ratio: sector-conditional liquidity; trading runs leaner
# inventory cycles, services hold more cash.
cr_center <- c(Trading = 1.6, Manufacturing = 1.9, Services = 2.3,
               `Agro-processing` = 1.8, Construction = 1.7,
               `Transport & Logistics` = 2.0)[sector]
current_ratio <- round(exp(rnorm(n_loans, log(cr_center), 0.30)), 2)
current_ratio <- pmax(0.4, pmin(6, current_ratio))

## Attach to the development sample --------------------------------

sme_dev_sample <- sme_dev_sample |>
  mutate(
    branch         = branch,
    loan_amount    = loan_amount,
    tenor_months   = factor(tenor_num, levels = tenor_levels),
    collateral_type = collateral_type,
    ltv            = ltv,
    rate_pct       = rate_pct,
    dscr           = dscr,
    current_ratio  = current_ratio
  )

## Sanity checks (self-checking generation) ------------------------

stopifnot(
  all(is.na(ltv) == (collateral_type == "Unsecured")),  # structural NA only
  all(loan_amount >= 5e5 & loan_amount <= 2e7),
  all(ltv[!is.na(ltv)] >= 0.35 & ltv[!is.na(ltv)] <= 0.93),
  all(rate_pct >= 9 & rate_pct <= 18),
  all(dscr > 0 & dscr <= 6),
  all(current_ratio > 0 & current_ratio <= 6),
  all(rowSums(tenor_prob) == 1),
  all(rowSums(collat_purpose) == 1)
)

