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

## -- 8. True-PD ground truth ----------------------------------------
##
## Logistic ground truth with fixed, documented coefficients.
## The linear predictor is built from CENTERED transforms so the
## intercept can be calibrated to the target base rate, mirroring
## portfolio-level calibration of production scorecards. The
## design is main-effects only, matching the additive model class
## of the scorecard estimator: the verification question is
## whether the pipeline recovers known structure, not whether
## logistic beats a nonlinear DGP.
##
## Signal design (IV spectrum):
##   strong : bureau_score, check_returns_12m, dscr, sector
##   medium : ltv (unsecured -> effective 1.0), tenor, U-shaped
##            business_age, loan_purpose, current_ratio
##   weak   : log(employees)
##   null   : annual_sales, rate_pct, branch -- zero DIRECT effect;
##            any apparent signal is confounding through dscr,
##            loan_amount, tenor and collateral, by design.

## Effective LTV: unsecured facilities carry the risk position of
## a maximum-LTV loan; secured loans use observed LTV.
ltv_eff <- ifelse(is.na(ltv), 1.0, ltv)

## U-shaped business age via hinge terms: risk falls as firms
## mature toward 12 years, is flat through mid-life, and rises
## mildly for elderly firms (succession risk, dated plant).
age_young <- pmax(0, 12 - business_age)
age_old   <- pmax(0, business_age - 25)

sector_fx <- c(Trading = 0.35, Manufacturing = 0.00,
               Services = -0.15, `Agro-processing` = 0.05,
               Construction = 0.25, `Transport & Logistics` = 0.10)[sector]

purpose_fx <- c("Working Capital"    =  0.15,
                "Machinery Purchase" = -0.20,
                "Business Expansion" =  0.05,
                "Trade Finance"      =  0.25)[loan_purpose]

lp <- -0.010 * (bureau_score - 645) +    # strong: 100 pts = -1.0 logit
  0.55 * check_returns_12m +        # strong: per dishonoured cheque
  -0.45 * (dscr - 2.0) +             # strong: debt coverage
  sector_fx +                       # strong: direct sector effect
  1.80 * (ltv_eff - 0.60) +         # medium: collateral position
  0.011 * (tenor_num - 36) +        # medium: exposure months
  0.055 * age_young +               # medium: U-shape, left arm
  0.022 * age_old +                 # medium: U-shape, right arm
  purpose_fx +                      # medium
  -0.18 * (current_ratio - 2.0) +    # medium: liquidity
  -0.06 * (log(employees) - log(12)) # weak: firm scale

## Intercept calibration: solve for the offset that sets the
## PORTFOLIO-MEAN true PD to the target base rate. Calibrating on
## the mean of the linear predictor is not equivalent: plogis() is
## convex over the low-probability region, so mean(plogis(a + lp))
## exceeds plogis(a + mean(lp)) -- with the lp dispersion here the
## gap is several percentage points. Root-finding on the portfolio
## mean calibrates exactly.
target_bad <- 0.10
intercept  <- uniroot(
  function(a) mean(plogis(a + lp)) - target_bad,
  c(-10, 10)
)$root
pd_true    <- plogis(intercept + lp)

## -- 9. Twelve-month outcomes -----------------------------------------
##
## Bad definition: ever 90+ DPD within the 12-month observation
## window (standard application-scorecard definition; Siddiqi,
## 2006). All cohorts matured before the monitoring period.

ever_90dpd_12m <- rbinom(n_loans, 1, pd_true)

## Month of first 90+ DPD for defaults; early months weighted --
## failure reveals itself in the first payment cycles.
def_month_pool <- sample(3:12, n_loans, replace = TRUE,
                         prob = c(0.06, 0.10, 0.12, 0.13, 0.13,
                                  0.12, 0.11, 0.09, 0.07, 0.07))
months_to_default <- as.integer(
  ifelse(ever_90dpd_12m == 1, def_month_pool, 12L))

## Competing risk: a fraction of non-defaulted loans settle early,
## more often for small revolving trade facilities.
p_early <- ifelse(loan_purpose == "Trade Finance" & loan_amount < 2e6,
                  0.12, 0.05)
early_settle <- ever_90dpd_12m == 0 & runif(n_loans) < p_early

status_12m <- factor(
  ifelse(ever_90dpd_12m == 1, "Defaulted",
         ifelse(early_settle, "Early-settled", "Performing")),
  levels = c("Performing", "Defaulted", "Early-settled"))

## Attach --------------------------------------------------------------

sme_dev_sample <- sme_dev_sample |>
  mutate(
    ever_90dpd_12m    = as.integer(ever_90dpd_12m),
    months_to_default = months_to_default,
    status_12m        = status_12m
  )

## Sanity checks: rate target, internal consistency, and DESIGN
## INVARIANTS. The generator refuses to emit data violating its
## own ground truth: bad rates must fall across bureau and DSCR
## quartiles and rise with dishonoured cheques.
br_bureau <- tapply(ever_90dpd_12m,
                    cut(bureau_score, quantile(bureau_score, 0:4/4),
                        include.lowest = TRUE), mean)
br_dscr <- tapply(ever_90dpd_12m,
                  cut(dscr, quantile(dscr, 0:4/4),
                      include.lowest = TRUE), mean)
br_ret <- tapply(ever_90dpd_12m,
                 factor(pmin(check_returns_12m, 2), levels = c(0, 1, 2)),
                 mean)
stopifnot(
  mean(ever_90dpd_12m) > 0.085,
  mean(ever_90dpd_12m) < 0.115,
  all(months_to_default %in% 3:12),
  all(months_to_default[ever_90dpd_12m == 0] == 12L),
  all((status_12m == "Defaulted") == (ever_90dpd_12m == 1)),
  all(ever_90dpd_12m[status_12m == "Early-settled"] == 0),
  all(diff(br_bureau) < 0),
  all(diff(br_dscr) < 0),
  all(diff(br_ret) > 0)
)

## -- 10. Informative missingness -------------------------------------
##
## Observation process layered over the unchanged ground truth:
##   (a) Bureau thin-file: young, small firms have no bureau
##       record -- bureau_score is NA for them.
##   (b) Unaudited statements: small-turnover applicants and
##       FDR-secured facilities (cash-collateralised; statements
##       not demanded) supply no financials -- dscr AND
##       current_ratio are missing together.
## Missingness depends on observable mechanisms, never on the
## masked value itself, and is NOT part of the true-PD function:
## the DGP from section 8 is untouched. Complete values are
## retained in-script as recovery ground truth for tests.

## (a) Bureau thin-file ----------------------------------------------

target_bureau_na <- 0.18
bureau_na_lp <- -0.16 * (business_age - 15) -
  0.50 * (log(employees) - log(12))
bureau_na_a <- uniroot(
  function(a) mean(plogis(a + bureau_na_lp)) - target_bureau_na,
  c(-10, 10)
)$root
bureau_na <- runif(n_loans) < plogis(bureau_na_a + bureau_na_lp)

## (b) Unaudited financial statements --------------------------------

target_fin_na <- 0.12
fin_na_lp <- -0.35 * (log(annual_sales) - log(3e7)) +
  1.00 * as.numeric(collateral_type == "FDR")
fin_na_a <- uniroot(
  function(a) mean(plogis(a + fin_na_lp)) - target_fin_na,
  c(-10, 10)
)$root
fin_na <- runif(n_loans) < plogis(fin_na_a + fin_na_lp)

## Preserve complete values as ground truth, then mask ---------------

bureau_score_full <- bureau_score
dscr_full        <- dscr
current_ratio_full <- current_ratio

bureau_score_obs <- bureau_score
bureau_score_obs[bureau_na] <- NA_integer_

dscr_obs <- dscr
current_ratio_obs <- current_ratio
dscr_obs[fin_na]        <- NA_real_
current_ratio_obs[fin_na] <- NA_real_

sme_dev_sample <- sme_dev_sample |>
  mutate(
    bureau_score  = bureau_score_obs,
    dscr          = dscr_obs,
    current_ratio = current_ratio_obs
  )

## Sanity checks: rates, joint structure, and informativeness ------
## Thin-file must be riskier THROUGH CORRELATES (young age, lower
## latent bureau), even though missingness itself carries no
## direct effect in the DGP.
stopifnot(
  mean(is.na(sme_dev_sample$bureau_score)) > 0.15,
  mean(is.na(sme_dev_sample$bureau_score)) < 0.21,
  mean(is.na(sme_dev_sample$dscr)) > 0.10,
  mean(is.na(sme_dev_sample$dscr)) < 0.14,
  identical(which(is.na(sme_dev_sample$dscr)),
            which(is.na(sme_dev_sample$current_ratio))),
  all(is.na(sme_dev_sample$ltv) == (collateral_type == "Unsecured")),
  mean(pd_true[bureau_na]) - mean(pd_true[!bureau_na]) > 0.012,
  mean(ever_90dpd_12m[bureau_na]) > mean(ever_90dpd_12m[!bureau_na]),
  mean(business_age[bureau_na]) < mean(business_age[!bureau_na]) - 3,
  mean(employees[bureau_na]) < mean(employees[!bureau_na]),
  mean(log(annual_sales)[fin_na]) < mean(log(annual_sales)[!fin_na]) - 0.2,
  mean(fin_na[collateral_type == "FDR"]) >
    mean(fin_na[collateral_type != "FDR"])
)