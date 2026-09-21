# dev/verify_iv_ranking.R
# ---------------------------------------------------------------
# First live-fire verification of the woe_table() engine.
#
# Assembles the exploratory IV ranking across all 15 application
# characteristics of the development sample and compares it with
# the ground-truth signal spectrum encoded in the DGP
# (data-raw/sme_datagen.R, section 8).
#
# Provisional binning -- EXPLORATORY, not production coarse
# classing: categorical characteristics enter directly; numeric
# characteristics get quintile bins; check_returns_12m gets
# domain-guided classing 0 / 1 / 2+ (the raw 3 and 4 bins hold
# 39 and 3 loans -- too sparse to estimate; the 3-loan bin may
# trip the zero-event error, by design). NA observations become
# Missing bins automatically. The production binning module
# (monotonic, business-reviewed) replaces these provisional
# bins later.
# ---------------------------------------------------------------

devtools::load_all()

dat <- sme_dev_sample
y   <- dat$ever_90dpd_12m

iv_of <- function(x) sum(woe_table(x, y)$iv_contribution)

## Designed DGP tier per characteristic, for side-by-side
## comparison with the measured IV. collateral_type carries no
## direct effect in the DGP; its measured IV arrives entirely
## through the collateral-to-LTV-band mapping (each collateral
## class maps to a hardwired LTV range), making it a coarsened
## LTV proxy in this data.
dgp_tier <- c(
  bureau_score      = "strong",
  check_returns_12m = "strong",
  dscr              = "strong",
  sector            = "strong",
  ltv               = "medium",
  tenor_months      = "medium",
  business_age      = "medium (U-shaped)",
  loan_purpose      = "medium",
  current_ratio     = "medium",
  employees         = "weak",
  annual_sales      = "null",
  rate_pct          = "null",
  branch            = "null",
  rm_tenure         = "null",
  loan_amount       = "null",
  collateral_type   = "indirect (via ltv)"
)

cat_chars <- c("sector", "loan_purpose", "collateral_type",
               "tenor_months", "branch")
num_chars <- c("bureau_score", "check_returns_12m", "dscr", "ltv",
               "business_age", "current_ratio", "rate_pct",
               "loan_amount", "annual_sales", "employees",
               "rm_tenure")

rows <- list()

for (ch in cat_chars) {
  rows[[ch]] <- data.frame(characteristic = ch, iv = iv_of(dat[[ch]]))
}

for (ch in num_chars) {
  x <- dat[[ch]]
  if (ch == "check_returns_12m") {
    x <- factor(pmin(x, 2), levels = c(0, 1, 2))
  } else {
    br <- unique(quantile(x, probs = seq(0, 1, 0.2), na.rm = TRUE))
    x <- cut(x, breaks = br, include.lowest = TRUE, dig.lab = 8)
  }
  rows[[ch]] <- data.frame(characteristic = ch, iv = iv_of(x))
}

iv_ranking <- do.call(rbind, rows)
iv_ranking$dgp_tier <- dgp_tier[iv_ranking$characteristic]
iv_ranking <- iv_ranking[order(-iv_ranking$iv), ]
iv_ranking$iv <- round(iv_ranking$iv, 4)

print(iv_ranking, row.names = FALSE)