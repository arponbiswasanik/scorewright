# dev/verify_binning.R
# Live-fire: bin_numeric on the development sample.
devtools::load_all()
dat <- sme_dev_sample
y <- dat$ever_90dpd_12m

for (ch in c("bureau_score", "dscr", "business_age", "ltv",
             "current_ratio", "annual_sales", "branch_rank")) {
  x <- dat[[ch]]
  if (ch == "branch_rank") x <- as.numeric(dat$branch)  # null control
  b <- tryCatch(
    bin_numeric(x, y),
    error = function(e) { cat(ch, ":", conditionMessage(e), "\n\n"); NULL }
  )
  if (!is.null(b)) {
    cat("----", ch, "| IV =", round(sum(b$iv_contribution), 4),
        "| direction:", attr(b, "direction"), "\n")
    print(b[, c("bin", "n", "n_bad", "n_good", "woe")], row.names = FALSE)
    cat("\n")
  }
}