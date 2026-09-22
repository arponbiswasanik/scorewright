# dev/verify_cutoff.R
# Cutoff policy analysis: TWO policies and the swap between them.
#   Policy A (volume-max): loosest cutoff with approved bad rate <= 8%
#   Policy B (risk-control): median-score cutoff
# Swap: tightening A -> B, with the sacrifice ratio (goods declined
# per bad declined) -- the core policy trade-off number.
devtools::load_all()
source("dev/fit_scorecard.R")

ct <- cutoff_curve(score_pts, y)

## Policy A: volume-maximizing subject to <= 8% approved bad rate
target <- min(which(ct$bad_rate_approved <= 0.08))
c_vol  <- ct$cutoff[target]
ca_vol <- cutoff_analysis(score_pts, y, cutoff = c_vol)

## Policy B: risk-control at the median score
c_risk  <- median(score_pts)
ca_risk <- cutoff_analysis(score_pts, y, cutoff = c_risk)

cat("---- Policy A (volume-max, <=8% bad): cutoff", round(c_vol, 1),
    "----\n")
cat("approval", round(ca_vol$approval_rate, 3),
    "| approved bad rate", round(ca_vol$bad_rate_approved, 3),
    "| bads rejected", round(ca_vol$bad_rejected_share, 3), "\n")

cat("---- Policy B (risk-control, median): cutoff", round(c_risk, 1),
    "----\n")
cat("approval", round(ca_risk$approval_rate, 3),
    "| approved bad rate", round(ca_risk$bad_rate_approved, 3),
    "| bads rejected", round(ca_risk$bad_rejected_share, 3), "\n")

## Swap: tighten from A to B -- what the risk policy costs
sw <- cutoff_swap(score_pts, y, from = c_vol, to = c_risk)
sacrifice <- sw$n_out_good / max(sw$n_out_bad, 1)
cat("\n---- swap: tighten", round(c_vol, 1), "->", round(c_risk, 1),
    "----\n")
print(unlist(sw))
cat("sacrifice ratio (goods declined per bad declined):",
    round(sacrifice, 2), "\n")

stopifnot(
  ca_vol$bad_rate_approved <= 0.08,     # risk target met
  ca_vol$approval_rate > 0.90,          # volume-max property
  ca_risk$bad_rejected_share > 0.70,    # risk-control property
  sw$n_out_bad > 200,                   # tightening removes real bads
  sacrifice > 3                         # and costs real goods
)
cat("\nAll policy assertions passed.\n")