# Scorewright Design Decisions

This document records the substantive engineering and methodology decisions made during development, including errors caught by the verification protocol and corrected. It exists because the process of a project's construction is itself evidence of its quality.

---

## Verification Protocol

Every methodology function was built test-first: a hand-computed worked example was independently verified with R-as-calculator before any test literal was written, the test was committed red, and only then was the implementation written to satisfy it. Expected values enter test files only from the independent calculator route, never from the author's working notes.

---

## The Postmortem Ledger

Errors caught by the protocol during development, recorded with their lessons:

1. **Intercept calibration via the mean logit (data generator).** Calibrating the DGP intercept on `mean(lp)` put `mean(plogis(...))` at ~14-15%, not the 10% target: plogis is convex over the low-probability region, so the mean of the curve exceeds the curve at the mean. Caught by a generator invariant assertion (mean bad rate outside 8.5-11.5%). Fix: root-finding on the portfolio-mean PD (`uniroot`). Lesson: calibrate on the population mean, not the mean of the linear predictor.

2. **Spearman-rho orientation rule for monotonic binning (`bin_numeric` spec).** A hand-traced zigzag example (bad rates 33/13/53%) drove a negative rank correlation while the sensible orientation was clearly decreasing; the rho rule would have forced the wrong orientation and collapsed toward IV = 0. Caught on paper before any code existed. Fix: the two-run rule (merge under both orientations, keep the higher IV).

3. **Rounding-transcription slip (`bin_numeric` spec, Example 4).** A 7th-decimal rounding error (0.1727684 vs 0.1727683) survived author self-review and was caught only by the calculator battery. Lesson: self-review caught two other slips that day; independent verification caught the one that mattered.

4. **Summing rounded literals (same battery).** Summing 7-digit literals produced both a false alarm (Examples 1-3, off by 1e-7) and a false pass (Example 4, agreeing with the wrong total). Rule: adjudicate totals only with full-precision expressions.

5. **Categorical PSI literal (`psi_table` test).** The expected value 0.08109302 was bin A's contribution only; the two-bin total is 0.16218604. The error was masked until an earlier crash (a `table(levels=)` API misuse) was fixed. Lesson: one bug can hide another; verify after every fix.

6. **Miscounted approvals and inverted swap narrative (`cutoff_analysis` test).** At cutoff 600 the worked example approves 5 loans, not 6; and 600 → 560 is a loosening, not a tightening — mixed-direction swap expectations were mathematically impossible under nested thresholds.

7. **`bad_capture_above` naming (`cutoff_analysis`).** The formula computed the share of bads REJECTED, but the name asserted the opposite. The original worked example was symmetric (1 bad above, 1 below) and could not distinguish the readings; the live-fire caught it. Fix: renamed `bad_rejected_share`; asymmetric assertions now pin the semantics. Lesson: symmetric examples cannot pin directional semantics.

8. **`check_returns` classing dropped in transcription (`scorewright_model`).** The Shiny model function omitted the 0/1/2+ classing branch carried by the verification script; raw counts of 3-4 scored to NA, and the zero-proportion PSI guard refused to launch. The guard did its job; the transcription was the bug.

9. **Points-table columns assumed (`mod_scorecard`).** The module selected `n/n_bad/n_good/iv` columns that `scale_scorecard()` never returns; fixed by joining from the `woe_tables`.

---

## Key Methodology Decisions

**Why WOE binning rather than one-hot encoding**
WOE makes each characteristic's relationship with the outcome directly auditable, produces monotone, business-reviewable coarse classes, handles missing values as an informative bin, and aligns with the logistic model form. One-hot encoding on raw values overfits small structured datasets and defies policy review.

**Why logistic regression rather than gradient boosting**
Regulatory explainability (every point traceable to a bin), stability on ~500 events (Lessmann et al. 2015 find classical methods competitive or superior at this scale), and the scorecard form factor itself. The oracle ceiling analysis (98-95% of achievable AUC) confirms little discrimination is left on the table.

**Missing values as their own bin**
The DGP encodes three informativeness levels: structural (`ltv` NA for unsecured, riskiest by construction), informative (bureau NA for thin-file, +2.9pp bad rate), and mechanism-rich but risk-flat (financials NA for unaudited). Missing-bin WOEs recovered all three levels, including the flat one — the methodology quantifies informativeness per characteristic rather than assuming it uniformly.

**PSI thresholds as materiality, not significance**
The monitoring data contains a statistically significant but monitoring-irrelevant shift (employees, ~3 sigma at pooled n, d = 0.12 SD, CSI ~ 0.007): at volume, significance and materiality decouple, which is precisely why PSI uses fixed materiality thresholds rather than hypothesis tests.

---

## Headline Findings

1. **Score-level masking.** Max score PSI 0.046 (stable) across all six monitoring months while the population shifted hard: drift channels offset inside the score. Characteristic-level CSI caught what score-level PSI masked. This is the argument for characteristic monitoring alongside score monitoring.

2. **Derived-channel amplification.** `rm_tenure`, registered as a mild drift channel, produced the loudest CSI (0.50+) purely through its generative dependence on `business_age`. The monitor found a propagation channel the drift designers under-weighted.

3. **Tenor at the detectability boundary.** A designed 30% one-level tenor stretch fired 1 of 3 drifted months at n = 400/month: the effect sits exactly at monthly detection sensitivity.

4. **The policy trade-off.** Tightening from the volume-max cutoff (456.6, 93.4% approval, 8.0% approved bad rate) to the median cutoff (558.2, 50.1% approval, 3.1% approved bad rate) declines 1,875 good applicants to remove 295 bad ones: a sacrifice ratio of 6.36 goods per bad.

5. **Univariate IV vs multivariate value.** `tenor_months` carries IV 0.13 univariately but a multivariate coefficient of -0.009: its WOE is mostly a purpose proxy, absorbed once purpose and collateral enter. IV ranks; the model decides.

---

## Known Limitations

- In-sample validation only; out-of-time and out-of-sample evaluation are the natural next steps (the scoring snapshots would provide the out-of-time frame once outcomes mature).
- Reject inference is not applied; the development sample contains accepted applications only, with the acceptance screen (compensating-factor DSCR truncation) encoded in the generator and documented.
- Single development sample; no cross-validation or bootstrap interval reporting yet.
- Calibration assessed only through decile bad rates, not formally (e.g., Hosmer-Lemeshow or calibration curves).