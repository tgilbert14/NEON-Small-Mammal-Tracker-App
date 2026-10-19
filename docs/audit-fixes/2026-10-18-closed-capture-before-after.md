# Closed-capture N-hat: before and after the real-tag fix (2026-10-18)

Branch `fix/closed-capture-real-tags`. Estimator spec from the 2026-10-18 suite audit.
All numbers below come from the committed bundles in `data/sites/*.rds`; nothing was
downloaded.

## What changed

1. **Marks are real ear tags only.** NEON SOP C.8 issues a placeholder tagID
   (`NEON.MAM.Dxx.O<plot><trapCoordinate><YYYYMMDD>`, about 1% in an older short form)
   once per untagged capture. The old code counted each one as a marked animal that was
   never recaptured, which inflated N-hat and deflated p-hat. Placeholders now never enter
   C, U, R or M; each placeholder capture event is added once to its bout as a known-alive
   animal (`n_unmarked`). A placeholder row with `recapture = "Y"` adds nothing
   (`n_unmarked_recap`).
2. **Dead animals leave the marked pool** (`M_t = sum(U before t) - sum(D before t)`).
3. **Bout limits** are Krebs (1999) Poisson limits on the recapture count (finite for
   sumR >= 3); the 1/N normal inversion is kept only for Schnabel bouts with sumR >= 50.
4. **The monthly series is a mean per estimable grid**, not a sum over whichever grids
   passed the gate. Two bouts on one plot in one month are averaged first. The monthly
   95% interval follows a three-case rule (lead decision 2026-10-18): a month with one
   estimable bout keeps that bout's own limits; a month with several bouts uses a Chao (1989)
   log-normal interval on the animals beyond MNKA when f0 > 0 and varN > 0, else lo = MNKA and
   hi = max(N, mean of the bouts' finite hi), with hi = Inf if any bout hi is unbounded.
   `design_break` marks a month whose number of estimable grids, or rounded nights per
   bout, differs from the previous estimable month; which plots they are does not count.
   The chart breaks the line there.

Bout assignment, nights (k), the number of bouts, the number of single-night bouts and
the number of estimable bouts are unchanged at all 46 sites (placeholders never added a
within-bout recapture, so the sumR >= 3 gate passes for exactly the same bouts).

## Commands

```sh
# NEW values, written to docs/audit-fixes/2026-10-18-closed-capture-by-site.csv
Rscript --vanilla scripts/measure_closed_capture.R

# OLD values: the same script against the pre-fix helpers (R/helpers.R at 7c0a59b)
git show 7c0a59b:R/helpers.R > /tmp/old/R/helpers.R
Rscript --vanilla scripts/measure_closed_capture.R --helpers=/tmp/old/R/helpers.R --out=/tmp/old.csv
```

## Before and after

`sum N` is the sum of N-hat over estimable bouts. `mean_p` is the pooled per-night
p-hat, weighted by k x N_marked (old: k x N). `mean_detect` is completeness,
MNKA / N-hat weighted by N-hat.

| Site | sum N old | sum N new | mean_p old | mean_p new | mean_detect old | mean_detect new | peak bout old | peak bout new |
|------|----------:|----------:|-----------:|-----------:|----------------:|----------------:|---------------|---------------|
| SRER | 5734.4 | 5276.7 | 0.446 | 0.492 | 0.799 | 0.862 | SRER_007#6 290.1 | SRER_007#6 165.6 |
| ABBY | 3411.4 | 2598.2 | 0.328 | 0.455 | 0.669 | 0.875 | ABBY_004#19 150.8 | ABBY_004#14 101.2 |
| HARV | 11150.5 | 7712.7 | 0.296 | 0.434 | 0.568 | 0.815 | HARV_008#53 1735.9 | HARV_008#53 353.2 |
| JORN | 2605.5 | 2533.5 | 0.627 | 0.656 | 0.948 | 0.962 | JORN_017#2 89.1 | JORN_017#2 87.2 |

- **HARV_001#3** (2013-10, k = 3, sumR = 6): N-hat 116.0 [64.4, 580.5] old, 42.7 [37, 78.3]
  new (audit measured 42.67). Regression check in `scripts/test_helpers.R`.
- **HARV monthly peak** (2022-08): 2,160.3 summed over grids old, 268.1 per grid new.
- **Monthly mean N** is not comparable across versions (old: sum over grids; new: mean per
  grid): SRER 147.0 -> 52.5, ABBY 117.6 -> 43.3, HARV 218.6 -> 48.9, JORN 84.0 -> 29.3.
- **Untagged captures** added as known alive (all bouts): SRER 371, ABBY 1,151, HARV 1,762,
  JORN 164; 14,306 across the 46 sites. Real-tag deaths removed from the marked pool:
  1,375 across the 46 sites.

### Numbers used in the app copy and docs

- HARV vs JORN pooled per-night p-hat: **0.434 vs 0.656, ratio 1.51** ("roughly 1.5x").
- HARV vs JORN completeness: **0.815 vs 0.962** ("about 81% vs 96%").
- Site completeness over the 45 sites with an estimable bout: **median 0.862, range 0.472
  (LAJA) to 0.964 (SERC)**; JORN 0.962 is second. Old: median 0.799, range 0.425 (LAJA)
  to 0.948 (JORN).
- Pooled p-hat over sites: median 0.435 -> 0.482, range 0.178-0.627 -> 0.195-0.662.

### Difference from the audit's expected p-hat

The audit expected mean_p HARV 0.296 -> 0.367 and JORN 0.627 -> 0.623 (ratio about 1.7x).
Those values reproduce exactly as `sum(C_real) / (k x N_total)`, real-tag captures over the
N-hat that already includes the untagged add-on. The spec for this fix says p-hat uses
N_marked before the add-on and never mixes real-tag captures with the total N, which gives
0.434 and 0.656 (ratio 1.51). The copy uses the spec's values. Sums of N and completeness
match the audit (SRER 5277, HARV 7717 expected vs 7713, ABBY 2598; HARV 0.81, JORN 0.962).

## Sanity checks (SRER, HARV, ABBY, JORN)

- Every monthly row: lo <= N <= hi, lo >= MNKA, no NaN, dates ascending. No month has an
  unbounded upper limit at these four sites.
- Every estimable bout: lo <= N <= hi and lo >= MNKA.
- Audit regression check, single-grid months, monthly hi >= N + 1.96 N / sqrt(sumR) of that
  bout: SRER 0 of 1, HARV 1 of 8, ABBY 0 of 9, JORN 0 of 3 months fall below it. The HARV
  month (2019-07, a clamped "detection near-complete" bout) is one where the bout's own
  Poisson hi (28.7) is below that bound. All single-grid months at these four sites have one
  bout, so they now carry the bout's own limits. (The first version of this branch used the
  Chao interval in every month and fell below the bound in 8 months.)
- Design breaks: SRER 14 of 39 months, HARV 30 of 51, ABBY 26 of 29, JORN 12 of 31, almost
  all from the number of estimable grids changing between months. Isolated months (drawn as
  a point with an interval bar): SRER 11, HARV 20, ABBY 25, JORN 8.

## Not changed by this fix

- The picker map's detection chips (`mean_p`) read `data/site_index.rds`, which is rebuilt
  by the refresh workflow (`scripts/build_site_index.R`). Until the next index rebuild they
  still show the old estimator's p-hat (HARV 0.30, JORN 0.63). The index was deliberately
  not rebuilt here.
- Captures, CPUE, effort, `mnka_series()`, the individuals tile count, species counts and
  the map are unchanged. Placeholder tagIDs are not set to NA anywhere.
