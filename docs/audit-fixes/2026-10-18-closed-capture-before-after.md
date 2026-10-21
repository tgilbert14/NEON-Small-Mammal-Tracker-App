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
   `design_break` marks a month whose modal nights per bout (`k_mode`, the most frequent k
   among its estimable bouts, ties to the larger k) differs from the previous estimable
   month: the line breaks where nights per bout change. A change in the
   number or identity of estimable grids is not a break (the per-grid mean keeps those months
   comparable); `n_grids` and `grids` record them.

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

### Completeness caveat (HK review, 2026-10-18)

Completeness is MNKA over N-hat. When raw Schnabel N falls below MNKA (the trap-happy
signature) the bout is clamped to MNKA and its completeness is forced to 1 (status
"detection near-complete"). That happens on 582 of 3,113 estimable bouts suite-wide
(`clamp_share` in the per-site CSV): SRER 0.102 (11/108), HARV 0.147 (22/150), ABBY 0.197
(12/61), JORN 0.453 (39/86); SERC 0.380 (27/71). Untagged captures enter both MNKA and N-hat,
so they also read as fully detected. On ear-tagged animals only (`mean_detect_marked`),
completeness is HARV 0.780 and JORN 0.960, suite median 0.846, range 0.460 to 0.960, next to
the shipped 0.815, 0.962, 0.862 and 0.472 to 0.964. The app copy keeps the shipped numbers
and says that trap-happy desert sites read high and untagged captures count as fully detected.

### What the add-on is

- Species: untagged captures are mostly shrews at eastern sites (ABBY 88% *Sorex*, HARV 48%
  *Blarina*) and escaped heteromyids at desert sites (SRER 46% *Chaetodipus*, 16%
  *Dipodomys*; JORN 36% *Chaetodipus*, 21% *Dipodomys*).
- Share of summed estimable N-hat that is the add-on (`addon_share`): SRER 5.7%, HARV 15.6%,
  ABBY 23.8%, WREF 23.9% (JORN 4.9%).
- Placeholder capture events counted vs dropped as recapture Y (`n_unmarked` /
  `n_unmarked_recap`): SRER 371 / 42, HARV 1,762 / 71, ABBY 1,151 / 9.
- Two biases. An untagged animal caught again on a later night of the bout gets a new
  placeholder and counts again, so the add-on can run high. And real tags whose first-ever
  capture is flagged recapture Y (SRER 3.6%, HARV 5.2%, ABBY 3.3%, JORN 4.8%) may be animals
  first handled untagged, which bounds from above how often one animal is counted both ways.
- HARV_001#3's lower limit 37 is the MNKA floor (15 tagged + 22 untagged), not a Poisson
  limit; the Poisson lower limit on the tagged part falls below the 15 known alive.

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
- Design breaks (modal nights per bout, `k_mode`): SRER 4 of 39 months, HARV 10 of 51, ABBY 2
  of 29, JORN 4 of 31. Isolated months (drawn as a point with an interval bar): SRER 1, HARV 6,
  ABBY 1, JORN 2. HARV still breaks where 2-night and 3-night bouts trade places as the most
  common bout length (14 breaks with round(k_mean)).

## Not changed by this fix

- The picker map's detection chips (`mean_p`) read `data/site_index.rds`, which is rebuilt
  by the refresh workflow (`scripts/build_site_index.R`). Until the next index rebuild they
  still show the old estimator's p-hat (HARV 0.30, JORN 0.63). The index was deliberately
  not rebuilt here.
- Captures, CPUE, effort, `mnka_series()`, the individuals tile count, species counts and
  the map are unchanged. Placeholder tagIDs are not set to NA anywhere.

## 2026-10-20: k from trapped nights and marked-only individuals

Branch `fix/k-trapped-nights-marked-individuals` (on top of `fix/closed-capture-real-tags`).

1. **k counts trapped nights.** Bouts are now assigned over distinct trapped plot-nights
   (any row with `trap_effort > 0`), and capture rows join their bout by plot and night.
   A trapped night with no tagged capture counts in k and carries M forward, so a 3-night
   bout with an empty middle night is Schnabel, not Chapman. A bout that trapped and caught
   nothing stays in the table (`sumC = 0`, NA estimates, status "single-night" or
   "insufficient recaptures"), so `n_bouts` and the "estimable bouts (of N)" chip count
   trapped bouts. A capture on a plot-night with no effort stops with the plot and date.
   HARV has 737 trapped plot-nights, 690 of them with a tagged capture.
2. **Individuals are real ear tags.** `community_stats()$individuals`, `species_summary()`,
   `build_leaderboard()`, the Chao1 f1/f2 counts and Hill abundances use `mam_tag_marked()`.
   Placeholder capture events are reported as `untagged_captures`. Species richness, captures,
   CPUE, effort and MNKA are unchanged (checked on all 46 bundles: species and total captures
   identical before and after).

| | SRER | ABBY | HARV | JORN |
|---|---|---|---|---|
| n_bouts | 228 -> 229 | 174 -> 174 | 357 -> 372 | 170 -> 170 |
| bouts whose k changed | 2 | 3 | 19 | 1 |
| Chapman -> Schnabel (estimable) | 1 | 1 | 0 | 0 |
| zero-capture bouts (new rows) | 1 | 0 | 15 | 0 |
| n_estimable | 108 -> 108 | 61 -> 61 | 150 -> 150 | 86 -> 86 |
| sum N over estimable bouts | 5276.7 -> 5276.8 | 2598.2 -> 2599.1 | 7712.7 -> 7712.7 | 2533.5 -> 2533.5 |
| mean_p | 0.492 -> 0.491 | 0.455 -> 0.453 | 0.434 -> 0.434 | 0.656 -> 0.656 |
| mean_detect | 0.862 -> 0.862 | 0.875 -> 0.875 | 0.815 -> 0.815 | 0.962 -> 0.962 |
| Individuals tile | 3,976 -> 3,563 (413 untagged) | 3,128 -> 1,968 (1,160 untagged) | 6,532 -> 4,718 (1,834 untagged) | 2,252 -> 2,048 (204 untagged) |

HARV_001#3 N-hat is 42.7 before and after (its three nights all carried captures). Bouts
whose k changed are matched by plot and start date (each old bout lies inside one new bout;
no old bouts were merged at these four sites). The k change is small here because NEON
almost always catches something on a trapped pathogen-grid night. Across all 46 bundles,
540 bouts changed k, 17 estimable bouts moved from Chapman to Schnabel, 7 old bouts merged
into a neighbour, and 972 zero-capture bouts appeared (CPER 173, LENO 70, TOOL 69, DSNY 66);
most of these are single-night diversity grids that trapped and caught nothing, which raises
the single-night share in the detection header. The Individuals tile drops most at forest
sites with many untagged shrews (WREF -38%, ABBY -37%, TALL -32%, HARV -28%).

Command (from the repository root):

    Rscript --vanilla scripts/measure_closed_capture.R

which rewrites `docs/audit-fixes/2026-10-18-closed-capture-by-site.csv` with the new
per-site columns `individuals_marked`, `untagged_captures`, `n_bouts_trapped`,
`n_zero_capture_bouts` and `n_chapman_to_schnabel`. The before column was measured with the
same script logic on `R/helpers.R` from `origin/fix/closed-capture-real-tags` (da774bb).

Sanity (SRER, HARV, ABBY, JORN): every estimable bout and every monthly row has
lo <= N <= hi and lo >= MNKA, no NaN, and every tagged capture plot-night lies in exactly
one bout.

`data/site_index.rds`, `data/search_index.rds` and `data/species_ranges.rds` are rebuilt only
by `refresh-data.yml`; the build scripts now count ear-tagged individuals, but the committed
indexes keep the old individuals counts until the next refresh. `scripts/verify_bundle.R`
checks only that each index loads and has rows, so it does not disagree.
