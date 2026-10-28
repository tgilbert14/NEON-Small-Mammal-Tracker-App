# Science finish: helper fixes, before and after (2026-10-22)

Branch `finish/sm-science` from main `c1a229d`. "Before" is `R/helpers.R` and
`R/seasonal_env.R` at `c1a229d`, "after" is the branch. All numbers are from the
committed bundles in `data/sites` and `data/env`.

Commands:

```
Rscript --vanilla scripts/measure_science_finish.R          # items 1-12, 16
Rscript --vanilla scripts/measure_env_null_calibration.R 400 500   # item 8 table
Rscript --vanilla scripts/test_helpers.R                    # 63 checks
```

Captures, effort, CPUE, MNKA and the closed-capture estimates are unchanged
(the existing SRER, HARV and ABBY regression checks pass with their recorded
values).

## 1. Leaderboard first life stage

The summarise() read `lifeStage` after reassigning it to the modal stage, so
`first_stage` was the modal stage when the first dated row came first in the
table, and NA otherwise.

| site | animals | first_stage changed | age prefix (>= vs ~) changed | approx age changed |
|---|---|---|---|---|
| SRER | 3,563 | 209 (all were NA) | 253 | 336 |
| HARV | 4,718 | 346 (all were NA) | 336 | 471 |

After: SRER juvenile 164 to 237, subadult 309 to 485; HARV juvenile 216 to 335,
subadult 328 to 545. The other summarise() calls in R/helpers.R and
R/seasonal_env.R were checked; no other read-after-reassign was found.

## 2, 3, 6. Per-animal flags (flag counts, all tags)

| site (animals caught 2+) | stage high | stage warn | weight warn | species info | species warn |
|---|---|---|---|---|---|
| SRER (1,764) before | 70 | 0 | 32 | 151 | 0 |
| SRER after | 6 | 64 | 18 | 106 | 24 |
| HARV (2,273) before | 121 | 0 | 63 | 755 | 0 |
| HARV after | 8 | 113 | 32 | 492 | 17 |
| ONAQ (2,265) before | 271 | 0 | 36 | 177 | 0 |
| ONAQ after | 35 | 236 | 20 | 98 | 26 |

Multi-species flag rate among animals caught 2+ times: SRER 8.6% to 7.4%,
HARV 33.2% to 22.4%, ONAQ 7.8% to 5.5% (genus-only "sp." names no longer count
as a second species). Other flags are unchanged.

## 4. cf. identifications

identificationQualifier tokens across all 46 bundles: cf. species 45,368;
cf. genus 354; cf. variety 219; cf. subspecies 47; aff. species 32; cf. family
29; aff. genus 3; aff. subspecies 1; aff. variety 1.

| site | species before | species after | species_confirmed |
|---|---|---|---|
| SRER | 29 | 29 | 22 |
| MOAB | 22 | 22 | 16 |
| TOOL | 10 | 10 | 7 |
| HARV | 17 | 17 | 15 |

Hill q0 and q1 unchanged at those four. Across 46 sites, 34 have cf./aff. genus
or family rows (UNDE 144 rows); richness drops by one at BONA, DEJU, DSNY, HEAL,
NIWO, RMNP and TEAK. Outputs that read species_level_only() move with it:
community_stats$species, hill_numbers, species_accum (sobs, Chao1),
species_measurements, species_adult_values / flagged_measure_captures,
min_known_lifespan, report_pdf's species list, and the indexes built by
scripts/build_site_index.R and scripts/build_search_index.R on their next run.
species_summary() does not use it and does not move.

## 5, 16. Accumulation band and Chao1 interval

| site | f1 | f2 | Chao1 | 95% CI before | after |
|---|---|---|---|---|---|
| SRER | 7 | 2 | 36 | 30-107 | 30-107 |
| MOAB | 4 | 2 | 24 | 22-56 | 22-56 |
| BARR | 2 | 0 | 4 | 3-15 | 3-16 |

Only the f2 = 0 branch changed. The band half-width at a quarter of the curve:
SRER 2.72 to 2.24, MOAB 1.78 to 2.03, BARR 0.44 to 0.62 (real shuffles instead
of 40 structured orderings). The mean curve endpoint is unchanged.

## 7. site_qc_report

SRER 948 rows (412 tags) in 2.0 s; HARV 1,434 rows (769 tags) in 2.6 s.
HARV by flag: stage_regression 23 high / 226 warn, sex_flip 201 warn,
multi_species_id 699 info / 21 warn, hindfoot_spread 145 info, weight_jump 64
warn, measurement_outlier 23 warn, mover_identity 27 warn, same_day_two_plots 3
high, career_span 2 high. (Report rows are one per implicated date, so they
exceed the per-animal flag counts above.)

## 8. env_corr_pvalue observed statistic

| site | p before | p after | observed best abs(r) | driver, lag |
|---|---|---|---|---|
| SRER | 0.73 | 0.73 | 0.4799 | temp, 3 |
| HARV | 0.176 | 0.176 | 0.5090 | temp, 9 |
| JORN | 0.32 | 0.32 | 0.4960 | temp, 9 |

The rounding did not cross a null value at these three sites. Calibration:
see 2026-10-22-env-null-calibration.md.

## 9. Month-adjusted annual index (seasonal card)

SRER: pooled CPUE vs index by year. 2020 (1 month, 684 trap-nights) drops out.
2016 37.7 / -3.9, 2017 65.9 / +21.1, 2018 31.2 / -10.6, 2019 35.1 / -4.2,
2021 40.5 / -4.2, 2022 66.2 / +23.8, 2023 37.9 / -3.9, 2024 24.0 / -13.6.
Monsoon (lag 1) link: before r +0.72, n 7, p 0.059, p_adj 0.276; after r +0.83,
n 6, p 0.016, p_adj 0.180. Winter rain drops to n 5 (no p).

HARV: no year drops out. Links barely move (spring temp r -0.11 to -0.13;
winter rain -0.42 to -0.32; monsoon 0.13 to 0.15; all p > 0.5).

## 11, 12. MNKA edges and clamped bouts

SRER: 222 plot-months, 16 edge; 108 estimable bouts, 11 clamped to MNKA.
HARV: 350 plot-months, 17 edge; 150 estimable bouts, 22 clamped.
insert_gap_breaks(max_gap_months = 2) adds 62 break rows at SRER, 71 at HARV.
