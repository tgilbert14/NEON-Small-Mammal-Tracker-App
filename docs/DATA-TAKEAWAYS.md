# NEON Small Mammal Tracker — Data Takeaways & Critical Review

_Suite audit — June 2026. NEON DP1.10072.001 (Small Mammal Box Trapping)._

> **Finding-state update (2026-07-18):** this document preserves the June audit evidence, but the
> implementation moved. The `id_uncertain` aggregation bug is fixed; single-night coverage and
> cross-site detection are surfaced; compare rows carry p̂/N̂ and suppress misleading raw-count
> winners; tidy capture/monthly exports and a codebook are production-shipped in runtime merge
> `1615ab4`. Pinned main CI, semantic Connect health, Pages publication, and the JORN interaction
> funnel passed on 2026-07-18.

This app is the **flagship / gold standard** for its nine companion suite apps. Audited
through four lenses at once: NEONize (suite cohesion + honest-stats machinery), Fauna (field method
defensibility), Quinn (analysis-ready export), and Statistics (small-n honesty). Every number below
was recomputed from the bundled `.rds` files, not taken from the README.

## What the data actually shows

- **The bundle is national and substantial.** 46 site bundles totalling **178,216 captures of
  93,169 individuals**, 2013–2024. Per-site sampling span: median **10 years** (range 1–12). Top
  sites by captures: **HARV 12,809**, **ONAQ 11,562**, **SRER 9,962**, **KONZ 7,897**, **UNDE
  7,776** (`site_index.rds`: `captures`, `individuals`, `species`, `year_min/max`).
- **The consumer rung is a deer-mouse story.** *Peromyscus* is the most-caught species at **29 of 46
  sites** and **~50% of all national species-level captures** (84,518 / ~169k). *P. maniculatus*
  appears at 32 sites and *P. leucopus* at 25 — the two most widespread taxa in the whole suite
  (`species_ranges.rds`). Deserts are the exception: SRER, JORN, and MOAB are *Dipodomys*-dominated.
- **Species richness ≠ abundance, and the desert is the richest, not the densest.** Top richness:
  **SRER 29 species**, MOAB 22, **JORN 21** — all arid heteromyid sites — while the highest-*capture*
  sites (HARV, ONAQ) hold only 16–17. Cross-site `cor(richness, individuals) = 0.58`. This is the
  composition-vs-productivity inversion the cascade memory flags: in drylands richness rises where
  abundance does not.
- **CPUE and MNKA tell different stories — most at the desert.** Monthly site-total
  `cor(MNKA, CPUE)`: JORN **0.83**, HARV **0.72**, but **SRER only 0.51**. The two abundance indices
  diverge most exactly where the cascade leans on the consumer signal — promoting MNKA over the
  cascade's CPUE is the right call (`mnka_series()`).
- **Detection completeness swings 2× across sites — the raw count is honest only at deserts.** Mean
  per-bout detection completeness (MNKA / N̂, Schnabel/Chapman): site median **0.80**, range **0.42
  (LAJA) → 0.95 (JORN)**. Temperate **HARV catches only ~57%** of the animals present per bout
  (mean p̂ 0.30); desert **JORN catches ~95%** (mean p̂ 0.63). An uncorrected count undercounts
  temperate sites far worse than deserts (`closed_capture_series()`).
- **Half of all trapping bouts cannot be detection-corrected at all.** Of **8,200 bouts** pooled,
  **49% are single-night** (k=1) — no within-bout recaptures, so Schnabel/Chapman can't run and only
  MNKA/CPUE apply. The app correctly gates these to "single-night / index only."
- **Effort per site is real and uneven.** Trap-nights computed from each site's own
  `trapStatus` (sprung = ½ TN): HARV **68,176 TN**, CPER **73,520 TN**, JORN **31,584 TN**. Recapture
  rate ranges from **64.8% (JORN)** down to **41.4% (WOOD)** — the desert residents return; the
  prairie-vole boom-bust turns over (`community_stats()`).
- **The data is clean where it counts, sparse where measurement is optional.** Among 178,216
  captures: `lifeStage`, `trapCoordinate`, `scientificName` are **0% NA**; but **weight is 23.2% NA**
  and **hindfoot 28.5% NA** (recaptures often aren't re-measured), and **7.6%** of captures have no
  M/F sex. Genus-only / ambiguous IDs are **5.0%** of named captures (8,892 rows) — correctly
  excluded from richness by `species_level_only()`.
- **Tag-identity QC is near-silent on impossible histories; multi-species IDs are now active.** `tag_suspect`
  fires on only **52 of 93,169 individuals (0.06%)** — 47 spatially-impossible same-day-two-plot
  records + 5 careers > 5 yr. Clean data, no crying wolf. The June audit found `id_uncertain` dead;
  current code computes distinct species in a separate raw-column aggregation and joins it back,
  restoring the expected multi-species-tag QC path.
- **The environmental link to the consumer rung is weak, lagged, and biome-inconsistent.**
  Deseasonalized best-lag |r| with monthly catch-per-effort never exceeds ~0.5 at any site and the
  "winning" driver differs by site: SRER temp r −0.48 @ lag 3, JORN temp r −0.50 @ lag 9, HARV temp
  r +0.51 @ lag 9, ONAQ flowering r +0.65 @ lag 3 (`env_corr_all()`, n = 24–52 months). This is the
  multicollinear "lead worth a look, not a cause" regime the panel honestly labels.

## How it's built

**Pipeline:** `scripts/refresh_data.R` (`neonUtilities::loadByProduct(DP1.10072.001)` per site →
trim to a 33-column `keep` vector → xz-compress) writes one `data/sites/<SITE>.rds`. ALTREP/arrow
columns are materialized to base vectors before save (a documented silent-empty-column gotcha).
`scripts/build_site_index.R` rolls each bundle up to `data/site_index.rds` (one picker row/site) and
`data/species_ranges.rds` (per species × site). `scripts/refresh_env_data.R` builds the co-located
monthly overlays `data/env/<SITE>.rds` (precip/temp + three phenology yes-share signals).

**App side:** `global.R` loads the bundles defensively (`read_bundle()` returns NULL on
missing/corrupt, never crashes boot); `R/helpers.R` is the analytical engine; `server.R` renders;
`ui.R` is the bslib `page_sidebar` with the national-picker splash + tabbed dashboard. The deploy is
**bundle-only** — `neonUtilities` is referenced by a computed name (`paste0("neon","Utilities")`) so
the rsconnect scanner won't pin it into `manifest.json`.

**Metric definitions (the defensible core):** MNKA = Minimum Number Known Alive (Krebs 1966), an
individual counted alive in every month between its first and last capture in a plot. CPUE = captures
per **100 trap-nights**, denominator computed from this site's actual `trapStatus` (a trap available
one night; sprung/disturbed = ½ TN; Nelson & Clark 1973) — never a fixed 100-trap grid. Detection-
corrected abundance = closed-capture per bout: **Schnabel** (k≥3 nights) or **Chapman** (k=2), gated
to ≥3 within-bout recaptures, clamped to ≥ MNKA, CI built in the 1/N domain then inverted (Otis et al.
1978). Richness/Chao1, Hill q0/q1/q2, and the species-accumulation curve all run on
`species_level_only()` so a "Rodentia sp." is never its own species. The "Chonk Index" is honestly an
adult weight percentile within species — NOT a Scaled Mass Index, because hindfoot barely scales with
mass here (r≈0.15) — a deliberate, documented omission.

## Critical findings by lens

### NEONize (suite cohesion / honest-stats machinery)
- **[strength]** This is the reference implementation for the suite's honest-stats kit:
  `species_level_only()`, Hill/Chao1 with CI + instability flag, the deseasonalize-before-correlate
  env scan, the "answer up front" `insight_banner()` pattern, the n-gates. All of it ports cleanly
  and the cascade reuses it.
- **[RESOLVED LOW] README live-app badge and hosting language were stale.** Current authority docs
  point to the healthy Connect app and describe the restricted review-branch plus verified-republish
  release flow. The retired shinyapps target is no longer presented as current.
- **[RESOLVED LOW] README "140+ species" vs computed 145.** README and the cover now state the
  exact **145** distinct species represented by `species_ranges.rds`.

### Ecological (Fauna — field method)
- **[strength]** The field method is represented correctly: bout structure (pathogen grids ~3
  consecutive nights vs 1-night diversity grids) drives the estimator choice; effort is from real
  `trapStatus` not an assumed grid; "not detected ≠ absent" is stated on-chart. A journal reviewer
  would accept MNKA + gated closed-capture as defensible indices.
- **[RESOLVED MED] Single-night dominance is surfaced as a coverage stat.** The detection card now
  reports the percent of bouts that are single-night/index-only, including the all-single-night case,
  so a sparse N̂ series reads as sampling design rather than missing data.
- **[RESOLVED LOW] Detection completeness is exposed as a cross-site qualifier.** Site cards and the
  compare view carry p̂; compare adds mean monthly N̂ and suppresses raw-count “winners” when sites'
  detection differs materially or is un-estimable.

### Data science (Quinn — analysis-ready export)
- **[RESOLVED MED] Site-level tidy exports and a codebook now exist.** The About panel offers the
  cleaned event/handling-row capture table, monthly MNKA/CPUE/N̂/p̂ series, and a column codebook with
  units, grain, source/license, estimator caveats, and NA conventions.
- **[RESOLVED LOW] Measurement-NA conventions are published.** The UI and codebook state that blank
  weight/hind-foot values are unmeasured handling/empty-trap rows rather than zeros, and measurement
  summaries identify their handled-and-measured denominator.

### Statistics (small-n honesty / correctness)
- **[RESOLVED HIGH] `id_uncertain` was always FALSE; the multi-species-tag QC flag was dead code.**
  In `build_leaderboard()` the `summarise()` computes `scientificName = mode_chr(...)` (a scalar)
  and then `n_species_ids = n_distinct(scientificName[...])` **in the same `summarise()` call**, so
  `n_distinct` runs on the 1-value scalar and returns 1 for every animal. Verified: SRER has **151 of
  3,976 tags (3.8%) genuinely recorded under >1 species** (e.g. tag `…056017` = *Chaetodipus
  intermedius* + *C. penicillatus*), yet `sum(id_uncertain) = 0` across all 93,169 individuals. This
  silently disabled dossier QC flag #7 AND the `min_known_lifespan()` ambiguous-ID exclusion. **This
  is the exact "summarise sees earlier new columns" gotcha the playbook documents for plant
  richness.** Current implementation performs the distinct-species aggregation outside the
  shadowing `summarise()` and joins it back; helper fixtures protect the repair.
- **[strength]** Otherwise the stats are honest: deseasonalization before correlation, n≥8-month
  overlap floor, the "bars aren't independent evidence / r is not r²" caveats, Chao1 flagged as a
  lower bound when doubletons are scarce, the MNKA floor on N̂. Don't touch these.
- **[low] Cross-site correlations on n=46 are descriptive.** `cor(richness, individuals)=0.58` etc.
  are fine as patterns but shouldn't be reported with p-values; keep them framed as description.

## Honest-stats & caveats — what this app must NOT be read to claim

- **Per-site env correlations are leads, not mechanisms.** |r| ≤ 0.5, scanned over ≤13 lags and 3–5
  collinear drivers on 24–52 months — exactly the false-positive-prone regime. The "best driver"
  flips between temp and flowering across sites and even the *lag* differs between two deserts (SRER
  3 mo, JORN 9 mo). Read as "investigate," never "X drives the population."
- **CPUE is a within-site index, not a cross-site density.** Different grids, night-counts, and
  detection (0.42–0.95) mean a raw CPUE comparison across biomes is not an abundance comparison.
- **Detection-corrected N̂ is a defensible index, not a census** — it assumes a closed population and
  equal catchability over the bout, and exists for only ~half of bouts (the multi-night ones).
- **Richness is composition, not productivity** — SRER's 29 species over its sparse desert is the
  textbook dryland inversion; do not read high richness as high abundance.
- **"Longest confirmed alive" is a right-censored floor, not a lifespan**, and reaches higher at
  more-trapped species purely from more chances — the app states this; keep it.

## Place in the cascade

This app is the **consumer rung** (climate → plants → **consumers**) and the source of the suite's
shared machinery. It contributes:

1. **The defensible consumer signal: MNKA, not CPUE.** SRER's loose `cor(MNKA,CPUE)=0.51` and the
   0.42–0.95 detection spread confirm the cascade decision to promote MNKA and reuse this app's
   `helpers.R` (`mnka_series`, `bout_closed_capture`) rather than a bespoke CPUE.
2. **A consumer rung that is deer-mouse-dominated nationally but heteromyid at the deserts** — the
   biome split the cascade depends on. The desert sites (SRER, JORN) where the consumer signal is
   *Dipodomys* are also where the env link is weakest and most lagged, consistent with the
   pulse–reserve / annual-aggregation-artifact story.
3. **Corroboration, not the headline.** The robust pooled suite link is temperature → green-up
   onset; the small-mammal env scan here is the weaker downstream rung — its honest job is to show
   *whether* the consumer tracks the plant/climate signal at all, with the lag mechanics intact. It
   does, weakly and biome-conditionally — which is the truthful cascade result, not a failure.
