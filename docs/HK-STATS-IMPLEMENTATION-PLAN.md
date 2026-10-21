# HK Statistical Improvement Plan — NEON Small Mammal Tracker

> **Purpose:** This is an implementation hand-off. It was authored in an environment **without R**, so
> nothing here was executed. Every item names the file/lines, the change, an R code sketch, a citation,
> a **verification step to run with R**, and the plain-language payoff. Work top-down: Item 0 (capture
> effort) is foundational — every CPUE and every environment correlation rides on the denominator it fixes.
>
> **Branch:** `claude/hk-neon-small-mammal-pesmiu`
> **Audit date:** 2026-06-28 · Source: HK statistics team (Hadley, Tukey, Fisher, Stan, Hutch, Tobler, Tufte, Joe, Few)
> **Overall verdict:** Strong, statistically-literate app. These are upgrades on a solid base, not rescues.
> The *descriptive* numbers (richness, MNKA, Hill, detection-corrected abundance) are defensible and honest.
> The *monthly environment-correlation* numbers are **not defensible as-is** (search-inflated, autocorrelation-blind).

---

## Item 0 — Capture effort / trap-status audit (THE foundational fix)

**This is the item you explicitly asked about: "are we taking into account all trap statuses correctly?"**
Short answer: **mostly, for the common codes — but there are four concrete gaps, two of them real bugs.**

### How effort is computed today

`R/helpers.R:220–228` (`clean_mam`), per **row** of `mam_pertrapnight`:

```r
d$is_set     <- !grepl("^1", d$trapStatus %||% "")          # "1 - trap not set"
ts1 <- substr(as.character(d$trapStatus %||% ""), 1, 1); ts1[is.na(ts1)] <- ""
d$trap_effort <- ifelse(ts1 == "1", 0, ifelse(ts1 %in% c("2","3"), 0.5, 1))
```

Then everywhere (`mnka_series` h:872–875, `community_stats` h:471, `env_corr_scan` h:1448, `compare` h:1328,
`env_pairs` h:1492) effort = `sum(d$trap_effort)`. CPUE = `100 * captures / trap_nights`.

NEON `trapStatus` domain for DP1.10072.001:

| code | meaning | current effort | correct? |
|---|---|---|---|
| 1 | trap not set | 0 | ✅ |
| 2 | trap disturbed/door closed but empty | 0.5 | ✅ (Nelson & Clark half-night) |
| 3 | trap door open or closed w/ spoor left | 0.5 | ⚠️ mixed — "door **open** w/ spoor" never sprung, was available all night → arguably 1.0 |
| 4 | >1 capture in one trap | 1 **per row** | ❌ **double-counts** (see below) |
| 5 | capture | 1 | ✅ |
| 6 | trap set and empty | 1 | ✅ |
| 0 | "no data" / sampling not performed | **1** (falls through `else`) | ❌ should be 0 / excluded |
| NA / "" | missing | **1** (falls through `else`) | ❌ should be 0 / excluded, not full effort |

### The four gaps

1. **Multi-capture (code 4) double-counts effort — REAL BUG.** In `mam_pertrapnight`, a trap that catches
   N animals produces **N rows** (one per individual), all sharing the same `nightuid` + `trapCoordinate`.
   `sum(trap_effort)` therefore counts that *single* trap-night as N trap-nights. The numerator (captures)
   correctly counts N animals, but the denominator is inflated → **CPUE biased low** at exactly the
   productive trap-nights. Magnitude is small (multi-captures are uncommon) but it is a true error and trivial to fix.

2. **`trapStatus == "0"` ("no data") and `NA`/blank both map to full effort (=1) — REAL BUG.** Both fall
   through the `else` branch. A row NEON shipped with no real sampling (or a parsing miss) silently adds a
   full trap-night to the denominator. This is the same class of phantom-effort the code already fights in
   `mnka_series` (h:876–882, dropping zero-effort plot-months) — but caught one level too late.

3. **The authoritative effort table is fetched then discarded.** `loadByProduct("DP1.10072.001", ...)`
   returns `mam_perplotnight` with `trapsSet` (the **count of traps set per plot-night**, NEON's own effort
   number), but `refresh_data.R:76–92` keeps **only** `raw$mam_pertrapnight` and throws the rest away. We
   reconstruct effort from per-trap codes instead of joining/cross-checking NEON's published value. At
   reduced-sampling sites where per-trap rows are incomplete, summed row-effort measures "rows NEON
   published," not "traps set."

4. **Code 3 half-night is defensible but undocumented as a choice.** The Nelson & Clark (1973) sprung-trap
   half-night correction is legitimate and well-cited; the wrinkle is that code 3 mixes "door open" (full
   availability) with "closed w/ spoor" (partial). 0.5 is a reasonable midpoint, but it should be a *stated*
   decision, not buried.

### The fix (one verification script, then two small code changes)

**0a. VERIFICATION FIRST — compare reconstructed effort vs NEON's `trapsSet` (run before trusting anything).**
This is item #1 of the HK plan and it gates everything below. Pull one site's `mam_perplotnight` and compare.

```r
library(neonUtilities); library(dplyr)
raw <- loadByProduct("DP1.10072.001", site = "SRER", startdate = "2013-01",
                     enddate = format(Sys.Date(), "%Y-%m"), package = "basic", check.size = "F")

# (a) NEON's authoritative effort, per plot-night
neon_eff <- raw$mam_perplotnight %>%
  transmute(plotID, nightuid, ym = substr(collectDate, 1, 7),
            trapsSet = as.integer(trapsSet))

# (b) our reconstruction, per plot-night — note dedup to DISTINCT trap-nights
source("R/helpers.R")
ptn <- clean_mam(raw$mam_pertrapnight)
recon <- ptn %>%
  distinct(nightuid, trapCoordinate, .keep_all = TRUE) %>%   # <-- the dedup fix (gap 1)
  group_by(plotID, nightuid) %>%
  summarise(recon_effort = sum(trap_effort, na.rm = TRUE), .groups = "drop")

cmp <- full_join(neon_eff, recon, by = c("plotID","nightuid")) %>%
  mutate(diff = recon_effort - trapsSet,
         pct  = 100 * diff / pmax(trapsSet, 1))
cmp %>% summarise(n = n(), median_pct = median(pct, na.rm = TRUE),
                  p95_abs_pct = quantile(abs(pct), .95, na.rm = TRUE),
                  n_big = sum(abs(pct) > 5, na.rm = TRUE))
# Decision rule: if |pct| < 5% essentially everywhere -> document the reconstruction as a
# deliberate design choice. If not -> adopt 0b (join trapsSet) as the denominator.
```

**0b. Fix the reconstruction (gaps 1 & 2), regardless of 0a outcome.** In `clean_mam` (`helpers.R:227–228`),
make "no data"/NA explicit-zero and keep the half-night rule:

```r
ts1 <- substr(as.character(d$trapStatus %||% ""), 1, 1); ts1[is.na(ts1)] <- ""
d$trap_effort <- dplyr::case_when(
  ts1 %in% c("", "0", "1") ~ 0,          # not set / no-data / missing -> NOT a trap-night
  ts1 %in% c("2", "3")     ~ 0.5,        # sprung/disturbed -> half (Nelson & Clark 1973)
  TRUE                      ~ 1           # 4,5,6 -> full
)
```

Then **dedup multi-capture rows before summing effort.** The cleanest place is a single helper the five
effort call-sites share, so the denominator is defined once:

```r
# one trap-night per DISTINCT (nightuid, trapCoordinate); effort is a property of the
# trap-night, NOT of each captured animal in it.
trap_night_effort <- function(d, ...) {   # ... = grouping cols, e.g. plotID, ym
  d %>% dplyr::distinct(.data$nightuid, .data$trapCoordinate, .keep_all = TRUE) %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(trap_nights = sum(.data$trap_effort, na.rm = TRUE), .groups = "drop")
}
```

Swap `sum(trap_effort)` for this in `mnka_series`, `community_stats`, `env_corr_scan`, `env_pairs`, and the
`compare`/`trapnights` packs. **Captures stay row-counted** (each animal is a real capture); only effort dedups.

> ⚠️ **Caveat to verify with R:** confirm multi-capture traps really do emit one row per individual in the
> bundled `.rds` (they do in raw NEON, but `refresh_data.R`'s column-subset could in principle collapse
> them — it doesn't, but verify with `ptn %>% count(nightuid, trapCoordinate) %>% filter(n > 1)`).

**0c. (Optional, after 0a) Join `trapsSet` as the denominator.** If 0a shows >5% drift, save
`mam_perplotnight` in `refresh_data.R` (add `raw$mam_perplotnight` alongside the trap table, subset to
`plotID, nightuid, collectDate, eventID, trapsSet`) and join it on `nightuid` (uniqueness `stopifnot`)
into the effort series, preferring `trapsSet` where present and falling back to the reconstruction.

**Citations:** Nelson & Clark 1973 (*J. Mammal.* 54:295 — sprung-trap correction); Beauvais & Buskirk 1999
(corrected trap-nights); Wickham 2014 §3 (each observational unit its own table); NEON DP1.10072.001 User Guide.
**Payoff:** every catch-per-effort number — and every rain-vs-rodent correlation built on it — uses a
denominator that counts real trap-nights once, excludes phantom effort, and matches NEON's own `trapsSet`.

---

## Tier 1 — honesty fixes (highest impact)

### Item 1 — Port the circular-shift null to the monthly env-correlation; gate the verdict word on it
**Files:** `R/helpers.R` `env_corr_scan` (~h:1439–1481) + the env panel in `server.R`. **Owner:** Fisher + Tobler. **Effort:** M.

**Problem:** the monthly scan reports `max|r|` over ~13 lags with **no p-value, no multiplicity penalty, no
autocorrelation correction**, then a badge declares "Strong/Moderate/Weak link, r = +0.XX". The seasonal
cascade sitting inches below it (`seasonal_env.R`) already does this correctly via `.seas_adj_p`. **The cure
already lives in the repo.**

```r
# circular-shift the response, re-run the FULL lag scan per shuffle, take max|r|.
env_scan_adj_p <- function(cpue_ts, driver_ts, lags = 0:12, nperm = 2000) {
  obs <- max(abs(vapply(lags, function(L) lag_cor(cpue_ts, driver_ts, L), numeric(1))), na.rm = TRUE)
  n <- length(cpue_ts); set.seed(7L)
  perm_max <- replicate(nperm, {
    k <- sample.int(n - 1L, 1L)
    sh <- cpue_ts[((seq_len(n) - 1L + k) %% n) + 1L]      # preserves serial structure
    max(abs(vapply(lags, function(L) lag_cor(sh, driver_ts, L), numeric(1))), na.rm = TRUE)
  })
  list(obs_r = obs, p_adj = mean(perm_max >= obs - 1e-9, na.rm = TRUE))
}
# Gate the badge on p_adj, not raw |r|. Also report Pyper-Peterman effective df (n_eff)
# so the r carries an honest sample size.
```

**Citations:** the repo's own `.seas_adj_p` (`seasonal_env.R:81`); Pyper & Peterman 1998 (*CJFAS* 55:2127,
effective df for autocorrelated series); Zuur et al. 2009; KN2015 Ch11.
**Verify with R:** expect several "strong" bars to wash out once the penalty applies — that wash-out *is* the fix.
**Payoff:** the two environment panels finally apply the same evidentiary standard; the monthly badge stops
advertising a search-inflated r as fact.

### Item 2 — Replace `lm(cpue ~ value)` with an effort-offset negative-binomial GLM + CI band
**Files:** env scatter in `server.R` (the fitted line) + a model helper in `helpers.R`. **Owner:** Fisher (model) + Tufte (band). **Effort:** M.

**Problem:** OLS on a pre-divided CPUE ignores count error structure, unequal effort, and temporal
autocorrelation; the chart draws a bare line with no CI.

```r
library(glmmTMB); library(DHARMa); library(ggeffects)
m_nb <- glmmTMB(cap ~ scale(value) + offset(log(tn)), family = nbinom2, data = dat)
sim <- simulateResiduals(m_nb)
testDispersion(sim); testZeroInflation(sim); testTemporalAutocorrelation(sim, time = dat$ym)
pr <- ggpredict(m_nb, terms = "value [all]")          # plot as a ribbon, not a line
```
Put `n` and "best of ≤13 lags — CI is for this one lag" on the chart face.

**Citations:** R2023 Ch13; Bolker et al. 2009 (*TREE* 24:127); Harrison et al. 2018 (*PeerJ* 6:e4794);
Hartig DHARMa; W2016 §3.8. **Verify with R:** full DHARMa battery must pass (or document the failing one).
**Payoff:** a fitted relationship that respects clumpy counts, unequal effort, and non-independent months.

### Item 3 — Screen weight/hindfoot with the existing 5·MAD rule BEFORE condition & size-scaling
**Files:** `helpers.R` `compute_condition` (h:238–275) and `species_scaling` (h:280–299). **Owner:** Tukey. **Effort:** S.

**Problem:** the 5·MAD outlier screen exists for the measurements *table* ("verify N records") but
`compute_condition` (chonk percentile) and `species_scaling` (size↔mass slope) ingest the **same raw weights
unscreened** — one fat-fingered 1000 g weight still crowns the chonkiest animal and bends the reference line.

```r
mad_keep <- function(x, k = 5) {                    # MAD floored to avoid over-tight screens
  med <- median(x, na.rm = TRUE); s <- mad(x, na.rm = TRUE)
  s <- max(s, 0.1 * med, na.rm = TRUE)
  is.na(x) | abs(x - med) <= k * s
}
# apply WITHIN species before percent_rank (chonk) and before the SMA slope; report
# "M flagged weights set aside (still shown in the QC table)".
```
Also raise the condition-percentile floor from `n >= 4` (h:266) to `n >= 8` to match `species_summary` (h:497).

**Citations:** Tukey 1977 (EDA); Zuur et al. 2010 §3 (*MEE* 1:3); Peig & Green 2009 (*Oikos* 118:1883).
**Verify with R:** inject a ×10 weight at one site, confirm the chonk crown and SMA slope don't move.
**Payoff:** one mis-keyed weight can no longer crown the chonkiest animal or bend the size reference line.

### Item 4 — Fix the Chao1 `f2 == 0` variance branch (or delegate the CI to iNEXT)
**Files:** `helpers.R:~944` (the Chao1 CI). **Owner:** Hutch. **Effort:** S.

**Problem:** point estimate and the `f2 > 0` CI are canonically correct, but the `f2 == 0` variance branch
appears to **drop the leading `f1*(f1-1)/2` term** and carry an extra `/(f2+1)` — and `f2 == 0` is common at
sparse sites, exactly where the CI matters most.

```r
# cross-check the existing branch against the canonical implementation:
iNEXT::ChaoRichness(v)     # gives estimate + correct asymmetric CI for both f2>0 and f2==0
# either fix the branch to the Chao & Chiu (2016) f2==0 variance, or replace the whole CI with this.
```
Since Item 6 imports iNEXT anyway, delegating is the low-risk path.

**Citations:** Chao & Chiu 2016 (*Annu. Rev. Ecol. Evol. Syst.* 47:45); Magurran 2004 App. A.
**Verify with R:** compare current CI vs `ChaoRichness` on a sparse site; confirm the discrepancy direction.
**Payoff:** a correctly-sized interval where it's currently mis-computed.

---

## Tier 2 — coverage & modeling upgrades (creative, research-backed)

### Item 5 — Coverage-standardize diversity with iNEXT; gate the compare-modal richness row
**Files:** Hill/Chao computation in `helpers.R` + the compare-two-sites modal in `server.R`. **Owner:** Hutch. **Effort:** M.

**Problem:** raw Hill q0/q1/q2 and Chao1 are effort-confounded across 46 unequal-effort sites; the compare
modal highlights the richness row **ungated** (the most effort-sensitive comparison is the unprotected one).

```r
library(iNEXT)
out <- iNEXT(abund_list, q = c(0,1,2), datatype = "abundance")
Dc  <- estimateD(abund_list, q = c(0,1,2), datatype = "abundance",
                 base = "coverage", level = target_C)   # read every site at equal completeness
```
Carry achieved coverage on the number; gate the green "higher" highlight on CI overlap. **Keep Hill on
distinct individuals — do NOT divide by p̂** (coverage *is* the diversity-side detection correction).

**Citations:** Gotelli & Colwell 2001 (*Ecol. Lett.* 4:379); Chao et al. 2014 (*Ecol. Monogr.* 84:45);
Hsieh, Ma & Chao 2016 (iNEXT, *MEE* 7:1451). **Verify with R:** confirm `iNEXT` q=0 extrapolation reconciles
with the existing Chao1 point estimate (they should match). **Payoff:** "diversity compared fairly — every
site read at the same sampling completeness."

### Item 6 — Stop dropping low-recapture bouts; pool them hierarchically (N-mixture)
**Files:** the closed-capture estimator in `helpers.R` (the `status = "insufficient recaptures"` path) + a precomputed bundle. **Owner:** Stan. **Effort:** L. **THE biggest honesty + coverage win.**

**Problem:** bouts with too few recaptures get **no estimate and are silently filtered out**, biasing the
per-month abundance series toward high-detection bouts.

```r
library(ubms)   # Stan-backed unmarked
fm <- stan_pcount(~ (1|siteID),                       # detection
                  ~ scale(year) + (1|siteID/grwhere), # abundance
                  data = umf, mixture = "P",
                  prior_intercept_state = normal(0, 5),
                  chains = 4, iter = 2000)
# gate: Rhat < 1.01, ESS > 400, 0 divergences; gof(fm) PPC; LOO for P vs NB.
```
Verify the `stan_pcount` formula order against the installed `ubms` version. **Fit offline; bundle the
posterior — never fit Stan in a live Shiny session.**

**Citations:** KN2015 Ch7/Ch9/Ch14; Royle & Dorazio 2008 (*Hierarchical Modeling and Inference in Ecology*);
MacKenzie et al. 2018; Vehtari et al. 2017 (LOO, *Stat. Comput.* 27:1413).
**Verify with R + Fauna/NEON sign-off** on the M0 closure / trap-response assumption (is trap-happiness real
for *Dipodomys*/*Peromyscus*? — M0 vs Mb). **Payoff:** every bout gets a (possibly wide) posterior instead of
vanishing, so the abundance series stops being conditioned on the well-sampled bouts.

### Item 7 — Add the trend model that doesn't exist yet (NB GLMM with effort offset)
**Files:** new helper + a Population-tab panel. **Owner:** Fisher. **Effort:** M.

**Problem:** there is **no trend model anywhere today** — the MNKA line is a picture, not an inference.

```r
m_tr <- glmmTMB(cap ~ year + s(month, bs = "cc") + (1|plotID) + offset(log(tn)),
                family = nbinom2, data = dat)   # or mgcv::gam for the smooth
```
n-gate: no verdict below ~6 sampled years (respect the COVID-2020 gap — do not interpolate across it).

**Citations:** R2023 Ch13–14; Bolker et al. 2009; Harrison et al. 2018.
**Verify with R:** DHARMa residuals + `testTemporalAutocorrelation`. **Payoff:** "is this population trending?
— a coefficient and a CI, not a connected line through a gap."

### Item 8 — Bayesian multilevel driver model (pools the short per-site series)
**Files:** offline script → bundled posterior → Driver panel. **Owner:** Stan. **Effort:** L.

**Problem:** the per-site permutation p at n≈7 is fragile and can't return a directional probability.

```r
library(brms)
m_drv <- brm(captures ~ z_monsoon + (1 + z_monsoon | siteID) + offset(log(tn/100)),
             family = negbinomial(),
             prior = c(prior(normal(0, 1), class = b)),
             chains = 4, iter = 2000)
# MANDATORY at this n: prior predictive check + prior-sensitivity rerun. Report P(effect>0)
# and the natural-scale multiplier with 90% CrI. Fit offline; bundle the posterior.
```

**Citations:** KN2015 Ch7/Ch9/Ch15/Ch17; McElreath *Statistical Rethinking*; Hobbs & Hooten 2015.
**Verify with R + Cass/NEON-Driver-Cascade sign-off** on the biome/lag priors (reuse `seasonal_biome` /
`.seas_expected` verbatim — don't re-derive the ecology). **Payoff:** "0.9X probability a wetter monsoon
raises next-year catch across water-limited sites" instead of a fragile single-site p.

---

## Tier 3 — performance, reproducibility, small honesty hardening

### Item 9 — Precompute the deterministic heavy stats into a build-time bundle
**Files:** new `scripts/build_stats_bundle.R` (mirror `build_site_index.R`) → `data/stats_bundle.rds`; one cached `site_pack()` reactive in `server.R`. **Owner:** Joe. **Effort:** M.

**Problem:** zero caching — `env_corr_all` runs 3×/load, `hill_numbers` 4×, and the **2000-iteration seeded
seasonal/closed-capture nulls run on a click**, byte-identical every time (seeds are fixed).

```r
# build-time: write the deterministic stats (seasonal nulls, closed-capture, accumulation) per site.
# live: collapse the per-site stats into ONE reactive, bindCache(rv$site_code, site_data_version()).
```
⚠️ Add `data/stats_bundle.rds` to `scripts/write_manifest.R`'s allowlist **and** regenerate it after every
`refresh_data.R` / `refresh_env_data.R`, or it silently won't deploy to Connect Cloud.

**Citations:** Mastering Shiny §3.4, §14; the existing `build_site_index.R` pattern.
**Payoff:** the 2000-permutation null never runs on a worker again; the new models become deployable.

### Item 10 — Key every cache on (site, data-version)
**Files:** the hand-rolled `cmp_cache` + any new `bindCache`. **Owner:** Joe. **Effort:** S.

**Problem:** `cmp_cache` is keyed on **site only**; the moment a stats bundle or `bindCache` lands, a warm
worker serves a pre-fix number past a fix.

```r
site_data_version <- function() as.integer(file.mtime("data/stats_bundle.rds"))  # in every cache key
```
**Payoff:** no false-green after a bug fix. **Verify in the running app.**

### Item 11 — Replace the accumulation SD band's modular-permutation trick with seeded real shuffles
**Files:** species-accumulation band in `helpers.R`. **Owner:** Hadley + Hutch. **Effort:** S.

**Problem:** the ±SD band is built from 40 arithmetic orderings `(i·odd) mod (k+1)`, not real shuffles, so it
isn't a valid sampling band. (Superseded if Item 5's iNEXT is adopted.)

```r
withr::with_seed(seed, replicate(perms, sample.int(k)))   # reproducible AND genuinely random
```
**Citations:** Gotelli & Colwell 2001; Advanced R Ch6 (withr). **Payoff:** an uncertainty band that means what the citation says.

### Item 12 — Small reproducibility + honesty hardening
**Owner:** Hadley. **Effort:** S each.
- (a) `mode_chr` tie-break is locale-ordered → make it first-seen: `top[which.min(match(top, x))]` (reproducible "home plot").
- (b) Surface `as.numeric` coercion failures in `clean_mam` (h:205–209) instead of `suppressWarnings` (loud-fail on data drift).
- (c) Make `deseason`'s `tapply` lookup factor-safe via `match()`.
- (d) Surface unsexed-adult counts in `repro_by_month` so "n too low" reads as effort, not absence.
- (e) Add `renv` (the docs name two R versions — exactly what `renv` locks) and `here::here()` for script paths.

**Citations:** R4DS 2e Ch6/7/19; Zuur et al. 2010. **Payoff:** reproducible screenshots and loud-failing data drift.

---

## Editorial / clarity pass (Few) — apply alongside the stats work

- **The driver block says "does rain drive the boom?" four ways on one tab, two openly disagreeing.** SPLIT by
  epistemic status: relabel the monthly rank panel **"Exploratory — leads to investigate"** and the seasonal
  cascade **"The seasonal read — closer to a test,"** with one bridging sentence so the disagreement is the
  point, not a bug. COMBINE the redundant `envCorrNote` hero sentence into the rank panel's title.
- **MOVE the env response scatter behind a "show the scatter" click** (third restatement of the rank bar's r for the public).
- **Headline vs click:** CI bands, n, plain-language significance stay on the **headline**; ACF / iNEXT
  coverage internals / residual plots go **behind a "model details" click**. The caveat and the denominator
  **never** go behind a click.
- **CUT the Chao1 restatement from the `site_insights` Overview list** (already in the accumulation banner + chart).
- **Compare-two-sites modal: SPLIT into two labeled blocks, honest-comparison first** — "Compare directly
  (detection-corrected)" on top, "Raw counts — read with care" below. Pure reorder, nothing cut.
- **Discipline (verbatim from Few):** never cut a caveat, an uncertainty interval, a denominator, or an n.

---

## Suggested execution order for the R session

1. **Item 0a** (effort verification script) — run first; it tells you whether 0c is needed.
2. **Item 0b** (effort reconstruction fix: no-data/NA → 0, dedup multi-capture) — small, foundational.
3. **Item 1** (monthly env circular-shift p) — highest-impact honesty fix; expect bars to wash out.
4. **Item 3** (MAD screen before condition/scaling) + **Item 4** (Chao1 f2==0) — small, self-contained.
5. **Item 2** (NB GLM + band), **Item 5** (iNEXT coverage), **Item 7** (trend GLMM) — the live-fit models.
6. **Item 6** + **Item 8** (Stan/ubms + brms) — fit OFFLINE, bundle posteriors; need Fauna/Cass sign-off.
7. **Item 9–11** (precompute bundle, cache keys, real shuffles) — performance + correctness plumbing.
8. **Item 12** + the editorial pass — hardening and clarity.

## Things that MUST be verified with R before any number ships
Items **0a, 1, 2, 3, 4, 5, 6, 7, 8** all need a live fit / residual battery / CI. The audit was authored
without R — treat every code sketch above as a starting point to run and check, not a finished result.

## Deploy / data reminders
- Re-bundling data (`refresh_data.R`) or adding `data/stats_bundle.rds` requires updating
  `scripts/write_manifest.R`'s allowlist, or the file silently won't deploy to Connect Cloud.
- If Item 0c lands (saving `mam_perplotnight`), the per-site `.rds` schema changes → re-run the full refresh
  and re-bundle; bump any cached stats.
- Hand the live profiling / cache-hit confirmation / deployed-app changes to NEONize (Connor for Connect Cloud).

## Domain sign-off still needed
- **Fauna / NEON:** closed-capture closure + trap-response (M0 vs Mb) for Item 6; the `trapStatus`→effort
  half-night rule + `trapsSet` reconciliation for Item 0.
- **Cass / NEON Driver Cascade:** multilevel driver-model framing + biome/lag priors for Item 8.
