# Small mammal box trapping — Expert Review by Mara (NEON DP1.10072.001)
_Devoted product-expert review — June 2026._

> **Current finding state (verified 2026-07-20):** this document preserves the
> June review and scorecard as historical evidence. Pass 1 resolved its release
> blockers: Compare now carries detection-qualified p̂/N̂ context and suppresses
> indefensible raw-count winners; the single-night/index-only share is explicit;
> tidy capture and monthly-series exports plus a codebook are live; and README/live
> links and the exact 145-species inventory agree. Pinned CI, semantic Connect
> health, Pages publication, and the JORN funnel passed. Cover V5 later changed only
> cover UI/media/manifest inventory, leaving those scientific and data contracts
> unchanged. Findings at lines below that say “remaining,” “Fix,” or show C+/B/B−
> grades are the June baseline, not current open work.

> I walked my own product's engine end to end against the trapping SOP and the closed-capture canon, and I'll say it plainly: this is the most honest small-mammal abundance app I have reviewed. The mark-recapture is gated by bout structure exactly the way the literature demands — Schnabel for k≥3, Chapman for k=2, nothing for k=1, ΣR≥3 to report, N̂ clamped to the MNKA floor, the CI built in the 1/N domain then inverted. MNKA is promoted over CPUE as the consumer signal, the Chonk Index honestly refuses to be a Scaled Mass Index, and the long-career flag correctly does NOT treat a 3-year desert heteromyid as a tag error. The `id_uncertain` bug the prior audit logged is fixed (computed outside the `summarise()` now). My remaining quarrel is in one place and it matters: the **two-site Compare table races raw captures / individuals across biomes with a winner-highlight and no detection warning** — that is exactly the within-site-index-as-cross-site-density trap I exist to stop, and the app's own detection card already holds the antidote (a 2× p̂ spread). Fix the compare, surface the single-night share and per-site p̂ as first-class numbers, ship a tidy export, and this is publishable-grade. The science is right; a handful of presentation surfaces let a raw count quietly outrun what the bout structure can defend.

## Method fidelity (is the NEON protocol represented correctly?)

Faithful, and faithful in the load-bearing details — not just the headline.

- **The robust-design bout grouping is correct and data-driven.** `bout_closed_capture()` (`helpers.R:1038`–`1045`) assigns a new bout when the gap to the previous trapping night is `> 2` days, keyed off consecutive `collectDate`s rather than a hardcoded "3 nights" — the right call because NEON reduced sampling cadence at some sites and a fixed-k assumption would shatter at those. Within-bout recapture status is recomputed (`M = cumsum(U) - U`, `R = C - U`, `helpers.R:1065`) instead of trusting the raw `recapture` column, which carries cross-bout history. This is Pollock (1982) realized honestly.
- **Effort is real, from `trapStatus`, with the sprung-trap ½-TN convention.** `clean_mam()` (`helpers.R:216`–`217`) maps code 1 → 0 TN, codes 2/3 (disturbed/sprung) → ½ TN (Nelson & Clark 1973), codes 4/5/6 → 1 TN. CPUE keeps the precise denominator; `community_stats()` rounds only the display value (`helpers.R:460`). No assumed 100-trap grid anywhere — verified.
- **Identity physics is right.** The reliable flag is spatial impossibility — same tag at >1 plot on a single day (`spatial_conflict`, `helpers.R:347`–`350`), plots km apart, beyond a heteromyid's daily move. The code explicitly does NOT flag long careers (`tag_suspect` only fires on spatial conflict or `career_days > 1825`, i.e. >5 yr; `helpers.R:404`) and the comment cites *D. merriami* ≥3.5 yr wild (Zeng & Brown 1987). Per the verified data this flags only 52 of 93,169 individuals (0.06%) — clean data, no crying wolf (DATA-TAKEAWAYS).
- **The detection narrative is stated where it lives.** The empty-state note when no multi-night bout exists (`server.R:2554`–`2556`) reads "This site's grids are single-night… MNKA & CPUE above are the right index for these" — screenshot-safe honesty, not a buried footnote.

One method-labelling nit: the README's env table (line 154) says air temperature is "single-aspirated" `DP1.00002.001`. That is the correct product, but the app pulls `timeIndex = 30` (README line 181) — worth a one-line note that the monthly aggregate is built from the 30-min table, not triple-aspirated tower data, so a reviewer doesn't assume a different sensor stream.

## Analysis & metrics — defensible? (with the literature)

The abundance ladder is gated by design and labelled by tier — a journal reviewer accepts it.

- **MNKA (Krebs 1966)** is implemented as an individual "known alive" in every monthly session between first and last capture in a plot (`mnka_series()`, `helpers.R:856`), and is correctly promoted as the consumer signal over CPUE. The literature backs this: MNA tracks true N well only when capture probability is high and stable, and degrades as p falls (Slade & Blair 2000) — which is precisely why the app pairs it with a detection correction rather than trusting it alone.
- **Closed-capture N̂** uses Schnabel (1938) for k≥3 (`N = ΣCₜMₜ / ΣRₜ`, `helpers.R:1087`) and Chapman (1951) for k=2 (`helpers.R:1096`), with p̂ under Otis et al. (1978) Model M0 (`helpers.R:1103`). The three guards are all present and correct: ΣR≥3 to report (`RECAP_GATE`, `helpers.R:1030`, 1085 — Schnabel → ∞ as ΣR→0), N clamped ≥ MNKA (`helpers.R:1101`), and the Schnabel CI built in the 1/N domain then inverted because N̂ is right-skewed (`helpers.R:1089`–`1092`). This is the discipline the canon demands; do not touch it.
- **The roll-up is statistically clean.** `closed_capture_series()` (`helpers.R:1135`–`1152`) SUMS N̂ across grids per month (abundance adds), pools p̂ as `ΣC/Σ(kN̂)`, and carries a delta-method `varN` (`helpers.R:1093`) so the monthly band has a real variance, not a fudge. Months with no estimable bout stay index-only — correct.
- **Diversity runs on species-level IDs only.** `species_level_only()` (`helpers.R:891`) drops "X sp." / "A/B" before Chao1/Hill/accumulation; Chao1 is the bias-corrected `Sobs + f1(f1−1)/(2(f2+1))` form (`helpers.R:926`), flagged a soft lower bound when `f2 < 5` (`helpers.R:940`), with a Chao (1987) log-normal CI. Hill q0/q1/q2 are computed over distinct **individuals** per species (`helpers.R:992`), not captures — de-pseudoreplicated. Textbook-correct (Gotelli & Colwell 2001; Chao & Chiu 2016; Jost 2006).
- **The Chonk Index honestly refuses to be an SMI.** `compute_condition()` (`helpers.R:227`–`239`) defines it as an adult weight percentile within species and documents WHY a Peig & Green (2009) Scaled Mass Index is omitted: hindfoot barely scales with mass here (r≈0.15), so an SMI would rank measurement noise. This is the honesty bar working as designed.
- **Env links are deseasonalized before correlating** (`env_corr_scan()`, `helpers.R:1439`–`1445`), gated at n≥8 months of overlap (`helpers.R:1428`), framed as "Correlation, not cause… a lead to investigate" (`server.R:2670`) with an r-vs-r² popover (`server.R:2669`). |r| never exceeds ~0.5 and the winning driver/lag flips between sites — the panel labels this honestly ("best of N drivers × ≤13 lags… bars aren't independent evidence", `server.R:238`). This is exactly the multicollinear small-n regime, handled correctly.

## What the field would add (collection / analysis / presentation / use)

These are the things the wider mark-recapture literature would expect to see, ranked by what actually changes a defensible read:

1. **Density, not just abundance — be explicit that N̂ is an index, not per-ha.** The app correctly declines SECR (Efford), and that is the right call for a single 10×10 grid per plot — naïve N/area is wrong because of edge effects. But the field's state-of-the-art answer to "how many per hectare" is spatially-explicit capture-recapture, and a reviewer will ask. The app should keep declining it but say so on the detection card in one line: "N̂ is animals on the sampled grid(s), not a per-hectare density — that needs SECR." Right now the y-axis says "animals on the grid(s)" (`server.R:2606`), which is good; make the SECR caveat explicit so the omission reads as a choice, not an oversight.
2. **Surface per-site detection p̂ and completeness as a publishable cross-site number.** Detection completeness swings 0.42 (LAJA) → 0.95 (JORN) and is biome-structured — deserts catch ~95%, closed-canopy temperate ~57% (DATA-TAKEAWAYS). The detection card shows the within-site `mean_p` and `mean_detect` chips (`server.R:2524`–`2525`), which is good, but this 2× spread is a genuine cross-site signal that directly qualifies every raw-count comparison. It belongs on the picker map or a cross-site card, not only inside one site's detection tab.
3. **An apparent-survival (CJS φ) read would be the natural next rung** — the app correctly declines it (`min_known_lifespan()` note, `helpers.R:583`–`586`) because it needs an offline-validated session definition and would over-claim, shipping a right-censored "longest confirmed alive" floor instead. Agreed; this is the honest choice. If you ever add it, gate it behind a validated session table, never the raw monthly bins.
4. **Collection-side, nothing to add** — NEON's design (fixed grids, robust-design bouts, life-long ear tags unique within a site) is sound and the app represents it faithfully. The field's only collection wish is more multi-night (pathogen-grid) bouts, since 49% single-night is what caps the detection-corrected coverage — but that's NEON's design constraint, not the app's to fix.

## Product-specific honesty & QC traps

The five "do not let this slide" rules for this product — checked against the app:

1. **An index is not an absolute.** ✅ MNKA/CPUE labelled as indices; only gated N̂ carries a detection-corrected claim; single-night bouts routed to index-only. Held everywhere I checked.
2. **A 0 is "not caught," a gap is "not detected," not death/absence.** ✅ Zero-effort plot-months are dropped (`mnka_series()`, `helpers.R:871`) so CPUE=0/0=NaN can't shatter the line (the SRER "spotty" bug). Seasonal gaps are not read as death. "An empty trap means 'not detected,' not 'absent'" is in the README methods (line 124).
3. **Cross-site scale: CPUE is a within-site index, not a cross-site density.** ⚠️ **This is the one place the app breaks its own rule.** The two-site Compare table (`server.R:824`–`832`) races raw **Captures**, **Individuals**, and **Trap-nights** head-to-head with a `cmp-win` winner-highlight (`server.R:805`–`811`), and the only honesty note is "richness is the raw species count" (`server.R:837`). With a 2× detection spread between biomes, "HARV has more captures than JORN" is mostly a trapability statement, not an abundance one — and the green winner-highlight actively invites the wrong read. **Fix:** (a) add a footnote on the compare table — "Captures and individuals reflect detection as well as abundance; detection runs ~0.57 at closed-canopy temperate sites vs ~0.95 at deserts, so a raw cross-site count is a relative comparison, not a density"; (b) better, add a detection-corrected N̂-per-month and per-site p̂ row to `compare_pack()` (`server.R:781`–`786`) so the comparison the app most prominently offers across biomes carries the correction it computes everywhere else; (c) at minimum, suppress the winner-highlight on Captures/Individuals across sites with materially different p̂.
4. **A capture career is a right-censored floor, not a lifespan; adult-first is left-censored.** ✅ `min_known_lifespan()` is explicit ("biased LOW… absence ≠ death"); `approx_age_years` floors an adult-first animal at ~90 d and flags `age_is_minimum` (`helpers.R:419`–`422`); the AnAge captive max is shown only as a sanity ceiling (`helpers.R:564`–`567`, 608). Shown with "≥". Correct.
5. **Richness is not abundance, and inverts in drylands.** ✅ Hill q0 (richness) and individuals/N̂ (abundance) are separate axes; SRER's 29 species over sparse desert is not read as high density.

**The `id_uncertain` bug is fixed — confirmed.** The prior audit logged a HIGH-severity dead flag: `n_distinct(scientificName)` computed inside the same `summarise()` that reassigns `scientificName = mode_chr(...)` collapsed to 1 for every animal. The current code computes `n_species_ids` in a **separate join outside** the summarise (`helpers.R:375`–`380`), with a clear comment explaining the shadowing gotcha. `id_uncertain = n_species_ids > 1` (`helpers.R:405`) now drives QC flag #7 (`helpers.R:833`) and the `min_known_lifespan()` ambiguous-ID exclusion (`helpers.R:596`). This is the exact "summarise sees earlier new columns" trap the playbook documents — and it is now correctly handled. Do not regress it.

**One remaining surfacing gap (med):** the single-night share (~49% of bouts pooled) is never stated as a first-class number. The detection chip says "estimable bouts (of N)" (`server.R:2526`), which implies it, but a user reading a sparse N̂ series can't tell design from missing data. **Fix:** add a chip or note — "X% of this site's bouts are single-night (index-only)" — computed as `1 - n_estimable_excl_insufficient/n_bouts` on the detection card, so the sparsity reads as a sampling-design fact.

## Place in the suite / cascade

This is the flagship and the source of the suite's shared honest-stats machinery — `species_level_only()`, Hill/Chao1 with CI + instability flag, the deseasonalize-before-correlate scan, the n-gates, the "answer up front" `insight_banner()`. It contributes three things to the climate → plants → **consumers** cascade:

1. **The defensible consumer signal is MNKA, not CPUE** — confirmed by SRER's loose `cor(MNKA,CPUE)=0.51` and the 0.42–0.95 detection spread (DATA-TAKEAWAYS). The cascade should reuse this app's `helpers.R` (`mnka_series`, `bout_closed_capture`) rather than a bespoke CPUE, and label the handed-up signal as detection-qualified.
2. **A deer-mouse-dominated rung nationally, heteromyid at the deserts** — the biome split the cascade depends on. The desert sites where the consumer signal is *Dipodomys* are exactly where the env link is weakest and most lagged, consistent with the pulse–reserve / annual-aggregation-artifact story in project memory.
3. **Corroboration, not the headline.** The robust pooled suite link is temperature → green-up onset; the small-mammal env scan here is the weaker downstream rung. Its honest job is to show *whether* the consumer tracks the plant/climate signal at all — it does, weakly and biome-conditionally — which is the truthful cascade result, not a failure. The one thing I insist on: the signal I hand up must be detection-qualified MNKA before Cass synthesizes it, and the cross-site comparison that feeds the cascade must not be a raw CPUE race (see honesty trap #3).

## Scorecard

| Dimension | Grade | One-line why |
| --- | --- | --- |
| Method fidelity (NEON protocol) | **A** | Bout grouping, ½-TN effort, life-long unique tags, spatial-impossibility flag all correct and cited. |
| Abundance estimators (defensibility) | **A** | Schnabel/Chapman gated by k and ΣR≥3, N̂≥MNKA floor, 1/N-domain CI — exactly the Otis canon. |
| Diversity & condition metrics | **A** | Species-level-only Chao1/Hill over individuals; Chonk honestly refuses SMI. |
| Env-correlation honesty | **A−** | Deseasonalized, n≥8 gate, "lead not cause", r-vs-r² popover — handled right for a hard regime. |
| Cross-site comparison honesty | **C+** | Compare table races raw captures/individuals across biomes with a winner-highlight and no detection warning — the one rule the app breaks. |
| Coverage transparency (single-night share, per-site p̂) | **B** | Computed and shown within-site, but the 49% k=1 share and the cross-site p̂ spread aren't first-class numbers. |
| Analysis-ready export / FAIR | **B−** | Only a per-individual CSV (`qcHistoryCsv`); no tidy site capture table, no MNKA/CPUE/N̂ series, no codebook. |
| QC flag system | **A** | Ranked "verify, not wrong" flags correctly ordered by reliability; `id_uncertain` bug fixed and confirmed. |
| README / docs accuracy | **B+** | Live badge still points to legacy `shinyapps.io/RatTrapHistory`; "140+ species" vs computed 145 — small, but the gold standard should be exact. |

— Mara
