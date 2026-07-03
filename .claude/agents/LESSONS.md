# LESSONS — NEON Small Mammal Tracker (project-local)

> Project-specific institutional memory for THIS app. Agents boot cold: read this on start (grep for your
> own name, `· <agent> ·`) and append a one-line lesson after a run that taught something durable.
>
> The **canonical, cross-cutting** log lives in `TG-Data-Apps/.claude/agents/LESSONS.md`; the deep NEON
> methodology lives in `docs/neonize-playbook.md`. `curator` promotes recurring lessons up to the canonical
> log and into the owning agent's `.md`. Format + protocol: `TG-Data-Apps/.claude/agents/_CONVENTIONS.md`.

## How to write an entry
```
- [YYYY-MM-DD] <agent> · <verdict: confirmed|over-flagged|wrong|gap> · <the durable lesson, one line>
```

## Lessons

<!-- newest at the bottom; append, don't rewrite history. Seeded 2026-07 from the playbook + connor. -->
- [2026-07-01] connor · confirmed · Pin `terra` to `1.8-50` in `manifest.json` (Connect's GDAL 3.4.1 can't compile terra ≥ 1.8-54's GDAL-3.8 multidim code; leaflet→raster→terra; install-only, zero runtime impact). This app's CI regenerates the manifest, so the re-pin must ALSO live in the manifest-writing step or the monthly refresh re-breaks the deploy.
- [2026-07-01] neonize · confirmed · Deploy = direct `git push` to `main` (Connect Cloud auto-republishes); no shinyapps secrets, no `deploy.R`. Confirm the build goes GREEN — a failed publish silently leaves the previous good build serving, so "the app still loads" is not proof your change deployed.
- [2026-07-01] neonize · gap · `skip_download` in `refresh-data.yml` must gate BOTH the fetch AND the bundle step, or the bundler runs on missing `<SITE>_raw.rds` and dies.
- [2026-07-01] hadley · confirmed · dplyr `summarise()` evaluates sequentially — the flagship's `id_uncertain` flag read a modal length-1 scalar and fired 0/93,169 despite a real ~4–12% congener-swap rate. Recompute a QC flag's base rate from raw data; a flag that fires 0 times is guilty until proven innocent.
- [2026-07-01] neonize · confirmed · Recompute any landing/README headline straight from the `.rds` (a one-line `count()`/`cor()`) — it caught "140+ species" that was actually 145.
- [2026-07-01] mara · confirmed · Label the abundance metric honestly: MNKA and CPUE are indices, not population size; detection-corrected N̂ (Schnabel ≥3 nights / Chapman 2) needs enough recaptures or it's gated. A detection-index ≠ a population.
- [2026-07-03] connor · confirmed · CDN-vendored client-side libs (www/vendor/*) deploy clean on Connect IF: (1) each file is git-committed AND in the manifest files-map with a checksum that matches the on-disk md5 (verify with md5sum vs the manifest checksum — a stale hash = silent bundle mismatch), (2) referenced ONLY via tags$script/tags$link in ui.R with a RELATIVE href/src ("vendor/x", no leading slash — a leading /vendor/ breaks behind Connect's per-app URL prefix), (3) nothing in the R boot path (global.R/server.R/R/*) fetches them. Vendored files are client-side only, so they are NOT R packages and never touch the terra/GDAL compile. onerror data-cdn fallback stays dormant when local files 200.
- [2026-07-03] fable · confirmed · QC-flag lifecycle: a possible-error flag (±5·MAD) is not a dead-end tooltip. ONE shared rule (`.is_meas_outlier` + `species_adult_values` in helpers.R) feeds ALL surfaces so a value flagged in one place is flagged in every place — the species-table ⚠, the QC modal (tags now DRILL to the animal's dossier via `pick_individual` + `input$qcDrillTag`), a red ⚠ ring on measPlot + morphoPlot, an inline ⚠ in capHistory, AND the Chonk exclusion (`compute_condition` drops flagged weights before `percent_rank` so one bad reading can't skew a rank or pollute the species pool; the "heaviest" annotation skips a flagged high outlier too), with the exclusion disclosed in the chonk caption. The rule: a flagged value must be traceable to its source, marked everywhere it's drawn, excluded from every derived metric, and the exclusion disclosed.
