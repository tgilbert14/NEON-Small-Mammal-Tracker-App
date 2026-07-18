# NEON Small Mammal Tracker — working context for Claude

> Read this first. It orients an agent that boots cold in this repo. Depth lives in the docs it points at.
> This is a **Desert Data Labs (DDL)** project; the DDL business context + the full agent suite live in the
> `TG-Data-Apps` repo (and in user scope, so every agent is available here too).

## What this is

A **Shiny web app** exploring NEON's **small-mammal box-trapping** data product (**DP1.10072.001**) — it
reconstructs each captured animal's history from its ear-tag and turns ~46 field sites of capture records
into maps, per-individual profiles, abundance estimates, and diversity reads. It is the **flagship** of the
DDL **NEON explorer suite** and the reference build every sibling app is measured against.

- **Public state verified 2026-07-18:** the Pages landing works; the Connect app returns `Startup
  Error`. Draft PR #73 is the release-recovery branch. Do not call production healthy before semantic
  post-deploy smoke passes.
- **Stack:** R / Shiny · `global.R` + `server.R` + `ui.R` + `R/*.R` helpers · per-site `.rds` data bundles
  (no network round-trip at runtime) · Leaflet maps · `manifest.json` for Connect Cloud.

## The stack + how it deploys (the load-bearing facts)

- **Deploy = reviewed merge to watched `main`.** Connect is git-backed, but refresh automation writes
  only `automation/small-mammal-data-refresh` and opens/updates a PR. No workflow or agent pushes
  production directly. No shinyapps secrets. See `DEPLOY.md` and `docs/neonize-playbook.md`.
- **The terra/GDAL landmine (the #1 publish killer).** Connect compiles native packages from source on
  jammy (**GDAL 3.4.1**); `leaflet → raster → terra`, and terra ≥ 1.8-54 needs GDAL 3.8 → **pin `terra` to
  `1.8-50`** and the seven-package companion closure as real installed CRAN sources. The manifest writer
  verifies actual versions and repository lanes; never rewrite Version/RemoteSha after generation.
- **Auto-refresh:** `refresh-data.yml` runs monthly (first-Saturday gate, off-peak AZ), rebuilds and
  validates a candidate, then proposes it on a review branch. A `skip_download` input gates the fetch
  and bundle replacement for a fast pipeline test.
- **Hosting migration is complete.** Connect Cloud is the only runtime target; the retired
  shinyapps.io records and deploy script are absent.

## Which agents own what here

- **`neonize`** — the suite methodology: build/upgrade this or a sibling to the gold standard, the QC-flag
  system, site-picker map contract, auto-refresh CI, suite cohesion. Reads/writes `docs/neonize-playbook.md`.
- **`connor`** — Connect Cloud deploy: failed publishes, the terra pin, manifest correctness, redeploy.
- **`mara`** — the DP1.10072.001 domain expert (small-mammal trapping science: MNKA vs CPUE vs Schnabel/
  Chapman, mark-recapture, tag identity). **`cass`** — the cross-product Driver-Cascade synthesis.
- **`hk`** and its stats team (Hadley, Tukey, Fisher, Stan, Hutch, Tobler, Tufte, Joe, Few) — the statistics.
- **`vgs` (R/Shiny mode)** — a full team review of the app.
- Call any of them by name (`run neonize`, `ask connor`, `run HK`); they're installed in user scope.

## The learning loop (this repo)

- **`.claude/agents/LESSONS.md`** — project-local, one-line lessons for THIS app. Agents read it on cold
  boot and append to it after a durable run. The **canonical, cross-cutting** LESSONS.md lives in
  `TG-Data-Apps`; `curator` promotes recurring lessons up.
- **`docs/neonize-playbook.md`** — the deep methodology playbook (the real learning surface for the suite);
  `docs/project-status.md` — current version + open items + what's deferred. Read both before flagging work.

## Working notes

- **Default the demo/example site to SRER** (or an AZ/desert site) — DDL is AZ-based and the desert is the
  clearest story.
- **Honesty discipline:** the caveat goes ON the number (detection-index ≠ population; n-gate short series;
  publish match rates; snapshot-not-pooled). Recompute any headline count straight from the `.rds` before
  trusting it, and recompute a QC flag's base rate from the raw data — a flag that fires 0 times is guilty
  until proven innocent.
