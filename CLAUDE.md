# NEON Small Mammal Tracker — working context for Claude

> Read this first. It orients an agent that boots cold in this repo. Depth lives in the docs it points at.
> This is a **Desert Data Labs (DDL)** project; the DDL business context + the full agent suite live in the
> `TG-Data-Apps` repo (and in user scope, so every agent is available here too).
>
> **Worked by more than one agent (Claude Code + ChatGPT/Codex).** The tool-neutral source of truth
> is the suite playbook (canonical home `../NEON-Driver-Cascade/docs/neonize-playbook.md`; the local
> `docs/neonize-playbook.md` is only a pointer), `.claude/agents/LESSONS.md`, `docs/BUILD-TEST-HANDOFF.md`, and
> `docs/project-status.md` — read those first; `CLAUDE.md` / `AGENTS.md` only add tool-specific notes
> on top. Close every session with a dated `BUILD-TEST-HANDOFF.md` entry tagged with your tool
> (`[Claude]` / `[Codex]`) and a one-line **next action**, so the other agent can pick up cold. See
> canonical playbook §8 (https://github.com/tgilbert14/NEON-Driver-Cascade/blob/master/docs/neonize-playbook.md).
>
> **Before you code: plan, question, and challenge the work** — and always name at least one
> improvement you spot. The how-we-work principles are the canonical playbook §9.

## What this is

A **Shiny web app** exploring NEON's **small-mammal box-trapping** data product (**DP1.10072.001**) — it
reconstructs each captured animal's history from its ear-tag and turns ~46 field sites of capture records
into maps, per-individual profiles, abundance estimates, and diversity reads. It is the **flagship** of the
DDL **NEON explorer suite** and the reference build every sibling app is measured against.

- **Live state:** recorded in `docs/project-status.md` with the revision it was checked on. Do not call
  production healthy before the post-deploy smoke passes on the merged revision (it checks the
  `ddl-build` digest, the title and `lang`).
- **Stack:** R / Shiny · `global.R` + `server.R` + `ui.R` + `R/*.R` helpers · per-site `.rds` data bundles
  (no network round-trip at runtime) · Leaflet maps · `manifest.json` for Connect Cloud.

## The stack + how it deploys (the load-bearing facts)

- **Deploy = reviewed merge to watched `main`.** Connect is git-backed, but refresh automation writes
  only `automation/small-mammal-data-refresh` and opens or updates one `data-refresh` issue (never a PR).
  No workflow or agent pushes production directly. No workflow reads the legacy `SHINYAPPS_*` secrets. See `DEPLOY.md` and the canonical playbook §6.
- **The terra/GDAL landmine (the #1 publish killer).** Connect compiles native packages from source on
  jammy (**GDAL 3.4.1**); `leaflet → raster → terra`, and terra ≥ 1.8-54 needs GDAL 3.8 → **pin `terra` to
  `1.8-50`** and the seven-package companion closure as real installed CRAN sources. The manifest writer
  verifies actual versions and repository lanes; never rewrite Version/RemoteSha after generation.
- **Auto-refresh:** `refresh-data.yml` runs every Saturday 13:17 UTC and a calendar gate proceeds in the
  first week of the month. Bundles are pinned to NEON RELEASE-2026, so scheduled runs skip the pull until
  the pin moves; a `release_watch` job opens a `neon-release` issue when NEON publishes a newer release.
  A validated candidate goes to the review branch with one `data-refresh` issue. `skip_download` rebuilds
  indexes and the manifest from committed bundles; `env_precip_full` re-pulls precipitation history.
- **Hosting migration is complete.** Connect Cloud is the only runtime target; the retired
  shinyapps.io records and deploy script are absent.

## Which agents own what here

- **`neonize`** — the suite methodology: build/upgrade this or a sibling to the gold standard, the QC-flag
  system, site-picker map contract, auto-refresh CI, suite cohesion. Reads/writes the canonical playbook in `NEON-Driver-Cascade`.
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
- **The suite playbook** (`../NEON-Driver-Cascade/docs/neonize-playbook.md`; the local copy is a pointer) is the deep methodology playbook (the real learning surface for the suite);
  `docs/project-status.md` — current version + open items + what's deferred. Read both before flagging work.

## Working notes

- **Default the demo/example site to SRER** (or an AZ/desert site) — DDL is AZ-based and the desert is the
  clearest story.
- **Honesty discipline:** the caveat goes ON the number (detection-index ≠ population; n-gate short series;
  publish match rates; snapshot-not-pooled). Recompute any headline count straight from the `.rds` before
  trusting it, and recompute a QC flag's base rate from the raw data — a flag that fires 0 times is guilty
  until proven innocent.
