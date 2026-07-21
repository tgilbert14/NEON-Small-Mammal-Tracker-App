# Curator promotion queue → canonical `TG-Data-Apps`

> For **`curator`**, run in a session that has `TG-Data-Apps` in scope. This lists what this cross-agent
> pass added to the **flagship** copies that should be promoted to the **canonical** playbook / LESSONS so
> every suite app and both tools (Claude Code + ChatGPT/Codex) inherit it. Promote, then sync the per-repo
> copies. Check an item off when done. (Source PRs: #88 merged, #89 small-mammal, #11 veg, #43 Driver.)

## Promote into the canonical `docs/neonize-playbook.md`
- [ ] **§9 "How we work"** — plan/question/challenge before coding · curator-gated skills promotion ·
  know-your-tools + suggest new ones · always propose an improvement.
- [ ] **§8 "Working across agents"** + the **repo-map / branch-defaults** note — shared front door
  (`CLAUDE.md` + `AGENTS.md` → same tool-neutral source of truth), the `[tool]`-tagged handoff log as the
  async channel, cross-vendor PR review, contracts-as-the-trust-layer; and the split **`main` (Small
  Mammal, Vegetation) vs `master` (Driver-Cascade)** — documented, not renamed.
- [ ] **§6 manifest byte-determinism recipe** — strip `Built`, canonicalize the geo-pin lane, targeted
  text-substitution (never a `jsonlite` reserialize), pin `platform` + `locale=C` — plus the "no-local-R
  merge loop" and the `Regenerate manifest (manual)` reference workflow.
- [ ] **§7 Cover Contract** — `check_cover.mjs` / `check_in_app_landing.mjs` as mechanical enforcement of
  the Living Poster frame + `IMAGE-PROVENANCE.md` as a required hash-linked artifact + asset hygiene.

## Promote as a suite-standard doc
- [ ] **`docs/COVER-MOTION-KIT.md`** — the cover/motion **starting kit** (archetype, per-app skin, the
  reduced-motion-safe motion layer, the scroll-video ideas, the ChatGPT→Higgsfield asset pipeline, per-app
  divergence, the floor). Make it the reference every cover build starts from and alters per project.

## Promote into the canonical `.claude/agents/LESSONS.md`
- [ ] `connor` — manifest **byte-determinism** (both `manifest.json` and, where present, `search_index.rds`).
- [ ] `neonize` — the **CI-only manifest merge loop** (`contents: read` + no local R) and its escape hatch.
- [ ] `connor` — **Driver's semantic gate** (`compare_manifests.R`) is the more *robust* pattern than
  byte-exact `git diff`; a convergence candidate for the siblings — **not urgent** (they already de-flapped
  via the determinism recipe).
- [ ] `cass` — Driver is a **derived app** (clone siblings, run `build_cascade.R`); do NOT copy the
  siblings' regenerate workflow here.
- [ ] `mara` — Vegetation **channel separation** (`tree_dbh` vs `shrub_sapling_basal`).

## Then sync the per-repo copies
- [ ] Every suite repo has a `CLAUDE.md` + `.claude/agents/LESSONS.md` (done for veg #11 + Driver #43;
  verify the rest of the suite — Ground Beetle, Plant Diversity, Phenology, Breeding Birds, Mosquito Pulse).
- [ ] Each repo's `AGENTS.md` + `CLAUDE.md` front door points at the canonical source of truth and states
  its branch default; `.gitignore` tracks `.claude/agents/LESSONS.md` (Driver needed a targeted exception).

*Queue authored 2026-07-20 by the cross-agent pass. Delete this file once the queue is drained.*
