# Project status: NEON Small Mammal Tracker

> Read this before flagging anything. Maintained by `triage` and by the app's owner session after every
> review or ship. The **Deliberately deferred** section is binding: a cold-boot agent does not re-flag what it
> lists. Suite doctrine: https://github.com/tgilbert14/NEON-Driver-Cascade/blob/master/docs/neonize-playbook.md
> App authority: `docs/BUILD-TEST-HANDOFF.md`, `DEPLOY.md`, `docs/audit-fixes/`.

As of: 2026-10-26 · Version: `main` @ `c1a229d` (2026-10-21) · Stage: live; the audit finish PR is in review

## Release and data state

| Item | Value | How to check |
|---|---|---|
| Live app | https://019ec337-7100-317e-5052-c3bf32ffcb79.share.connect.posit.cloud/ | TG-Data-Apps `monitoring/sites.tsv` |
| Cover | https://tgilbert14.github.io/NEON-Small-Mammal-Tracker-App/ | |
| Deployed revision | `c1a229d` (live checks 2026-10-20: title, lang, SRER breeding curve, per-grid N-hat) | after the finish merge, `post-deploy.yml` waits for the live `ddl-build` meta to match the merged tree |
| NEON product | `DP1.10072.001` · `RELEASE-2026` · DOI https://doi.org/10.48443/a83h-tb34 | `data/bundle_meta.json` (written by the producer from the finish PR on) |
| Data through | `2024-11-12` (records 2013-06-19 to 2024-11-12, 46 sites) | the landing view; NEON publishes newer data only as PROVISIONAL, which the app does not bundle |
| Review branch | `automation/small-mammal-data-refresh` carries the audit finish candidate (code plus validated data) | `gh api repos/tgilbert14/NEON-Small-Mammal-Tracker-App/compare/main...automation/small-mammal-data-refresh` |
| Last executed refresh | run 40346798927, 2026-10-26 (full pull plus the one-time precipitation history rebuild) | TG-Data-Apps `node .claude/scripts/neon-suite-probe.mjs --app small-mammal` |
| 60-day schedule cutoff | 2026-12-20 (from the main head commit) | the probe |
| Suite lint | 0 red, 0 yellow on the finish tree, 2026-10-26 | TG-Data-Apps `node .claude/scripts/neon-suite-lint.mjs --app small-mammal` |
| Open PRs, in merge order | the audit finish PR (from the review branch); #65 (recommended to close) | `gh pr list --repo tgilbert14/NEON-Small-Mammal-Tracker-App` |

## Now (P1: blocks a ship, a refresh, a demo or a user)

- [ ] Merge the audit finish PR, then confirm `post-deploy.yml` passes on the merged revision · owner `Tim` · evidence: the PR's CI and the post-deploy run

## Next (P2: real, not urgent)

- [ ] Replace the circular-shift null for the monthly driver scan with one that rotates in calendar time (or a block bootstrap); until then the verdict gate is p < 0.02, calibrated in `docs/audit-fixes/2026-10-22-env-null-calibration.md` · owner `hk` (hutch, fisher)
- [ ] Bump `NEON_RELEASE_PIN` in `scripts/producer_lib.R` when RELEASE-2027 lands (late January); `release_watch` opens a `neon-release` issue · owner `Tim` or `neonize`

## Later (P3: polish)

- [ ] Browser end-to-end coverage (Playwright) for the picker, Back/Forward and downloads; today `scripts/test_server.R` covers the server side of those flows and the suite lint covers keepInView
- [ ] Replace the vendored sweetalert2 11.10.0 with a library without the upstream region-triggered block, or the native `<dialog>` element

## Deliberately deferred (DO NOT re-flag: decided, with the reason)

- **HK plan items 2, 5 (iNEXT), 6 (N-mixture), 7 (trend GLMM), 8 (Bayesian driver model), 9 (precompute bundle), 10 (cache keys), 12 and the editorial pass (PR #65)**: deferred because each adds packages or offline fits that touch the terra/manifest closure, and none is a defect; the compare-panel richness gate shipped with base-R Hurlbert rarefaction instead of iNEXT, and cache keys are moot because Connect restarts the app on every bundle change. Decided by the owner session on 2026-10-26. Reopen when a model upgrade is scheduled with HK sign-off.
- **NEON's `trapsSet` cross-check (HK plan item 0, gap 3)**: RELEASE-2026's `mam_perplotnight` does not publish `trapsSet` or `mammalGridSamplingType` (refresh run 40346798927). Decided by the owner session on 2026-10-26. Reopen when a release carries them; the producer already joins them when present.
- **MNKA counts each placeholder capture event once, including the few flagged recapture Y**, while the closed-capture estimator drops the recapture-Y ones (SRER 42 of 9,962 captures). Deferred because the effect is under 1% and both definitions are documented. Decided by the owner session on 2026-10-26. Reopen if a site's share exceeds 2%.
- **canvas-confetti CDN fallback**: jsDelivr regenerates that minified file, so its bytes differ from the vendored copy and it carries no SRI hash; the vendored file serves, and the unhashed fallback is skipped by design. Decided by the owner session on 2026-10-26. Reopen if the vendored file is ever removed.
- **The cover tagline "Meet the tiny lives reshaping the landscape."**: kept as owner-approved copy; the meta sentence apologising for it was removed. Decided by Tim (the audit left the tagline to him). Reopen only on Tim's word.
- **Suite decisions already settled (never re-flag in this repo):** the review-branch refresh contract (a person's merge of the review branch is the deploy; no workflow pushes the watched branch); `skip_download` runs may publish to the review branch; terra pinned at 1.8-50; `neonUtilities` out of `manifest.json`; SRER as the default site; no visible AI-art badges; the copy voice (no em dashes, no pitch, no hype, no sincerity labels, never state what the reader can already see).

## Owner decisions pending

- Close PR #65 (the June HK plan): recommendation close, because #96/#97 and the audit finish PR cover or defer every item · asked on 2026-10-26 in a comment on PR #65
- Delete the `SHINYAPPS_NAME`, `SHINYAPPS_SECRET` and `SHINYAPPS_TOKEN` repository secrets and revoke the shinyapps.io token (no workflow reads them) · asked on 2026-10-26 in `DEPLOY.md`
- Set `CARTO_BASEMAP_KEY` on the Connect content and republish (without it the maps use Esri's grey canvas) · asked on 2026-10-26 in `DEPLOY.md`
- Turn on secret scanning, push protection and Dependabot security updates, and add a `main` ruleset (PR required, no force-push or deletion) · options: settings now / later · recommendation now
- `.claude/launch.json` is tracked and holds Windows paths with a user name (the same exposure the audit found in `.vs/`): untrack it, or keep it as the owner's local preview config · recommendation untrack

## Recently shipped (so cold-boot agents do not re-open it)

- 2026-10-21 · indexes rebuilt on the real-tag estimator · PR #98 · merge `c1a229d` · live individuals and p-hat checked 2026-10-20 on the preceding merges
- 2026-10-21 · k from trapped nights, marked-only individuals · PR #97 · merge `2441fd0`
- 2026-10-21 · refresh cron and calendar gate off the midnight edge, token probe · PR #94 · merge `ba0ea9b`
- 2026-10-20 · real-tag closed-capture estimator, mean-per-grid roll-up, Poisson and log-normal intervals · PR #96 · merge `cf6902b`
- 2026-10-20 · exact reproductive-state tokens, QC card crash, page title and lang · PR #95 · merge `33b4716` · live SRER breeding curve Mar 41 / May 76 / Nov 22

## Keeping this file true

- "As of" and "Version" come from git, never from memory or from this file.
- An item moves to Recently shipped only with a merge SHA and a live check.
- When a deferral's reopen condition is met, move it back to Now or Next with the evidence.
