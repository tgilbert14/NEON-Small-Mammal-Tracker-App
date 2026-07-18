# Small Mammal Tracker build/test handoff

This is the durable current-state and chronological evidence record for this app.
Read it completely before work and reread the latest entry immediately before
editing it.

## Current state

- Repository: `tgilbert14/NEON-Small-Mammal-Tracker-App`
- Watched branch: `main`
- Audit baseline commit: `39dca56c69ef11188333effefd4b2d5bc28948ee`
- Product: NEON DP1.10072.001, small mammal box trapping
- Public cover: `https://tgilbert14.github.io/NEON-Small-Mammal-Tracker-App/`
- Public app: `https://019ec337-7100-317e-5052-c3bf32ffcb79.share.connect.posit.cloud/`
- Release state: **P0 OUTAGE / RELEASE UNSAFE** as of 2026-07-18
- Driver disposition: provisional `CONTEXT`; pass not complete

Do not replace this section with optimistic intent. Update it only from verified
evidence and keep the dated entries below.

## Required closeout fields

Every entry records starting state, changed files/classification, learned facts,
commands/environment, expected/actual results, PASS/FAIL/BLOCKED/N/A, artifacts and
hashes, invalidated evidence, failures/cleanup, residual risks, ownership/status,
Driver implication, and next action.

## Chronological evidence

### 2026-07-18 07:30 MST - pass 1 baseline and outage audit / root

- **Starting state:** clean `main` at
  `39dca56c69ef11188333effefd4b2d5bc28948ee`. No app-local instructions or durable
  handoff existed. The repository contains a stray tracked `12` output file and
  tracked editor state under `.vs/` plus `.vscode/`.
- **Objective:** establish the app-local evidence contract, characterize the public
  outage and release path, then correct the source-app/Driver trap-effort parity
  failure without publishing unverified bytes.
- **Public result (FAIL):** the direct Connect Cloud URL rendered Posit's `Startup
  Error` page after reload on 2026-07-18. The GitHub Pages cover rendered, so cover
  availability is not app availability.
- **Manifest result (FAIL):** ten tracked runtime files disagree with their
  manifest MD5 entries: `global.R`, `R/helpers.R`, `R/report_pdf.R`,
  `R/site_metadata.R`, `server.R`, `ui.R`, `www/app.js`, `www/confirm.js`,
  `www/pincards.js`, and `www/styles.css`. The manifest file list has no missing
  paths, but checksum coherence is a release blocker.
- **Workflow result (FAIL):** the refresh workflow uses moving action tags and a
  moving package snapshot, deletes all site bundles before validating their
  replacements, gives write permission to the combined build/validate/publish job,
  and pushes directly to watched `main`. The optional environment overlay is
  visible but can remain stale. The post-deploy smoke is HTTP-oriented and did not
  prevent the current semantic startup failure. Short successful scheduled gate
  runs are skips, not fresh health evidence.
- **Bundle gate result (FAIL):** `scripts/verify_bundle.R` checks only that at least
  one site bundle has rows. It does not require the expected 46-site set, exact
  manifest checksums, package provenance, or boot integrity. A partial pull could
  delete valid sites and publish a subset.
- **Scientific parity result (FAIL):** `clean_mam()` derives effort from the first
  trap-status digit independently on every row. Driver's pinned source audit found
  exact physical-event rules that require multi-capture collapse, reviewed
  double-trap summation, placeholder uncertainty, and fail-closed duplicate/key
  handling. Current source-app CPUE and exports can therefore disagree with Driver.
- **Pinned source facts:** across the Driver's 46-site input snapshot there are 376
  placeholder-coordinate rows, 392 reviewed one-trap multi-capture events, two
  reviewed double-trap events, 79 same-night repeated-tag groups at distinct
  coordinates, zero same-event repeated-tag groups, zero coordinate conflicts, ten
  tagged half-effort rows, and one untagged status-5 row. These are parity targets,
  not yet app-local test results.
- **Documentation result (FAIL):** `docs/project-status.md` is stale relative to
  the completed Connect migration and still lists old deployment concerns.
  Expert/data reviews are useful history but do not carry finding IDs with
  `OPEN/FIXED/VERIFIED` current status.
- **Export result (FAIL):** the captures export actually contains all trap-event
  rows, including empty/not-set rows, while comments call it a capture table. The
  monthly export emits `Nhat_lo` and `Nhat_hi`, but its codebook omits both.
- **Test environment:** repository/workflow/static audit and public in-app-browser
  check only. `Rscript` is unavailable in the current local environment, so no R
  helper, bundle, boot, or manifest test is claimed.
- **Classification/Driver implication:** `suite-platform` plus
  `scientific-contract` and `Driver-impacting`. Driver implication is `HOLD CURRENT
  OUTPUT`: do not alter Driver from this app until parity and pinned knowledge-package
  gates pass; its current consumer links remain context-only.
- **Changed:** added `AGENTS.md`, this handoff, and the initial
  `docs/DRIVER-KNOWLEDGE-PACKAGE.md` scaffold. No production bundle, manifest,
  workflow, app code, public state, or generated artifact changed in this baseline
  entry.
- **Residual risk:** the manifest drift is not proven to be the sole outage cause;
  public diagnostic logs are unavailable. Any code fix remains unverified until a
  matching pinned R/runtime gate runs.
- **Next action:** add the app-local physical-event effort resolver and adversarial
  fixtures; make exports/codebook event-grain-correct; then add exact manifest,
  expected-site, offline-boot, content-aware health, and restricted-publisher gates
  before regenerating or publishing anything.

### 2026-07-18 07:45 MST - source parity and fail-closed release foundation / root

- **Starting state:** continued the uncommitted pass-1 work above from clean-base
  commit `39dca56c69ef11188333effefd4b2d5bc28948ee`; no upstream, bundle,
  manifest, or public deployment was changed.
- **Objective/result:** implemented the app-side physical trap-event contract and
  converted the unsafe release path into a staged, evidence-producing candidate
  flow. This is a working-tree tranche, not a release or an outage-resolution
  claim.
- **Scientific-contract changes:** `R/helpers.R` now requires the seven raw effort
  fields, exact six status tokens, canonical A-J x 1-10 coordinates, the reviewed
  multi-capture and two exact double-trap-marker rules, row-level placeholder
  uncertainty, complete event keys, and fail-closed handling for every unreviewed
  duplicate. Capture identity is nonblank `tagID`. It emits event ID, effort,
  rule, source-row count, and effort-owner audit fields. CPUE uses `is_capture`;
  species search/MNKA uses species capture rows with the full site/month effort
  denominator instead of outcome-conditioned effort.
- **Fixture changes:** added `scripts/test_helpers.R` with exact-token, multi-capture,
  double-trap, placeholder, repeated-tag, incomplete-key, invalid-coordinate,
  invalid-marker, CPUE, and species-denominator contracts. These fixtures are
  authored but **BLOCKED / NOT EXECUTED** locally because `Rscript` is unavailable.
- **Export/docs changes:** server-side export descriptions and the generated
  codebook now identify the event/handling-row grain and include the trap-event
  audit fields plus monthly `Nhat_lo`/`Nhat_hi`. The Driver knowledge package now
  records the working-tree parity state while retaining `HOLD`.
- **Release architecture changes:** added read-only pinned CI; changed refresh to
  build all 46 sites in an empty `SMT_SITE_OUT_DIR`, require the exact site set,
  replace the ephemeral worktree copy only after that gate, run fixtures/bundle/
  manifest/offline-source checks, upload an immutable candidate, and give write
  permission only to a final job that updates a review branch/PR. Automated code
  no longer pushes refreshed bytes to watched `main`. Added a separate main-push
  semantic-health workflow that requires the app-specific ready marker and opens
  or updates an outage issue on failure.
- **Manifest/integrity changes:** exact 46-site/schema/index/loadability/file-MD5
  checks replace the partial-bundle floor. Package checks now require R 4.5.2, the
  dated jammy RSPM snapshot `2026-07-15`, all runtime roots, the explicit
  geospatial closure, CRAN provenance, and absence of `neonUtilities`/`arrow`.
  `write_manifest.R` now writes that dated snapshot instead of `latest`. The
  committed manifest was intentionally not hand-regenerated and remains a known
  blocker until pinned CI produces and verifies a candidate.
- **Health changes:** `ui.R` emits
  `ddl-app-ready=small-mammal-tracker-v1`; `post_deploy_smoke.sh` downloads the
  response body, rejects common Posit startup/error language, and requires that
  marker for the app endpoint rather than trusting HTTP 200 alone.
- **Static verification (PASS):** Ruby parsed `.github/workflows/ci.yml`,
  `refresh-data.yml`, and `post-deploy.yml`; `bash -n` parsed
  `scripts/post_deploy_smoke.sh`; `git diff --check` passed. The pinned official
  action commits used are checkout `de0fac2...`, r-lib/actions `d3c5be...`,
  upload-artifact `043fb46...`, and download-artifact `3e5f45b...`.
- **Runtime verification (BLOCKED):** there is no local R, Docker, or Podman
  runtime. No helper, raw-bundle parity, manifest regeneration, offline app boot,
  Shiny interaction, or Connect build test is claimed. Public app state remains
  the previously observed Posit `Startup Error`; semantic smoke has not been run
  against a restored deployment.
- **Changed/classification:** modified `.github/workflows/refresh-data.yml`,
  `R/helpers.R`, `scripts/build_search_index.R`, `scripts/post_deploy_smoke.sh`,
  `scripts/refresh_data.R`, `scripts/verify_bundle.R`, `scripts/write_manifest.R`,
  `server.R`, and `ui.R`; added `.github/workflows/ci.yml`,
  `.github/workflows/post-deploy.yml`, `AGENTS.md`, this handoff,
  `docs/DRIVER-KNOWLEDGE-PACKAGE.md`, and `scripts/test_helpers.R`.
  Classification is `app-local`, `suite-platform`, `scientific-contract`, and
  `Driver-impacting`.
- **Failures/cleanup:** the first local Ruby command used an unsupported
  `YAML.load_file(..., aliases:)` keyword on system Ruby 2.6; reran with
  `YAML.safe_load` successfully. The required publish preflight then found the
  saved `tgilbert14` GitHub CLI token invalid; no branch, commit, push, PR, or
  workflow run was created. No temporary artifact or partial production data was
  created. No tracked bundle/index/manifest bytes were modified.
- **Ownership/status:** all listed changes are current-session work owned by
  `root`; they are uncommitted and unpublished. The pass remains **IN PROGRESS / P0
  OUTAGE / RELEASE UNSAFE**.
- **Driver implication:** `HOLD CURRENT OUTPUT`. The source definition now mirrors
  Driver in code, but Driver inputs/claims must not change until pinned fixture and
  46-site parity evidence pass and the knowledge package is promoted from draft.
- **Residual risks:** workflow syntax has not yet been accepted by GitHub Actions;
  package-version availability/build compatibility on Connect remains unproven;
  the current public outage cause is not isolated; `docs/project-status.md` and
  finding-state documentation are still stale; UI funnels, accessibility, mobile,
  cover imagery, and visual retheme remain for the product pass after release
  restoration.
- **Next action:** reauthenticate with `gh auth login -h github.com`, publish this
  tranche to a non-watched review branch, let pinned PR CI produce the manifest/
  runtime evidence, address failures without merging, then restore and semantically
  verify production before completing the product/UI pass or reintegrating any
  metric into Driver.
