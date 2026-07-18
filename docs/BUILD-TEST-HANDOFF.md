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

### 2026-07-18 08:19 MST - draft PR and first pinned CI result / root

- **Starting/published review state:** macOS-keyring authentication passed outside
  the sandbox. Created branch `agent/small-mammal-release-foundation`, committed the
  exact 15-file pass-1 scope as
  `5a0aa46f972c16f608e0ff08ff138182a1709688`, pushed only that non-watched branch,
  and opened draft PR #73. Watched `main`, production data, manifest, refresh, and
  Connect deployment remain untouched.
- **PR creation path:** the GitHub connector returned `403 Resource not accessible
  by integration`; the authenticated `gh pr create` fallback opened
  `https://github.com/tgilbert14/NEON-Small-Mammal-Tracker-App/pull/73` as a draft.
  The local publish-body file is outside this repository and is not a release
  artifact.
- **Static pre-push result (PASS):** Ruby 2.6 `YAML.safe_load` parsed all three
  workflows, `bash -n scripts/post_deploy_smoke.sh` passed, and staged
  `git diff --check` passed after removing two Markdown trailing-space markers and
  one extra EOF blank line. No R result is included in this PASS.
- **Pinned CI result (FAIL):** Actions run `29649574212`, job `88093462881`, on
  exact head `5a0aa46f...` passed checkout and R 4.5.2 setup, then failed the
  dependency-install step. All later source, helper, manifest, bundle, offline-boot,
  artifact, and committed-match steps were skipped and remain untested.
- **Observed root cause:** setup-r-dependencies resolved and source-built
  `terra 1.9-34` even though the later manifest policy expects `terra 1.8-50`.
  Terra 1.9-34 called the three-argument `GDALMDArray::AsClassicDataset`, but the
  pinned Ubuntu 22.04 runner exposes GDAL 3.4.1's two-argument API; compilation
  stopped in `gdal_multidimensional.cpp`. This proves the dated repository alone
  does not install the declared known-good geospatial closure, and the current
  manifest writer's post-hoc version rewrite is not adequate package provenance.
- **Focused proposed fix (approval pending):** install the declared geospatial
  closure as actual version-pinned CRAN package sources in both CI and refresh,
  stop mutating package versions/RemoteSha after manifest generation, and retain
  fail-closed checks that the installed versions and dated repository metadata
  match policy. Rerun PR CI before touching any later gate or production.
- **Classification/Driver implication:** `suite-platform` and
  `scientific-contract`; Driver implication remains `HOLD CURRENT OUTPUT` with no
  Driver byte change.
- **Failures/cleanup/ownership:** the bundled CI-inspection script first failed
  because `python` is absent; rerunning it with available `python3` retrieved the
  exact log. The completed watch process was closed. Repository worktree now has
  only this required handoff update beyond pushed head `5a0aa46`; no generated
  data, manifest, cache, lock, stage, backup, or production state changed.
- **Residual risk:** the exact archive/reference syntax and full closure still need
  a green pinned run; helper/R syntax, raw 46-site parity, manifest semantics,
  offline boot, UI, and public health remain unverified. The app remains a P0
  outage and PR #73 must stay draft/unmerged.
- **Next action:** after explicit approval, pin the real geospatial install inputs,
  remove post-hoc manifest version mutation, statically verify, commit/push the
  focused fix plus this handoff, and inspect the next PR run without weakening any
  downstream gate.

### 2026-07-18 09:28 MST - real closure passes; isolated helper pipe fails

- **Approved/published fix:** explicit approval was received. Commit
  `1bd1e24b088ebf8b7a953b9f4afb5bddbaccf850` installs the declared geospatial
  versions from exact CRAN source URLs in both CI and refresh, removes post-hoc
  manifest platform/version/RemoteSha mutation, and retains fail-closed checks of
  generated manifest truth. It was pushed only to draft PR #73; `main`, production,
  the watched refresh path, and the user's separate idea branch remain untouched.
- **Pinned CI evidence (partial PASS / terminal FAIL):** Actions run
  `29650640599`, job `88096273438`, on exact head `1bd1e24...` completed the full
  native dependency build in 16m40s. Dependency installation, pinned R 4.5.2,
  deterministic Haswell/single-thread OpenBLAS, complete R/JS/shell parsing, and
  scientific helper contracts 01-10 passed. This closes the prior terra/GDAL
  compile failure. Manifest generation, bundle/index/checksum verification,
  offline app source, artifact upload, and committed-match remained skipped.
- **Observed next root cause:** `scripts/test_helpers.R` intentionally sources
  `R/helpers.R` without `global.R`. `mnka_series()` then reached an unqualified
  `%>%` and failed because the pipe existed only as an ambient effect of
  `global.R` attaching dplyr. Production startup attaches dplyr first, but the
  analytical module was not independently sourceable as its fixture contract
  requires.
- **Approved focused repair:** bind `%>%` explicitly from the already-declared
  dplyr dependency at the top of `R/helpers.R`; do not weaken the isolated fixture
  by attaching packages in the test. The user approved completing the whole app,
  including continued release/science gates followed by product/UI work.
- **Classification/Driver implication:** `app-local` and `scientific-contract`;
  Driver implication remains `HOLD CURRENT OUTPUT`. This binding has no intended
  metric-semantic change and no Driver bytes change.
- **Cleanup/residual risk:** the completed watcher exited normally on the failed
  check and the bundled inspector retrieved the exact log. No data, manifest,
  cache, lock, artifact, or deployment state changed. The rest of the fixture and
  every later CI gate remain unverified until the next exact-head run.
- **Next action:** statically verify the explicit pipe binding, commit/push it plus
  this evidence to the existing draft branch, and watch CI through the remaining
  gates without relaxing their contracts.

### 2026-07-18 09:47 MST - all helper contracts pass; repository-lane gate next

- **Published repair:** commit
  `587b15747f322ea4b161ee954e63b0b4fa9a1b77` explicitly binds the dplyr pipe in
  `R/helpers.R` and was pushed only to draft PR #73.
- **Pinned CI evidence (partial PASS / terminal FAIL):** Actions run
  `29652072343`, job `88099991390`, on exact head `587b157...` rebuilt the native
  closure, passed R 4.5.2, deterministic OpenBLAS, complete source/static parsing,
  and **all 11 scientific helper contracts**. It then generated a 91-package
  manifest candidate before the repository-provenance gate stopped the run.
  Bundle/index/checksum verification, offline source, artifact upload, and
  committed-match remained skipped.
- **Observed root cause:** exact `url::` CRAN tarball installs truthfully record
  top-level manifest `Repository: CRAN`, while ordinary dependencies record the
  dated Posit snapshot URL. The gate required one repository string for all 91
  packages and therefore rejected the honest two-lane result
  (`[dated snapshot, CRAN]`). This is a gate-model bug, not package drift.
- **Focused repair:** retain exact version checks for all eight geospatial pins;
  require `CRAN` only for those eight URL-installed packages; require the dated
  Posit snapshot for every other package; reject blank, crossed, or third
  repository values. Do not rewrite repository provenance after generation.
- **Classification/Driver implication:** `suite-platform`; Driver implication
  remains `HOLD CURRENT OUTPUT`. No metric or Driver byte changes.
- **Cleanup/residual risk:** no generated manifest candidate was uploaded because
  the gate failed before the artifact step. No data, cache, lock, branch, main,
  deployment, or separate idea-branch state changed. Exact manifest semantics,
  bundle parity, offline source, and public restoration remain unverified.
- **Next action:** statically verify and push the repository-lane gate, then rerun
  the same draft CI through the first still-unreached bundle/offline gates.

### 2026-07-18 10:08 MST - exact bundle passes; downstream provenance gate aligned

- **Published input:** commit `e02361c2a6e19d0cf822a1730f93928791056be0`
  implemented the honest two-lane manifest writer gate and was pushed only to draft
  PR #73. It did not touch `main`, Connect, production data, or the user's separate
  idea branch.
- **Pinned CI evidence (partial PASS / terminal FAIL):** Actions run
  `29652716146`, job `88101660852`, passed the actual source-built eight-package
  geospatial closure, pinned R 4.5.2, deterministic Haswell/single-thread OpenBLAS,
  complete R/JS/shell parsing, all 11 scientific helper contracts, and manifest
  generation. The exact bundle gate loaded all **46/46** expected sites with rows
  and required effort schema; `site_index.rds` passed with 46 rows and both
  `search_index.rds` and `species_ranges.rds` passed with 604 rows. The generated
  manifest listed 117 runtime files.
- **Observed next root cause:** `scripts/verify_bundle.R`, downstream of the corrected
  writer, still demanded the dated Posit snapshot for every package. It therefore
  rejected the same eight exact-URL packages whose truthful `Repository: CRAN`
  provenance the writer had just accepted. This is a duplicated-policy mismatch,
  not package, site, schema, checksum, or scientific drift.
- **Focused repair:** the bundle gate now requires `CRAN` for exactly the eight
  pinned geospatial packages and the dated snapshot for every ordinary package,
  while retaining package-name, nonblank-version, `Source: CRAN`, exact pin, R,
  runtime-root, forbidden-package, file-existence, and MD5 checks. No manifest field
  is rewritten to satisfy the validator.
- **Product/UI/documentation pass:** the previously blank 849-byte social image was
  replaced with a reviewed 1200x630 desert-night habitat card, retaining its
  1774x887 source as `docs/og-habitat-v2.png`. The landing page no longer turns an
  opaque no-CORS pre-warm response into a false “ready” status and now has explicit
  keyboard focus rings. README, deploy, project-status, data-takeaway, agent-context,
  and playbook authority now state the verified outage and restricted review-merge
  release contract. The in-app About panel now links all nine companions in the
  ten-app suite and describes bundle-only production plus reviewed monthly updates.
- **Visual verification (PASS, cover only):** the revised Pages source rendered in
  the in-app browser at desktop width with no horizontal overflow or console errors;
  title, social meta, launch/repository links, and neutral cold-start copy matched
  source. The browser runtime did not honor attempted mobile viewport overrides, so
  no visual mobile PASS is claimed. Static CSS retains responsive, reduced-motion,
  keyboard, and 44px link-target contracts. The public Connect endpoint remains the
  previously verified `Startup Error` because this draft has not been merged.
- **Local verification:** `git diff --check`, shell syntax, image dimensions, and
  the local browser checks passed. There is still no local R, Docker, or Podman; all
  R parse/helper/bundle/offline claims come only from the cited pinned GitHub job.
- **Classification/Driver implication:** `app-local`, `suite-platform`,
  `scientific-contract`, `product/UI`, and `Driver-impacting`. Driver remains `HOLD`
  until the committed validator-generated manifest matches and an exact-head offline
  source passes; production health remains a separate post-merge requirement.
- **Next action:** commit/push this coherent validator + product tranche, let pinned
  CI reach offline source and upload its exact manifest candidate, commit only that
  artifact, then require one green exact-head PR run. Do not merge or claim the
  outage restored without explicit production authority and semantic smoke.

### 2026-07-18 10:25 MST - URL-origin provenance made exact and diagnosable

- **Published input:** product/provenance commit
  `c92158716953f9f1a8d5d329d0d6f95fd8e966bc` was pushed only to draft PR #73.
  GitHub evaluated its pull-request merge revision
  `7b0df673ee2d82510b06530bc3c53dfef57dbbfa` against unchanged `main`.
- **Pinned CI evidence (partial PASS / terminal FAIL):** Actions run
  `29653368059`, job `88103373049`, passed the source-built geospatial closure,
  R 4.5.2, loaded OpenBLAS 0.3.20 Haswell/one thread, complete R/JS/shell parsing
  (including the revised About panel and cover fallback), all 11 helper contracts,
  and 91-package/117-file manifest generation. The bundle gate again loaded 46/46
  sites with rows and effort schema plus the 46/604/604 index rows. It failed only
  the broad package-provenance predicate for the eight URL-installed packages;
  offline source, validated artifact upload, and committed match remained skipped.
- **Observed root cause:** repository-lane alignment was necessary but insufficient.
  The verifier still required the ordinary `Source: CRAN` tuple for URL-origin
  packages. Official rsconnect source confirms URL-installed records are a distinct
  reproducible source type. This is the same eight-record model mismatch, not data,
  package-version, checksum, or scientific drift.
- **Focused repair:** both writer and bundle gates now require, for each of the eight
  packages, the exact installed version, symbolic `Repository: CRAN`, a recognized
  CRAN/URL source record, `RemoteType: url`, and the exact declared CRAN tarball URL.
  Ordinary packages remain strictly `Source: CRAN` on the dated Posit snapshot. CI
  now retains an explicitly **UNVALIDATED**, one-day diagnostic manifest after a
  failed downstream gate so future provenance failures can be inspected without
  repository write access or gate relaxation.
- **Additional release/docs hardening:** refresh runs are serialized; review-branch
  replacement uses an explicit remote-head lease; recovered semantic health closes
  the matching outage issue; stale direct-redeploy wording was removed from the
  refresh script, README, data-bundling pattern, smoke comments, and finding ledger.
  These changes do not alter runtime manifest contents beyond the still-pending
  validator artifact.
- **Driver implication:** `HOLD CURRENT OUTPUT`; no Driver artifact change. The
  source-app definition now remains aligned with Driver's physical-event resolver,
  but package promotion waits for offline source, exact committed manifest, merge,
  and semantic public health.
- **Observed diagnostic result:** exact-head run `29654014025` (branch head
  `a700ae0587c765ad8672f49049a781e8a303f745`, pull-request merge revision
  `eadd5cf7f1f52fb32925f049baaef2a448ae9bee`) again passed dependency installation,
  deterministic OpenBLAS, complete parsing, and all 11 helper contracts. Its writer
  then rejected the still-assumed `RemoteUrl` shape. The one-day UNVALIDATED artifact
  showed the authoritative installed tuple for every geospatial pin:
  `Source: URL`, `Repository: CRAN`, `RemoteType: url`, and
  `RemotePkgRef: url::<exact tarball URL>`. No diagnostic bytes were promoted.
- **Focused correction:** writer and verifier now require that observed four-field
  tuple exactly, including the `url::` prefix and full declared tarball URL; ordinary
  packages remain `Source: CRAN` on the dated snapshot lane.
- **Next action:** statically verify this observed-shape correction, push it to the
  existing draft branch, and require the next pinned run to pass bundle/offline gates
  before its validated manifest artifact can be considered for promotion.
