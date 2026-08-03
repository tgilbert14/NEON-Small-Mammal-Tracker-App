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
- Release state: **PRODUCTION HEALTHY / PASS 1 + COVER V5 COMPLETE**. PR #86
  merged as `c4c46fce3725126231504d8f9610f52e8f929ef8`; Pages published that merge
  and Connect deployment #125 published the same exact revision. Suite Living
  Poster V1, the 46-site picker, and primary exploration funnels are verified live.
- Driver disposition: accepted `CONTEXT`; central suite receipt merged with no Driver
  generation or scientific-artifact byte change

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

### 2026-07-18 11:04 MST - validated runtime manifest promoted exactly

- **Published input:** observed-provenance commit
  `ebf833937dbf19a864e4d2e1f2500ca298409895` was pushed only to draft PR #73.
  GitHub evaluated pull-request merge revision
  `45b5c82640a20b8c181b6b60949e20ebbeb9c5b0` in Actions run `29654547344`, job
  `88106432853`.
- **Pinned evidence (candidate PASS / equality FAIL):** R 4.5.2 and the complete exact
  geospatial source closure installed; OpenBLAS loaded as Haswell/one thread; all R,
  JavaScript, and shell source parsed; all 11 helper contracts passed; the writer
  emitted 91 packages and 117 files; bundle verification loaded all 46/46 expected
  sites with rows and effort schema plus exact 46/604/604 index rows; package origins,
  file checksums, and complete offline app source passed.
- **Designed terminal gate:** the validated candidate uploaded successfully, and only
  then did the final committed-manifest equality check fail against the obsolete
  repository manifest. Generated data remained unchanged.
- **Promoted artifact:** only validated artifact
  `small-mammal-manifest-45b5c82640a20b8c181b6b60949e20ebbeb9c5b0`
  was copied byte-for-byte to `manifest.json`; SHA-256
  `3fba04eb885b3cb6a9437b8c8b25ade25d44d47f6dcb50add025e754a6de04d7`.
  Independent inspection confirmed R 4.5.2, 91 packages, 117 files, exact source
  tuples/versions for all eight URL pins, and every app-file checksum against this
  checkout. The earlier UNVALIDATED diagnostic was not promoted.
- **Knowledge disposition:** the app-local Driver package is now explicit: `CONTEXT`,
  no current Driver byte change; physical-event parity is closed, mixed-community
  CPUE remains a qualified within-site index, and current-source site/year join parity
  remains `UNKNOWN / HELD` until suite synthesis.
- **Next action:** commit the exact manifest and knowledge package, obtain one green
  exact-head PR run, then merge intentionally and require semantic public health before
  the production receipt can be promoted to the central suite ledger.

### 2026-07-18 11:26 MST - source-build clock noise canonicalized

- **Published input:** exact-manifest commit
  `1066cd30d486e03c4ffd6f220b2ba0195246476c` was pushed only to draft PR #73.
  Actions run `29655286162`, job `88108361222`, evaluated that immutable head.
- **Pinned evidence (substantive PASS / byte equality FAIL):** the run passed the same
  R 4.5.2/package installation, deterministic OpenBLAS, complete parse, 11 helper,
  package provenance, 46-site/schema/index/checksum, and offline-source ladder. The
  final diff contained exactly eight changes: each exact URL-built package's DESCRIPTION
  `Built` field recorded the new source-compilation wall clock. Versions, origins,
  repositories, remote refs/etags, R/platform, app checksums, data, and indexes matched.
- **Root cause:** `Built` is emitted from local source-compilation time, not package
  identity or an install input. Requiring that timestamp to match makes a fresh source
  build inherently non-reproducible.
- **Focused correction:** `scripts/write_manifest.R` now removes only `description.Built`
  for the named eight-package URL closure, then gates its absence alongside exact
  version/source/repository/remote-type/ref. `scripts/verify_bundle.R` independently
  requires the same. Ordinary snapshot-package records are untouched.
- **Canonical release candidate:** R 4.5.2, 91 packages, 117 files; SHA-256
  `395fc36faa11f408a3ef4483f6c6ff2da13c09ab1f5d2498f2744bddbee0606c`.
  Independent JSON/file-checksum inspection passed with all eight clock fields absent.
- **Driver/suite learning:** promote deterministic removal of non-semantic build clocks
  to later app/subagent release briefs, but never remove identity, origin, platform,
  compatibility, or checksum evidence.
- **Next action:** statically verify and push this exact canonicalization, then require
  one green exact-head run before readying PR #73.

### 2026-07-18 15:08 MST - production exposed a manifest network-contract gap

- **Merged evidence:** PR #73 merged as
  `6df138c316a1dc71ac9ee3960120c7716a53e92d`. Exact-head run `29655948807`
  and merge run `29656583272` both passed the complete pinned validator ladder.
- **Semantic evidence (FAIL):** post-deploy run `29656583275` correctly opened outage
  issue #74 after the public endpoint continued to serve `Startup Error`. Signed-in
  Connect inspection showed the content was still on July 8 commit `39dca56`; an
  authorized republish fetched exact merged commit `6df138c` and then failed dependency
  resolution with `Get "CRAN/src/contrib/Archive/wk/wk_0.9.5.tar.gz": unsupported
  protocol scheme ""`.
- **Root cause:** the installed package DESCRIPTION truthfully labels its repository
  `CRAN`, but Connect treats the manifest package's top-level `Repository` value as a
  network location. The validator proved package identity and origin but did not prove
  that this field was a usable absolute URL. The result was reproducible in production;
  it is not a data, application-runtime, R-version, or package-version failure.
- **Focused correction:** canonicalization now changes only the eight exact URL
  packages' top-level deployable lane to `Source: CRAN` and repository location to
  `https://cran.r-project.org`; each exact installation tarball remains hard-gated in
  `RemotePkgRef`, and DESCRIPTION's own provenance is preserved. This matches the last
  healthy Connect manifest shape for the same package versions and lets Connect choose
  current versus archive paths correctly. Writer and independent verifier both require
  that deployable shape.
  Canonical manifest SHA-256 is
  `e619343d1d6404f52260481ec611ccbd1c9f5cd349657c0799f6d535a7bc0b11`.
- **Learning promoted:** release validation must distinguish installed-record truth
  from the deployment platform's network contract. Future app/subagent briefs must
  require absolute manifest repository URLs and a real Connect dependency-resolution
  receipt before a package strategy becomes suite-standard.
- **Next action:** require an exact-head green run for this focused correction, merge
  through review, republish the resulting `main`, and rerun semantic health. Do not
  close issue #74 or promote the production receipt until the public app exposes its
  semantic ready marker.

### 2026-07-18 15:13 MST - production restored; final console contract follow-up

- **Merged/runtime evidence:** corrective PR #75 passed Actions run `29662807227`,
  job `88128209497`, on exact head
  `ff55d6bccdbb7f2c11d5c2bea8b066ae818515f6` and merged to `main` as
  `9f86b13e65d333b1f911a880ab987b5b37f48a72`. Connect republished that exact
  commit at 15:08 MST. The public app exposed
  `ddl-app-ready=small-mammal-tracker-v1`, and the JORN exploration funnel loaded
  6,093 captures, 2,252 individuals, 21 species, 31,584 reviewed trap-nights, and
  the complete Overview navigation without a Startup Error.
- **Pages evidence:** the public cover rendered the expected Small Mammal title,
  live/repository controls, all nine companion links, exact 46-site/145-species
  framing, and the reviewed `og-image.png` social reference.
- **Final browser finding:** the console contained one first-party Shiny 1.14 error,
  `handler must be a function that takes one argument.` Four no-payload custom
  handlers used zero-argument functions. This can make counter, loading-overlay,
  map-refit, and QC-reveal registration brittle even though the tested JORN flow
  succeeded. Five bootstrap-datepicker language deprecation warnings are upstream
  dependency noise, not application failures.
- **Focused repair:** every one of the six custom message handlers now accepts
  exactly one payload argument. A new static contract enumerates all six handlers
  across `app.js` and `pincards.js`, fails on zero/multiple parameters or inventory
  drift, and runs in CI after JavaScript parsing.
- **Artifact boundary:** `app.js` and `pincards.js` are manifest-tracked runtime
  files, so this follow-up must obtain and promote a fresh validator-generated
  manifest before merge. No hand-authored manifest checksum is allowed.
- **Validated artifact:** PR #76 run `29663094833`, job `88128953998`, passed the
  handler inventory, complete pinned dependency/runtime/science/bundle/checksum and
  offline ladder, uploaded validated artifact
  `small-mammal-manifest-254517e3b071d71a947638a6f7f4beebe11cb1a8`, and stopped
  only at designed committed-manifest equality. The candidate differs from the
  previous manifest only in exact MD5 values for `www/app.js` and
  `www/pincards.js`; SHA-256
  `f6c4a5ff74053b95e22fac7394f1930d2fe2329663737031b1c32f7a1f70bc54`.
- **Next action:** publish the focused console-contract branch, promote only its
  validated manifest artifact, require an exact-head green run, merge, republish,
  and repeat semantic marker, interaction, and first-party-console verification.

### 2026-07-18 15:27 MST - pass 1 production closeout / root

- **Changed/classification:** merged fully green PR #76 into `main` as
  `1615ab4e74fd16a2698de8431acb862d6cc4cebf`, deleted the remote release branch,
  republished that exact revision in Connect, and completed final browser/runtime
  QA. This closeout is `app-local`, `suite-platform`, `scientific-contract`,
  `product/UI`, and `Driver-impacting`; the Driver artifact decision remains
  **NONE / HOLD CURRENT BYTES** with learning disposition **CONTEXT**.
- **Pinned evidence:** exact-head run `29663236510`, job `88129323716`, and final
  main run `29663335706`, job `88129588478`, passed R 4.5.2, the exact eight-package
  source closure, Haswell/one-thread OpenBLAS, complete parse, six-handler arity,
  all 11 scientific fixtures, exact 46-site/schema/index/checksum gates, offline
  source, and committed manifest/data equality. Final manifest SHA-256 is
  `f6c4a5ff74053b95e22fac7394f1930d2fe2329663737031b1c32f7a1f70bc54`.
- **Publication evidence:** Connect **Last deployed** reports commit `1615ab4` at
  15:23 MST. Semantic run `29663335708`, job `88129588525`, and Pages run
  `29663335341` passed on that merge. Outage issue #74 is closed.
- **Public browser result:** a fresh public session exposed
  `ddl-app-ready=small-mammal-tracker-v1`, restored saved JORN state, and rendered
  6,093 captures, 2,252 individuals, 21 species, 31,584 reviewed trap-nights, the
  Overview, and all ten app tabs without Startup Error. The first-party handler
  error is gone; the only warning/error-level entries were five upstream
  bootstrap-datepicker language deprecations. Pages rendered the reviewed Small
  Mammal cover, launch/repository controls, all nine companion links, exact
  46-site/145-species framing, and `og-image.png` metadata.
- **Expected versus actual:** expected the final PR head, merge, manifest, deployed
  revision, semantic marker, interaction output, console, and Pages cover to agree;
  actual matched.
- **Evidence invalidated:** every earlier `release candidate`, `draft PR`, `outage`,
  `republish pending`, and `first-party console error pending` statement is
  superseded. Historical failures remain factual diagnostics.
- **Artifacts/non-impact:** no Driver generation or scientific artifact changed.
  Small Mammal bundled data and all derived indexes reproduced exactly. The separate
  idea branch remained untouched.
- **Residual risk:** visual mobile QA remains unclaimed because the browser harness
  did not expose a reliable viewport override; static responsive, reduced-motion,
  focus, and touch-target contracts passed. Five upstream datepicker deprecations
  remain non-blocking. Exact current-source Driver site/year join parity remains
  held for the later suite synthesis.
- **Next action:** publish this documentation-only app closeout, then vendor its
  exact `CONTEXT` receipt into the Driver suite ledger/playbook with Driver artifact
  hashes unchanged. Do not begin Phenology before that Driver documentation merge.

### 2026-07-18 16:23 MST - habitat-led cover V3 release candidate / root

- **Starting state:** clean synchronized `main` at
  `957e56cc3af15d62387bfefbd37ee31623ae682b`; production runtime remains the
  verified `1615ab4e74fd16a2698de8431acb862d6cc4cebf`. The central Driver `CONTEXT`
  receipt is already merged, so this tranche does not block or alter Driver bytes.
- **Changed/classification:** this `product/UI`, `suite-platform`, documentation-only
  candidate completely replaces the visible Pages experience. It adds a full-bleed
  habitat journey, a dedicated portrait mobile composition, one outcome-led H1,
  explicit can/cannot claims, dated data and runtime facts, method-denominator
  guidance, grouped suite navigation, current-product state, and a purpose-built
  1200×630 social card. All three live-app controls open safely in a new tab.
- **Evidence/claims:** visible cover facts remain pinned to 46 sites, 93,169 tagged
  individuals, 145 represented species, 2013–2024 bundled vintage, 178,216
  capture/handling rows, NEON `DP1.10072.001`, R 4.5.2, runtime `1615ab4`, and
  Driver disposition `CONTEXT`. CPUE, abundance, richness, detection, and causation
  limits are stated rather than implied.
- **Asset provenance:** `docs/IMAGE-PROVENANCE.md` identifies every AI-illustrated
  asset as non-documentary. Delivery SHA-256 values are desktop habitat
  `28f78aaf2b02c1907a1ea2762c8b5a7068d50c718aa09d2ffa422a2c9c5294dc`,
  portrait mobile JPEG
  `14e447c12b60f1b1adf0e2cfe3985ad25e310ae0fdeddc3428f08b86a083b410`,
  and exact 1200×630 social card
  `d089443e74cedb2d8b9f1f02dde6bd4c5358a7d05c94ec66a9124ca79bdad0bd`.
- **Local gates (PASS):** `node --check scripts/check_cover.mjs`,
  `node scripts/check_cover.mjs`, and `git diff --check` pass. The new CI contract
  enforces one main/H1, skip/navigation names, dated facts, can/cannot language,
  canonical/social metadata and dimensions, safe suite registry, no insecure
  source links, no unsolicited `fetch`, unique IDs, and both responsive assets.
- **Browser QA (PASS):** the in-app browser rendered the local cover at desktop,
  390×844, and the 320px reflow boundary. DOM reads confirmed one H1/main/current
  marker, no horizontal overflow at 390px or 320px, 44px minimum mobile launch
  control and 57px primary actions, correct `_blank`/`noopener` controls, and no
  warning/error console entries. Review found and fixed the prior 42px nav target
  and a forced `min-width: 320px` that clipped the scrollbar-adjusted 320px layout.
  Reduced-motion and increased-contrast alternatives remain explicit CSS contracts.
- **Published validator (PASS):** draft PR #78 evaluated exact candidate
  `4d7c8b0c58b16479918d858fc6afeed6e3337e8e` in Actions run `29665245800`,
  job `88134486578`. In 2m04s the job passed pinned R 4.5.2 dependency
  installation, deterministic OpenBLAS, source parsing, the new cover and existing
  message-handler contracts, all scientific fixtures, manifest regeneration,
  exact bundle/index/file checksums, complete offline source, validated-manifest
  upload, and committed manifest/data equality. This evidence-only handoff edit
  now requires one final exact-head rerun before merge.
- **Expected versus actual:** expected the cover to explain the app before listing
  features, preserve measurement boundaries, keep Driver as integrator, and reflow
  without cropping; local and first published validator actuals match. Public Pages
  and final evidence-pinned-head CI are **PENDING**, so this entry is not a
  publication claim.
- **Artifacts/non-impact:** no Shiny runtime file, bundled datum, `manifest.json`,
  scientific helper, Connect deployment, or Driver artifact changes. Connect does
  not need republishing for this Pages-only release.
- **Next action:** commit this immutable validator receipt to draft PR #78, require
  one green exact evidence-pinned head, merge intentionally, verify the public Pages
  cover and metadata at desktop/mobile widths, then append the publication closeout
  before beginning the Plant Phenology pass.

### 2026-07-18 16:38 MST - cover V3 publication closeout / root

- **Changed/classification:** PR #78 merged the two-commit Pages release to `main`
  as `2492056ee62f21b5581de69d6da6d46f2fd37b34`. This is `product/UI` and
  `suite-platform`; Driver implication is **NONE / HOLD CURRENT BYTES** and the
  existing learning disposition remains accepted `CONTEXT`.
- **Pinned PR evidence (PASS):** final evidence-pinned head
  `506c4513d815adb584ed2f4740900d4e994bd5ce` passed run `29665334021`, job
  `88134713870`, in 2m08s after the earlier design-head pass. The exact final head
  therefore passed the complete pinned R, OpenBLAS, parse/static, scientific,
  bundle/index/checksum, offline-source, validated-manifest, and manifest/data
  equality ladder before merge.
- **Post-merge evidence (PASS):** main validator run `29665434677`, job
  `88134964949`, passed in 1m45s. Semantic production run `29665434664`, job
  `88134965010`, passed in 52s. Pages run `29665434190` passed build job
  `88134966473`, report job `88134997115`, and deploy job `88134997089` in 41s;
  the published Pages artifact digest is
  `61a841fc7986304648e995ea56f50d06cefa2c233d4cba14e60a6e4e30e74243`.
- **Public browser result (PASS):** the live Pages URL rendered the full habitat-led
  desktop composition and the 390px portrait source `hero-mobile-v1.jpg`, one H1,
  the current-suite marker, production receipt, and all companion groups. At 390px
  the DOM reported client/scroll width 375/375, no horizontal overflow, 44px nav
  launch and 57px primary actions, and no warning/error console entries. Desktop
  and mobile both loaded their declared image sources without asset failures.
- **Expected versus actual:** expected PR head, merge SHA, main validator, semantic
  health, Pages artifact, desktop layout, mobile source selection, responsive
  geometry, and metadata to agree; actual matched. The prior mobile-QA and cover-
  publication residuals are superseded.
- **Artifacts/non-impact:** the Shiny runtime, bundled data, manifest SHA-256
  `f6c4a5ff74053b95e22fac7394f1930d2fe2329663737031b1c32f7a1f70bc54`,
  Connect deployment `1615ab4`, and Driver generation/scientific artifacts did not
  change. No Connect republish was needed or performed.
- **Residual risk:** GitHub's platform-owned Pages workflow reports a non-blocking
  Node.js 20 deprecation warning for its internal `actions/checkout@v4` and
  `actions/upload-artifact@v4`; the deploy succeeds and repository-authored pinned
  validation actions are already current. Five upstream runtime datepicker
  deprecations and the held current-source Driver join remain unchanged.
- **Next action:** publish this documentation-only closeout, delete the merged cover
  branches after the final green main receipt, then begin Plant Phenology from its
  clean pinned baseline while carrying forward the responsive-asset, image-
  provenance, semantic-cover, and evidence-pinned release contracts.

### 2026-07-18 18:06 MST - documentary cover V4 release candidate / root

- **Starting state:** clean synchronized `main` at
  `56de0436e7846154b5109aadd45703f420509842`; production runtime and Connect
  deployment remain the verified
  `1615ab4e74fd16a2698de8431acb862d6cc4cebf`.
- **Changed/classification:** this `product/UI`, `suite-platform`, Pages-only
  candidate replaces the rejected generated-habitat pitch with a documentary,
  question-led product story. The first viewport now gives the Sherman trap and
  animal equal billing with one promise—“One trap night. A whole population
  story.”—then offers three real tasks: follow a returning animal, test a supported
  population signal, or explore the nocturnal community. Repetitive feature prose
  was removed; the field logic, scientific boundary, release receipt, and suite
  continuation now each use a distinct editorial rhythm.
- **Documentary provenance:** the hero/evidence source is Cheryl Brehme's USGS
  “Pacific pocket mouse in Sherman trap,” public domain, stored unmodified at
  4000×3000 with SHA-256
  `b678af26331a98747eba64c382f1ee7e2e1f7c1f77baecbd81dd46622a9e950a`.
  The visible cover states that it is real field equipment, credits the source,
  and does not present the photograph as a NEON observation. The project-owned
  HTML/CSS social source renders the same documentary asset at exact 1200×630;
  delivery JPEG SHA-256 is
  `b36c62094270bf1598f38c6162ea6fda1b5c607f513a3faeea4ed62819fdf19a`.
- **Claims and evidence:** visible facts remain pinned to 46 sites, 178,216 reviewed
  capture/handling rows, 93,169 tagged individuals, 145 species, 2013–2024, and
  NEON `DP1.10072.001`. CPUE remains a within-site catch-per-effort index, MNKA a
  conservative floor, and N-hat a gated estimate. CAN/CANNOT claims and the dated
  July 18, 2026 production receipt remain present without dominating the pitch.
- **Local gates (PASS):** `node --check scripts/check_cover.mjs`,
  `node scripts/check_cover.mjs`, and `git diff --check` pass. The contract now pins
  the documentary and social-card hashes and dimensions, requires the promise and
  three question-led entries, all nine suite destinations, six safe purposeful app
  controls, claim boundaries, verified facts, social metadata, visible credit,
  unique IDs, and no insecure source, generated V3 hero reference, or unsolicited
  prewarm.
- **Browser QA (PASS):** local desktop and 390×844 review loaded both 4000×3000
  documentary image instances, one H1/main/current marker, six `_blank`/`noopener`
  app controls, and no horizontal overflow. At 390px the launch control is 44px,
  the primary action 50px, and question targets 172–197px. A separate 320×800 pass
  found a 39px navigation overflow, collapsed the brand wordmark below 360px, and
  then passed exact 305/305 client/scroll widths while retaining the 44px launch
  target.
- **Reusable suite learning:** the app-local Driver package now records documentary
  media, persuasion-first information architecture, question-to-task mapping,
  product-specific visual theses, progressive evidence receipts, and 320px QA as
  defaults for later companions. Driver disposition remains **CONTEXT / HOLD
  CURRENT OUTPUT**.
- **Expected versus actual:** expected a cover that earns curiosity before teaching
  method, makes the trap unmistakable, and is more credible because every visual
  claim is inspectable; local visual, semantic, asset, and responsive actuals match.
  GitHub validation and public Pages publication are **PENDING**.
- **Artifacts/non-impact:** no Shiny runtime file, bundled datum, index,
  `manifest.json`, scientific helper, Connect deployment, or Driver generated
  artifact changed. Connect does not need republishing for this Pages-only release.
- **Next action:** commit and push the V4 candidate, require a green exact-head
  validator, merge intentionally, verify public Pages at desktop/390/320 and social
  metadata, then append the immutable publication closeout before resuming the
  in-flight Plant Phenology release.

### 2026-07-18 18:14 MST - documentary cover V4 publication closeout / root

- **Changed/classification:** PR #80 merged the documentary Pages release to `main`
  as `dd72d518e53b5b8fa421ead831486a02c53c056a`. This is `product/UI` and
  `suite-platform`; Driver disposition remains accepted **CONTEXT / HOLD CURRENT
  OUTPUT**.
- **Pinned PR evidence (PASS):** exact design/head
  `37d9f6050e3275cb4f09fc0547de40e69ed5d2a3` passed run `29668112141`, job
  `88142131181`, in 2m06s and uploaded validated artifact
  `small-mammal-manifest-f2cefbb30ebbd2f8c0c3b1d99fc6c6bb7d66ebc7`.
  The run passed pinned R/package/OpenBLAS closure, static cover and handler
  contracts, scientific fixtures, exact 46-site bundle/index/file checksums,
  offline source, and committed manifest/data equality.
- **Post-merge evidence (PASS):** main validator run `29668199348`, job
  `88142357580`, passed in 1m59s and uploaded
  `small-mammal-manifest-dd72d518e53b5b8fa421ead831486a02c53c056a`.
  Semantic production run `29668199351`, job `88142357582`, passed in 53s. Pages
  run `29668199127` passed build job `88142358474`, deploy job `88142395878`, and
  report job `88142395897` in 47s.
- **Public browser result (PASS):** the live Pages URL renders the exact headline,
  full-width documentary mouse/trap photograph, visible USGS/public-domain credit,
  six live-app controls, nine companion destinations, one current-product marker,
  and the evidence receipt. Both 4000×3000 image instances loaded successfully;
  Open Graph metadata points to the published `og-image.jpg`. Desktop matched the
  reviewed split composition. At 390×844 the client/scroll widths were 375/375 and
  the nav launch remained 44px. At 320×800 they were 305/305, the compact DDL mark
  replaced the wordmark, and the launch remained 44px. No responsive overflow or
  missing asset remained.
- **Runtime confirmation:** a fresh public Connect session still exposed the
  verified JORN baseline—6,093 captures, 2,252 individuals, 21 species, 64.8%
  recapture rate, 31,584 trap-nights, and 92 legends—without Startup Error. The
  Pages-only release required and performed no Connect republish.
- **Expected versus actual:** expected exact PR head, merge SHA, main validator,
  semantic health, Pages deployment, social metadata, documentary assets, desktop
  composition, and both mobile boundaries to agree; actual matched. The rejected
  V3 cover remains only in history and its generated assets are not referenced by
  V4.
- **Artifacts/non-impact:** the Shiny runtime, bundled data, indexes, scientific
  helpers, manifest SHA-256
  `f6c4a5ff74053b95e22fac7394f1930d2fe2329663737031b1c32f7a1f70bc54`,
  Connect deployment `1615ab4`, and Driver generated/scientific artifacts did not
  change.
- **Residual risk:** GitHub's platform-owned Pages workflow reports the same
  non-blocking Node.js 20 deprecation for internal `actions/checkout@v4` and
  `actions/upload-artifact@v4`; deployment succeeds. Five upstream runtime
  datepicker deprecations and the held current-source Driver join remain unchanged.
- **Next action:** publish this documentation-only closeout, delete the merged
  cover branch, and resume Plant Phenology at its first validator finding:
  `data/sites/KONA.rds` carries an empty `trend` object that must normalize to
  `NULL` or a non-empty data frame before artifact promotion.

### 2026-07-19 17:04 MST - in-app Living Poster working-tree candidate / small_mammal_inapp_living_poster

- **Starting state:** clean synchronized `main` at
  `b05cecc832790b05912a1732626a768b31a0665d`; created local branch
  `agent/small-mammal-inapp-living-poster`. Public Pages remains documentary V4
  from merge `dd72d518e53b5b8fa421ead831486a02c53c056a`; Connect remains the
  verified scientific runtime `1615ab4e74fd16a2698de8431acb862d6cc4cebf`.
- **Observed mismatch:** Pages opens with the concise “One trap night. A whole
  population story.” documentary composition and a prominent real Sherman trap,
  while Connect still opened with a generic blue title banner, a long explanatory
  paragraph, and the control stack. The two surfaces did not feel like one product.
- **Changed/classification:** `ui.R` and `www/styles.css` now give the unchanged
  site-picker a brief Living Poster face: one seven-word hook, one nine-word
  promise, one `Pick a place` anchor, the same real field photograph and visible
  source boundary, then the existing map. The persistent bar now uses the DDL/suite
  wordmark and its mobile Help label collapses visually while retaining an explicit
  accessible name. The old fixed splash mascot/bubble was removed so it cannot
  overlay or visually compete with the documentary invitation; loader and result
  celebration mascots remain unchanged. The documentary credit now sits on a
  high-contrast dark scrim instead of relying on a text shadow over the photograph.
  Added the optimized runtime derivative, provenance record,
  `scripts/check_in_app_landing.mjs`, and its CI invocation. Classification is
  `product/UI` plus `suite-platform`; Driver implication is **NONE / NO DRIVER BYTE
  CHANGE**.
- **Behavior/science boundary:** existing picker IDs and paths are retained:
  `pickerMap`, `pickMode`, `rangeSpecies`, `stateSel`, `site`, `dateRange`,
  `provisional`, `loadBtn`, and `compareBtn`. No server, bundle, index, helper,
  estimator, export, scientific claim, Pages cover, or Driver file changed. The
  photo is visibly labeled documentary and not a NEON observation.
- **Asset receipt:** `www/assets/small-mammal-field-usgs.jpg` is a non-generative
  1800 x 1350, quality-82 resize of the provenance-pinned USGS public-domain source;
  SHA-256 is
  `e9d0158ac56f95437f0958c5aa4037701c95f2b616e87bb54440ab08e3a50f55`.
  It is approximately 605 KB instead of transferring the 2.5 MB archival source.
- **Local static gates (PASS):** `git diff --check`; Node syntax and the new in-app
  landing contract; the existing documentary cover contract; Ruby safe YAML parse
  of CI; `bash -n scripts/post_deploy_smoke.sh`; and tree-sitter parse of all 16 R
  files passed. A headless structural mock at exact CDP-emulated 390 x 844 and
  320 x 800 reported client/scroll widths 390/390 and 320/320, a 59 px single-row
  top bar, 44 px Help control, and 48 px full-width poster CTA. Desktop composition
  also rendered as the intended split poster. This is not a live Shiny/Leaflet or
  screen-reader result.
- **Runtime/manifest status (BLOCKED by design):** local `Rscript` is unavailable,
  so no R runtime, helper, offline boot, Shiny interaction, or manifest generation
  is claimed. The current manifest correctly remains unedited and is now stale:
  `ui.R` MD5 changed from `db1f5cf3469e11da3f383bf759bec9a5` to
  `388d9bae920a3e6631b796a4ccc156a1`; `www/styles.css` changed from
  `48466260299d0818ad41bb79d929299e` to
  `3c7622a4cc3ae6baa12d34ca598f407e`; `www/app.js` changed from
  `b2d4f27f0935a7a73a145499ef37b622` to
  `4fab7f639eafc7839bd929dafcd32fb0`; and the new image MD5
  `23dab12d5ed9283b0ad10c199277116c` is absent. A pinned validator-generated
  manifest must be promoted before merge or Connect deployment.
- **Expected versus actual:** expected a short, artistic, immediately recognizable
  bridge from Pages into the functional picker without inventing imagery or
  changing control behavior; static copy, visual, responsive, provenance, and ID
  contract actuals match.
- **Failures/cleanup:** Chrome's command-line `--window-size=390` retained a
  500-pixel layout viewport and only cropped the screenshot; this was rejected and
  rerun with CDP device metrics at exact 390 and 320 CSS pixels. No generated V3
  asset was reintroduced. No commit, push, PR, manifest edit, Connect republish, or
  public state change was made.
- **Ownership/status/residual risk:** all current changes on
  `agent/small-mammal-inapp-living-poster` belong to this focused candidate; no
  unrelated user changes were present. Actual Shiny binding, Leaflet behavior,
  light/dark browser rendering, keyboard/screen-reader use, manifest equality, and
  deployed mobile geometry remain unverified until the pinned candidate path runs.
- **Next action:** review the working-tree poster, then intentionally commit/push
  the focused branch; require the pinned validator to emit the exact manifest,
  promote only that artifact, rerun exact-head CI, merge, republish Connect, and
  verify semantic readiness plus the unchanged site-selection funnel at desktop,
  390, and 320 pixels.

### 2026-07-19 17:20 MST - exact manifest candidate promoted / root

- **Exact source revision:** PR #82 head
  `22b18e751bd33d8ec5295ffae5354eb1a03ef464`; GitHub evaluated immutable PR merge
  revision `66229ecca3c9a7ec374b527bbf89f895657b01b2` in run `29709248008`, job
  `88250818109`.
- **Visual follow-up before validation:** independent review caught a mobile CTA
  border box clipped against the card edge, cramped label/arrow spacing, a legacy
  fixed mascot/bubble competing with the documentary poster, and photo-credit
  contrast that depended on the image. Commit `22b18e7` fixed all four: border-box
  containment plus gap, removal of the old splash ornament/listener, and opaque
  white credit text on a 90%-dark scrim. Loader and result-celebration mascots remain.
- **Pinned candidate evidence (PASS):** R 4.5.2, the complete exact dependency
  closure, deterministic one-thread Haswell OpenBLAS, all static and scientific
  contracts, exact bundle/index/checksum verification, and complete offline app
  source passed. The validated manifest uploaded successfully; only the designed
  committed-manifest equality check failed against the obsolete repository copy.
- **Artifact receipt:** promoted only
  `small-mammal-manifest-66229ecca3c9a7ec374b527bbf89f895657b01b2/manifest.json`;
  SHA-256 is
  `0e9ed990c44cda902d66428e45a6765aee7abcd98d8324f97b3248cecf7aecb9`.
  It records 91 installed packages and 118 runtime files. Semantic comparison with
  the prior manifest found unchanged package and metadata records, one new exact
  runtime photograph, and only the expected checksum changes for `ui.R`,
  `www/app.js`, and `www/styles.css`. All four checksums match the exact PR-head
  files.
- **Production status:** manifest promotion is a reviewed candidate action only;
  `main`, Connect, Pages, scientific bundles, and Driver artifacts remain unchanged.
  Require one green equality run from the new exact PR head before merge or publish.

### 2026-07-19 17:35 MST - production poster QA found a 320 px chrome wrap / root

- **Published revision (PASS):** PR #82 merged as
  `22e7b9489677c1bddfc869e2854e97b298160538`; exact-head run `29709380251` and
  main run `29709445267` passed the complete validator. Connect deployment #120
  successfully published exact commit `22e7b94` with R 4.5.2 in five seconds.
- **Live functional QA (PASS):** the documentary poster, 1800 x 1350 image,
  source boundary/scrim, concise hook and promise, CTA-to-picker anchor, Help
  dialog, dark-mode control, JORN picker-to-dashboard funnel, loading completion,
  and no-disconnect state all passed. Desktop and 390 px had no horizontal
  overflow; the 390 px card and CTA retained symmetric insets, the Help target
  was 44 px, and the CTA was 48 px tall. No legacy floating mascot remained.
- **Narrow defect (FAIL, release-polish):** at an exact 320 x 800 viewport the
  app was functionally usable with no horizontal overflow, but the 184 px brand
  plus 101 px action cluster wrapped the persistent top bar to 97 px. The static
  mock missed this because it did not reproduce bslib's 24 px body gutters or the
  complete dark-mode control.
- **Focused correction:** at 420 px and below, bslib page gutters contract to
  12 px; at 390 px and below, the top bar is one non-wrapping row and the
  redundant decorative theme glyph is hidden while the functional 44 px theme
  control remains. The poster gains useful image/copy width at the same
  breakpoint; the final compact wordmark scale and gaps apply only at 340 px and
  below. CI now contracts these rules.
- **Exact local correction check (PASS):** a 320 x 800 browser viewport with the
  complete 29 px theme-control surrogate measured client/scroll widths 305/305,
  12 px bslib gutters, a single-row 59 px top bar, a 44 px Help target, a 257 px
  poster card, and a 48 px CTA with symmetric 21 px card insets.
- **Release boundary:** this is CSS plus its static contract and evidence only;
  no app behavior, data, science, picker ID, image, public Pages cover, or Driver
  artifact changes. Because `www/styles.css` is manifest-tracked, a fresh pinned
  manifest candidate and exact-head green run are still required before the
  320 px fix may replace deployment #120.

### 2026-07-19 17:52 MST - 320 px manifest candidate promoted / root

- **Exact source revision:** PR #83 head
  `517482fa4751bbe45d1fd413fdfb739e6a56df99`; GitHub evaluated immutable PR merge
  revision `64e026f1b07dd0fa11b997059e47bcc2234c2c2d` in run `29709950504`, job
  `88252416500`.
- **Pinned candidate evidence (PASS):** R 4.5.2 setup, deterministic OpenBLAS,
  source/static parsing, scientific helper contracts, exact bundle/index/file
  verification, and complete offline app source all passed. The validated
  manifest uploaded successfully; only the designed equality checkpoint failed
  because the repository still contained the pre-fix stylesheet checksum.
- **Artifact receipt:** promoted only
  `small-mammal-manifest-64e026f1b07dd0fa11b997059e47bcc2234c2c2d/manifest.json`;
  manifest SHA-256 is
  `90c1366fcd51c507cb786a45a60dd59607a6980f97fc2e4d2e21b29af326d28e` and the
  artifact archive digest reported by GitHub is
  `sha256:1fc493a8a7d0a555a59f7df3123fd43447b9e1c06a392c71140a29f0f1692e22`.
  Semantic comparison found identical top-level metadata, 91-package closure,
  and 118-file inventory; the sole change is `www/styles.css` MD5 from
  `3c7622a4cc3ae6baa12d34ca598f407e` to
  `1f7eeeecf91ef0e805a2df6318c9dcfe`. All 118 manifest checksums independently
  match the exact branch files.
- **Production status:** this promotion changes no app behavior, science, data,
  photograph, Pages cover, Connect deployment, or Driver artifact. Require an
  exact-head green run before merge and republish.

### 2026-07-19 18:08 MST - artistic Living Poster production closeout / root

- **Exact release:** PR #83 merged as
  `bdf56b0482ac76364e7055107361d58d8728d782`. Exact-head run `29710106316`,
  job `88252772796`, and post-merge run `29710189453`, job `88252947859`,
  passed every pinned runtime, science, bundle, offline-source, and manifest
  equality gate. Semantic production run `29710189426` also passed.
- **Connect receipt:** deployment #122 successfully published exact commit
  `bdf56b0` at 18:00 MST with R 4.5.2, all 91 manifest packages provided, and a
  four-second publish duration. The live app served the documentary Living
  Poster and completed the saved JORN load without a disconnect.
- **Responsive live QA (PASS):** at 320 x 800 the viewport/client/scroll widths
  were 320/305/305 with 12 px bslib gutters, a single-row 59 px top bar, 169 px
  brand, 77 px action cluster, 44 px Help target, 257 px poster card, and 48 px
  CTA. At 390 x 844, client/scroll widths were 375/375 with the same 59 px bar,
  a 327 px poster, and 48 px CTA. At 1280 x 720, client/scroll widths were
  1265/1265 and the split poster was 1072 px wide. The 1800 x 1350 app image
  loaded at every viewport, its credit scrim remained readable, and no legacy
  floating mascot or horizontal overflow appeared.
- **Interaction/browser QA (PASS):** Help opened its dialog; the functional
  theme control changed the document to dark mode and back; the poster-to-picker
  transition, JORN dashboard, and loading overlay completed. Browser logs held
  no error-level entries. Remaining messages were upstream datepicker
  deprecations and Leaflet's benign conditional-output lookup logs.
- **Pages receipt (PASS after platform recovery):** Pages run `29710189059`
  initially failed when GitHub's own Pages API returned HTTP 503 during Jekyll
  metadata and deployment creation. Attempt 3 passed build job `88253461931`
  and deploy job `88253454825` for exact merge `bdf56b0`. The public desktop and
  390 px cover both rendered “One trap night. A whole population story.” with
  the 4000 x 3000 rights-cleared source photograph and no horizontal overflow.
- **Driver/suite disposition:** **NONE / NO DRIVER BYTE CHANGE**. Reusable suite
  learning is the dual-surface convergence rule: a public cover is not complete
  when the functional app still opens generically; carry the same concise promise
  into the app, verify the real deploy commit, and gate 320 px utility chrome in
  addition to the cover itself.

### 2026-07-19 19:01 MST - public Pages Living Poster correction candidate / small_mammal_live_recheck

- **Starting state:** fetched clean `origin/main` at documentation closeout
  `8d650b787075bb548d17f8380060597f5a8ff7f9` and created local branch
  `agent/small-mammal-pages-living-poster`. Connect remains production deployment
  #122 at runtime merge `bdf56b0`; GitHub Pages is configured from `main:/docs`.
- **Observed mismatch/root cause:** the owner correctly reported that the artistic
  public cover was not up. A cache-busted Pages response was byte-for-byte equal to
  current `docs/index.html` (SHA-256
  `4ca381e05d792761db77ac5fe105dd0ac69f88ef01839d276f9bda14e5d952e2`), and the
  latest Pages build API reported `built` with no error on exact closeout merge
  `8d650b7`. The file's last content commit was documentary V4 `37d9f60`; later
  Living Poster PRs #82/#83 intentionally changed only the Connect landing. Connect
  independently served the exact concise hook/promise/CTA and runtime image hash
  `e9d0158a...`. This was a missing Pages implementation, not cache or publication
  drift.
- **Changed/classification:** replaced the long documentary V4 Pages experience
  with a `product/UI` plus `suite-platform` Living Poster: one dominant real
  Sherman-trap photograph, hook “One trap night. A whole population story.”,
  promise “Follow tagged small mammals across years of return visits.”, and one
  `Pick a place` CTA. Metric bands, methods blocks, repeated task cards, secondary
  actions, and the full companion directory no longer compete above or below the
  poster. A short below-fold Driver bridge, photo/data attribution, project links,
  canonical/social metadata, skip link, focus states, reduced-motion behavior, and
  52 px CTA remain. The static cover contract now enforces that concise boundary.
- **Media/provenance:** no image bytes changed. Pages still uses the reviewed
  4000 x 3000 USGS public-domain photograph with SHA-256
  `b678af26331a98747eba64c382f1ee7e2e1f7c1f77baecbd81dd46622a9e950a` and the
  independently composed 1200 x 630 social card with SHA-256
  `b36c62094270bf1598f38c6162ea6fda1b5c607f513a3faeea4ed62819fdf19a`.
- **Local static result (PASS):** `git diff --check`; Node parse and execution of
  `scripts/check_cover.mjs`; the unchanged in-app poster contract; all six custom
  message-handler arities; `node --check www/app.js`; safe CI YAML parse; shell
  syntax; exact local HTTP byte serving; and photo/social hash/dimension gates
  passed. The candidate contains one H1, one main, one safe live CTA, unique IDs,
  no insecure asset, no unsolicited fetch, and no synthetic metric/method band.
- **Accessibility result (PASS, static):** computed contrast ratios are acid/ink
  15.77:1, paper/ink 18.50:1, below-fold eyebrow/paper 6.12:1, bridge text/paper
  8.13:1, footer/paper 6.84:1, muted poster copy/ink 8.88:1, and product label/ink
  5.98:1. The skip link, visible focus, semantic landmarks/headings, meaningful
  image alt, 52 px CTA, 44 px footer controls, reduced-motion, increased-contrast,
  and forced-colors alternatives are present.
- **Independent root-owner browser result (PASS, visual/layout):** the local
  preview passed desktop, 390 px, and 320 px review with zero horizontal overflow.
  Desktop gives the real trap dominant visual weight and preserves the exact
  hook/promise/CTA. At 320 x 720 the photograph measured 310 x 263, the headline
  265 x 215, and the CTA 265 x 52; the full CTA remained visible by y=683 and all
  substantive links were at least 44 px high. The subagent backend itself exposed
  no browser, so console, full keyboard, and screen-reader interaction are not
  separately claimed. No R/runtime, scientific fixture, manifest, Connect
  interaction, or deployment result is required or claimed because only Pages
  HTML and its static test changed.
- **Failures/cleanup:** the first Ruby YAML probe omitted `require "date"` and was
  rerun successfully with the class loaded. An old system HTML4 `tidy`/libxml
  parser rejected standard HTML5 landmarks (`nav`, `main`, `section`, `figure`), so
  it is not used as evidence. No generated asset, temporary repository file, or
  production state was created.
- **Ownership/status/Driver implication:** current working-tree edits to
  `docs/index.html`, `scripts/check_cover.mjs`, and this evidence entry belong to
  this focused candidate. They are uncommitted and unpublished. Driver implication
  is **NONE / NO DRIVER BYTE CHANGE**; only the previously accepted artistic-poster
  suite pattern is corrected.
- **Next action:** commit/push the focused Pages branch, require exact-head CI,
  merge intentionally, and bind the resulting Pages build plus cache-busted public
  desktop/390/320 response before calling the dual-surface release complete.

### 2026-07-20 08:05 MST - owner-selected Small Mammal Living Poster V5 candidate / root

- **Starting state:** fetched clean `origin/main` at Pages merge
  `eb9e1a3e9e91096a1c1a82ebf116bf85d33405e3` and created focused branch
  `agent/small-mammal-living-poster-v5`. Production still serves the Cover V4
  documentary page from `eb9e1a3`; Connect remains deployment #122 at runtime
  `bdf56b0`.
- **Root cause:** the owner-approved “Who moves after dark?” / “Meet the tiny lives
  reshaping the landscape.” artistic direction was never promoted. Its exact
  900 x 600 screenprint existed only as the `living` concept embedded in the local
  suite direction board, while PR #85 changed the Pages layout but intentionally
  kept the USGS photograph, old copy, and validators that required both. The live
  result was therefore current source—not a cache or Pages failure.
- **Changed/classification:** Pages now uses the Vegetation-derived **Suite Living
  Poster V1** frame: focusable skip target; DDL topline plus one Driver route;
  app/unofficial eyebrow; two-line approved hook; one seven-word promise; one
  `Meet the mammals` CTA; responsive editorial art; visible illustration boundary;
  and a compact scope/honesty/Source/Feedback footer. The second marketing bridge,
  documentary credit, photo seam, metric/method bands, and full suite directory are
  absent. Connect carries a compact echo of the same hook, promise, art, boundary,
  Driver route, and CTA into the unchanged site picker. Classification is
  `product/UI` plus `suite-platform`; Driver scientific implication is **NONE**.
- **Art/provenance receipt:** preserved the exact approved concept as
  `docs/assets/small-mammal-living-poster-concept.jpg`, SHA-256
  `9fd6cd3d5e4fe2d54156aadb373bae94f8acc0cd526f963a94ae53de25cdd42a`.
  OpenAI image generation faithfully expanded it to a 1672 x 941 production
  screenprint with the metal box trap dominant and visibly much larger than the mouse;
  production PNG SHA-256 is
  `b414540c6ec0172ee586ef016fc204cb2aef7bc60ad73f7dc6f8f0ea27372ef1`.
  Responsive 1672 x 941 and 840 x 473 WebPs are 542,556 and 105,152 bytes with
  SHA-256 `7a1f9c78895868d0e540ad3126cf5af6b46a25ed4d02b8cf9113fa613cf2fe29`
  and `5fc560601bbb0146ffdfc03e2cf662bc7a8e20dce790f52f218ea1bdba8904a5`.
  Pages and Connect copies are byte-identical. The separately composed 1200 x 630
  social card is 1,162,569 bytes with SHA-256
  `8001d35b5e905570773a1fb4916ec8ac78ce101e6de0f8c7eb3f0a552951f8b4`.
- **Science/data boundary:** the art contains no text, logo, UI, data value, species
  assertion, injury, handling, or implied NEON observation. Both covers visibly say
  it is editorial illustration, and the footer states that sampled capture and
  recapture records do not alone measure total population size, complete movement,
  landscape-wide abundance, causal change, landscape effects, or ecosystem
  engineering. The Connect first frame carries the same measured-effect boundary.
  Picker IDs, server logic, helpers,
  bundles, indexes, estimators, exports, and Driver generated/scientific artifacts
  are unchanged.
- **Static gates (PASS):** `scripts/check_cover.mjs` and
  `scripts/check_in_app_landing.mjs`; syntax checks for all browser JavaScript and
  both cover validators; all six Shiny custom-message arities; safe YAML parse of
  all workflows; `bash -n scripts/post_deploy_smoke.sh`; asset hash/dimension/budget
  gates; and `git diff --check`. The public contract requires the shared frame,
  exact approved copy, one CTA, responsive `picture`, 44/52 px controls, high
  contrast disclosure, 700/420/340 seams, reduced-motion/high-contrast/forced-color
  handling, unique IDs, no external fonts, no prewarm, and one same-tab app link.
- **Visual evidence:** the production art and rendered social card were inspected at
  their original sizes; both preserve the selected analog screenprint, recognizable
  mouse, and oversized trap. The in-app browser security boundary correctly refused
  a local `file:` URL, and its isolated backend could not reach the temporary local
  server, so no local full-page browser or live Shiny claim is made. Deployed
  desktop/390/320 focal crops, keyboard order, Connect binding, and console remain
  release gates.
- **Pinned runtime/manifest receipt (PASS 1 / candidate promoted):** draft PR #86
  opened from head `016c7792e5b361ba2a377fe258ea42b5b3284c26`. Pinned R 4.5.2
  run #40 (`29754771218`, PR merge revision
  `93d426787312fb02925ccbee92f6277a721389eb`) passed dependency installation,
  deterministic OpenBLAS, R parsing and static assets, scientific helper
  contracts, manifest generation, exact bundle/index/checksum verification, and
  complete offline app sourcing. As designed, only the committed-manifest
  equality step failed on the first pass.
- **Manifest artifact:** validated artifact `8466215095`,
  `small-mammal-manifest-93d426787312fb02925ccbee92f6277a721389eb`, had
  ZIP digest SHA-256
  `3196a5e4388c8b6f488f5490992899f1f32fbfc9b0e1d8437dc8065fba855d34`.
  Its sole file, `manifest.json` (292,684 bytes), had SHA-256
  `3fdf334febde34f93f75430bd5ef7daa61cc36f1d6ef7f540578051bee24d3fc`
  and was promoted byte-for-byte. Package records are unchanged. The file
  inventory diff is exactly `ui.R` MD5
  `0269b7c678d0e22f55efae8e69095010`, `www/styles.css` MD5
  `7a854fe0d93704c0b3f8b7753e433a6a`, removal of the retired runtime photo,
  and addition of PNG/full-WebP/compact-WebP MD5s
  `c99f9ea9bde20941cf7e51fff38e0af6`,
  `eef94d4022f688e42fe2791cf38195b5`, and
  `e78e20a1ffd7915e15deb8e8047dbc72`. The `wk` 0.9.5 package remains pinned
  to repository `https://cran.r-project.org` with full `RemotePkgRef`
  `url::https://cran.r-project.org/src/contrib/wk_0.9.5.tar.gz`, closing the
  previously observed missing-protocol failure mode. Exact-head equality is the
  remaining pre-merge gate.
- **Documentation/learning:** corrected the app-local Driver knowledge package,
  project status, data takeaway, provenance, and neonize playbook. The reusable
  rule is now explicit: every companion cover shares the Living Poster structural
  frame while keeping app-native art, palette, copy, crop, and scientific boundary;
  Driver is the suite ambassador and companion covers link there instead of
  repeating the directory. The unused 619,324-byte Cover V4 runtime photo
  derivative was removed from `www/`; its source and release evidence remain in
  `docs/` and git history, so Connect no longer bundles dead cover media.
- **Ownership/status/residual risk:** all working-tree changes belong to this focused
  correction; no unrelated user or idea-branch bytes were touched. Production,
  data, and Driver repositories remain unchanged; only the reviewed CI artifact
  changed `manifest.json`. Historical Cover V4
  and rejected V3 source/social media stay versioned but unreferenced as release
  evidence; the unused V4 runtime derivative is recoverable from git history.
- **Next action:** commit and push the promoted manifest, require the next exact-head
  run to be fully green, merge PR #86, republish Connect, and verify both live
  entrances at desktop, 390, and 320 pixels before writing the final closeout and
  central Driver learning receipt.

### 2026-07-20 09:02 MST - Cover V5 publication closeout / root

- **Starting state:** clean release branch at `3e66ddcab6119b4ab0cace85a64616cb19cf766b`
  with the validator-made manifest promoted. Production still exposed Cover V4
  before the intentional merge; no data, estimator, export, picker, or Driver
  scientific artifact changed in this release.
- **Publication result (PASS):** exact-head pinned R 4.5.2 run #41
  (`29755133857`) passed R parsing, helper contracts, static cover contracts,
  bundle/index/checksum integrity, offline app sourcing, and committed-manifest
  equality. PR #86 merged as `c4c46fce3725126231504d8f9610f52e8f929ef8`.
  Main CI `29755368217`, semantic production smoke `29755368297`, and Pages
  build/deploy `29755366998` all completed successfully.
- **Manifest/runtime receipt (PASS):** the promoted `manifest.json` SHA-256 is
  `3fdf334febde34f93f75430bd5ef7daa61cc36f1d6ef7f540578051bee24d3fc`.
  Connect deployment #125, history
  `019f8025-2682-eb5d-2ee6-84b1f3872a6b`, published exact commit `c4c46fce`
  in five seconds using R 4.5.2 and 91 packages supplied by Connect. Its log
  resolved `wk@0.9.5` from the complete pinned CRAN URL and the worker reached its
  listening state; the earlier unsupported empty-protocol failure did not recur.
- **Pages visual receipt (PASS):** the cache-busted live page was inspected at
  1200 x 630, 390 x 844, 320 x 568, 320 x 720, and both sides of the 700 and 980 px
  responsive seams. It showed the exact hook, promise, CTA, Driver route, visible
  illustration boundary, dominant large trap and recognizable mouse, with no old
  photograph or horizontal overflow. Live full/compact WebPs matched the versioned
  SHA-256 hashes `7a1f9c78895868d0e540ad3126cf5af6b46a25ed4d02b8cf9113fa613cf2fe29`
  and `5fc560601bbb0146ffdfc03e2cf662bc7a8e20dce790f52f218ea1bdba8904a5`.
- **Connect visual/funnel receipt (PASS):** the same poster rendered in the live app
  at desktop, 390 x 844, 320 x 568, and 320 x 720 with zero horizontal overflow.
  On short 320 px devices the persistent app chrome correctly puts the CTA below
  the first viewport rather than clipping it. Clicking `Meet the mammals` focused
  `DIV#site-picker-start`; the 46-site picker loaded; JORN opened with all ten tabs,
  headline metrics, report route, and exploration controls present.
- **Interaction receipt (PASS):** clicking a species-composition bar opened the
  full ranked-species modal. Clicking the live population environmental-driver bar
  changed the overlay from `None` to `Plants flowering (% in flower)` at its
  selected nine-month lead. The two startup-time Plotly registration warnings are
  therefore non-blocking initialization noise, not broken click funnels. Browser
  logs contained only vendor datepicker deprecations and Leaflet hidden-map lookup
  messages during dynamic view changes; there was no JavaScript exception or
  Shiny disconnect.
- **Documentation/classification:** this closeout corrects stale V4/candidate status
  in README, deploy, project-status, provenance, handoff, and the app-local Driver
  knowledge package. Classification is `product/UI` plus `suite-platform` and
  documentation; scientific Driver implication remains **NONE / CONTEXT / HOLD
  CURRENT OUTPUT**.
- **Evidence invalidated:** PR #85 / `eb9e1a3` and Connect deployment #122 remain
  historical Cover V4 receipts, not evidence for the owner-selected artistic
  poster. The canonical dual-surface Living Poster release is PR #86 / `c4c46fce`
  plus Connect deployment #125.
- **Failures/cleanup:** GitHub connector writes returned 403, so authenticated `gh`
  CLI was used for the draft PR and merge. A temporary Connect history-tab URL
  failed to load, but the signed-in content dashboard supplied the exact deployment
  receipt. No generated candidate, failed asset, stale runtime photo, or temporary
  repository file remains.
- **Residual risk/ownership:** historical Cover V4 and rejected V3 source media stay
  versioned and unreferenced as provenance evidence. Connect may emit harmless
  package-build-minor and Plotly initialization warnings; both primary chart click
  paths are proven live. The separate owner idea branch was not touched.
- **Next action:** merge this docs-only closeout, publish the corrected Small Mammal
  receipt and Suite Living Poster V1 frame contract to the central Driver learning
  loop, then pause before selecting the next companion app.

### 2026-07-20 14:45 MST - manifest merge-loop fix + cross-agent working / [Claude] Claude Code

- **Starting state:** clean `main` at `047204e`. Branch
  `claude/chatgpt-merge-issues-workflow-dmur4d` opened for this work; draft PR #88.
  No local R/GDAL runtime in this session, so `manifest.json` cannot be regenerated here.
- **Objective:** diagnose why the ChatGPT/Codex cover rework repeatedly "failed merges,"
  fix the workflow, capture reusable lessons, and let Claude + Codex collaborate through
  durable artifacts (owner switches between the two sessions).
- **Root cause (confirmed):** the self-referential manifest/derived-bytes CI gate —
  `ci.yml` regenerates `manifest.json` then `git diff --exit-code` with
  `permissions: contents: read` — plus no local R and `cancel-in-progress` forces a
  manual push -> CI-red-by-design -> promote-artifact -> re-run loop on every runtime
  change. Not a git error; every affected PR merged. Evidence: veg PR #4 = 13/30 CI
  runs; small-mammal cover = ~10 PRs. Codex was following its own `AGENTS.md` rule
  ("generate only in the pinned validator; promote the exact artifact").
- **Changed files / classification (suite-platform; docs + one additive workflow):**
  `.github/workflows/regenerate-manifest.yml` (NEW, Option A); `AGENTS.md`; `CLAUDE.md`;
  `docs/neonize-playbook.md` (§6 merge-loop + byte-determinism recipe, §7 Cover Contract,
  §8 working-across-agents); `.claude/agents/LESSONS.md` (2 entries: connor determinism,
  neonize CI-only loop); `docs/project-status.md`; this handoff. NO runtime, data,
  manifest, or scientific-artifact byte changed; `ci.yml` stays read-only.
- **New workflow:** `Regenerate manifest (manual)` — `workflow_dispatch`, job-scoped
  `contents: write`, refuses `main`, reuses the pinned R 4.5.2 + terra-1.8-50 closure,
  regenerate-twice determinism guard, `verify_bundle.R`, commits `manifest.json` back to
  the dispatched review/PR branch under `neon-data-bot`. Removes the manual artifact
  round-trip while keeping "write only to the final restricted publisher."
- **Results:** `python3 yaml.safe_load` PASS on all four workflows; `git diff --check`
  PASS; PR #88 exact-head CI in progress (docs/workflow only -> manifest gate expected
  GREEN, no manifest-tracked file changed). A cross-vendor code review of the Codex cover
  system (Claude reviewing Codex, adversarial verification) ran as a background workflow;
  findings are a follow-up, not applied here.
- **Residual risk:** the regenerate workflow is unexercised until it is on `main` (a
  `workflow_dispatch` file must be on the default branch to be triggerable). Cover-review
  fixes were deliberately NOT applied in this PR because they touch runtime files and
  would re-trigger the very manifest loop being fixed.
- **Ownership/status:** current-session work, owner `root`, pushed to
  `claude/chatgpt-merge-issues-workflow-dmur4d`, PR #88 (draft). Merge pending CI green.
- **Driver implication:** NONE — no Driver artifact byte changed.
- **Next action:** (1) merge PR #88 once exact-head CI is green (docs/workflow only, so
  Connect re-serves unchanged runtime); (2) after merge, dispatch `Regenerate manifest
  (manual)` once on a scratch branch to smoke-test it; (3) mirror the `AGENTS.md`
  escape-hatch, `CLAUDE.md` front-door note, `regenerate-manifest.yml`, and playbook §8 to
  Vegetation and Driver-Cascade (veg lacks `CLAUDE.md`/`LESSONS.md`; Driver default is
  `master` — consider renaming to `main`); (4) triage the cover cross-review findings —
  recorded in `docs/cover-cross-review-2026-07-20.md` (8 confirmed; 7 are manifest-neutral
  scripts/docs, only the `ui.R` `<html lang>` fix touches runtime, so land that one via the
  new regenerate flow); (5) promote the two
  new LESSONS + playbook §6/§8 to the canonical `TG-Data-Apps` playbook via `curator`.

### 2026-08-03 08:45 EDT - scheduled refresh retention and publisher repair / [Codex]

- **Starting state:** fetched current `origin/main` and created isolated worktree
  branch `codex/small-refresh-retention-fix` at exact default head
  `2b4993c291763d0893b6849441fe5572cebc3e63`. The watched branch remains `main`;
  public targets remain the Pages cover and Connect content recorded above. The
  source checkout was clean, and the existing local closeout branch was not changed.
- **Failure diagnosis/objective:** the scheduled validator reached dependency setup
  through moving CRAN-main source paths that do not retain historical tarballs; the
  suite audit observed missing historical `sf`/`sp` inputs before the publisher could
  run. Repair every active install/provenance lane without weakening exact versions,
  changing data, or publishing to production.
- **Dependency repair:** `sf 1.1-1`, `s2 1.1.11`, `units 1.0-1`, `wk 0.9.5`,
  `classInt 0.4-11`, `raster 3.6-32`, and `sp 2.2-1` now resolve from the immutable
  `https://packagemanager.posit.co/cran/2026-07-15/src/contrib/` source snapshot in
  CI, scheduled refresh, manual manifest regeneration, the manifest writer, and the
  independent bundle verifier. `terra 1.8-50` remains on its exact CRAN archive URL.
  All affected dependency caches moved from v2 to v3 so no stale failed closure can
  mask the repair.
- **Publisher repair:** the restricted refresh publisher now refuses a producer SHA
  that does not contain current `main`, detects untracked scoped candidate files,
  requires the promotion commit to be a direct child of the validated SHA, verifies
  the remote review branch resolves to that exact commit, and fails on ambiguous PRs.
  It no longer invokes `gh pr create`. With no open PR it emits a reviewer-action
  notice; with one exact `main <- automation/small-mammal-data-refresh` PR it verifies
  base/head/OID and records that a write user must approve the held exact-head checks.
- **Static/source results (PASS):** `git diff --check`; Ruby/Psych parsing of CI,
  refresh, and regeneration YAML; Bash syntax of the publisher block; parse of all
  16 R files under local R 4.5.3; both cover contracts; stale-URL/cache/`gh pr create`
  scans; and exact checks that `manifest.json` was not modified all passed. HTTPS HEAD
  probes returned 200 for the retained terra archive and all seven dated Posit source
  tarballs.
- **Runtime results (expected BLOCKED):** `scripts/verify_bundle.R` loaded the exact
  46-site family and 46/604/604 indexes, then rejected exactly the seven old
  `RemotePkgRef` values in the intentionally unchanged committed manifest. This is the
  required pre-promotion state, not a relaxed gate. `scripts/test_helpers.R` could not
  run because this local R library lacks `dplyr`; the pinned R 4.5.2 dependency build,
  helper fixtures, regenerated-manifest equality, and complete offline source remain
  unclaimed until CI.
- **Changed/classification:** modified `.github/workflows/ci.yml`,
  `.github/workflows/refresh-data.yml`, `.github/workflows/regenerate-manifest.yml`,
  `scripts/write_manifest.R`, `scripts/verify_bundle.R`, and this handoff. Classification
  is `suite-platform` plus `app-local` release maintenance. `manifest.json`, all data,
  indexes, app/runtime/scientific code, Pages, Connect, and Driver artifacts are unchanged.
- **Failures/cleanup/ownership:** sandboxed URL probes initially failed DNS and were
  rerun read-only with network access; all eight returned 200. At audit time no
  temporary repository artifact, cache, candidate, commit, push, PR, dispatch, or
  deployment had been created. Publication still requires the exact receipts below.
- **Driver implication:** **NONE / NO DRIVER BYTE CHANGE**. Existing Small Mammal
  `CONTEXT` semantics and scientific outputs are unchanged.
- **Residual risk:** only pinned Ubuntu 22.04/R 4.5.2 can prove source compilation and
  validator-made manifest bytes. The scheduled publisher behavior also remains
  unexecuted until this repair is reviewed and merged.
- **Next action:** review and publish this focused branch, run pinned CI, use the manual
  regeneration workflow to commit its exact new `manifest.json`, require green checks
  on that exact head, then merge. On the next validated refresh, a signed-in reviewer
  opens the `main` PR (or approves the held checks on the exact updated existing PR).

### 2026-08-03 10:10 EDT - moving Plotly provenance blocked before merge / [Codex]

- **Exact evidence:** manual manifest promotion run `30818827839` committed direct
  child `a34b25221ef1ba29970b8a5e749f6054fbf251b6`; explicitly dispatched exact-head
  validator run `30820291958` passed. A semantic inspection then found the promoted
  Plotly record came from moving `https://cran.rstudio.com` at version `4.12.1`, while
  its top-level repository had been canonicalized to the July 15 Posit snapshot.
- **Why green was insufficient:** the independent verifier checked only top-level
  repository lanes for ordinary packages, so it did not compare `RemoteRepos` or
  prove that the version existed in the declared snapshot. Direct HTTP checks proved
  `plotly_4.12.1.tar.gz` is absent from that snapshot (404) and retained
  `plotly_4.12.0.tar.gz` is available (200). Rewriting the moving URL would therefore
  create false provenance and risk a Connect restore failure.
- **Focused correction:** CI, scheduled refresh, and manual regeneration now install
  exact retained Plotly 4.12.0 from the July 15 source snapshot and use fresh v4 caches.
  The writer rejects `cran.rstudio.com` before canonicalization, pins Plotly's exact
  version/source URL, removes only its non-semantic `Built` clock, and requires the
  dated top-level lane. The independent verifier repeats that exact-source check and
  rejects any standard package whose `RemoteRepos` is not the dated snapshot.
- **Boundary/status:** the green `a34b252` run remains valuable diagnostic evidence
  but is not merge authorization. No runtime, scientific, data, Pages, Connect, or
  Driver byte changed in this follow-up. Its promoted manifest remains intentionally
  superseded until the restricted workflow regenerates it from the corrected closure.
- **Next action:** validate and publish this additive correction, rerun manual manifest
  regeneration on the review branch, inspect the bot commit byte-for-byte, and require
  one green validator on that exact final head before merging PR #91.
