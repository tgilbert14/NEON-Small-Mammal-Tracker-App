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
- Release state: **PRODUCTION HEALTHY / PASS 1 + COVER V3 COMPLETE**; Connect
  runtime `1615ab4e74fd16a2698de8431acb862d6cc4cebf` remains verified and Pages
  cover V3 is published from merge `2492056ee62f21b5581de69d6da6d46f2fd37b34`.
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
