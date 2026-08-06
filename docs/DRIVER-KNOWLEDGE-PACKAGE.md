# Small Mammal Tracker -> Driver knowledge package

Status: **PASS 1 + COVER V5 COMPLETE / PRODUCTION VERIFIED**

Learning disposition: **CONTEXT**

Driver action: **HOLD CURRENT OUTPUT; no Driver artifact byte change**

This package promotes only evidence that passed the pinned app validator and final
semantic production health. The central suite learning ledger must vendor this exact
receipt before another companion pass begins.

## Product identity and immutable evidence

- App/repository: NEON Small Mammal Tracker /
  `tgilbert14/NEON-Small-Mammal-Tracker-App`
- NEON product/table: `DP1.10072.001` / `mam_pertrapnight`
- Audited base: `39dca56c69ef11188333effefd4b2d5bc28948ee`
- Pass 1 scientific/runtime merge: `1615ab4e74fd16a2698de8431acb862d6cc4cebf`
- Pass 1 exact-head validator: run `29663236510`, job `88129323716`
- Pass 1 main validator: run `29663335706`, job `88129588478`
- Pass 1 semantic production health: run `29663335708`, job `88129588525`
- Pass 1 Pages publication: run `29663335341`
- Pass 1 validated raw manifest artifact: R 4.5.2, 91 packages, 117 files, SHA-256
  `3fba04eb885b3cb6a9437b8c8b25ade25d44d47f6dcb50add025e754a6de04d7`
- Pass 1 canonical deployable manifest: R 4.5.2, 91 packages, 117 files, current runtime
  checksums, eight wall-clock URL-package `Built` timestamps removed, and the
  top-level deployment lane normalized to `Source: CRAN` plus the absolute
  `https://cran.r-project.org` origin Connect requires; SHA-256
  `f6c4a5ff74053b95e22fac7394f1930d2fe2329663737031b1c32f7a1f70bc54`
- Bundle/index contract: 46/46 expected site bundles loaded with rows and the
  physical-effort schema; national index row counts are 46/604/604; 145 species.
- Cover V5 / Suite Living Poster V1: PR #86, source head `3e66ddca`, merge
  `c4c46fce3725126231504d8f9610f52e8f929ef8`; exact-head validator
  `29755133857`, main validator `29755368217`, semantic production health
  `29755368297`, and Pages publication `29755366998` all passed.
- Cover V5 canonical manifest: R 4.5.2, 91 packages, 120 files, SHA-256
  `3fdf334febde34f93f75430bd5ef7daa61cc36f1d6ef7f540578051bee24d3fc`;
  Connect deployment #125 published exact `c4c46fce` and passed live JORN and
  chart-interaction checks.

Final exact-head and main runs passed deterministic Haswell/one-thread OpenBLAS,
complete R/JavaScript/shell parsing, the six-handler Shiny registration contract, all
11 scientific helper fixtures, exact package provenance, file checksums, all 46 site
bundles, the 46/604/604 indexes, complete offline app source, and committed
manifest/data equality. Connect restored the absolute CRAN dependency lane, served
the semantic ready marker, loaded the JORN interaction funnel, and exposed no
first-party console warning/error in a fresh browser session.

## Unit, support, and opportunity

- Source row: a trap event, an empty-trap/status record, or an animal handling row.
  Multiple animal rows can describe one physical multi-capture trap event.
- Animal identity: nonblank `tagID`; repeat rows are recaptures/handling events, not
  independent animals.
- Spatial support: plot/trap coordinates within one of 46 terrestrial NEON sites.
- Temporal support: trap night, bout, month, and calendar year. The bundled record is
  2013–2024; site spans range from 1–12 years (median 10).
- Reviewed effort weights: not set = 0; disturbed/door/spoor tokens 2 and 3 = 0.5;
  multi-capture, capture, and set-empty tokens 4, 5, and 6 = 1 trap-night.
- Canonical coordinates are A–J x 1–10. Repeated status-4/5 animal rows at one
  canonical event collapse to one trap-night while every unique tag remains a capture.
- Two exact source-reviewed remark markers denote two physical traps at one coordinate
  and therefore sum the two row weights.
- AX–JX, X1–X10, and XX are non-unique placeholders retained as explicitly uncertain
  row-level effort. Unknown status, incomplete keys, unreviewed coordinates, and
  ambiguous canonical duplicates fail closed.

This definition now matches the Driver physical-event resolver at the contract level;
there is no parity patch to re-apply to the current Driver.

## Trusted signals

| Signal | Definition | Honest meaning | Disposition |
|---|---|---|---|
| CPUE | 100 x nonblank-tag capture rows / reviewed physical trap-nights | within-site catch-per-effort activity index | `CONTEXT`; never a raw cross-site abundance vote |
| MNKA | distinct tagged individuals known alive in the registered window | minimum-known-alive index, not population size | `CONTEXT`; app-local preferred consumer summary |
| Closed-capture N-hat | gated Schnabel/Chapman estimate with interval and p-hat | detection-corrected estimate under bout/model assumptions | app-local only; do not absorb into Driver yet |

Coverage is a scientific qualifier, not a UI footnote: 49% of 8,200 pooled bouts are
single-night/index-only. Mean detection completeness varies from about 0.42 to 0.95
among sites. The app therefore carries p-hat/mean N-hat into Compare and suppresses a
raw-count winner when detection differs materially or is un-estimable.

## Driver joins and mechanism claim

- Candidate join key: terrestrial NEON `siteID` x registered calendar year.
- Exact current-source site/year join and match rate: **UNKNOWN / HELD**. This pass did
  not rebuild the Driver or substitute a new eligible source pin.
- The current Driver's older pinned source oracle already checks source/derived
  calendar parity. That does not establish parity for this newly reviewed app bundle.
- Candidate mechanism record: monsoon precipitation -> next-year small-mammal CPUE;
  expected sign `+`, lag 1 year, July–September window, water-limited prior.
- Mechanism status: **context only**. The bundle lacks a seed-crop mediator, pools
  guilds with different responses, has short/uneven support, and shows weak,
  biome-inconsistent environmental associations.

## CAN / CANNOT / HELD

CAN:

- describe within-site capture activity and tagged-animal histories;
- report MNKA and supported closed-capture estimates with assumptions and coverage;
- expose reviewed effort, QC fields, detection qualification, and tidy exports; and
- inform a later suite-wide consumer synthesis as contextual evidence.

CANNOT:

- claim absolute density from CPUE or MNKA;
- compare raw cross-site magnitude without detection/method qualification;
- treat every source row as one physical trap-night;
- turn a mixed-community sign into a universal population response; or
- infer rainfall causality from the short observational series.

HELD:

- current-source Driver site/year join parity and eligible-source pin changes;
- pooled seasonal/lag promotion until support, guild stratification, and mediators are
  evaluated across all nine companion packages.

## Reusable learning package

Promote to every later app/subagent brief:

- **Infrastructure:** empty-stage exact-entity production; pinned R/system/package
  closure; manifest generated from the installed runtime; exact checksum/offline boot
  gates; read-only CI; immutable reviewed release; semantic post-deploy health that
  opens an outage issue and closes it on recovery.
- **Provenance:** validate both the installed-package record and the deployable
  network contract. Direct URL installs retain `RemoteType: url` and exact
  `RemotePkgRef` in DESCRIPTION metadata; Connect's top-level fields must use
  `Source: CRAN` plus an absolute repository URL so current/archive resolution works.
  Failed candidates may be retained briefly only when unmistakably labelled
  UNVALIDATED. Source-package wall-clock `Built` timestamps must be deterministically
  removed while all identity, origin, compatibility, and checksum fields remain
  hard-gated.
- **Publication:** a merge only makes a source revision available; verify Connect's
  **Last deployed** commit, explicitly republish when it lags, and require a semantic
  production receipt. Treat installed-package provenance and the platform's network
  contract as separate gates.
- **Browser/runtime:** Shiny custom message handlers must accept exactly one payload
  argument, including no-payload handlers; inventory and gate them so a dependency
  upgrade cannot silently disable registration.
- **Data/release UX:** report bundle freshness as reviewed/committed, not live; never
  turn an opaque no-CORS pre-warm into a readiness claim; automated data refreshes
  create restricted review candidates rather than writing production directly.
- **Product/UI:** preserve a mature task flow, add explicit focus/reduced-motion
  contracts, connect all ten suite products in About, and keep the reviewed social
  cover versioned separately so a code fallback cannot overwrite it.
- **Scientific presentation:** carry opportunity, detection, support, units, and NA
  conventions into both UI and tidy export; suppress winners the measurement design
  cannot defend.

### Cover V5 / Suite Living Poster V1 learning (2026-07-20)

- An approved concept is not a release until its exact copy and media become
  versioned product assets. Small Mammal's selected “Who moves after dark?”
  screenprint lived only in a concept board while the repository and both deployed
  surfaces still enforced Cover V4. Preserve the selected source, production asset,
  prompt, hashes, and validators together so approval cannot be mistaken for ship.
- Standardize the **frame**, not the subject: DDL identity plus one Driver suite
  route; a 3–7-word hook; one plain-language promise; one contextual CTA; one
  dominant editorial image with meaningful alt text and durable provenance; no
  ornamental illustration badge; compact scope, honesty,
  Source, and Feedback footer. Each app still owns its palette, motif, copy, focal
  position, and scientific limitation.
- A companion cover should earn the first click before explaining the whole app.
  Small Mammal now uses the owner-approved hook, seven-word promise, and one action;
  it removes the second marketing bridge, feature directory, metrics, and methods
  from the cover. Driver—not every companion cover—is the suite ambassador.
- Artistic imagery needs the same evidence discipline as documentary media. The
  screenprint is identified as editorial illustration in alt text and provenance,
  avoids data marks and exact-species claims, and is paired with an honesty note that capture records do
  not alone measure total population, complete movement, causal change, or
  landscape-wide abundance.
- Treat Pages and Connect as one invitation with two surfaces. The same hook,
  promise, illustration authority, Driver route, and CTA intent now lead into the
  unchanged site picker; a Pages-only swap would repeat the mismatch this release
  exists to fix.
- Treat narrow responsive QA as a release gate. Validate the image-first seam,
  focal crop, full CTA, one-row utility chrome, 44px controls, and zero horizontal
  overflow at 320, 390, and the desktop breakpoint using the real bslib gutters.
- A green source merge is not publication evidence. Record the exact Connect
  deployment commit and Pages artifact, then inspect the deployed illustration,
  focus order, CTA-to-picker path, utility controls, focal crop, overflow, and browser
  console. Retry platform HTTP 503s without misclassifying them as source defects.
- Small Mammal V5 passed that contract: PR #86 merged as `c4c46fce`; exact-head
  pinned run #41 and all three `main` workflows were green; Pages served the
  versioned poster assets byte-for-byte; and Connect deployment #125 published the
  same revision, focused the CTA on the 46-site picker, loaded JORN, and preserved
  working species- and environmental-driver bar interactions.

Driver disposition remains **CONTEXT / HOLD CURRENT OUTPUT**. These are reusable
suite design and provenance rules; they do not change a Driver scientific input,
join, eligible-source pin, or generated artifact.

Do not generalize without product-specific review:

- the six `trapStatus` weights, coordinate resolver, MNKA window, and closed-capture
  gates are small-mammal sampling contracts, not generic suite helpers;
- a richness/CPUE pattern from a mixed mammal community cannot become a Driver
  magnitude or sign vote without guild and detection qualification.

## Final decision and next dependency

Decision: **CONTEXT**. App-local physical-event parity is closed, but the current
Driver remains unchanged. Preserve CPUE/MNKA/N-hat as qualified consumer context and
revisit ingestion only after all companion packages are complete and the synthesis
re-evaluates the exact eligible source pin, join coverage, support, and mechanism.

Next dependency: publish this exact Cover V5 `CONTEXT` receipt to the central
Driver suite ledger and playbook, with Driver artifacts unchanged, then pause.
Do not select another companion app until the owner resumes the suite pass.
