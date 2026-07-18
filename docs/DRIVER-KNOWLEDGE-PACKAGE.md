# Small Mammal Tracker -> Driver knowledge package

Status: **RELEASE CANDIDATE / APP-LOCAL CONTRACT PASS**

Learning disposition: **CONTEXT**

Driver action: **HOLD CURRENT OUTPUT; no Driver artifact byte change**

This package promotes only evidence that passed the pinned app validator. The final
`main` merge and semantic public-health receipt belong in the central suite learning
ledger because this file cannot self-pin a future merge commit.

## Product identity and immutable evidence

- App/repository: NEON Small Mammal Tracker /
  `tgilbert14/NEON-Small-Mammal-Tracker-App`
- NEON product/table: `DP1.10072.001` / `mam_pertrapnight`
- Audited base: `39dca56c69ef11188333effefd4b2d5bc28948ee`
- Immutable runtime implementation: `c92158716953f9f1a8d5d329d0d6f95fd8e966bc`
- Provenance correction head: `ebf833937dbf19a864e4d2e1f2500ca298409895`
- Pinned validator: Actions run `29654547344`, pull-request merge revision
  `45b5c82640a20b8c181b6b60949e20ebbeb9c5b0`
- Validated raw manifest artifact: R 4.5.2, 91 packages, 117 files, SHA-256
  `3fba04eb885b3cb6a9437b8c8b25ade25d44d47f6dcb50add025e754a6de04d7`
- Canonical deployable manifest: the same validated record with the eight
  wall-clock URL-package `Built` timestamps removed and their top-level deployment
  lane normalized to `Source: CRAN` plus the absolute `https://cran.r-project.org`
  origin Connect requires; SHA-256
  `e619343d1d6404f52260481ec611ccbd1c9f5cd349657c0799f6d535a7bc0b11`
- Bundle/index contract: 46/46 expected site bundles loaded with rows and the
  physical-effort schema; national index row counts are 46/604/604; 145 species.

Run `29654547344` passed the deterministic Haswell/one-thread OpenBLAS assertion,
complete R/JavaScript/shell parsing, all 11 scientific helper fixtures, exact package
provenance, file checksums, bundle/index validation, and complete offline app source.
It stopped only at the designed committed-manifest equality gate after uploading the
validated candidate cited above. Exact-head run `29655286162` proved that all
substantive bytes and fields reproduced, but exposed eight wall-clock `Built`
timestamps from source compilation. The writer now canonicalizes only that
non-semantic field; a later exact-head run must match before merge.

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
  evaluated across all nine companion packages; and
- production-shipped status until reviewed merge plus semantic public health pass.

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
- **Data/release UX:** report bundle freshness as reviewed/committed, not live; never
  turn an opaque no-CORS pre-warm into a readiness claim; automated data refreshes
  create restricted review candidates rather than writing production directly.
- **Product/UI:** preserve a mature task flow, add explicit focus/reduced-motion
  contracts, connect all ten suite products in About, and keep the reviewed social
  cover versioned separately so a code fallback cannot overwrite it.
- **Scientific presentation:** carry opportunity, detection, support, units, and NA
  conventions into both UI and tidy export; suppress winners the measurement design
  cannot defend.

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

Next dependency: commit the validated manifest, obtain one green exact-head PR run,
merge intentionally, and record semantic production health plus the immutable merge
receipt in the central suite ledger before beginning Phenology.
