# Small Mammal Tracker -> Driver knowledge package

Status: **DRAFT / PASS 1 IN PROGRESS**
Pinned audit source: `39dca56c69ef11188333effefd4b2d5bc28948ee`
Decision: provisional `CONTEXT`; not yet eligible for reintegration

## Product identity

- App/repository: NEON Small Mammal Tracker /
  `tgilbert14/NEON-Small-Mammal-Tracker-App`
- NEON product: DP1.10072.001
- Source table: `mam_pertrapnight`
- Bundle shape: one committed data frame per site under `data/sites/<SITE>.rds`
- Exact bundle/schema version and release hash: **pending pass verification**

## Unit and support

- Source row: trap event or handling record; multi-capture traps can emit multiple
  animal rows for one physical trap event.
- Animal identity: nonblank `tagID`; repeat rows are recaptures/handling events, not
  independent animals.
- Spatial support: plot and trap coordinate within a NEON terrestrial site.
- Temporal support: night/bout/month/year; actual revisit and completeness ranges
  must be recomputed from the pinned app bundle during this pass.

## Opportunity and effort

- Exact status weights: not set = 0; disturbed/door/spoor codes 2 and 3 = 0.5;
  multi-capture, capture, and set-empty codes 4, 5, and 6 = 1 trap-night.
- Canonical coordinates are A-J x 1-10. Status-4/5 multi-animal rows from one
  canonical physical event collapse to one trap-night while every unique tag remains
  a capture.
- Two exact source-reviewed remark markers document two traps at one coordinate and
  sum their row weights.
- AX-JX, X1-X10, and XX are non-unique placeholders retained as explicitly uncertain
  row-level effort.
- Unknown status, incomplete key, unreviewed coordinate, or ambiguous canonical
  duplicate fails closed.
- The current working-tree implementation mirrors this contract and adds
  adversarial fixtures, but it remains `HOLD` until the pinned R validator and
  bundle parity checks execute successfully.

## Trusted candidate signals

| Signal | Definition | Honest meaning | Current status |
|---|---|---|---|
| CPUE | 100 x nonblank-tag capture rows / reviewed physical trap-nights | within-site catch-per-effort index | working-tree parity fix; `HOLD` pending pinned execution |
| MNKA | distinct tagged individuals known alive in the registered window | minimum-known-alive index, not population | `CONTEXT`; window/detection support review pending |
| Closed-capture N-hat | supported Schnabel/Chapman-style estimate with interval and p-hat | detection-corrected app estimate under model assumptions | app-local; not proposed for Driver until support/model review |

## Driver joins and mechanisms

- Candidate join: terrestrial NEON `siteID` x registered calendar year.
- Exact site/year match rates: pending this pass.
- Existing Driver links involving mammal CPUE remain context-only because rainfall
  responses are delayed, nonlinear, guild-specific, and lack a measured seed-resource
  mediator.
- A mixed-community CPUE sign must not be treated as a population or universal
  bottom-up consumer response.

## CAN / CANNOT / HELD

CAN:

- describe within-site capture activity and tagged-animal histories;
- show MNKA and supported detection-corrected estimates with their assumptions; and
- expose effort/QC audit fields and uncertainty.

CANNOT:

- claim absolute density from CPUE;
- compare raw cross-site magnitude without detection/method qualification;
- treat every source row as one physical trap-night; or
- infer a causal rainfall-to-mammal link from a short site series.

HELD:

- Driver CPUE parity until the app-local physical-event resolver and fixtures pass;
- any pooled seasonal/lag signal until registered support and guild stratification
  are evaluated; and
- all release claims until the public app is restored from a coherent manifest.

## Reusable engineering and next dependency

Candidate reusable pattern: a companion-app release template with staged exact-site
promotion, dated package provenance, offline boot checks, semantic public health,
and review-branch-only automated publication. It becomes reusable only after it
passes here.

Next dependency: implement and verify the source-app effort resolver against the
pinned 46-site contract, then restore a coherent public release and measure Driver
join/support before final disposition.
