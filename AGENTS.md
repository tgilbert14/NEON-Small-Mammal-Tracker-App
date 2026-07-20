# Repository instructions

This repository publishes the NEON Small Mammal Tracker and supplies a source
product used by the separate Driver Response Atlas. Treat scientific definitions,
bundles, manifests, and the watched `main` branch as release-critical.

## Required reading

Before any repository work:

1. read this file completely;
2. read `docs/BUILD-TEST-HANDOFF.md` completely; and
3. for suite or Driver work, read the canonical
   `NEON-Driver-Cascade/docs/NEON-SUITE-LEARNING-LOOP.md` and
   `docs/NEON-SUITE-REVAMP-PLAN.md` when that repository is available.

Immediately before editing the handoff, reread its latest entry. Every session,
including a read-only audit, closes with an app-local handoff update.

## Start and close state

- Start with `git status --short`, branch, exact commit, and public/deployed target.
- Preserve unrelated work and known line-ending-only changes.
- End with `git diff --check`, an explicit status/ownership report, failed-attempt
  cleanup, residual risks, and the next action.
- Never claim a test passed if the required R/browser/deployment environment was not
  available.

## Scientific contract

- DP1.10072.001 rows are trap-event/handling records, not independent animals or
  population counts.
- CPUE is a within-site catch-per-effort index, not density or abundance.
- MNKA is a minimum-known-alive index, not detection-corrected population size.
- Detection-corrected estimates are withheld when capture support is inadequate.
- Trap effort is resolved at the physical-event level. Exact trap-status tokens,
  canonical A-J x 1-10 coordinates, reviewed multi-capture events, two exact
  documented double-trap markers, and placeholder coordinates are a fail-closed
  contract. Unknown duplicate/key/status patterns abort rather than guess.
- A source-app metric that disagrees with the independently implemented Driver
  adapter is a parity failure; fix it here and verify both sides.

## Data and generated surfaces

- App boot must remain bundle-only and network-independent.
- Do not delete the committed site set before a replacement set is validated in a
  staging directory.
- A refresh must prove the expected site set, schema, support, freshness, and bundle
  portability before promotion.
- Generated indexes, data bundles, social images, and `manifest.json` change only
  through their supported builders in a pinned runtime.
- A manifest is valid only when its runtime file checksums and package provenance
  pass semantic verification.

## Verification and publication

Run applicable gates in this order: static syntax/format, helper fixtures,
raw-source/scientific contracts, bundle/index integrity, deterministic build,
manifest semantics, offline boot/mutation tests, primary UI funnels,
desktop/mobile/accessibility, then publication.

The `main` branch is watched by Posit Connect Cloud; pushing to it is a publication
action. Do not push, dispatch a write-enabled refresh, regenerate production data,
or publish merely to diagnose. Prefer read-only producer/validator evidence and give
write permission only to the final restricted publisher.

HTTP 200 is not app health. Public verification must reject Posit startup/error
pages and require an app-specific ready marker after a real render.

## Landing a manifest/derived-bytes change without thrashing

`manifest.json` (and, in siblings, `data/search_index.rds`) are byte-exact gates: CI
regenerates them in the pinned validator and fails the run if the committed copy
differs. CI is read-only, so it cannot commit the fix for you. If you have no local
pinned R + GDAL-3.4.1 toolchain you CANNOT pre-generate a matching manifest — so the
first run of any runtime-touching change fails that gate BY DESIGN. Promote the
artifact; do not flail:

1. Make the change and push ONCE. Do not rapid re-push — `cancel-in-progress` cancels
   the running check, and each run source-compiles the geo closure (~10–40 min).
2. When the run fails the "Require committed manifest ... to match" step, download the
   VALIDATED `small-mammal-manifest-<sha>` artifact it uploaded (retention 3 days),
   NOT the `small-mammal-manifest-UNVALIDATED-<sha>` diagnostic artifact (which must
   never be committed). If an EARLIER gate failed, fix that first — no valid manifest
   exists to promote yet.
3. Commit that manifest byte-for-byte in the same PR, push once, and require the exact
   head to go fully green before merge.
4. If the gate still flaps after a faithful promotion, that is a determinism
   regression (a non-canonical `Built`/`locale` field), not something to brute-force —
   see the byte-reproducibility recipe in `docs/neonize-playbook.md` §6.

One PR per change — do not open separate "closeout/receipt" PRs; fold evidence into
the feature PR or the handoff. A cover/art change is a coordinated multi-file commit
(the images + their `www/assets` and `docs/assets` copies + the SHA-256 pins in
`scripts/check_cover.mjs` / `check_in_app_landing.mjs` + `docs/IMAGE-PROVENANCE.md`);
binaries cannot auto-merge, so rebase (do not merge) when the base moved.

Durable fix (implemented on this branch — owner to adopt): the pinned validator now
writes its own output back via the **`Regenerate manifest (manual)`** workflow
(`.github/workflows/regenerate-manifest.yml`). Once it lands on `main`, dispatch it on
your review/PR branch and it regenerates, verifies, and commits `manifest.json` for you —
steps 1–3 above collapse to "push, dispatch, re-run." It is manual-only, refuses to run
on `main`, and commits under a restricted bot identity, so "write permission only to the
final restricted publisher" still holds. It is deliberately NOT an auto-commit on every
PR, which would break that boundary.

## Suite learning

Each completed app pass must update:

- `docs/BUILD-TEST-HANDOFF.md`;
- `docs/DRIVER-KNOWLEDGE-PACKAGE.md`;
- current finding status in data/review documentation;
- the Driver suite evidence register and implication backlog; and
- any reusable playbook rule that actually passed here.

Classify changes as `app-local`, `suite-platform`, `scientific-contract`, and/or
`Driver-impacting`. State the Driver implication explicitly, including `NONE`.
