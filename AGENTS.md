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

## Suite learning

Each completed app pass must update:

- `docs/BUILD-TEST-HANDOFF.md`;
- `docs/DRIVER-KNOWLEDGE-PACKAGE.md`;
- current finding status in data/review documentation;
- the Driver suite evidence register and implication backlog; and
- any reusable playbook rule that actually passed here.

Classify changes as `app-local`, `suite-platform`, `scientific-contract`, and/or
`Driver-impacting`. State the Driver implication explicitly, including `NONE`.
