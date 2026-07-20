# Project status — NEON Small Mammal Tracker

> The context file the coordinators (`vgs`, `hk`, `neonize`) inject into every specialist brief so a
> cold-booted agent doesn't re-flag shipped or deliberately-deferred work. Owned by `triage`; keep it
> current after every review or ship. Depth: `README.md`, `DEPLOY.md`, `docs/neonize-playbook.md`.

**Last verified against the build and public endpoints:** 2026-07-19 (production
runtime and Pages merge `eb9e1a3`; Connect remains deployment #122 from `bdf56b0`).

## Stage
- **Pass 1 complete / Cover V5 correction in review:** production Pages and Connect
  remain healthy, but both still show the Cover V4 documentary treatment until the
  current Suite Living Poster V1 candidate passes the pinned manifest cycle.
- Pages source is `eb9e1a3`; Connect deployment #122 remains runtime `bdf56b0`.
  Cover V5 changes runtime art/CSS/UI and therefore requires an exact validator-made
  manifest, merge, republish, and live desktop/390/320 verification.
- The app is again the **flagship / reference** for honest statistics, reviewed releases, and
  product patterns. The V5 candidate preserves the selected “Who moves after dark?”
  screenprint, adopts Vegetation's shared Living Poster frame, and converges Pages
  and Connect without changing picker or scientific behavior. Unrelated idea
  branches remain outside this pass.

## Shipped (don't re-flag as missing)
- National site-picker Leaflet map (by-site / by-species toggle, accessible list fallback, one-tap load).
- Per-site `.rds` bundles (instant load, no runtime network call); ~46 sites, 145 species.
- Detection-corrected abundance (Schnabel ≥3 nights / Chapman 2), MNKA, per-night detection, gated + clamped.
- Hill-number diversity profile; per-individual dossiers + holographic trading-card export; site compare; PDF report card.
- QC-flag system (ranked "verify, not wrong" flags + downloadable offending rows).
- Review-branch refresh CI: empty-stage producer, exact bundle/index/manifest/offline validator,
  restricted publisher, and semantic post-deploy outage issue.

## Production-shipped in pass 1

- Cross-site detection-qualified compare, single-night/index-only coverage, tidy capture + monthly
  series exports, and a codebook are live.
- Current production retains the reviewed 1200×630 Cover V4 documentary social
  card, makes no opaque pre-warm request, and has explicit focus treatment.
- Current Connect production opens with the concise Cover V4 documentary promise,
  visible photo boundary, and one CTA into the existing picker. Its persistent
  controls remain one row at 320px.
- Authority docs now agree on the review-branch release boundary, pinned package provenance,
  semantic health, and the ten-app suite. The in-app About panel links all nine companions.

## Open / in-flight
- Cover V5: promote the pinned manifest candidate, require an exact-head green run,
  merge, republish Connect, and verify both public entrances live. The candidate
  converges them on the same owner-approved screenprint, large metal box trap,
  concise hook/promise, illustration boundary, Driver route, and action.
- Long-term: Shinylive/WebAssembly static export (zero cold-start endgame) — see `DEPLOY.md` Option B.

## Deliberately deferred / non-goals
- Not a framework rewrite; not a multi-tenant app (it's a single public explorer).
- Live NEON API calls at runtime are intentionally avoided (bundled `.rds` is the design).

## P-items
- **P0 COMPLETE:** pinned release gates and semantic production health are green.
- **P1 IN REVIEW:** Cover V5 Suite Living Poster assets, social metadata,
  provenance, and static contracts are complete. Pinned run #40 validated and
  emitted the exact manifest candidate; that artifact is promoted, while the
  fully green equality rerun and live mobile QA remain.
- **P2 COMPLETE:** reviewed `CONTEXT` package is in the central Driver ledger; no Driver byte change.
- **P3:** evaluate Shinylive only after the Connect release path is stable.
