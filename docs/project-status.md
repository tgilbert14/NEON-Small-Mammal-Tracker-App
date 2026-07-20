# Project status — NEON Small Mammal Tracker

> The context file the coordinators (`vgs`, `hk`, `neonize`) inject into every specialist brief so a
> cold-booted agent doesn't re-flag shipped or deliberately-deferred work. Owned by `triage`; keep it
> current after every review or ship. Depth: `README.md`, `DEPLOY.md`, `docs/neonize-playbook.md`.

**Last verified against the build and public endpoints:** 2026-07-19 (production
runtime and Pages merge `bdf56b0`; Connect deployment #122).

## Stage
- **Pass 1 plus Living Poster closeout complete / production healthy:** Pages and
  Connect pass pinned validation and semantic health.
- Runtime merge `bdf56b0` is deployed as Connect #122; exact-head and main CI,
  semantic health, Pages publication, and desktop/390/320 browser QA passed.
- The app is again the **flagship / reference** for honest statistics, reviewed releases, and
  product patterns. The documentary cover V4 and matching in-app Living Poster are
  published and verified at desktop and mobile breakpoints. Unrelated idea branches
  remain outside this pass.

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
- The social image is a reviewed 1200×630 titled habitat card; the cover no longer converts an
  opaque pre-warm request into a false readiness claim and has explicit focus treatment.
- The Connect app now opens with the same documentary promise as Pages, including a
  large Sherman trap, visible public-domain credit, and a single CTA into the existing
  picker. Its persistent controls remain one row at 320px.
- Authority docs now agree on the review-branch release boundary, pinned package provenance,
  semantic health, and the ten-app suite. The in-app About panel links all nine companions.

## Open / in-flight
- Long-term: Shinylive/WebAssembly static export (zero cold-start endgame) — see `DEPLOY.md` Option B.

## Deliberately deferred / non-goals
- Not a framework rewrite; not a multi-tenant app (it's a single public explorer).
- Live NEON API calls at runtime are intentionally avoided (bundled `.rds` is the design).

## P-items
- **P0 COMPLETE:** pinned release gates and semantic production health are green.
- **P1 COMPLETE:** cover V3, responsive assets, social metadata, provenance, and public mobile QA are verified.
- **P2 COMPLETE:** reviewed `CONTEXT` package is in the central Driver ledger; no Driver byte change.
- **P3:** evaluate Shinylive only after the Connect release path is stable.
