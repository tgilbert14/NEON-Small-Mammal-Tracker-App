# Project status — NEON Small Mammal Tracker

> The context file the coordinators (`vgs`, `hk`, `neonize`) inject into every specialist brief so a
> cold-booted agent doesn't re-flag shipped or deliberately-deferred work. Owned by `triage`; keep it
> current after every review or ship. Depth: `README.md`, `DEPLOY.md`, `docs/neonize-playbook.md`.

**Last verified against the build and public endpoints:** 2026-07-20 (Cover V5
merge `c4c46fce`; Connect deployment #125 from that exact revision).

## Stage
- **Pass 1 and Cover V5 complete:** Pages and Connect now share the owner-selected
  Suite Living Poster V1. Exact-head pinned CI, merge, Connect publish, semantic
  startup, responsive visual QA, and both primary launch funnels are verified.
- Merge `c4c46fce` is the canonical cover release. Connect deployment #125 published
  that exact revision with the validator-made manifest, including the complete
  absolute `wk` 0.9.5 tarball URL that closes the earlier protocol failure.
- The app is again the **flagship / reference** for honest statistics, reviewed releases, and
  product patterns. Cover V5 preserves the selected “Who moves after dark?”
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
- Current production uses the reviewed 1200×630 Cover V5 screenprint social card,
  makes no opaque pre-warm request, and has explicit focus treatment.
- Current Connect production opens with the concise Cover V5 promise, visible
  editorial-illustration boundary, one Driver route, and one CTA into the existing
  46-site picker. The CTA moves keyboard focus to that picker.
- Authority docs now agree on the review-branch release boundary, pinned package provenance,
  semantic health, and the ten-app suite. The in-app About panel links all nine companions.

## Open / in-flight
- **Suite-platform (draft PR #88):** manifest merge-loop fix — adds the manual
  `Regenerate manifest` workflow (Option A: validator writes its own manifest back,
  killing the download-artifact round-trip) plus the cross-agent working docs (playbook
  §6 byte-determinism recipe + §8 Claude/Codex collaboration, `CLAUDE.md`/`AGENTS.md`
  shared front door). Docs + one additive workflow; no runtime/manifest byte change.
  Merge once CI is green, then mirror to Vegetation and Driver-Cascade.
- Long-term: Shinylive/WebAssembly static export (zero cold-start endgame) — see `DEPLOY.md` Option B.

## Deliberately deferred / non-goals
- Not a framework rewrite; not a multi-tenant app (it's a single public explorer).
- Live NEON API calls at runtime are intentionally avoided (bundled `.rds` is the design).

## P-items
- **P0 COMPLETE:** pinned release gates and semantic production health are green.
- **P1 COMPLETE:** Cover V5 Suite Living Poster assets, social metadata,
  provenance, static contracts, exact-head pinned run #41, live desktop/390/320
  QA, and Pages/Connect release receipts are complete.
- **P2 COMPLETE:** reviewed `CONTEXT` package is in the central Driver ledger; no Driver byte change.
- **P3:** evaluate Shinylive only after the Connect release path is stable.
