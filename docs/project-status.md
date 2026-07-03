# Project status — NEON Small Mammal Tracker

> The context file the coordinators (`vgs`, `hk`, `neonize`) inject into every specialist brief so a
> cold-booted agent doesn't re-flag shipped or deliberately-deferred work. Owned by `triage`; keep it
> current after every review or ship. Depth: `README.md`, `DEPLOY.md`, `docs/neonize-playbook.md`.

**Last verified against the build:** 2026-07-01 (agent sweep — content below carried from README/DEPLOY/playbook; re-verify against HEAD before trusting a specific claim).

## Stage
- **Live** on Posit Connect Cloud (auto-republish on push to `main`); GitHub Pages landing fronts it.
- The **flagship / reference** app of the NEON explorer suite — the gold standard siblings are measured against.

## Shipped (don't re-flag as missing)
- National site-picker Leaflet map (by-site / by-species toggle, accessible list fallback, one-tap load).
- Per-site `.rds` bundles (instant load, no runtime network call); ~46 sites, 145 species.
- Detection-corrected abundance (Schnabel ≥3 nights / Chapman 2), MNKA, per-night detection, gated + clamped.
- Hill-number diversity profile; per-individual dossiers + holographic trading-card export; site compare; PDF report card.
- QC-flag system (ranked "verify, not wrong" flags + downloadable offending rows).
- Auto-refresh CI (`refresh-data.yml`, monthly first-Saturday gate, `skip_download` fast path), now with
  pre-deploy bundle verification (`scripts/verify_bundle.R`) + post-deploy smoke (`scripts/post_deploy_smoke.sh`)
  — the refresh loop is closed (verify before and after deploy).

## Open / in-flight
- **Migration off shinyapps.io → Connect Cloud** (shinyapps sunsets end-2026): the standard for the suite;
  confirm this app is fully on Connect Cloud and retire any legacy `rsconnect/shinyapps.io/` + `deploy.R`.
- **terra 1.8-50 pin in the CI manifest regen** — the monthly refresh regenerates `manifest.json`, so the
  re-pin must live in the manifest-writing step or the refresh re-breaks the deploy (see `connor`).
- Long-term: Shinylive/WebAssembly static export (zero cold-start endgame) — see `DEPLOY.md` Option B.

## Deliberately deferred / non-goals
- Not a framework rewrite; not a multi-tenant app (it's a single public explorer).
- Live NEON API calls at runtime are intentionally avoided (bundled `.rds` is the design).

## P-items
- **P1:** confirm terra pin survives the CI manifest regen (deploy stays green after a monthly refresh).
- **P2:** finish/verify the shinyapps→Connect Cloud retirement.
- **P3:** evaluate Shinylive static export.
