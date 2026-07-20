# Cover cross-review — 2026-07-20 (Claude reviewing the Codex Living Poster system)

A **cross-vendor** review: Claude Code reviewing ChatGPT/Codex's shipped cover system across
correctness, contract-integrity, honesty/provenance, and accessibility, with every finding
**adversarially verified** by an independent skeptic. **11 raised → 8 confirmed** (3 rejected as
not-real, so no false positives in this list). All 8 are real and still-actionable — 4 medium, 4 low.
Two degrade the live product for real users *today* (#1, #2); the rest are gaps in an otherwise-solid
guardrail.

These findings are **not applied** — this is the follow-up backlog. The **Manifest impact** column
says which are safe to land in an ordinary PR (`scripts/` and `docs/` are not manifest-tracked
runtime files) versus which touch a runtime file and must land via the `Regenerate manifest (manual)`
workflow so the manifest gate stays green.

## Confirmed findings (most severe first)

| # | Sev | File:line | Issue | Fix | Manifest impact |
|---|-----|-----------|-------|-----|-----------------|
| 1 | MED | `docs/index.html:73` | Focus ring is `outline: 3px solid var(--acid)` (#dce319) on the `--paper` (#f3e8cb) footer = **1.14:1** → effectively invisible to keyboard users (WCAG 2.4.7 + 1.4.11 AA), on the one currently-working surface | scope a dark ring for light backgrounds: `footer a:focus-visible, .honesty summary:focus-visible { outline-color: var(--ink); }` (≥3:1 vs paper) | none (Pages) |
| 2 | MED | `ui.R:87` | `bslib::page_fillable()` sets no `<html lang>` → rendered Connect app has no language (WCAG 3.1.1 Level A) while the cover correctly sets `lang="en"` | pass `lang = "en"` to `page_fillable(...)`; add a lang assertion to `check_in_app_landing.mjs` | **RUNTIME → regenerate flow** |
| 3 | MED | `docs/IMAGE-PROVENANCE.md:45` | The primary AI-generated cover (live on both surfaces) records tool/date/hash/alt but **no license/rights note** — below the veg sibling's own standard | add a Rights note mirroring the veg record (generated for this project; subject to the OpenAI account terms; NEON data licensing separate); optionally assert it | none (docs) |
| 4 | MED | `scripts/check_in_app_landing.mjs:71` + `check_cover.mjs:124` | `docs/assets` and `www/assets` copies are **never asserted byte-identical**; each script pins its own side against its own literals, so a one-sided art swap passes both contracts while the two entrances diverge | one direct cross-check: `readFileSync('docs/assets/X').equals(readFileSync('www/assets/X'))` per shared asset | none (scripts) |
| 5 | LOW | `scripts/check_cover.mjs:146` | `og-image.png` is hash-pinned but never tied to its `social-card.html` source; a card-copy edit re-pins the source but leaves the stale PNG green → crawlers serve outdated wording | render `og-image.png` from the card in CI and git-diff it, or fail when only one of the two social pins changed | none (scripts) |
| 6 | LOW | `scripts/check_cover.mjs:90` | og/twitter `image:alt` checked by attribute **name only** (substring match, also matches a comment) → an empty or commented-out alt passes | require `property=["']og:image:alt["']\s+content=["'][^"']+["']` and the twitter equivalent (match the veg sibling) | none (scripts) |
| 7 | LOW | `scripts/check_custom_message_handlers.mjs:4` | Handler regex requires literal `function` before `(`, so **arrow and named** handlers evade both the `seen === 6` tripwire and the arity guard (reproduced: a 7th arrow handler with wrong arity ships green). Suite-wide — `veg/scripts/check_browser_contracts.mjs:6` shares it | match format-agnostically (`function\s*[\w$]*\s*\(([^)]*)\)` OR `\(([^)]*)\)\s*=>`); replace magic `=== 6` with a `handlers.length` + duplicate-name guard | none (scripts) |
| 8 | LOW | `docs/IMAGE-PROVENANCE.md:34` | The CI-hash-pinned concept JPEG that seeds the cover states what it is *not*, but no tool/origin — AI-generated vs third-party isn't classified; the cited `neon-cover-directions.html` is absent from the repo | state the JPEG's tool/origin; commit `neon-cover-directions.html` or drop the dead reference | none (docs) |

## What Codex did well

The covers genuinely improved, and it shows in the machinery, not just the pixels. Codex built a **real
contract layer** — `check_cover.mjs`, `check_in_app_landing.mjs`, and `check_custom_message_handlers.mjs`
are all wired into `ci.yml` as gating steps, so the cover, the in-app landing, and the JS handler surface
each have an enforced spec. Assets are SHA-256-pinned with dimension and byte-budget cross-checks, and the
honesty ethic (AI-art disclosure, alt text, reduced-motion/contrast/forced-colors seams) is encoded as
machine checks rather than left to memory. `IMAGE-PROVENANCE.md` carries per-asset tool/date/brief/hash/alt
and separates production art from retired versions and from the attributed USGS documentary photo. Every
surviving finding is a *gap in a present guardrail* (a too-narrow regex, a hash pinned one-side, a `content`
requirement dropped on one of four adjacent lines) — the signature of a system designed to be checkable and
then under-tightened in spots, a far healthier failure mode than absent contracts.

## Still worth a deeper (non-static) pass

- Confirm #1 (focus contrast) and #2 (`<html lang>`) against the **actually-served** pages in a real
  browser + assistive tech — this review was static, and the lang mechanism was traced through bslib/shiny
  source because R wasn't runnable in-environment.
- Visually diff `social-card.html` against the pinned `og-image.png` once — a raster can't be text-checked.
- Suite sweep: #6 (alt name-only) and #7 (handler regex) both have veg analogues and are likely fleet-wide.
- Exercise the WebP dimension parser against a real lossless VP8L file if a future art update ships lossless.

_Source: the `codex-cover-cross-review` workflow (16 agents, dimension → find → adversarially verify →
synthesize). This file is the durable backlog; triage into a follow-up PR._
