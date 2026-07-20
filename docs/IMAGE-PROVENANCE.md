# Small Mammal cover image provenance

Status: **CURRENT / COVER V5 / SUITE LIVING POSTER V1**

Production receipt: PR #86 merged as `c4c46fce`; exact-head run
`29755133857`, main CI `29755368217`, semantic smoke `29755368297`, and Pages
deployment `29755366998` passed. Connect deployment #125 published that exact
revision, and the live Pages/Connect poster assets matched the versioned hashes
below during desktop/390/320 verification on 2026-07-20.

Cover V5 promotes the owner-approved artistic direction that was previously
shown only in the suite concept board. It replaces Cover V4's documentary-photo
treatment on both the public Pages cover and the Connect site-picker landing.
The illustration is an invitation, never a field photograph, NEON observation,
species record, or data visualization.

## Approved concept source

- Preserved file: `docs/assets/small-mammal-living-poster-concept.jpg`
- Original context: the `living` direction embedded in the local suite concept
  board `neon-cover-directions.html`
- Owner-approved copy paired with the concept:
  - “Who moves after dark?”
  - “Meet the tiny lives reshaping the landscape.”
  - “Meet the mammals”
- Original concept-board alt (preserved verbatim): “A stylized screenprint
  illustration of a mouse leaving an oversized humane live trap.”
- Dimensions: 900 x 600 JPEG
- Bytes: 195,163
- SHA-256:
  `9fd6cd3d5e4fe2d54156aadb373bae94f8acc0cd526f963a94ae53de25cdd42a`
- Use: provenance evidence only; the browser does not download this source.

This file is the selected visual direction, not one of the retired V3 generated
images. Preserving it in the repository closes the gap between the approved
concept and the production asset.

## Production Living Poster illustration

- Primary file: `docs/assets/small-mammal-living-poster.png`
- Dimensions: 1672 x 941 PNG
- Bytes: 3,123,255
- SHA-256:
  `b414540c6ec0172ee586ef016fc204cb2aef7bc60ad73f7dc6f8f0ea27372ef1`
- Tool: OpenAI image generation, using the approved 900 x 600 concept as the
  visual reference
- Generated: 2026-07-20
- Portable generation receipt: the reviewed output was copied byte-for-byte to
  the primary file above; its dimensions, date, tool, reference asset, prompt,
  and SHA-256 are the durable record (the transient tool path is not required).
- Use: lossless fallback for the Pages cover and Connect landing; visual source
  for responsive derivatives and the social card
- Alt text: “Editorial screenprint of a small nocturnal mouse emerging from an
  oversized metal box live trap beneath a starry sky.”
- Visible disclosure: “Editorial illustration—not a field photograph or data
  record.”

Generation brief:

> Faithfully extend and refine the approved Small Mammal screenprint into a
> production-ready 16:9 landscape. Preserve the realistically recognizable
> nocturnal mouse emerging from an oversized humane metal live trap, with the
> trap dominant and visibly much larger than the mouse. Preserve the low angle,
> rough ink grain, warm cream paper, charcoal night shapes, acid-chartreuse
> ground and stars, and restrained burnt-orange foliage. Keep the subject on the
> center-right with calm negative space left. No typography, labels, logos, UI,
> numbers, watermark, photographic realism, glossy 3D, cartoon anatomy, extra
> animals, tiny trap, injury, handling, or implied harm.

The production image is a faithful wide expansion of the approved concept, not
a synthetic field record. The exact composition was reviewed for the requested
large-trap scale, non-photographic treatment, and Vegetation Living Poster vibe.

## Responsive delivery files

Created mechanically from the production PNG with Sharp; no generative changes
were made after the production image was selected.

| Surface | File | Dimensions | Bytes | SHA-256 |
|---|---|---:|---:|---|
| Pages full | `docs/assets/small-mammal-living-poster.webp` | 1672 x 941 | 542,556 | `7a1f9c78895868d0e540ad3126cf5af6b46a25ed4d02b8cf9113fa613cf2fe29` |
| Pages compact | `docs/assets/small-mammal-living-poster-840.webp` | 840 x 473 | 105,152 | `5fc560601bbb0146ffdfc03e2cf662bc7a8e20dce790f52f218ea1bdba8904a5` |
| Connect fallback | `www/assets/small-mammal-living-poster.png` | 1672 x 941 | 3,123,255 | `b414540c6ec0172ee586ef016fc204cb2aef7bc60ad73f7dc6f8f0ea27372ef1` |
| Connect full | `www/assets/small-mammal-living-poster.webp` | 1672 x 941 | 542,556 | `7a1f9c78895868d0e540ad3126cf5af6b46a25ed4d02b8cf9113fa613cf2fe29` |
| Connect compact | `www/assets/small-mammal-living-poster-840.webp` | 840 x 473 | 105,152 | `5fc560601bbb0146ffdfc03e2cf662bc7a8e20dce790f52f218ea1bdba8904a5` |

The Pages and Connect files are byte-identical copies so both entrances keep the
same art, crop vocabulary, disclosure, and approved promise. Because the `www/`
files are runtime assets, they require a validator-generated `manifest.json`
before Connect deployment.

The full WebP has a 600 KB app-local cap rather than Vegetation's 400 KB cap so
the grain-rich screenprint can retain its ink texture. The 840 px first-choice
mobile derivative remains 105,152 bytes; the PNG is a lossless fallback only.

## Social card

- Code-native layout source: `docs/social-card.html`
- Layout-source SHA-256:
  `fed45cb67e270e8f8de92d93a5c59d9b8f1734bdd75c14120edf999da6956a02`
- Delivery file: `docs/og-image.png`
- Delivery dimensions: 1200 x 630 PNG
- Delivery bytes: 1,162,569
- Delivery SHA-256:
  `8001d35b5e905570773a1fb4916ec8ac78ce101e6de0f8c7eb3f0a552951f8b4`
- Rendered: 2026-07-20 from the same project-owned illustration, palette, and
  exact approved copy
- Alt: “Editorial screenprint of a mouse leaving an oversized metal box live trap
  beside the words Who moves after dark?”

Exact card text:

```text
NEON SMALL MAMMAL TRACKER
Who moves
after dark?
Meet the tiny lives reshaping the landscape.
NEON Explorer Suite · unofficial
```

## Historical Cover V4 documentary assets

The following USGS public-domain files remain in the repository as historical
release evidence but are no longer referenced by either live cover:

- `docs/assets/pacific-pocket-mouse-sherman-trap-usgs.jpg`
- `docs/og-card-v4-source.html`
- `docs/og-image.jpg`

The unreferenced 619,324-byte runtime derivative
`www/assets/small-mammal-field-usgs.jpg` was removed in V5 so it does not ship in
every Connect bundle; its history remains recoverable from Cover V4 commits.

Their source was Cheryl Brehme / U.S. Geological Survey, “Pacific pocket mouse
in Sherman trap,” retrieved 2026-07-18 from
<https://www.usgs.gov/media/images/pacific-pocket-mouse-sherman-trap>. They must
not be relabeled as NEON observations.

## Retired V3 illustration assets

These rejected pseudo-photographic assets remain historical artifacts only and
must not return to a live surface:

- `docs/og-habitat-v2.png`
- `docs/hero-mobile-v1.png`
- `docs/hero-mobile-v1.jpg`
- `docs/og-card-v3-source.png`

## Release contract

`scripts/check_cover.mjs` verifies the exact approved hook, one-promise/one-CTA
poster budget, shared Suite Living Poster frame, responsive image set, visible
illustration disclosure, provenance hashes, social metadata, scope boundary,
and responsive/accessibility rules. `scripts/check_in_app_landing.mjs` verifies
the same copy and asset authority inside Connect while preserving the functional
site-picker controls. Any art, copy, crop, or asset change requires this record
and both contracts to change together.
