# Small Mammal cover image provenance

Status: **CURRENT / COVER V3**

These assets are project-created AI illustrations, not documentary field
photographs and not evidence of a named species, site, or trapping outcome. They
must not be used to imply animal injury, capture success, population density, or
ecosystem condition.

## Desktop habitat source

- File: `docs/og-habitat-v2.png`
- Use: desktop landing-page hero source
- Dimensions: 1774 x 887 PNG
- SHA-256: `28f78aaf2b02c1907a1ea2762c8b5a7068d50c718aa09d2ffa422a2c9c5294dc`
- Added: 2026-07-18 in commit `c92158716953f9f1a8d5d329d0d6f95fd8e966bc`
- Historical limitation: the original generation prompt was not retained. This is
  preserved as a provenance gap rather than reconstructed and mislabelled as the
  original prompt.
- Alt text: “A desert mouse standing beside a humane box trap and survey flag in
  moonlit desert habitat.”

## Mobile habitat companion

- Source file: `docs/hero-mobile-v1.png`
- Delivery file: `docs/hero-mobile-v1.jpg`
- Use: landing-page hero at viewports up to 700 CSS pixels
- Source dimensions / SHA-256: 852 x 1846 PNG /
  `7d62b3b1066c512f40452bedb374732a0e4a3e7924e82e90ac803efe53ccaf04`
- Delivery dimensions / SHA-256: 852 x 1846 JPEG /
  `14e447c12b60f1b1adf0e2cfe3985ad25e310ae0fdeddc3428f08b86a083b410`
- Generated: 2026-07-18 with the built-in OpenAI image generation workflow,
  using `docs/og-habitat-v2.png` as the edit target
- Alt text: same as the desktop source; the `<picture>` element exposes one image
  alternative regardless of selected crop.

Generation prompt:

> Recompose and extend the provided desert-night small-mammal scene into a
> polished portrait mobile hero image. Preserve the same realistic desert mouse,
> humane metal box trap, pink survey flag, moonlit Sonoran-desert vegetation,
> distant mountains, and deep indigo/cyan/warm-sand visual identity. Keep the
> mouse and trap fully visible in the lower half and leave calm, dark, low-detail
> negative space across the upper half for HTML text. No text, logos, watermark,
> extra animals, people, bait, injury, trapped animal, charts, or health symbolism.

## Social card

- Source file: `docs/og-card-v3-source.png`
- Delivery file: `docs/og-image.png`
- Use: Open Graph and Twitter large-image card
- Source dimensions / SHA-256: 1730 x 909 PNG /
  `708ce2d0ab1d3fcf0f927152c7a9574b06a802c8d2eea7fa817b66cbed882845`
- Delivery dimensions / SHA-256: 1200 x 630 PNG /
  `d089443e74cedb2d8b9f1f02dde6bd4c5358a7d05c94ec66a9124ca79bdad0bd`
- Generated: 2026-07-18 with the built-in OpenAI image generation workflow,
  using the prior habitat-only `docs/og-image.png` as the edit target; resized
  deterministically with macOS `sips`
- Alt text: “A moonlit desert mouse beside a humane box trap under the title NEON
  Small Mammal Tracker.”

Exact card text:

```text
TERRESTRIAL CONSUMER EXPLORER
NEON Small Mammal Tracker
DESERT DATA LABS · EXPLORER SUITE
```

Generation prompt:

> Preserve the provided moonlit desert mouse, humane box trap, survey flag,
> vegetation, mountains, lighting, crop, and right-side photographic details.
> Add the exact three text lines above in the dark left-side negative space with
> modern off-white, warm-sand, and pale-blue sans-serif typography, generous
> social-safe margins, and one restrained cyan-to-sand three-node constellation
> mark. Add no other words, numbers, logos, labels, badges, charts, watermarks, or
> data claims.

## Release contract

`scripts/check_cover.mjs` verifies that the delivery assets exist, the social card
is exactly 1200 x 630, the mobile image is the declared size, social alt metadata
is present, all suite destinations are registered, and the landing page makes no
automatic prewarm request. Asset changes require this provenance record and the
cover contract to change together.
