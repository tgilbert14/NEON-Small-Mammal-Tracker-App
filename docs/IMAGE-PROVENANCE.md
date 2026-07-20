# Small Mammal cover image provenance

Status: **CURRENT / COVER V4**

Cover V4 replaces the generated V3 habitat scene with a documentary field
photograph. The live hero, evidence close-up, and social card now show a real
small mammal with the humane field equipment described by the app.

## Documentary hero photograph

- File: `docs/assets/pacific-pocket-mouse-sherman-trap-usgs.jpg`
- Title: “Pacific pocket mouse in Sherman trap”
- Subject: Pacific pocket mouse emerging from a Sherman live trap
- Photographer: Cheryl Brehme, U.S. Geological Survey Western Ecological
  Research Center
- Rights: U.S. Geological Survey public domain
- Source page:
  <https://www.usgs.gov/media/images/pacific-pocket-mouse-sherman-trap>
- Original file:
  <https://d9-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/s3fs-public/thumbnails/image/DSCN0070.JPG>
- Retrieved: 2026-07-18
- Dimensions: 4000 x 3000 JPEG
- SHA-256:
  `b678af26331a98747eba64c382f1ee7e2e1f7c1f77baecbd81dd46622a9e950a`
- Use: desktop and mobile cover hero, plus the evidence close-up
- Transformations: none to the stored source. Responsive crops, gradients, and
  text treatment are applied non-destructively with CSS.
- Alt text: “A Pacific pocket mouse emerges from the open door of a metal
  Sherman live trap on leaf-strewn ground.”

The photograph documents equipment and a species encounter. It is not a NEON
site photograph, is not part of product `DP1.10072.001`, and must not be used to
imply a NEON capture outcome, population density, or ecosystem condition.

## Connect Living Poster derivative

- Runtime file: `www/assets/small-mammal-field-usgs.jpg`
- Source: the unchanged documentary hero photograph above
- Use: first-screen documentary image in the Connect site-picker landing
- Dimensions: 1800 x 1350 JPEG
- SHA-256:
  `e9d0158ac56f95437f0958c5aa4037701c95f2b616e87bb54440ab08e3a50f55`
- Transformation: resized to a maximum 1800-pixel edge and JPEG quality 82 with
  macOS `sips`; no generative edit, compositing, content removal, or synthetic
  detail was introduced
- Visible credit: “Documentary field photograph · not a NEON observation” and
  “Cheryl Brehme · USGS · public domain”
- Alt text: the same documentary description used for the public cover

This derivative keeps the interactive app visually continuous with the public
Living Poster while avoiding a 2.5 MB first-screen transfer. Because it lives in
`www/`, it is a runtime asset and must be included in a validator-generated
`manifest.json` before any Connect deployment.

## Social card

- Source layout: `docs/og-card-v4-source.html`
- Delivery file: `docs/og-image.jpg`
- Use: Open Graph and Twitter large-image card
- Delivery dimensions: 1200 x 630 JPEG
- Delivery SHA-256:
  `b36c62094270bf1598f38c6162ea6fda1b5c607f513a3faeea4ed62819fdf19a`
- Rendered: 2026-07-18 from the project-owned HTML/CSS source at an exact
  1200 x 630 browser viewport
- Documentary source: the USGS public-domain photograph listed above
- Alt text: “A Pacific pocket mouse emerges from a Sherman live trap beside the
  title One trap night. A whole population story.”

Exact card text:

```text
NEON EXPLORER SUITE
One trap night.
A whole population story.
NEON Small Mammal Tracker
DESERT DATA LABS
46 RESEARCH SITES
Pacific pocket mouse · Cheryl Brehme, USGS · Public domain
```

## Retired V3 illustration assets

The following generated illustrations remain in version history and in the
repository only as retired release artifacts. Cover V4 does not reference them:

- `docs/og-habitat-v2.png`
- `docs/hero-mobile-v1.png`
- `docs/hero-mobile-v1.jpg`
- `docs/og-card-v3-source.png`

Their full prompts, hashes, and limitations are preserved in the Cover V3
version of this file. They must not be reintroduced as documentary evidence.

## Release contract

`scripts/check_cover.mjs` verifies the documentary source and social-card hashes,
the exact 1200 x 630 delivery size, social alt metadata, suite destinations,
scientific claim boundaries, safe live-app controls, and the absence of any V3
hero reference or automatic app prewarm. `scripts/check_in_app_landing.mjs`
verifies the Connect poster copy, source disclosure, runtime derivative hash and
dimensions, functional site-picker anchor, and responsive/accessibility CSS
contracts. Asset changes require this provenance record and the matching contract
to change together.
