# NEON Small Mammal Tracker

A Shiny web app for exploring the [National Ecological Observatory Network's](https://data.neonscience.org/)
small-mammal box-trapping data — reconstructing each captured animal's history from its ear-tag and
turning 46 field sites of capture records into maps, charts, and individual profiles.

[![Shiny](https://img.shields.io/badge/built%20with-Shiny-1f9bcf?logo=rstudio)](https://shiny.posit.co/)
[![R](https://img.shields.io/badge/R-%E2%89%A5%204.0-276DC3?logo=r)](https://www.r-project.org/)
[![Live App](https://img.shields.io/badge/live-shinyapps.io-75AADB)](https://t-lama.shinyapps.io/RatTrapHistory/)
[![Data](https://img.shields.io/badge/data-NEON%20DP1.10072.001-1a7f37)](https://data.neonscience.org/data-products/DP1.10072.001)

**Live app:** <https://t-lama.shinyapps.io/RatTrapHistory/>

![Species composition for a NEON site](assets/JORN_captures.png)

---

## What it does

The app opens to a **national map of every NEON site** — tap a dot to dive in (dot size = animals
caught there, color = the dominant mammal family), or flip to **"by species"** to map where a single
animal turns up across the country. Each site loads instantly from a per-site data bundle that ships
with the app (no network round-trip). From there it reconstructs each animal's capture history from
its ear-tag ID, ranks the regulars, profiles individuals, estimates abundance, and maps where they
were caught. A sidebar with the classic state → site → date picker is still there for power users.

It is built for two audiences: anyone curious about NEON small-mammal sampling, and new field
technicians getting to know the species at their site.

## Highlights

**Select-your-site map.** A national Leaflet map of all 46 bundled sites — sized by total captures,
colored by the most-caught mammal family — with a "by site / by species" toggle, an accessible list
fallback, and a one-tap load. A 30-second guided tour points out the rest.

**Explore by species.** Pick any of 150+ species and the map redraws to just the sites where it's
caught, sized by local abundance — a live national range map.

**Detection-corrected abundance.** Closed-capture estimates per trapping bout (Schnabel for ≥3
nights, Chapman for 2) with a per-night detection probability, shown alongside MNKA, gated when
recaptures are too few, and clamped to the minimum known alive — defensible, with caveats shown.

**Diversity profile.** Hill numbers (q0 richness, q1 effective-common, q2 effective-dominant) plus
an evenness read, computed over distinct individuals.

**Shareable trading cards, compare, and report cards.** Export any individual's dossier as a
holographic PNG trading card, put two sites head-to-head, or print a one-page site report card to PDF.

**Overview — the story of a site.** Species ranked by abundance, an automatically written
plain-English summary, and quick-jump navigation to every view.

**Hall of Fame — rank every individual.** A leaderboard of the most-caught animals, re-sortable by
captures, weight, career length, roaming, or weight-for-its-species, with rarity tiers.

![Capture leaderboard](assets/JORN_captureleaderboard.png)

**Site map.** Species diversity by plot on a satellite basemap; the selected individual's plots are
highlighted.

![Site map](assets/JORN_capturemap.png)

**Measurements over time.** An individual's weight and hind-foot length tracked across captures,
against the species' typical range.

![Measurements through time](assets/JORN_measurecompare.png)

**Body-size map.** Where an animal sits in its species' weight-by-length cloud, with a fitted
size–mass line drawn only where the relationship is statistically real.

![Body-size map](assets/JORN_bodysizemap.png)

**Community body-size profile.** The weight distribution of every species at a site, lightest to
heaviest, with the selected animal marked.

![Body-size distribution by species](assets/JORN_weightdistribution.png)

**Population signals.** Minimum Number Known Alive (MNKA), catch-per-unit-effort, and a
species-accumulation curve with a Chao1 richness estimate.

![Population indices](assets/JORN_population.png)

It also includes a trap-grid home-range heatmap with an animated capture replay, a breeding-phenology
chart, and tap-any-statistic ranked breakdowns.

## How the numbers work

| Metric | Definition |
| --- | --- |
| Captures | Times an individual (ear-tag ID) was handled in the window. |
| Career span | Days between an individual's first and last capture (flagged when it exceeds plausible lifespan — a likely reused tag). |
| Roam radius / Max move | Mean displacement from, and maximum distance between, capture locations (traps are 10 m apart). A grid-bounded dispersion index, not a true home-range area. |
| Chonk Index | Adult weight percentile within species. NEON rarely records body length and hind-foot barely scales with mass in these taxa, so a Scaled Mass Index would mostly rank noise — the body-size map shows the real relationship where it exists. |
| MNKA / CPUE | Minimum Number Known Alive (Krebs 1966) and captures per 100 trap-nights — transparent abundance indices. |
| Recapture rate | Share of handling events flagged as recaptures. |

Methods reviewed against Peig & Green (2009), Krebs (1966), and Gotelli & Colwell (2001). NEON
ear-tag numbers can be reused across years (the obvious cases are flagged), and an empty trap means
"not detected," not "absent."

![About and methods](assets/about.png)

## Data

All records come from NEON data product
[DP1.10072.001 — Small Mammal Box Trapping](https://data.neonscience.org/data-products/DP1.10072.001),
pre-downloaded with [`neonUtilities::loadByProduct()`](https://www.neonscience.org/neonUtilities).

Each site's full record is pre-downloaded into `data/sites/<SITE>.rds` (trimmed and compressed), and
two national indexes (`data/site_index.rds`, `data/species_ranges.rds`) power the picker and range
maps — so the app runs **entirely from the bundle, instantly, with no `neonUtilities` dependency at
runtime.** `neonUtilities` is optional: it's loaded lazily only for the live-fetch toggle, which
appears only where the package is installed (set `SMT_LIVE=0` to force bundle-only). The bundle is
rebuilt automatically **late on the first Saturday night of each month** (~11 pm Arizona time, an
off-peak window so the brief redeploy doesn't interrupt active users) by a GitHub Action
(`scripts/refresh_data.R`); the approach is documented in
[docs/data-bundling-pattern.md](docs/data-bundling-pattern.md).

## Run it locally

```r
install.packages(c(
  "shiny", "bslib", "bsicons", "shinyjs", "shinycssloaders",
  "plotly", "dplyr", "tidyr", "stringr", "tibble",
  "RColorBrewer", "leaflet", "DT", "htmltools"
))
# neonUtilities is OPTIONAL — only needed for the live-fetch toggle:
# install.packages("neonUtilities")

shiny::runApp()
```

The app opens to the national site-picker map; tap any site, or click "explore the Jornada demo" to
start immediately from the bundled dataset.

## Project layout

```
global.R                  libraries, theme, data loaders, lazy NEON fetch
ui.R                      bslib dashboard (sidebar + hero + tabs)
server.R                  data flow and all outputs (incl. picker/range maps)
R/helpers.R               analytical engine (leaderboards, indices, closed-capture, Hill)
R/site_metadata.R         site code -> name / state / domain / bio
www/                      theme CSS and JS (counters, loader, confetti, tour, card export)
data/sites/               per-site .rds bundle ("the database")
data/site_index.rds       national picker-map index (per-site stats)
data/species_ranges.rds   per-species national ranges (the "by species" map)
scripts/refresh_data.R    rebuild the per-site data bundle
scripts/build_site_index.R  rebuild the picker + species-range indexes
scripts/make_og_image.R   draw the docs/ social card
scripts/write_manifest.R  (re)generate manifest.json for Connect Cloud (lean, bundle-only)
scripts/deploy.R          legacy shinyapps.io push (being retired — see DEPLOY.md)
docs/                     landing page + og card, design & data-bundling write-ups
DEPLOY.md                 deploy & migration runbook (Connect Cloud / shinylive / Pages)
```

## Deploy

The app is live on shinyapps.io today, but that platform retires at the end of 2026. The migration
plan — a hosted app (Posit Connect Cloud or shinylive) plus a GitHub-Pages landing page with a social
card and cold-start pre-warm — is in **[DEPLOY.md](DEPLOY.md)**. The Connect Cloud `manifest.json` is
generated by `scripts/write_manifest.R` and is intentionally lean (bundle-only, no `neonUtilities`).

## Built by Desert Data Labs

Custom data apps, dashboards, and analytics for science, sports, and beyond. Want one for your
project? **desertdatalabs@gmail.com** · [desertdatalabs.com](https://desertdatalabs.com)

Not affiliated with NEON, Battelle, or the NSF. An educational data-exploration tool.
