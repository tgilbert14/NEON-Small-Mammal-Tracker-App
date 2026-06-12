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

Pick a state, then a NEON site (each with a one-line habitat description), choose a date window, and
the app loads every published small-mammal capture for that window — instantly, from a per-site data
bundle that ships with the app (a live download is the fallback). It then reconstructs each animal's
capture history from its ear-tag ID, ranks the regulars, profiles individuals, and maps where they
were caught.

It is built for two audiences: anyone curious about NEON small-mammal sampling, and new field
technicians getting to know the species at their site.

## Highlights

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
fetched with [`neonUtilities::loadByProduct()`](https://www.neonscience.org/neonUtilities).

Each site's full record is pre-downloaded into `data/sites/<SITE>.rds` (trimmed and compressed) so
the app loads instantly without a network round-trip; a live download is the fallback. The bundle is
rebuilt monthly by a GitHub Action (`scripts/refresh_data.R`); the approach is documented in
[docs/data-bundling-pattern.md](docs/data-bundling-pattern.md).

## Run it locally

```r
install.packages(c(
  "shiny", "bslib", "bsicons", "shinyjs", "shinycssloaders",
  "neonUtilities", "plotly", "dplyr", "tidyr", "stringr", "tibble",
  "RColorBrewer", "leaflet", "DT", "htmltools"
))

shiny::runApp()
```

The app opens to a site picker; click "explore the Jornada demo" to start immediately from the
bundled dataset.

## Project layout

```
global.R                  libraries, theme, data loaders, NEON fetch
ui.R                      bslib dashboard (sidebar + hero + tabs)
server.R                  data flow and all outputs
R/helpers.R               analytical engine (leaderboards, indices, home range)
R/site_metadata.R         site code -> name / state / domain / bio
www/                      theme CSS and JS (counters, loader, confetti)
data/sites/               per-site .rds bundle ("the database")
scripts/refresh_data.R    rebuild the data bundle
scripts/deploy.R          deploy to shinyapps.io
docs/                     design and data-bundling write-ups
```

## Built by Desert Data Labs

Custom data apps, dashboards, and analytics for science, sports, and beyond. Want one for your
project? **desertdatalabs@gmail.com** · [desertdatalabs.com](https://desertdatalabs.com)

Not affiliated with NEON, Battelle, or the NSF. An educational data-exploration tool.
