# 🪲 NEON Ground Beetle Tracker

An interactive R/Shiny app for exploring **ground beetle (Carabidae) biodiversity**
across the National Ecological Observatory Network, from NEON data product
[**DP1.10022.001 — Ground beetles sampled from pitfall traps**](https://data.neonscience.org/data-products/DP1.10022.001).

Ground beetles are a textbook **bioindicator** — they respond fast to habitat,
disturbance, and climate — so their richness, diversity, and seasonal activity
tell a rich story about each NEON site. This is the carabid sibling of the
[NEON Small Mammal Tracker](https://github.com/tgilbert14/NEON-Small-Mammal-Tracker-App),
sharing its Desert Data Labs house style and **bundle-first** data pattern.

> ⚠️ **Scaffold / MVP.** This is an early build. The bundled data for HARV, KONZ,
> and JORN is an **illustrative demo** (`data-sample/beetle_demo.csv`) — *not*
> real NEON records — so the app is usable before the real bundle is built. The
> UI badges demo data clearly. Run `scripts/refresh_data.R` to download and
> bundle the real product.

## What it does

| Tab | What you see |
| --- | --- |
| **Overview** | Carabid community composition — species ranked by abundance and catch-per-100-trap-nights, plus "meet the beetles" natural-history cards. |
| **Diversity** | Hill numbers (q0/q1/q2 effective species), Hurlbert rarefaction (richness at equal sample size), and species accumulation across bouts. |
| **Seasonality** | Activity-density by month (catch per 100 trap-nights), overall or split by the top species. |
| **Biogeography** | A national map of NEON sites sized by carabid richness; tap a site to load it; a sortable comparison table. |
| **About** | Data product, methods, and the demo-data caveat. |

## How the numbers work

- **Effort-normalised abundance.** Counts are expressed as **catch per 100 trap-nights**
  (effort = the sum of unique plot × bout trap-night totals), so sites and windows
  with different sampling effort compare fairly. This also absorbs NEON's protocol
  changes (4→3 traps/plot in 2018; 10→6 plots/site in 2023).
- **Diversity** — Hill numbers (Hill 1973; Jost 2006); individual-based rarefaction
  with an analytic SD (Hurlbert 1971; Heck et al. 1975); sample-based species
  accumulation averaged over random bout orders (Gotelli & Colwell 2001).
- **Taxonomy** — the real bundle reconciles parataxonomist IDs with the
  authoritative **expert taxonomist** IDs (`assemble_beetles()` in `R/helpers.R`).

## Run it locally

```r
install.packages(c(
  "shiny", "bslib", "bsicons", "shinyjs", "shinycssloaders",
  "plotly", "dplyr", "tidyr", "RColorBrewer", "leaflet", "DT", "htmltools"
))
# neonUtilities is OPTIONAL — only for the live-fetch path / refresh script:
# install.packages("neonUtilities")

shiny::runApp()
```

The app opens to a splash; pick a state + site and **Load this site**, or open the
Biogeography map and tap a marker. The demo sites (HARV, KONZ, JORN) load instantly.

## Build the real data bundle

```r
Rscript scripts/refresh_data.R
```

This downloads DP1.10022.001 per site, assembles the carabid long table (expert-ID
reconciliation + per-trap-night effort), and writes `data/sites/<SITE>.rds`. The app
prefers a real bundle over the demo automatically, and the source badge turns green.

## Project layout

```
global.R                  libraries, theme, data loaders, national site index, lazy NEON fetch
ui.R                      bslib dashboard (sidebar + hero + tabs)
server.R                  data flow and all outputs
R/helpers.R               analytical engine (Hill numbers, rarefaction, accumulation, assemble_beetles)
R/site_metadata.R         site code -> name / state / domain / coords / bio
scripts/refresh_data.R    build the per-site beetle bundle from NEON
data-sample/beetle_demo.csv  illustrative demo bundle (NOT real NEON data)
www/styles.css            theme CSS
```

## Built by Desert Data Labs

Tucson, AZ · custom data apps & analytics → **desertdatalabs@gmail.com**
Data: NEON DP1.10022.001. Not affiliated with NEON, Battelle, or the NSF. An
educational data-exploration tool.
