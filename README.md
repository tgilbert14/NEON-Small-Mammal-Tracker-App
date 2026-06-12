# 🐀 NEON Small Mammal Tracker

> A Shiny app for chasing individual rodents (and shrews, pocket mice, kangaroo rats, and the
> occasional cottontail) across the entire
> [National Ecological Observatory Network](https://data.neonscience.org/) — and celebrating the
> legends that just *keep* showing up in traps.

[![Shiny](https://img.shields.io/badge/built%20with-Shiny-1f9bcf?logo=rstudio)](https://shiny.posit.co/)
[![R](https://img.shields.io/badge/R-%E2%89%A5%204.0-276DC3?logo=r)](https://www.r-project.org/)
[![Live App](https://img.shields.io/badge/live-shinyapps.io-75AADB)](https://t-lama.shinyapps.io/RatTrapHistory/)
[![Data](https://img.shields.io/badge/data-NEON%20DP1.10072.001-2e7d32)](https://data.neonscience.org/data-products/DP1.10072.001)

---

## What it does

**Pick a state, then a NEON site** (each with a one-line habitat bio), choose a date window, and the
app pulls every small-mammal capture NEON has published — then reconstructs each animal's **capture
career** from its ear-tag ID. In a hurry? One click loads a bundled **Jornada demo** instantly.
Built for two audiences: anyone curious about NEON small-mammal sampling, and **new field techs**
getting to know the critters at their site.

### ✨ The good stuff

- **🧭 Overview (start here)** — quick-jump buttons to the best parts, the species ranked
  most-common-first, a plain-English auto-written **"story so far,"** and **"meet the locals"** cards
  with a fun fact for each animal.
- **👆 Tap-any-stat** — every headline number (captures, individuals, species, recapture rate,
  trap-nights, legends) opens a **ranked breakdown** (e.g. Species → every species by abundance;
  Individuals/Legends → tap an animal to open its dossier).
- **🏆 Hall of Fame** — every individual ranked, with medals, **rarity tiers** (Common → Legendary),
  and re-sortable leaderboards: *Most caught · Heaviest · Longest career · Biggest roamer · Chonkiest.*
- **🐀 Individual Dossier** — a trading-card profile for any animal: species art, career stats,
  first/last seen, home plot, and a confetti pop when you open a **Legendary** (10+ captures).
- **🧊 The Chonk Index** — an honest adult **weight percentile within species** ("heavy for its
  kind"), as a playful gauge with a vs-typical delta.
- **🎯 Body-size map** — the dossier's morphospace scatter: weight × hind-foot for the whole
  species, the selected animal in gold, and a size–mass fit line drawn *only* where the
  relationship is real (so you can see condition instead of trusting a black box).
- **📏 Measurements through time** — weight and hind-foot on a dual axis, with the species weight
  band shaded and the record capture called out.
- **🔥 Trap-grid home range** — a 10×10 heatmap with capture points overlaid, a hotspot-blur
  toggle, and a centroid marker.
- **▶️ Capture replay** — hit play and watch the individual's captures animate across the trap grid,
  drawing its path.
- **🗺️ Site map** — species diversity by plot on a satellite Leaflet basemap with plot labels; the
  selected individual's plots glow gold.
- **📊 Community Pulse** — species composition, sex ratio, age structure, captures-per-plot through
  time, and a **breeding-phenology** strip (when males go scrotal, when females cycle).
- **📈 Population** — defensible abundance signals: **MNKA** (Minimum Number Known Alive) and
  catch-per-unit-effort by plot, plus a **species-accumulation curve** with a Chao1 richness
  estimate — so you can tell whether trapping ran long enough.
- **🚩 Honesty flags** — careers that exceed plausible lifespan (likely reused ear tags) and
  multi-species tag IDs are badged, not silently trusted.
- **🎲 Try the demo** — opens instantly on real Jornada (JORN) data, 2017–2021, **no download** —
  so you can explore (and find "club foot" R2861) without waiting on the NEON API.

## Try it

🚀 **Live:** <https://t-lama.shinyapps.io/RatTrapHistory/>
📦 **Source:** <https://github.com/tgilbert14/NEON-Small-Mammal-Tracker-App>

## Run it locally

```r
install.packages(c(
  "shiny", "bslib", "bsicons", "shinyjs", "shinycssloaders",
  "neonUtilities", "plotly", "dplyr", "tidyr", "stringr", "tibble",
  "RColorBrewer", "reshape2", "leaflet", "DT", "htmltools"
))

# then from the project root
shiny::runApp()
```

Click **explore the Jornada demo** to start immediately — it loads the bundled
`data-sample/jorn_2017_2021.rds` and never touches the network.

## How to use it

1. **Pick a state, then a site** — the picker is grouped by state, and each site shows a one-line
   habitat bio.
2. **Pick a date window** — defaults to roughly the last ~6 years minus the most recent year
   (NEON data has a publication lag).
3. **Hit Load this site** (or **explore the Jornada demo**) — the **Overview** opens with the
   species composition, the auto-written story, and quick-jump buttons to the best parts.
4. **Open the Hall of Fame and tap any individual** (or **Surprise me**) — its dossier,
   measurements, Chonk Index, home range, and replay all unlock.

## How the numbers work

| Metric | Definition |
| --- | --- |
| **Captures** | Times an individual (ear-tag ID) was handled in the window. |
| **Career span** | Days between an individual's first and last capture (flagged if it exceeds plausible lifespan). |
| **Roam radius / Max move** | Mean displacement from, and max distance between, capture locations (traps are 10 m apart). A grid-bounded dispersion index, *not* a true home-range area. |
| **Chonk Index** | Adult **weight percentile within species**. NEON rarely records body length and hind-foot barely scales with mass in these taxa, so a Scaled Mass Index would just rank noise — the body-size map shows the real relationship where it exists. |
| **MNKA / CPUE** | Minimum Number Known Alive (Krebs 1966) and captures per 100 trap-nights — honest abundance *indices*. |
| **Rarity** | A playful tier from total captures; tracks trappability & residency, not ecological rarity. |
| **Recapture rate** | Share of handling events flagged as recaptures. |

> ⚠️ NEON ear-tag numbers can be reused across years (we flag the obvious cases). A trap that
> caught nothing means "not detected," not "absent." This is a data-exploration toy — but the
> metrics are built to survive a wildlife-methods review (Peig & Green 2009; Krebs 1966;
> Gotelli & Colwell 2001).

## Project layout

```
global.R                  libraries, theme, demo loader, NEON fetch wrapper
ui.R                      bslib dashboard (sidebar + hero stats + tabs)
server.R                  data flow + all outputs
R/helpers.R               the analytical engine (leaderboards, Chonk Index, home range)
R/site_metadata.R         NEON site code → name / domain / coordinates
www/styles.css            the light Desert Data Labs / Girth-Index theme (navy · cardinal · gold)
www/app.js                count-up counters + confetti
www/confirm.js            the "How it works" help dialog
data-sample/              bundled JORN demo dataset
docs/girth-index-patterns.md   reusable Desert Data Labs design/viz/interaction playbook
```

## Data source

All capture records come from NEON data product
[**DP1.10072.001 — Small mammal box trapping**](https://data.neonscience.org/data-products/DP1.10072.001),
fetched live via [`neonUtilities::loadByProduct()`](https://www.neonscience.org/neonUtilities).

## Built by Desert Data Labs

Custom data apps, dashboards, scrapers, and analytics — for science, sports, and beyond.
Want one for your project? **desertdatalabs@gmail.com** · [desertdatalabs.com](https://desertdatalabs.com)
· sister app: the [Big 12 Girth Index](https://girthindex.desertdatalab.com).

## Feedback

Bugs, feature ideas, or a cool find? Email **tsgilbert@arizona.edu** or open an
[issue](https://github.com/tgilbert14/NEON-Small-Mammal-Tracker-App/issues).
