# 🐀 NEON Small Mammal Tracker

> A Shiny app for chasing rodents (and shrews, and the occasional chipmunk) across the entire
> [National Ecological Observatory Network](https://data.neonscience.org/).

[![Shiny](https://img.shields.io/badge/built%20with-Shiny-1f9bcf?logo=rstudio)](https://shiny.posit.co/)
[![R](https://img.shields.io/badge/R-%E2%89%A5%204.0-276DC3?logo=r)](https://www.r-project.org/)
[![Live App](https://img.shields.io/badge/live-shinyapps.io-75AADB)](https://t-lama.shinyapps.io/RatTrapHistory/)
[![Data](https://img.shields.io/badge/data-NEON%20DP1.10072.001-2e7d32)](https://data.neonscience.org/data-products/DP1.10072.001)

---

## What it does

Pick a NEON site, pick a date range, and the app pulls every small mammal capture record NEON
has published for that window. Captures are ranked by individual (tagID) so you can spot the
absolute units — the rodents that just *keep* showing up in traps.

Click into a tag and the app digs deeper:

- 📋 **Capture history** — every event for that individual, side by side
- 📏 **Meso-measurements** — weight and hind-foot length over time
- 🔥 **Trap-grid heat maps** — where on the plot this animal liked to hang out
- 🗺️ **Site map** — species diversity by plot, across the whole site
- 📈 **Captures per plot** — monthly time series by species
- 🔗 **NEON BioRepository links** — auto-generated sample + image lookups for the species

## Try it

🚀 **Live:** <https://t-lama.shinyapps.io/RatTrapHistory/>
📦 **Source:** <https://github.com/tgilbert14/NEON-Small-Mammal-Tracker-App>

## Run it locally

```r
# install once
install.packages(c(
  "shiny", "shinythemes", "shinydashboard", "shinyjs", "shinyBS",
  "shinycssloaders", "neonUtilities", "plotly", "dplyr", "tidyverse",
  "reshape", "RColorBrewer", "grid", "leaflet", "ggmap", "DT"
))

# then from the project root
shiny::runApp()
```

## How to use it

1. **Pick a site** — any of the ~47 NEON terrestrial sites in the dropdown.
2. **Pick a date range** — defaults to roughly the last ~5 years minus the most recent year
   (NEON data has a publication lag).
3. **Hit Load** — the Capture Ranks table populates, sorted by total captures per individual.
4. **Pick a tagID** — the rest of the tabs unlock with the detailed view for that animal.

## Data source

All capture records come from NEON data product
[**DP1.10072.001 — Small mammal box trapping**](https://data.neonscience.org/data-products/DP1.10072.001),
fetched live via [`neonUtilities::loadByProduct()`](https://www.neonscience.org/neonUtilities).

## Feedback

Bugs, feature ideas, or a cool find? Email **tsgilbert@arizona.edu** or open an
[issue](https://github.com/tgilbert14/NEON-Small-Mammal-Tracker-App/issues).
