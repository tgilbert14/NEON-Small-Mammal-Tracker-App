# ===========================================================================
# NEON Small Mammal Tracker — global.R
# Loaded once per session: libraries, theme, helpers, and the bundled demo set.
# ===========================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(bsicons)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(tibble)
  library(plotly)
  library(leaflet)
  library(DT)
  library(shinyjs)
  library(shinycssloaders)
  library(RColorBrewer)
  library(neonUtilities)
  library(htmltools)
})

# ---- helpers + metadata ---------------------------------------------------
source("R/site_metadata.R", local = FALSE)
source("R/helpers.R", local = FALSE)

# ---- NEON data product ----------------------------------------------------
NEON_DPID <- "DP1.10072.001"   # Small mammal box trapping

# ---- bundled demo dataset (JORN 2017–2021) --------------------------------
# Lets the app open instantly with zero network — and showcases real legends
# like "club foot" (R2861) and the 20-capture champ R2626.
DEMO_PATH <- "data-sample/jorn_2017_2021.rds"
DEMO_META <- list(site = "JORN", start = "2017-01", end = "2021-12",
                  label = "JORN · Jornada Experimental Range · 2017–2021")

load_demo <- function() {
  if (!file.exists(DEMO_PATH)) return(NULL)
  tibble::as_tibble(readRDS(DEMO_PATH))
}

# ---- live NEON fetch ------------------------------------------------------
# Wrapped so the server can call it uniformly; dates are coerced to YYYY-MM,
# which is what loadByProduct expects.
fetch_neon_mam <- function(site, start_date, end_date) {
  sd <- format(as.Date(start_date), "%Y-%m")
  ed <- format(as.Date(end_date), "%Y-%m")
  raw <- neonUtilities::loadByProduct(
    dpID = NEON_DPID, site = site, startdate = sd, enddate = ed,
    package = "basic", check.size = "F"
  )
  tibble::as_tibble(raw$mam_pertrapnight)
}

# ---- theme ----------------------------------------------------------------
# Light, clean, card-based — the Desert Data Labs "Girth Index" house style,
# adapted to a NEON desert-field palette: warm paper background, white cards
# with colored top borders + headers, evergreen / terracotta / golden-hour
# triad, Rubik font (the Girth Index font), three weights.
#
# Palette (NEON desert):
#   pine    #1b6051  primary  (standard card headers, key text)
#   terra   #c1502e  accent   (featured/alert headers, links)
#   gold    #e0a32e  highlight(legendary, CTA)
#   sky     #2f7fb5  info
#   ink     #1f2a30  / muted #6b7a85 / bg #f4f6f2
DDL <- list(
  pine = "#1b6051", pine2 = "#0f4a3d", terra = "#c1502e", gold = "#e0a32e",
  sky = "#2f7fb5", green = "#3a9d5d", ink = "#1f2a30", muted = "#6b7a85",
  bg = "#f4f6f2", paper = "#ffffff", line = "#e2e7df"
)

app_theme <- bs_theme(
  version = 5,
  bg = "#ffffff", fg = DDL$ink,
  primary = DDL$pine, secondary = DDL$terra,
  success = DDL$green, info = DDL$sky, warning = DDL$gold, danger = DDL$terra,
  base_font    = font_google("Rubik"),
  heading_font = font_google("Rubik"),
  "border-radius" = "10px"
)

# ---- small UI utilities ---------------------------------------------------
# A clean tinted pill/badge (rarity & chonk tags) for the light theme.
glow_badge <- function(label, color = "#1b6051", glow = color) {
  span(
    class = "glow-badge",
    style = sprintf("color:#fff; background:%s; border-color:%s;", color, color),
    label
  )
}

# Format a date range nicely for headers.
fmt_range <- function(a, b) {
  if (is.null(a) || is.null(b) || is.na(a) || is.na(b)) return("")
  sprintf("%s → %s", format(a, "%b %Y"), format(b, "%b %Y"))
}
