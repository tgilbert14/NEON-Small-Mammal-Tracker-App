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

# ---- bundled per-site data ("the database") -------------------------------
# scripts/refresh_data.R pre-downloads each site's full record into
# data/sites/<SITE>.rds (trimmed + xz). When a site is bundled, the app loads
# it instantly from disk and only goes to NEON live for sites/windows not
# bundled. Refresh = re-run that script + redeploy.
SITE_DIR  <- "data/sites"
DEMO_PATH <- "data-sample/jorn_2017_2021.rds"   # fallback if the bundle isn't built
DEMO_META <- list(site = "JORN", label = "JORN · Jornada Experimental Range")

# Read a bundled site's full record, or NULL if not bundled.
load_site_bundle <- function(site) {
  f <- file.path(SITE_DIR, paste0(site, ".rds"))
  if (file.exists(f)) tibble::as_tibble(readRDS(f)) else NULL
}

# Demo = the JORN bundle if present, else the small committed sample.
load_demo <- function() {
  b <- load_site_bundle("JORN")
  if (!is.null(b)) return(b)
  if (file.exists(DEMO_PATH)) return(tibble::as_tibble(readRDS(DEMO_PATH)))
  NULL
}

# Filter a raw mam table to a [start, end] window. Uses ISO-string comparison
# (collectDate is "YYYY-MM-DD…", which sorts lexically) so we don't parse
# 100k+ dates on a full-site bundle — much faster than as.Date() on every row.
filter_window <- function(d, start_date, end_date) {
  if (is.null(d) || !"collectDate" %in% names(d)) return(d)
  ds <- substr(as.character(d$collectDate), 1, 10)
  lo <- format(as.Date(start_date), "%Y-%m-%d")
  hi <- format(as.Date(end_date), "%Y-%m-%d")
  d[!is.na(ds) & ds >= lo & ds <= hi, , drop = FALSE]
}

# ---- live NEON fetch ------------------------------------------------------
# Wrapped so the server can call it uniformly; dates are coerced to YYYY-MM,
# which is what loadByProduct expects.
fetch_neon_mam <- function(site, start_date, end_date, provisional = FALSE) {
  sd <- format(as.Date(start_date), "%Y-%m")
  ed <- format(as.Date(end_date), "%Y-%m")
  raw <- neonUtilities::loadByProduct(
    dpID = NEON_DPID, site = site, startdate = sd, enddate = ed,
    package = "basic", check.size = "F", include.provisional = isTRUE(provisional)
  )
  tibble::as_tibble(raw$mam_pertrapnight)
}

# ---- theme ----------------------------------------------------------------
# Light, clean, card-based — the Desert Data Labs "Girth Index" house style,
# adapted to a NEON desert-field palette: warm paper background, white cards
# with colored top borders + headers, evergreen / terracotta / golden-hour
# triad, Rubik font (the Girth Index font), three weights.
#
# Palette (Desert Data Labs / Girth Index house colors):
#   navy    #0C234B  primary  (card headers, key text, hero)
#   cardinal#AB0520  accent   (featured/alert, links, CTA)
#   gold    #FFD200  highlight(legendary, badges, accents)
#   sky     #2f7fb5  info
#   ink     #1c2733  / muted #6b7a89 / bg #eef2f8
DDL <- list(
  navy = "#0C234B", navy2 = "#16386e", cardinal = "#AB0520", gold = "#FFD200",
  gold2 = "#c9a300", sky = "#2f7fb5", green = "#1a7f37", ink = "#1c2733",
  muted = "#6b7a89", bg = "#eef2f8", paper = "#ffffff", line = "#dbe2ec"
)

app_theme <- bs_theme(
  version = 5,
  bg = "#ffffff", fg = DDL$ink,
  primary = DDL$navy, secondary = DDL$cardinal,
  success = DDL$green, info = DDL$sky, warning = DDL$gold, danger = DDL$cardinal,
  base_font    = font_google("Rubik"),
  heading_font = font_google("Rubik"),
  "border-radius" = "10px"
)

# ---- small UI utilities ---------------------------------------------------
# A clean tinted pill/badge (rarity & chonk tags) for the light theme.
glow_badge <- function(label, color = "#0C234B", glow = color) {
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
