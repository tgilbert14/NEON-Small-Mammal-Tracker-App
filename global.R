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
  library(htmltools)
})
# neonUtilities is loaded LAZILY (only for the optional live-fetch path) — see LIVE_FETCH
# below. Keeping it out of the startup block lets the app run bundle-only with no heavy
# dependency (local dev, a lean Connect Cloud build, or a shinylive static export).

# ---- helpers + metadata ---------------------------------------------------
source("R/site_metadata.R", local = FALSE)
source("R/helpers.R", local = FALSE)

# ---- NEON data product ----------------------------------------------------
NEON_DPID <- "DP1.10072.001"   # Small mammal box trapping

# ---- live-fetch toggle ----------------------------------------------------
# Live NEON downloads are OPTIONAL. They're enabled only when neonUtilities is
# installed AND not explicitly disabled (set SMT_LIVE=0 to force bundle-only).
# When off, the app serves entirely from the bundled per-site .rds files.
#
# The package is referenced by a *computed* name (.NEON_PKG), never the literal
# `neonUtilities::` / `requireNamespace("neonUtilities")`, so the rsconnect/renv
# dependency scanner does NOT pin it into manifest.json. The deployed showcase is
# bundle-only and must stay lean — neonUtilities has no wasm build (it would also
# block a future shinylive export) and a live NEON pull on a cold free-tier worker
# is a hang risk. The live-fetch path still works anywhere the package is actually
# installed (local dev), via the dynamic lookup in fetch_neon_mam().
.NEON_PKG <- paste0("neon", "Utilities")
LIVE_FETCH <- (Sys.getenv("SMT_LIVE", "1") != "0") &&
  requireNamespace(.NEON_PKG, quietly = TRUE)

# ---- bundled per-site data ("the database") -------------------------------
# scripts/refresh_data.R pre-downloads each site's full record into
# data/sites/<SITE>.rds (trimmed + xz). When a site is bundled, the app loads
# it instantly from disk and only goes to NEON live for sites/windows not
# bundled. Refresh = re-run that script + redeploy.
SITE_DIR  <- "data/sites"
DEMO_PATH <- "data-sample/jorn_2017_2021.rds"   # fallback if the bundle isn't built
DEMO_META <- list(site = "JORN", label = "JORN · Jornada Experimental Range")

# ---- national site index (the picker map) ---------------------------------
# scripts/build_site_index.R precomputes one row per bundled site with the
# headline numbers the landing map needs (captures, richness, dominant species
# + its group color/emoji). Loaded once here so the map is instant on boot.
SITE_INDEX <- local({
  f <- "data/site_index.rds"
  if (file.exists(f)) tibble::as_tibble(readRDS(f)) else NULL
})

# Per-species national ranges (where each species is caught + per-site abundance)
# powering the "explore by species" range map on the landing.
SPECIES_RANGES <- local({
  f <- "data/species_ranges.rds"
  if (file.exists(f)) tibble::as_tibble(readRDS(f)) else NULL
})

# Species choices for the range picker: grouped by family, labeled with emoji +
# how widespread, sorted by total individuals (most abundant first).
species_choices <- function() {
  r <- SPECIES_RANGES
  if (is.null(r) || nrow(r) == 0) return(NULL)
  s <- r %>% dplyr::group_by(.data$scientificName, .data$group_label, .data$emoji) %>%
    dplyr::summarise(sites = dplyr::n(), inds = sum(.data$individuals), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$inds))
  # restrict to species at >= 2 sites so the "range" map is meaningful
  s <- s[s$sites >= 2, , drop = FALSE]
  split_lab <- split(
    stats::setNames(s$scientificName,
      sprintf("%s %s — %d sites", s$emoji, s$scientificName, s$sites)),
    s$group_label)
  lapply(split_lab, as.list)
}

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
  if (!requireNamespace(.NEON_PKG, quietly = TRUE))
    stop("Live NEON download needs the neonUtilities package, which isn't available in this build.")
  sd <- format(as.Date(start_date), "%Y-%m")
  ed <- format(as.Date(end_date), "%Y-%m")
  # dynamic lookup (not neonUtilities::loadByProduct) keeps it off the manifest
  loadByProduct <- get("loadByProduct", envir = asNamespace(.NEON_PKG))
  raw <- loadByProduct(
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

# ---- static asset cache-busting -------------------------------------------
# Append a version query (the file's mtime) to www/ assets so browsers always
# fetch the current styles.css / app.js after a deploy instead of a stale cache.
asset_url <- function(path) {
  f <- file.path("www", path)
  v <- if (file.exists(f)) as.integer(as.numeric(file.mtime(f))) else 0L
  sprintf("%s?v=%s", path, v)
}

# ---- small UI utilities ---------------------------------------------------
# Loading spinner used by BOTH ui.R and server.R (the picker map), so it lives
# here in global scope rather than in ui.R.
spin <- function(x, img = "rat-72.gif")
  shinycssloaders::withSpinner(x, image = img, image.height = "120px",
                               proxy.height = "300px")

# a small "ⓘ" that opens an explanatory popover — used in ui.R AND server.R
# (e.g. the compare-sites modal), so it lives in global scope.
info_pop <- function(title, ..., placement = "auto")
  bslib::popover(tags$span(class = "info-dot", bsicons::bs_icon("info-circle")),
                 ..., title = title, placement = placement)

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
