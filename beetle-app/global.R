# ===========================================================================
# NEON Ground Beetle Tracker — global.R
# Loaded once per session: libraries, theme, helpers, and the bundled data.
#
# Sibling app to the NEON Small Mammal Tracker, same Desert Data Labs house
# style and bundle-first data pattern, but for ground beetles (Carabidae),
# NEON data product DP1.10022.001 — "Ground beetles sampled from pitfall traps".
# ===========================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(bsicons)
  library(dplyr)
  library(tidyr)
  library(plotly)
  library(leaflet)
  library(DT)
  library(shinyjs)
  library(shinycssloaders)
  library(RColorBrewer)
  library(htmltools)
})

# ---- helpers + metadata ---------------------------------------------------
source("R/site_metadata.R", local = FALSE)
source("R/helpers.R", local = FALSE)

# ---- NEON data product ----------------------------------------------------
NEON_DPID <- "DP1.10022.001"   # Ground beetles sampled from pitfall traps

# ---- live-fetch toggle (optional; bundle-first like the mammal app) --------
# Live downloads are OPTIONAL. neonUtilities is referenced by a computed name so
# the rsconnect/renv scanner doesn't pin it; the deployed showcase is bundle-only.
.NEON_PKG <- paste0("neon", "Utilities")
LIVE_FETCH <- (Sys.getenv("GBT_LIVE", "1") != "0") &&
  requireNamespace(.NEON_PKG, quietly = TRUE)

# ---- bundled per-site data ------------------------------------------------
# scripts/refresh_data.R pre-downloads + aggregates each site's carabid record
# into data/sites/<SITE>.rds (a tidy long table). The app loads it instantly.
SITE_DIR <- "data/sites"

# Illustrative demo bundle (clearly badged in the UI as NOT real NEON data) so
# the app is fully demonstrable without a live pull. Real data/sites/*.rds win.
BEETLE_DEMO <- local({
  f <- "data-sample/beetle_demo.csv"
  if (!file.exists(f)) return(NULL)
  d <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(d) || !nrow(d)) return(NULL)
  tibble::as_tibble(d)
})

# Read a site's bundle (real first, then demo), tagged with its source.
load_site_bundle <- function(site) {
  if (is.null(site) || site == "") return(NULL)
  f <- file.path(SITE_DIR, paste0(site, ".rds"))
  if (file.exists(f)) {
    d <- clean_beetle(readRDS(f)); attr(d, "source") <- "neon"; return(d)
  }
  if (!is.null(BEETLE_DEMO) && "siteID" %in% names(BEETLE_DEMO)) {
    sub <- BEETLE_DEMO[BEETLE_DEMO$siteID == site, , drop = FALSE]
    if (nrow(sub)) { d <- clean_beetle(sub); attr(d, "source") <- "demo"; return(d) }
  }
  NULL
}

# Filter a cleaned table to a [start, end] date window.
filter_window <- function(d, start_date, end_date) {
  if (is.null(d) || !"date" %in% names(d)) return(d)
  lo <- as.Date(start_date); hi <- as.Date(end_date)
  d[!is.na(d$date) & d$date >= lo & d$date <= hi, , drop = FALSE]
}

# Which sites have any data (real bundle or demo) — drives the picker + map.
available_sites <- function() {
  bundled <- sub("\\.rds$", "", list.files(SITE_DIR, pattern = "\\.rds$"))
  demo <- if (!is.null(BEETLE_DEMO)) unique(BEETLE_DEMO$siteID) else character(0)
  sort(unique(c(bundled, demo)))
}

# National site index for the map: one row per available site with headline
# numbers (richness, individuals, dominant species, source). Built once at boot.
SITE_INDEX <- local({
  sites <- available_sites()
  if (!length(sites)) return(NULL)
  rows <- lapply(sites, function(s) {
    d <- load_site_bundle(s); if (is.null(d) || !nrow(d)) return(NULL)
    ct <- community_table(d)
    sp <- ct[ct$species_level %in% TRUE, , drop = FALSE]   # richness = species only
    meta <- neon_sites[neon_sites$site == s, , drop = FALSE]
    tibble::tibble(
      site = s,
      name = if (nrow(meta)) meta$name else s,
      lat  = if (nrow(meta)) meta$lat else NA_real_,
      lng  = if (nrow(meta)) meta$lng else NA_real_,
      state = if (nrow(meta)) meta$state else NA_character_,
      richness = nrow(sp),
      individuals = sum(ct$individuals),
      dominant = if (nrow(sp)) sp$scientificName[1] else ct$scientificName[1],
      source = attr(d, "source") %||% "neon")
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) NULL else dplyr::bind_rows(rows)
})

# ---- cross-site aggregates (national views) -------------------------------
# All available sites' records, bound once at boot, for the ordination and the
# species range-map picker (independent of whichever single site is loaded).
ALL_DATA <- local({
  sites <- available_sites()
  if (!length(sites)) return(NULL)
  ds <- lapply(sites, function(s) {
    d <- load_site_bundle(s)
    if (is.null(d) || !nrow(d)) return(NULL)
    d$siteID <- s; d
  })
  ds <- ds[!vapply(ds, is.null, logical(1))]
  if (!length(ds)) NULL else dplyr::bind_rows(ds)
})

# PCoA ordination of every site x plot x year community (precomputed).
ORDINATION <- if (!is.null(ALL_DATA)) bray_ordination(ALL_DATA) else NULL

# Indicator species (IndVal) — which beetle signs which site (precomputed).
INDICATORS <- if (!is.null(ALL_DATA)) indicator_species(ALL_DATA) else NULL

# species -> sites with abundance, + the picker's choices (widest-ranging first).
SPECIES_SITES <- if (!is.null(ALL_DATA)) species_site_table(ALL_DATA) else NULL
species_choices <- function() {
  if (is.null(SPECIES_SITES)) return(NULL)
  sp <- SPECIES_SITES %>% dplyr::group_by(.data$scientificName) %>%
    dplyr::summarise(sites = dplyr::n(), inds = sum(.data$individualCount), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$sites), dplyr::desc(.data$inds))
  stats::setNames(sp$scientificName, sprintf("%s — %d site%s", sp$scientificName,
                  sp$sites, ifelse(sp$sites == 1, "", "s")))
}

# ---- live NEON fetch (optional) -------------------------------------------
# Pulls DP1.10022.001, reconciles parataxonomist with authoritative expert IDs,
# and normalises to the app's long schema (one row per plot/bout/species).
fetch_neon_beetles <- function(site, start_date, end_date) {
  if (!requireNamespace(.NEON_PKG, quietly = TRUE))
    stop("Live NEON download needs the neonUtilities package, which isn't in this build.")
  loadByProduct <- get("loadByProduct", envir = asNamespace(.NEON_PKG))
  raw <- loadByProduct(dpID = NEON_DPID, site = site,
                       startdate = format(as.Date(start_date), "%Y-%m"),
                       enddate   = format(as.Date(end_date), "%Y-%m"),
                       package = "basic", check.size = "F")
  assemble_beetles(raw)   # see scripts/refresh_data.R for the shared assembler
}

# ---- theme (Desert Data Labs house style; greens tuned for beetles) -------
DDL <- list(
  navy = "#0C234B", navy2 = "#16386e", cardinal = "#AB0520", gold = "#FFD200",
  gold2 = "#c9a300", sky = "#2f7fb5", green = "#1a7f37", forest = "#13632b",
  ink = "#1c2733", muted = "#6b7a89", bg = "#eef3ee", paper = "#ffffff",
  line = "#dbe7dc"
)

app_theme <- bs_theme(
  version = 5, bg = "#ffffff", fg = DDL$ink,
  primary = DDL$forest, secondary = DDL$cardinal,
  success = DDL$green, info = DDL$sky, warning = DDL$gold, danger = DDL$cardinal,
  base_font = font_google("Rubik"), heading_font = font_google("Rubik"),
  "border-radius" = "10px"
)

# ---- small UI utilities (shared by ui.R and server.R) ---------------------
spin <- function(x) shinycssloaders::withSpinner(x, color = DDL$forest,
                                                 proxy.height = "300px")

info_pop <- function(title, ..., placement = "auto")
  bslib::popover(tags$span(class = "info-dot", bsicons::bs_icon("info-circle")),
                 ..., title = title, placement = placement)

fmt_range <- function(a, b) {
  if (is.null(a) || is.null(b) || is.na(a) || is.na(b)) return("")
  sprintf("%s → %s", format(as.Date(a), "%b %Y"), format(as.Date(b), "%b %Y"))
}

# state pickers reused from the mammal app's metadata
state_choices <- function() {
  sites <- available_sites()
  st <- neon_sites[neon_sites$site %in% sites, ]
  if (!nrow(st)) return(NULL)
  s <- sort(unique(st$state))
  stats::setNames(s, state_names[s] %||% s)
}
sites_in_state <- function(state) {
  sites <- available_sites()
  st <- neon_sites[neon_sites$site %in% sites & neon_sites$state == state, ]
  if (!nrow(st)) return(NULL)
  stats::setNames(st$site, sprintf("%s · %s", st$site, st$name))
}
site_label <- function(site) {
  m <- neon_sites[neon_sites$site == site, ]
  if (nrow(m)) sprintf("%s · %s", site, m$name) else site
}
site_bio <- function(site) {
  m <- neon_sites[neon_sites$site == site, ]
  if (nrow(m)) m$bio else NULL
}
