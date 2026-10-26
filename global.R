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

# ---- deploy armor: never leak a raw R error to a visitor ------------------
# On a cold Connect Cloud worker a render can throw (a malformed bundle, an
# edge-case transform) BEFORE it reaches its note_plot() fallback. Sanitizing
# guarantees the user sees a generic message, not a stack trace with internals.
# Pairs with the note_plot() message-chart pattern (server.R) for the no-data
# states that ARE reachable, and with wrapping risky render bodies in tryCatch.
options(shiny.sanitize.errors = TRUE)
# neonUtilities is loaded LAZILY (only for the optional live-fetch path) — see LIVE_FETCH
# below. Keeping it out of the startup block lets the app run bundle-only with no heavy
# dependency (local dev, a lean Connect Cloud build, or a shinylive static export).

# ---- helpers + metadata ---------------------------------------------------
# ---- basemap --------------------------------------------------------------
# CARTO watermarks unauthenticated basemaps.cartocdn.com raster tiles ("API KEY
# REQUIRED", since 2026-08-26; suite record: NEON-Driver-Cascade
# docs/SUITE-BASEMAP-INCIDENT-2026-08.md). The key rides in the tile URL, so it
# is a public rate-limited identifier, not a credential; Sys.getenv keeps it out
# of git and makes rotation a Connect Cloud setting. addProviderTiles() cannot
# carry it (the bundled CartoDB template has no {apikey} slot), hence addTiles().
# Accepts either a leaflet provider name or a CARTO variant, so ui.R basemap
# choices stay exactly as they are and any non-CARTO provider passes straight
# through. Without the key it falls back to Esri's keyless grey canvas — clean,
# but content-free past z16 at rural sites, so the cap keeps the zoom honest.
add_suite_basemap <- function(map, basemap = "light_all", noWrap = FALSE) {
  variant <- switch(basemap,
    "light_all" = ,
    "CartoDB.Positron" = "light_all",
    "dark_all" = ,
    "CartoDB.DarkMatter" = "dark_all",
    NULL)
  if (is.null(variant))
    return(leaflet::addProviderTiles(map, basemap,
      options = leaflet::providerTileOptions(noWrap = noWrap)))
  key <- Sys.getenv("CARTO_BASEMAP_KEY", "")
  if (nzchar(key)) {
    leaflet::addTiles(map,
      urlTemplate = sprintf(
        "https://{s}.basemaps.cartocdn.com/%s/{z}/{x}/{y}{r}.png?key=%s", variant, key),
      attribution = paste(
        '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
        '&copy; <a href="https://carto.com/attributions">CARTO</a>'),
      options = leaflet::tileOptions(subdomains = "abcd", maxZoom = 20, noWrap = noWrap))
  } else {
    leaflet::addTiles(map,
      urlTemplate = sprintf(
        "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_%s_Gray_Base/MapServer/tile/{z}/{y}/{x}",
        if (identical(variant, "dark_all")) "Dark" else "Light"),
      attribution = 'Tiles &copy; Esri &mdash; Esri, HERE, Garmin, &copy; OpenStreetMap contributors',
      options = leaflet::tileOptions(maxNativeZoom = 16, maxZoom = 19, noWrap = noWrap))
  }
}

source("R/site_metadata.R", local = FALSE)
source("R/helpers.R", local = FALSE)
source("R/seasonal_env.R", local = FALSE)   # the cascade's seasonal-aggregate driver read

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

# Defensive bundle read: a tibble, or NULL when the file is missing OR can't be
# deserialized (corrupt .rds, or a non-portable serialization the deploy R can't
# read). A bad bundle must NEVER crash app startup or hang a render — an empty
# layer with a visible notice beats an infinite loading spinner. (readRDS throws
# on a bad file, so the bare `readRDS()` calls this replaces could take down the
# whole session before the UI ever painted.)
read_bundle <- function(f) {
  if (!file.exists(f)) return(NULL)
  out <- tryCatch(
    tibble::as_tibble(readRDS(f)),
    error = function(e) {
      warning(sprintf("read_bundle('%s') failed: %s", f, conditionMessage(e)))
      NULL
    })
  if (is.null(out) || !nrow(out)) NULL else out
}

# ---- national site index (the picker map) ---------------------------------
# scripts/build_site_index.R precomputes one row per bundled site with the
# headline numbers the landing map needs (captures, richness, dominant species
# + its group color/emoji). Loaded once here so the map is instant on boot.
SITE_INDEX <- read_bundle("data/site_index.rds")

# Per-species national ranges (where each species is caught + per-site abundance)
# powering the "explore by species" range map on the landing.
SPECIES_RANGES <- read_bundle("data/species_ranges.rds")

# ---- network search index (the "Search the network" tab) ------------------
# scripts/build_search_index.R precomputes one row per (species x site) with the
# within-site MNKA index, captures/individuals, and the site-level year span.
# Loaded once here so the search tab filters it in memory — instant, no fetch.
SEARCH_INDEX <- read_bundle("data/search_index.rds")

# Selectize choices for the species search box: every species in the index,
# labeled with its emoji + how widespread it is, grouped by family, most
# widespread first. Empty placeholder leads.
search_taxon_choices <- function() {
  r <- SEARCH_INDEX
  if (is.null(r) || nrow(r) == 0) return(c("Start typing a species name…" = ""))
  s <- r %>% dplyr::group_by(.data$scientificName, .data$group_label, .data$emoji) %>%
    dplyr::summarise(sites = dplyr::n(), inds = sum(.data$individuals), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$sites), dplyr::desc(.data$inds))
  labs <- sprintf("%s %s · %d site%s", s$emoji, s$scientificName,
                  s$sites, ifelse(s$sites == 1, "", "s"))
  c(stats::setNames("", "Start typing a species name…"),
    stats::setNames(s$scientificName, labs))
}

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
      sprintf("%s %s · %d sites", s$emoji, s$scientificName, s$sites)),
    s$group_label)
  lapply(split_lab, as.list)
}

# Site-code allowlist. A code reaches file.path() only when it is exactly four
# upper-case letters and, once the site index is loaded, one of its sites. This
# refuses path-shaped input such as "../site_index" before any file is opened.
is_valid_site_code <- function(site, index = SITE_INDEX) {
  if (!is.character(site) || length(site) != 1L || is.na(site)) return(FALSE)
  if (!grepl("^[A-Z]{4}$", site)) return(FALSE)
  if (!is.null(index) && "site" %in% names(index)) return(site %in% index$site)
  TRUE
}

# Read a bundled site's full record, or NULL if not bundled, unreadable, or the
# code is not an allowlisted site.
load_site_bundle <- function(site) {
  if (!is_valid_site_code(site)) return(NULL)
  read_bundle(file.path(SITE_DIR, paste0(site, ".rds")))
}

# ---- co-located environmental overlays ("compare with environment") --------
# Other NEON data products are collected at these SAME sites, so they overlay
# cleanly on the small-mammal time-series. We pre-aggregate each to MONTHLY
# per-site values (see scripts/refresh_env_data.R) and bundle one tiny
# data/env/<SITE>.rds per site — a few KB each — mirroring the mammal bundle.
#
# ENV_LAYERS is the registry the UI + plots read: each entry maps a column in
# the monthly table to a NEON data product, a label/unit, an aggregation rule,
# and a house-palette color. Adding a layer = add a row here + a column in the
# refresh script. `lead` flags drivers we expect to LEAD abundance (so the lag
# slider is meaningful — e.g. a rain pulse precedes the rodent boom it feeds).
ENV_DIR <- "data/env"

# Built bundle = precip + air temperature + three plant-phenology STATUS yes-share
# signals (flowering, green-up, fruiting; see scripts/refresh_env_data.R). Flowering
# and green-up are the LEAD drivers for arid sites (SRER/JORN have no fruit); fruiting
# is the mast/forest lead. Relative humidity and soil moisture are intentionally NOT
# bundled (soil water is a very-high-volume product).
# `dig` = decimal places to show in hover/labels (precip mm & phenology %% are
# whole numbers; temperature reads to one decimal).
ENV_LAYERS <- list(
  precip  = list(col = "precip_mm",     label = "Precipitation",       unit = "mm/mo",
                 dpid = "DP1.00044.001", agg = "sum",   color = "#2f7fb5", lead = TRUE,  dig = 0),
  temp    = list(col = "temp_c",        label = "Air temperature",     unit = "°C",
                 dpid = "DP1.00002.001", agg = "mean",  color = "#d9480f", lead = FALSE, dig = 1),
  flower  = list(col = "flowering_pct", label = "Plants flowering",    unit = "% in flower",
                 dpid = "DP1.10055.001", agg = "share", color = "#d6336c", lead = TRUE,  dig = 0),
  greenup = list(col = "greenup_pct",   label = "Green-up (leaf-out)", unit = "% leafing out",
                 dpid = "DP1.10055.001", agg = "share", color = "#2f9e44", lead = TRUE,  dig = 0),
  fruit   = list(col = "fruiting_pct",  label = "Plants fruiting",     unit = "% in fruit",
                 dpid = "DP1.10055.001", agg = "share", color = "#9c6644", lead = TRUE,  dig = 0)
)

# Choices for the overlay picker: only layers that actually have data for the
# loaded site (so we never offer an empty overlay).
env_layer_choices <- function(env) {
  base <- c("None" = "none")
  if (is.null(env) || !nrow(env)) return(base)
  have <- vapply(names(ENV_LAYERS), function(k) {
    col <- ENV_LAYERS[[k]]$col
    col %in% names(env) && any(!is.na(env[[col]]))
  }, logical(1))
  if (!any(have)) return(base)
  labs <- vapply(ENV_LAYERS[have], function(m) sprintf("%s (%s)", m$label, m$unit), character(1))
  c(base, stats::setNames(names(ENV_LAYERS)[have], labs))
}

# Load a site's monthly environmental overlay table (data/env/<SITE>.rds), or
# NULL. The synthetic demo overlay is retired: a site without a real NEON
# overlay bundle has no overlay. The "source" attribute stays "neon" so callers
# that badge the source keep working.
load_site_env <- function(site) {
  if (!is_valid_site_code(site)) return(NULL)
  e <- read_bundle(file.path(ENV_DIR, paste0(site, ".rds")))
  if (is.null(e)) return(NULL)
  attr(e, "source") <- "neon"
  e
}

# Demo = the JORN bundle if present, else the small committed sample.
load_demo <- function() {
  b <- load_site_bundle("JORN")
  if (!is.null(b)) return(b)
  read_bundle(DEMO_PATH)
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

# ---- NEON provenance (written by the refresh bot) ---------------------------
# data/bundle_meta.json names the NEON release, DOI and data window behind the
# bundles; data/neon_issue_log.json carries NEON's change-log entries for the
# product. Both are optional at runtime: when a file is absent or unreadable the
# accessors return NULL / NA / an empty table and the UI shows nothing.
read_json_safely <- function(path, simplify = TRUE) {
  if (!file.exists(path)) return(NULL)
  tryCatch(jsonlite::fromJSON(path, simplifyVector = simplify),
           error = function(e) {
             warning(sprintf("could not read %s: %s", path, conditionMessage(e)))
             NULL
           })
}

BUNDLE_META <- local({
  m <- read_json_safely("data/bundle_meta.json", simplify = FALSE)
  if (is.list(m) && length(m)) m else NULL
})

NEON_ISSUE_LOG <- local({
  x <- read_json_safely("data/neon_issue_log.json", simplify = TRUE)
  if (is.data.frame(x) && nrow(x)) x else NULL
})

.meta_chr <- function(meta, key) {
  v <- meta[[key]]
  if (is.null(v) || length(v) != 1L || is.na(v)) return(NA_character_)
  v <- trimws(as.character(v))
  if (nzchar(v)) v else NA_character_
}

# The NEON citation line for the bundled release, or NULL without metadata.
neon_citation_text <- function(meta = BUNDLE_META) {
  if (is.null(meta)) return(NULL)
  release <- .meta_chr(meta, "release")
  doi <- .meta_chr(meta, "doi")
  if (is.na(release) || is.na(doi)) return(NULL)
  sprintf(paste0(
    "NEON (National Ecological Observatory Network). Small mammal box trapping ",
    "(%s), %s. %s. Dataset accessed from https://data.neonscience.org. ",
    "Licensed CC BY 4.0."),
    .meta_chr(meta, "product") %|na|% "DP1.10072.001", release, doi)
}
`%|na|%` <- function(a, b) if (length(a) != 1L || is.na(a)) b else a

# Last collection date covered by the bundles (Date), or NA.
neon_data_through <- function(meta = BUNDLE_META) {
  if (is.null(meta)) return(as.Date(NA))
  v <- .meta_chr(meta, "data_through")
  if (is.na(v)) return(as.Date(NA))
  out <- suppressWarnings(as.Date(substr(v, 1, 10)))
  if (length(out) != 1L) as.Date(NA) else out
}

.issue_rows <- function(log, keep) {
  empty <- data.frame(date = as.Date(character()), id = character(),
                      issue = character(), resolution = character(),
                      stringsAsFactors = FALSE)
  if (is.null(log) || !nrow(log) || !any(keep)) return(empty)
  col <- function(k) if (k %in% names(log)) as.character(log[[k]]) else rep(NA_character_, nrow(log))
  out <- data.frame(
    date = suppressWarnings(as.Date(substr(col("issueDate"), 1, 10))),
    id = col("id"), issue = col("issue"), resolution = col("resolution"),
    stringsAsFactors = FALSE)[keep, , drop = FALSE]
  out <- out[order(out$date, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

.issue_is_all <- function(loc) {
  !is.na(loc) & tolower(trimws(loc)) == "all"
}

# NEON change-log entries that name this site: its code anywhere in
# locationAffected (a comma list or free text), or its NEON domain code. Entries
# marked "All" belong to neon_product_notes(), not here.
neon_site_notes <- function(site, log = NEON_ISSUE_LOG, sites = neon_sites) {
  if (is.null(log) || !nrow(log) || !"locationAffected" %in% names(log) ||
      !is.character(site) || length(site) != 1L || !grepl("^[A-Z]{4}$", site))
    return(.issue_rows(NULL, logical(0)))
  loc <- as.character(log$locationAffected)
  dom <- if (!is.null(sites) && all(c("site", "domain") %in% names(sites)))
    sites$domain[match(site, sites$site)] else NA_character_
  hit_site <- grepl(sprintf("\\b%s\\b", site), loc, perl = TRUE)
  hit_dom <- if (!is.na(dom)) grepl(sprintf("\\b%s\\b", dom), loc, perl = TRUE) else FALSE
  keep <- !is.na(loc) & !.issue_is_all(loc) & (hit_site | hit_dom)
  .issue_rows(log, keep)
}

# NEON change-log entries that apply to every site ("All").
neon_product_notes <- function(log = NEON_ISSUE_LOG) {
  if (is.null(log) || !nrow(log) || !"locationAffected" %in% names(log))
    return(.issue_rows(NULL, logical(0)))
  .issue_rows(log, .issue_is_all(as.character(log$locationAffected)))
}

# ---- NEON explorer suite registry --------------------------------------------
# The ten Shiny explorers, in suite order. is_self marks this app.
SUITE_APPS <- data.frame(
  key = c("small-mammal", "plant-diversity", "breeding-birds", "phenology",
          "veg-structure", "ground-beetle", "water-chem", "mosquito", "inverts", "atlas"),
  name = c("Small Mammal Tracker", "Plant Diversity Explorer", "Breeding Bird Explorer",
           "Plant Phenology Explorer", "Vegetation Structure Explorer",
           "Ground Beetle Tracker", "Water Chemistry Analyte Viewer", "Mosquito Pulse",
           "My Little Inverts", "NEON Cross-Product Response Atlas"),
  app_url = c(
    "https://019ec337-7100-317e-5052-c3bf32ffcb79.share.connect.posit.cloud/",
    "https://019ee109-30ae-006e-cb3b-143afeac57e3.share.connect.posit.cloud/",
    "https://019ee116-75d9-5940-8ccd-9b8c7afabce4.share.connect.posit.cloud/",
    "https://019ee118-bf17-1622-bd5d-e59cab3b36a7.share.connect.posit.cloud/",
    "https://019ee110-8fd3-abae-aee3-02ea8e4274c8.share.connect.posit.cloud/",
    "https://019ec8ff-2a4b-e0e9-871d-07a047a571d3.share.connect.posit.cloud/",
    "https://019ebf59-b2ff-a20a-cb35-6d227ca6261a.share.connect.posit.cloud/",
    "https://019ef0b1-0099-c999-1edc-4d47826044cc.share.connect.posit.cloud/",
    "https://019ef4fb-c6c4-7ddf-2667-7e1021b2ef10.share.connect.posit.cloud/",
    "https://019ee1cf-a484-44eb-a181-cd495df24b3b.share.connect.posit.cloud/"),
  cover_url = c(
    "https://tgilbert14.github.io/NEON-Small-Mammal-Tracker-App/",
    "https://tgilbert14.github.io/NEON-Plant-Diversity/",
    "https://tgilbert14.github.io/NEON-Breeding-Birds/",
    "https://tgilbert14.github.io/NEON-Plant-Phenology-Explorer/",
    "https://tgilbert14.github.io/NEON-Vegetation-Structure-Explorer/",
    "https://tgilbert14.github.io/NEON-Ground-Beetle-Tracker/",
    "https://tgilbert14.github.io/NEON-WaterChemistry-Analyte-Viewer-App/",
    "https://tgilbert14.github.io/NEON-Mosquito-Pulse/",
    "https://tgilbert14.github.io/NEON-My-Little-Inverts/",
    "https://tgilbert14.github.io/NEON-Driver-Cascade/"),
  stringsAsFactors = FALSE)
SUITE_APPS$is_self <- SUITE_APPS$key == "small-mammal"

# ---- build digest: which revision is this worker serving? --------------------
# Published as <meta name="ddl-build"> so the post-deploy smoke can prove the
# live app runs the merged tree. Canonical text = one "<path>\t<md5>" line per
# file, sorted by path (C order), joined by "\n" with no trailing newline.
#   "m-" + md5(text from manifest.json's files map)  when manifest.json is readable
#   "f-" + md5(text from tools::md5sum over the runtime files)  otherwise
# scripts/post_deploy_smoke.sh computes both from the pushed tree and accepts
# either, so the check holds whether or not Connect ships manifest.json.
RUNTIME_FILE_GLOBS <- function(root = ".") {
  rel <- function(dir, ...) {
    f <- list.files(file.path(root, dir), full.names = FALSE, ...)
    if (length(f)) file.path(dir, f) else character()
  }
  out <- c("global.R", "ui.R", "server.R",
           rel("R", pattern = "\\.R$"),
           rel("www", recursive = TRUE, all.files = FALSE),
           rel("data", pattern = "\\.rds$", recursive = TRUE),
           rel("data", pattern = "\\.json$"),
           rel("data-sample", recursive = TRUE))
  out <- unique(out[file.exists(file.path(root, out))])
  sort(out, method = "radix")
}

.md5_text <- function(txt) {
  tf <- tempfile()
  on.exit(unlink(tf), add = TRUE)
  writeBin(charToRaw(enc2utf8(txt)), tf)
  unname(tools::md5sum(tf))
}

build_digest <- function(manifest = "manifest.json", root = ".") {
  man <- if (file.exists(manifest))
    tryCatch(jsonlite::fromJSON(manifest, simplifyVector = FALSE), error = function(e) NULL)
  files <- if (is.list(man)) man$files else NULL
  if (is.list(files) && length(files)) {
    paths <- sort(names(files), method = "radix")
    sums <- vapply(paths, function(p) tolower(as.character(files[[p]]$checksum %||% "")),
                   character(1))
    return(paste0("m-", .md5_text(paste(sprintf("%s\t%s", paths, sums), collapse = "\n"))))
  }
  paths <- RUNTIME_FILE_GLOBS(root)
  sums <- tolower(unname(tools::md5sum(file.path(root, paths))))
  paste0("f-", .md5_text(paste(sprintf("%s\t%s", paths, sums), collapse = "\n")))
}
BUILD_DIGEST <- tryCatch(build_digest(), error = function(e) "unknown")

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
# Desert-night creative system (matches the DDL suite cover). Key NAMES kept (server.R
# references DDL$sky/$gold2/etc.), VALUES remapped to the desert palette so the charts
# re-theme from one edit. NOTE: the app DEFAULTS to LIGHT (ui.R input_dark_mode
# mode="light"); DDL below is the DARK palette, applied only when the user toggles
# dark. Chart colours are chosen per-mode via is_dark()/dcol() in server.R — tune the
# LIGHT (default) branch first, it is what most visitors see.
DDL <- list(
  navy = "#0e1d40", navy2 = "#1b2e5c", cardinal = "#fb8a7e", gold = "#ffd24a",
  gold2 = "#e0b43a", sky = "#5cc6f5", green = "#5fb56a", ink = "#eaf2ff",
  muted = "#9fb0cf", bg = "#070d1f", paper = "#0e1d40", line = "rgba(255,255,255,0.12)"
)

# Server-side PDF report generator (sourced after DDL). Its palette `PG` is now
# DECOUPLED from DDL (it prints on white paper, so it stays the light house colors).
source("R/report_pdf.R", local = FALSE)

# Light "desert-day" base (shown if the user toggles light). DARK is the default +
# showcase; styles.css [data-bs-theme="dark"] carries the full desert-night system.
# Rubik is named as a PLAIN CSS font-family here (a bslib font_collection of bare
# strings), NOT font_google("Rubik"). font_google() defaults to local = TRUE, which
# makes bslib DOWNLOAD the Rubik files from Google's servers into app_cache/sass and
# compile them into the theme *at app startup*. On Connect Cloud that live fetch runs
# on EVERY cold start (the idle worker recycles and wipes the cache), and if Google
# Fonts is slow/unreachable the Sass compile blocks/fails during boot -> black screen /
# "start-up error" (confirmed in the Connect logs: "Downloading google font Rubik to
# local cache" immediately before "Stopping server..."). A manual republish only
# re-primes the cache until the next recycle. Naming the family as a string does ZERO
# network at boot; the actual Rubik glyphs are still delivered to the browser by the
# <link rel=stylesheet ...fonts.googleapis.com...> in ui.R (client-side, non-blocking,
# display=swap), with a system-sans stack as the guaranteed fallback if that link is
# ever blocked. Net: the theme compiles offline, and text always renders.
rubik_stack <- bslib::font_collection(
  "Rubik", "system-ui", "-apple-system", "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif"
)
app_theme <- bs_theme(
  version = 5,
  bg = "#ffffff", fg = "#16243a",
  primary = "#1f78c4", secondary = "#e0685a",
  success = "#3f9a52", info = "#2f8fc4", warning = "#d6a31c", danger = "#e0685a",
  base_font    = rubik_stack,
  heading_font = rubik_stack,
  "border-radius" = "12px"
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
# a11y (WCAG 4.1.2 name/role/value + 2.1.1 keyboard): bsicons::bs_icon() emits
# aria-hidden="true", so the bare <span> trigger is NAMELESS to a screen reader
# and — as a <span> — never in the tab order. Give it a keyboard-reachable role +
# an sr-only name built from the popover's own title, so every "ⓘ" announces
# "About: <title>, button" and opens on Enter/Space (the generic role="button"
# Enter/Space handler in app.js + bslib's own focus handling cover activation).
info_pop <- function(title, ..., placement = "auto") {
  aria <- paste0("About: ", if (is.character(title) && length(title) == 1) title else "more information")
  bslib::popover(
    tags$span(class = "info-dot", tabindex = "0", role = "button", `aria-label` = aria,
              bsicons::bs_icon("info-circle")),
    ..., title = title, placement = placement)
}

# A compact "answer up front" banner shown at the top of a data-heavy chart
# card: a tone-coloured left rail + icon + one plain-English finding (wrap the
# number that matters in tags$b() or <span class='ci-hero'>). Echoes the
# population driver card so every key chart leads with a sentence, not a raw
# plot. tone: navy | pine | gold | terra | muted.
insight_banner <- function(icon, ..., tone = "navy") {
  div(class = paste("chart-insight", paste0("ci-", tone)),
    bsicons::bs_icon(icon), div(class = "ci-text", ...))
}

# The figure-legend line that travels with a chart on export: metric + its n and
# denominator + the one honest read (what it does and does NOT show). Understated
# muted caption UNDER the plot; the live site+window scope travels via ctx_anno()
# on the plot itself, so this states the invariant method, not the reactive scope.
chart_caption <- function(...) {
  div(class = "chart-caption", bsicons::bs_icon("card-text"),
    tags$span(class = "cc-text", ...))
}

# A clean tinted pill/badge (rarity & chonk tags). Auto-picks DARK text on a bright
# fill (gold/teal/coral) and white on a dark fill, so it reads in both themes.
glow_badge <- function(label, color = "#1f78c4", glow = color) {
  txt <- tryCatch({
    rc <- grDevices::col2rgb(color)
    if ((0.299*rc[1] + 0.587*rc[2] + 0.114*rc[3]) / 255 > 0.6) "#16243a" else "#ffffff"
  }, error = function(e) "#ffffff")
  span(
    class = "glow-badge",
    style = sprintf("color:%s; background:%s; border-color:%s;", txt, color, color),
    label
  )
}

# Format a date range nicely for headers.
fmt_range <- function(a, b) {
  if (is.null(a) || is.null(b) || is.na(a) || is.na(b)) return("")
  sprintf("%s → %s", format(a, "%b %Y"), format(b, "%b %Y"))
}

# The app mascot — a flat (no-gradient, no-id so it's safely reusable) cute mouse
# in the Cobalt & Gold accent. Used as the loading spinner, the splash guide, and
# the celebration hop. Parts are classed so the CSS can wiggle ears / blink eyes.
MASCOT_MOUSE <- htmltools::HTML(paste0(
  '<svg class="mascot" viewBox="0 0 120 120" aria-hidden="true">',
  '<g class="mascot-ear-l"><circle cx="42" cy="34" r="14" fill="#5aa0d8"/><circle cx="43" cy="36" r="8" fill="#ffd24a"/></g>',
  '<g class="mascot-ear-r"><circle cx="78" cy="34" r="14" fill="#5aa0d8"/><circle cx="77" cy="36" r="8" fill="#ffd24a"/></g>',
  '<path class="mascot-tail" d="M88,82 Q110,94 113,72" fill="none" stroke="#5a93c8" stroke-width="4" stroke-linecap="round"/>',
  '<ellipse cx="60" cy="66" rx="32" ry="33" fill="#5aa0d8"/>',
  '<ellipse cx="60" cy="76" rx="20" ry="22" fill="#eaf2ff"/>',
  '<g stroke="#dcebfa" stroke-width="1.2" stroke-linecap="round" opacity=".85"><path d="M52,72 L24,68"/><path d="M52,78 L24,80"/><path d="M68,72 L96,68"/><path d="M68,78 L96,80"/></g>',
  '<path d="M55,70 Q60,68 65,70 Q62,77 60,77 Q58,77 55,70 Z" fill="#fb8a7e"/>',
  '<g class="mascot-eyes"><circle cx="50" cy="60" r="6.5" fill="#0a1a2e"/><circle cx="70" cy="60" r="6.5" fill="#0a1a2e"/>',
  '<circle cx="48" cy="57.5" r="2.4" fill="#ffffff"/><circle cx="68" cy="57.5" r="2.4" fill="#ffffff"/></g>',
  '</svg>'))
