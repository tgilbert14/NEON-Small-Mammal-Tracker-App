#!/usr/bin/env Rscript
# verify_bundle.R — pre-deploy integrity gate for the monthly refresh loop.
#
# THE VERIFICATION BOX. The refresh workflow builds a candidate in staging,
# rebuilds the indexes and manifest, and runs this script before it can publish a
# review branch. Merging that candidate to main remains the explicit deploy gate.
#
# Checks (all non-network, run on both the full-pull and skip-download paths):
#   1. data/sites/*.rds  — exact expected 46-site set, every file loadable,
#      non-empty, data-frame-shaped, and carrying the effort-contract schema.
#   2. data/site_index.rds, data/search_index.rds, data/species_ranges.rds — each
#      loads and has > 0 rows (these drive the map, the search tab, and ranges).
#   3. manifest.json — parses as JSON; every runtime file exists and its committed
#      checksum matches the current byte stream; R, repository snapshot, runtime
#      roots, geospatial closure, and forbidden live-only packages match policy.
#
# Exit non-zero (via stop()) on any hard failure. Emits a GitHub Actions
# ::error:: annotation for each so the failure is legible in the run log.

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

problems <- character(0)
note <- function(msg) { problems[[length(problems) + 1L]] <<- msg }
gha_error <- function(msg) cat(sprintf("::error title=Bundle verification::%s\n", msg))

rows_of <- function(x) {
  n <- tryCatch(nrow(x), error = function(e) NA_integer_)
  if (is.null(n)) NA_integer_ else n
}

EXPECTED_SITES <- c(
  "ABBY","BARR","BART","BLAN","BONA","CLBJ","CPER","DCFS","DEJU","DELA",
  "DSNY","GRSM","GUAN","HARV","HEAL","JERC","JORN","KONA","KONZ","LAJA",
  "LENO","MLBS","MOAB","NIWO","NOGP","OAES","ONAQ","ORNL","OSBS","RMNP",
  "SCBI","SERC","SJER","SOAP","SRER","STEI","STER","TALL","TEAK","TOOL",
  "TREE","UKFS","UNDE","WOOD","WREF","YELL")
MAMMAL_REQUIRED <- c(
  "collectDate","nightuid","plotID","trapCoordinate","trapStatus","tagID","remarks")
EXPECTED_R_PLATFORM <- "4.5.2"
EXPECTED_REPOSITORY <-
  "https://packagemanager.posit.co/cran/__linux__/jammy/2026-07-15"
REQUIRED_RUNTIME_PKGS <- c(
  "shiny","bslib","bsicons","dplyr","tidyr","stringr","tibble","plotly",
  "leaflet","DT","shinyjs","shinycssloaders","RColorBrewer","htmltools",
  "ggplot2","jsonlite")
FORBIDDEN_RUNTIME_PKGS <- c("neonUtilities","arrow")
EXPECTED_GEO_PINS <- c(
  terra="1.8-50", sf="1.1-1", s2="1.1.11", units="1.0-1",
  wk="0.9.5", classInt="0.4-11", raster="3.6-32", sp="2.2-1")
EXPECTED_GEO_URLS <- c(
  terra="https://cran.r-project.org/src/contrib/Archive/terra/terra_1.8-50.tar.gz",
  sf="https://packagemanager.posit.co/cran/2026-07-15/src/contrib/sf_1.1-1.tar.gz",
  s2="https://packagemanager.posit.co/cran/2026-07-15/src/contrib/s2_1.1.11.tar.gz",
  units="https://packagemanager.posit.co/cran/2026-07-15/src/contrib/units_1.0-1.tar.gz",
  wk="https://packagemanager.posit.co/cran/2026-07-15/src/contrib/wk_0.9.5.tar.gz",
  classInt="https://packagemanager.posit.co/cran/2026-07-15/src/contrib/classInt_0.4-11.tar.gz",
  raster="https://packagemanager.posit.co/cran/2026-07-15/src/contrib/raster_3.6-32.tar.gz",
  sp="https://packagemanager.posit.co/cran/2026-07-15/src/contrib/sp_2.2-1.tar.gz")

# ---- 1. per-site bundles -----------------------------------------------------
site_files <- list.files("data/sites", pattern = "\\.rds$", full.names = TRUE)
site_codes <- sub("\\.rds$", "", basename(site_files))
missing_sites <- setdiff(EXPECTED_SITES, site_codes)
extra_sites <- setdiff(site_codes, EXPECTED_SITES)
if (length(missing_sites) || length(extra_sites)) {
  note(sprintf("site set mismatch: missing=[%s] extra=[%s]",
               paste(missing_sites, collapse = ","),
               paste(extra_sites, collapse = ",")))
} else {
  loaded_ok <- 0L; with_rows <- 0L; schema_ok <- 0L
  for (f in site_files) {
    obj <- tryCatch(readRDS(f), error = function(e) e)
    if (inherits(obj, "error")) {
      note(sprintf("bundle failed to load: %s (%s)", f, conditionMessage(obj)))
      next
    }
    loaded_ok <- loaded_ok + 1L
    n <- rows_of(obj)
    if (!is.na(n) && n > 0L) {
      with_rows <- with_rows + 1L
    } else {
      note(sprintf("site bundle has no rows: %s", f))
    }
    if (!is.data.frame(obj)) {
      note(sprintf("site bundle is not a data frame: %s", f))
    } else {
      missing_cols <- setdiff(MAMMAL_REQUIRED, names(obj))
      if (length(missing_cols))
        note(sprintf("site bundle lacks required effort field(s): %s [%s]",
                     f, paste(missing_cols, collapse = ",")))
      else schema_ok <- schema_ok + 1L
    }
  }
  cat(sprintf("sites: %d expected, %d loaded, %d with rows, %d schema-valid\n",
              length(site_files), loaded_ok, with_rows, schema_ok))
  if (loaded_ok < length(site_files))
    note(sprintf("%d of %d site bundle(s) failed to load — likely corrupt/truncated.",
                 length(site_files) - loaded_ok, length(site_files)))
  if (with_rows != length(EXPECTED_SITES))
    note(sprintf("only %d/%d expected site bundles have rows.",
                 with_rows, length(EXPECTED_SITES)))
  if (schema_ok != length(EXPECTED_SITES))
    note(sprintf("only %d/%d expected site bundles satisfy the effort schema.",
                 schema_ok, length(EXPECTED_SITES)))
}

# ---- 2. top-level indexes ----------------------------------------------------
for (idx in c("data/site_index.rds", "data/search_index.rds", "data/species_ranges.rds")) {
  if (!file.exists(idx)) { note(sprintf("missing index: %s", idx)); next }
  obj <- tryCatch(readRDS(idx), error = function(e) e)
  if (inherits(obj, "error")) {
    note(sprintf("index failed to load: %s (%s)", idx, conditionMessage(obj))); next
  }
  n <- rows_of(obj)
  if (is.na(n) || n <= 0L)
    note(sprintf("index has no rows: %s (nrow=%s)", idx, n %||% "NA"))
  else
    cat(sprintf("index ok: %s (%d rows)\n", idx, n))
}

# ---- 3. manifest ↔ bundle coherence -----------------------------------------
if (!file.exists("manifest.json")) {
  note("manifest.json is missing — Connect Cloud deploys from it.")
} else {
  man <- tryCatch(jsonlite::fromJSON("manifest.json", simplifyVector = FALSE),
                  error = function(e) e)
  if (inherits(man, "error")) {
    note(sprintf("manifest.json is not valid JSON (%s)", conditionMessage(man)))
  } else {
    files <- man$files
    if (is.null(files) || length(files) == 0L) {
      note("manifest.json lists no files — writeManifest output looks empty.")
    } else {
      missing <- names(files)[!file.exists(names(files))]
      cat(sprintf("manifest lists %d file(s)\n", length(files)))
      if (length(missing) > 0L)
        note(sprintf("manifest references %d file(s) not on disk (Connect would serve a stale snapshot): %s",
                     length(missing), paste(utils::head(missing, 8L), collapse = ", ")))
      present <- setdiff(names(files), missing)
      bad_checksum <- vapply(present, function(path) {
        expected <- tolower(as.character(files[[path]]$checksum %||% ""))
        if (length(expected) != 1L || is.na(expected) || !nzchar(expected))
          return(TRUE)
        actual <- tolower(unname(tools::md5sum(path)))
        !identical(actual, expected)
      }, logical(1))
      if (any(bad_checksum))
        note(sprintf("manifest checksum mismatch for %d file(s): %s",
                     sum(bad_checksum),
                     paste(utils::head(present[bad_checksum], 12L), collapse = ", ")))
    }

    pkgs <- man$packages
    if (is.null(pkgs) || !length(pkgs)) {
      note("manifest.json lists no packages")
    } else {
      keys <- names(pkgs)
      missing_runtime <- setdiff(REQUIRED_RUNTIME_PKGS, keys)
      forbidden_runtime <- intersect(FORBIDDEN_RUNTIME_PKGS, keys)
      if (length(missing_runtime))
        note(sprintf("manifest lacks required runtime package(s): %s",
                     paste(missing_runtime, collapse = ",")))
      if (length(forbidden_runtime))
        note(sprintf("manifest contains live-fetch-only/heavy package(s): %s",
                     paste(forbidden_runtime, collapse = ",")))
      if (!identical(as.character(man$platform %||% ""), EXPECTED_R_PLATFORM))
        note(sprintf("manifest R platform is %s; expected %s",
                     as.character(man$platform %||% "<missing>"), EXPECTED_R_PLATFORM))

      package_problems <- vapply(keys, function(pkg) {
        x <- pkgs[[pkg]]
        version <- as.character(x$description$Version %||% "")
        declared <- as.character(x$description$Package %||% "")
        source <- as.character(x$Source %||% "")
        repo <- as.character(x$Repository %||% "")
        base_bad <- length(version) != 1L || is.na(version) || !nzchar(version) ||
          !identical(declared, pkg)
        if (pkg %in% names(EXPECTED_GEO_PINS)) {
          remote_type <- as.character(x$description$RemoteType %||% "")
          remote_ref <- as.character(x$description$RemotePkgRef %||% "")
          built <- as.character(x$description$Built %||% "")
          expected_ref <- paste0("url::", unname(EXPECTED_GEO_URLS[[pkg]]))
          base_bad || !identical(source, "CRAN") ||
            !identical(repo, "https://cran.r-project.org") ||
            !identical(remote_type, "url") ||
            !identical(remote_ref, expected_ref) || nzchar(built)
        } else {
          base_bad || !identical(source, "CRAN") ||
            !identical(repo, EXPECTED_REPOSITORY)
        }
      }, logical(1))
      if (any(package_problems))
        note(sprintf("manifest package provenance is invalid for %d package(s): %s",
                     sum(package_problems),
                     paste(utils::head(keys[package_problems], 12L), collapse = ",")))

      for (pkg in names(EXPECTED_GEO_PINS)) {
        if (!pkg %in% keys) {
          note(sprintf("manifest lacks required geospatial package: %s", pkg))
          next
        }
        got <- as.character(pkgs[[pkg]]$description$Version %||% "")
        want <- unname(EXPECTED_GEO_PINS[[pkg]])
        if (!identical(got, want))
          note(sprintf("manifest geospatial pin mismatch: %s=%s (expected %s)",
                       pkg, got, want))
      }
    }
  }
}

# ---- verdict -----------------------------------------------------------------
if (length(problems) > 0L) {
  for (p in problems) gha_error(p)
  stop(sprintf("Bundle verification FAILED with %d problem(s) — refusing to deploy. See ::error:: annotations above.",
               length(problems)), call. = FALSE)
}
cat("\nBundle verification PASSED — safe to deploy.\n")
