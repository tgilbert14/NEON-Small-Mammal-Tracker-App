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
#   4. runtime files -> manifest: every runtime file is listed in manifest.json.
#   5. privacy: no bundle remarks field carries an email address.
#   6. independent oracles: headline metrics for SRER, HARV, ABBY and JORN are
#      recomputed from raw bundle columns with base R written here and compared
#      with community_stats(), mnka_series() and data/site_index.rds.
#   7. provenance files from the refresh bot (bundle_meta.json, neon_issue_log.json)
#      and bundle_schema-2 column consistency.
#
# Every check runs and records PASS / FAIL / NOTE; one summary prints at the end,
# then the script stop()s if anything failed. Each failure is also emitted as a
# GitHub Actions ::error:: annotation so it is legible in the run log.

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

RESULTS <- data.frame(section = character(), status = character(),
                      detail = character(), stringsAsFactors = FALSE)
SECTION <- "setup"
record <- function(status, msg) {
  RESULTS[nrow(RESULTS) + 1L, ] <<- list(SECTION, status, msg)
  invisible(status == "PASS")
}
note <- function(msg) record("FAIL", msg)      # a hard failure (kept name: older checks use it)
pass <- function(msg) record("PASS", msg)
info <- function(msg) record("NOTE", msg)      # informational, never fails the run
expect <- function(ok, msg) if (isTRUE(ok)) pass(msg) else note(msg)
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
EXPECTED_SNAPSHOT_SOURCE_PINS <- c(plotly="4.12.0")
EXPECTED_SNAPSHOT_SOURCE_URLS <- c(
  plotly="https://packagemanager.posit.co/cran/2026-07-15/src/contrib/plotly_4.12.0.tar.gz")

# ---- 1. per-site bundles -----------------------------------------------------
SECTION <- "1 bundles"
site_files <- list.files("data/sites", pattern = "\\.rds$", full.names = TRUE)
site_codes <- sub("\\.rds$", "", basename(site_files))
missing_sites <- setdiff(EXPECTED_SITES, site_codes)
extra_sites <- setdiff(site_codes, EXPECTED_SITES)
if (length(missing_sites) || length(extra_sites)) {
  note(sprintf("site set mismatch: missing=[%s] extra=[%s]",
               paste(missing_sites, collapse = ","),
                               paste(extra_sites, collapse = ",")))
} else pass(sprintf("site set matches the %d expected sites", length(EXPECTED_SITES)))
SCHEMA2_COLS <- c("gridCompletion", "samplingImpractical", "mammalGridSamplingType", "eventID")
EMAIL_RE <- "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+[.][A-Za-z]{2,}"
bundle_max_date <- c(); bundle_schema2 <- c(); email_hits <- c()
{
  loaded_ok <- 0L; with_rows <- 0L; schema_ok <- 0L
  for (f in site_files) {
    obj <- tryCatch(readRDS(f), error = function(e) e)
    if (inherits(obj, "error")) {
      note(sprintf("bundle failed to load: %s (%s)", f, conditionMessage(obj)))
      next
    }
    loaded_ok <- loaded_ok + 1L
    code <- sub("\\.rds$", "", basename(f))
    if (is.data.frame(obj)) {
      if ("collectDate" %in% names(obj)) {
        ds <- suppressWarnings(as.Date(substr(as.character(obj$collectDate), 1, 10)))
        bundle_max_date[code] <- if (any(!is.na(ds))) as.character(max(ds, na.rm = TRUE)) else NA_character_
      }
      bundle_schema2[code] <- sum(SCHEMA2_COLS %in% names(obj))
      if ("remarks" %in% names(obj))
        email_hits[code] <- sum(grepl(EMAIL_RE, as.character(obj$remarks), perl = TRUE))
    }
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
  if (loaded_ok == length(site_files) && with_rows == length(EXPECTED_SITES) &&
      schema_ok == length(EXPECTED_SITES))
    pass(sprintf("%d bundles load, have rows and carry the effort schema", loaded_ok))
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
SECTION <- "2 indexes"
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
    pass(sprintf("index ok: %s (%d rows)", idx, n))
}

# ---- 3. manifest ↔ bundle coherence -----------------------------------------
SECTION <- "3 manifest"
MAN_FILES <- character(0)
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
      MAN_FILES <- names(files)
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
      if (!any(bad_checksum) && !length(missing))
        pass(sprintf("all %d manifest files exist and match their checksums", length(files)))
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
        } else if (pkg %in% names(EXPECTED_SNAPSHOT_SOURCE_PINS)) {
          remote_type <- as.character(x$description$RemoteType %||% "")
          remote_ref <- as.character(x$description$RemotePkgRef %||% "")
          built <- as.character(x$description$Built %||% "")
          expected_ref <- paste0(
            "url::", unname(EXPECTED_SNAPSHOT_SOURCE_URLS[[pkg]]))
          base_bad ||
            !identical(version, unname(EXPECTED_SNAPSHOT_SOURCE_PINS[[pkg]])) ||
            !identical(source, "CRAN") ||
            !identical(repo, EXPECTED_REPOSITORY) ||
            !identical(remote_type, "url") ||
            !identical(remote_ref, expected_ref) || nzchar(built)
        } else {
          remote_type <- as.character(x$description$RemoteType %||% "")
          remote_repos <- as.character(x$description$RemoteRepos %||% "")
          moving_standard <- identical(remote_type, "standard") &&
            !identical(remote_repos, EXPECTED_REPOSITORY)
          base_bad || !identical(source, "CRAN") ||
            !identical(repo, EXPECTED_REPOSITORY) || moving_standard
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
      for (pkg in names(EXPECTED_SNAPSHOT_SOURCE_PINS)) {
        if (!pkg %in% keys) {
          note(sprintf("manifest lacks required snapshot-source package: %s", pkg))
          next
        }
        got <- as.character(pkgs[[pkg]]$description$Version %||% "")
        want <- unname(EXPECTED_SNAPSHOT_SOURCE_PINS[[pkg]])
        if (!identical(got, want))
          note(sprintf("manifest snapshot-source pin mismatch: %s=%s (expected %s)",
                       pkg, got, want))
      }
    }
  }
}

if (!any(RESULTS$section == "3 manifest" & RESULTS$status == "FAIL"))
  pass("manifest packages, R platform and geospatial pins match policy")

# ---- 4. runtime files -> manifest (reverse check) ---------------------------
# Connect serves what the manifest lists. A runtime file the app reads that the
# manifest omits is a deploy that works locally and breaks on Connect. Keep this
# list in step with RUNTIME_FILE_GLOBS() in global.R, scripts/write_manifest.R
# and the digest in scripts/post_deploy_smoke.sh.
SECTION <- "4 runtime files"
runtime_files <- local({
  rel <- function(dir, ...) {
    f <- list.files(dir, full.names = FALSE, ...)
    if (length(f)) file.path(dir, f) else character()
  }
  out <- c("global.R", "ui.R", "server.R", rel("R", pattern = "\\.R$"),
           rel("www", recursive = TRUE), rel("data", pattern = "\\.rds$", recursive = TRUE),
           rel("data", pattern = "\\.json$"), rel("data-sample", recursive = TRUE))
  sort(unique(out[file.exists(out)]), method = "radix")
})
if (length(MAN_FILES)) {
  unlisted <- setdiff(runtime_files, MAN_FILES)
  if (length(unlisted))
    note(sprintf("%d runtime file(s) are not in manifest.json files: %s",
                 length(unlisted), paste(utils::head(unlisted, 12L), collapse = ", ")))
  else pass(sprintf("all %d runtime files are listed in manifest.json", length(runtime_files)))
} else note("manifest file list unavailable; runtime files cannot be checked against it")

# ---- 5. privacy: no email addresses in bundle remarks -----------------------
SECTION <- "5 email gate"
if (!length(email_hits)) {
  note("no bundle remarks were scanned for email addresses")
} else if (any(email_hits > 0L)) {
  hits <- email_hits[email_hits > 0L]
  note(sprintf("bundle remarks contain email address(es): %s",
               paste(sprintf("%s=%d", names(hits), hits), collapse = ", ")))
} else pass(sprintf("no email addresses in remarks across %d bundles", length(email_hits)))

# ---- 6. independent oracles for the headline metrics ------------------------
# Every number below is recomputed from raw bundle columns with base R written in
# this file. It never calls the helper being checked and does not reuse its rule
# tables (MAM_TRAP_STATUS_EFFORT, MAM_PLACEHOLDER_TAG_RE, species_level_only).
# Where a helper's definition differs from the oracle's by design, the comment
# names the difference and the check asserts the documented relation instead.
SECTION <- "6 oracles"
ORACLE_SITES <- c("SRER", "HARV", "ABBY", "JORN")
oracle_ok <- tryCatch({
  suppressPackageStartupMessages(library(dplyr))
  source("R/helpers.R", chdir = TRUE)
  TRUE
}, error = function(e) { note(sprintf("could not load R/helpers.R: %s", conditionMessage(e))); FALSE })
site_index <- tryCatch(as.data.frame(readRDS("data/site_index.rds")), error = function(e) NULL)

# The six NEON trapStatus tokens, by their leading code (literal table).
ORACLE_STATUS_WEIGHT <- c("1" = 0, "2" = 0.5, "3" = 0.5, "4" = 1, "5" = 1, "6" = 1)
oracle_placeholder <- function(tag) {
  # NEON.MAM.D<2 digits>.O... is an untagged capture event, unless the part after
  # the domain is a real ear tag: OL or OR followed only by digits.
  t <- ifelse(is.na(tag), "", tag)
  after <- substring(t, 14L)                         # text after "NEON.MAM.Dnn."
  starts <- substr(t, 1L, 10L) == "NEON.MAM.D" &
    grepl("^[0-9][0-9]$", substr(t, 11L, 12L)) & substr(t, 13L, 13L) == "." &
    substr(after, 1L, 1L) == "O"
  ear_tag <- grepl("^O[LR][0-9]+$", after)
  starts & !ear_tag
}
oracle_species_name <- function(nm) {
  # Two-word binomial "Genus epithet"; genus-only "X sp." and ambiguous "A/B" are not species.
  x <- ifelse(is.na(nm), "", trimws(nm))
  grepl("^[A-Z][a-z]+ [a-z][a-z-]*$", x) & !grepl("sp.", x, fixed = TRUE) &
    !grepl("/", x, fixed = TRUE)
}

if (oracle_ok) for (code in ORACLE_SITES) {
  f <- file.path("data/sites", paste0(code, ".rds"))
  raw <- tryCatch(as.data.frame(readRDS(f)), error = function(e) NULL)
  if (is.null(raw)) { note(sprintf("%s: oracle bundle unreadable", code)); next }
  d <- tryCatch(clean_mam(raw), error = function(e) e)
  if (inherits(d, "error")) { note(sprintf("%s: clean_mam failed: %s", code, conditionMessage(d))); next }
  cs <- community_stats(d)

  status_code <- substr(trimws(as.character(raw$trapStatus)), 1L, 1L)
  weight <- unname(ORACLE_STATUS_WEIGHT[status_code])
  tag <- as.character(raw$tagID)
  tag_present <- !is.na(tag) & nzchar(trimws(ifelse(is.na(tag), "", tag)))
  is_cap <- status_code %in% c("4", "5") & tag_present
  # Rows that carry a tag under a non-capture status (e.g. ABBY has one tagged
  # status-3 row). community_stats() and site_index count handled animals by tag
  # presence alone, so by design: helper captures = oracle captures + anomalies.
  anomalies <- sum(tag_present & !is_cap)
  n_cap <- sum(is_cap)
  expect(!anyNA(weight), sprintf("%s: every trapStatus is one of the six NEON tokens", code))

  # total captures
  expect(cs$total_captures == n_cap + anomalies,
         sprintf("%s captures: oracle %d (+%d tagged non-capture rows) vs community_stats %d",
                 code, n_cap, anomalies, cs$total_captures))
  # individuals: distinct real ear tags among captures
  ph <- oracle_placeholder(tag)
  n_ind <- length(unique(tag[is_cap & !ph]))
  ind_ok <- if (anomalies == 0L) cs$individuals == n_ind else
    cs$individuals >= n_ind && cs$individuals <= n_ind + anomalies
  expect(ind_ok, sprintf("%s individuals: oracle %d vs community_stats %d", code, n_ind, cs$individuals))
  if (!is.null(site_index) && code %in% site_index$site) {
    si <- site_index[site_index$site == code, , drop = FALSE]
    expect(si$captures == n_cap + anomalies && si$individuals == cs$individuals &&
             (anomalies > 0L || si$individuals == n_ind),
           sprintf("%s site_index: captures %d, individuals %d (oracle %d / %d)",
                   code, si$captures, si$individuals, n_cap, n_ind))
  } else note(sprintf("%s: not found in data/site_index.rds", code))

  # species count
  nm <- as.character(raw$scientificName)
  n_sp <- length(unique(nm[is_cap & oracle_species_name(nm)]))
  expect(cs$species == n_sp, sprintf("%s species: oracle %d vs community_stats %d", code, n_sp, cs$species))

  # recapture rate (percent of capture rows flagged recapture = Y)
  rec <- if (n_cap) round(100 * mean(toupper(ifelse(is.na(raw$recapture[is_cap]), "", raw$recapture[is_cap])) == "Y"), 1) else 0
  tol <- if (anomalies == 0L) 0 else 100 * anomalies / max(n_cap, 1L) + 0.05
  expect(abs(cs$recap_rate - rec) <= tol + 1e-9,
         sprintf("%s recapture rate: oracle %.1f%% vs community_stats %.1f%%", code, rec, cs$recap_rate))

  # trap-nights: one trap-night per trap per night, keyed (nightuid, plot, coordinate),
  # taking the trap's status weight once even when several animals share the trap.
  key <- paste(raw$nightuid, raw$plotID, raw$trapCoordinate, sep = "|")
  tn_per_trap <- sum(tapply(weight, key, max))
  # By design the helper cannot merge rows whose coordinate is not a grid cell
  # (placeholder "XX"/"AX"/"X5" positions name no single trap) and keeps both rows
  # of a crew-noted double-trap set ("double" in remarks). For those keys every
  # row is its own trap. Documented relation: helper == this row-level variant,
  # and helper >= the strict per-key count.
  grid_cell <- grepl("^[A-J]([1-9]|10)$", as.character(raw$trapCoordinate))
  dbl <- grepl("double", tolower(ifelse(is.na(raw$remarks), "", raw$remarks)), fixed = TRUE)
  per_key_row_level <- tapply(!grid_cell | dbl, key, any)
  tn_rows <- sum(vapply(split(weight, key), sum, numeric(1))[per_key_row_level[names(split(weight, key))]]) +
    sum(vapply(split(weight, key), max, numeric(1))[!per_key_row_level[names(split(weight, key))]])
  helper_tn <- sum(d$trap_effort, na.rm = TRUE)
  expect(isTRUE(all.equal(helper_tn, tn_rows)) && round(tn_rows) == cs$trap_nights &&
           helper_tn >= tn_per_trap - 1e-9,
         sprintf("%s trap-nights: oracle per-trap %.1f, with row-level unknown/double traps %.1f vs helper %.1f (shown %s)",
                 code, tn_per_trap, tn_rows, helper_tn, format(cs$trap_nights)))

  # peak monthly MNKA for the site's most-captured species: per plot, an animal is
  # known alive in every sampled month from its first to its last capture there;
  # the site value is the sum over plots, the peak its maximum over months.
  ym <- substr(as.character(raw$collectDate), 1L, 7L)
  top <- names(sort(table(nm[is_cap & !is.na(nm)]), decreasing = TRUE))[1]
  sampled <- unique(data.frame(plot = raw$plotID, ym = ym)[weight > 0 & !is.na(raw$plotID) & !is.na(ym), ])
  peak_of <- function(rows) {
    if (!any(rows)) return(0)
    k <- paste(raw$plotID[rows], tag[rows], sep = "|")
    first <- tapply(ym[rows], k, min); last <- tapply(ym[rows], k, max)
    kplot <- sub("\\|.*$", "", names(first))
    alive <- vapply(seq_len(nrow(sampled)), function(i)
      sum(kplot == sampled$plot[i] & first <= sampled$ym[i] & last >= sampled$ym[i]), numeric(1))
    max(tapply(alive, sampled$ym, sum))
  }
  # mnka_series() counts every tagged capture row, so a placeholder capture event
  # is a known-alive animal for its own month (the same convention as
  # bout_closed_capture's n_unmarked). The oracle asserts that definition and
  # prints the marked-only value beside it.
  base_rows <- tag_present & !is.na(nm) & nm == top & !is.na(raw$plotID) & !is.na(ym)
  peak_all <- peak_of(base_rows)
  peak_marked <- peak_of(base_rows & !ph)
  m <- mnka_series(d, top)
  helper_peak <- if (is.null(m)) 0 else max(tapply(m$mnka, m$ym, sum))
  expect(helper_peak == peak_all,
         sprintf("%s peak monthly MNKA (%s): oracle %d (marked tags only %d) vs mnka_series %d",
                 code, top, as.integer(peak_all), as.integer(peak_marked), as.integer(helper_peak)))
}

# ---- 7. provenance files and bundle schema ----------------------------------
# TODO(lead): these gates are tolerant until the refresh bot writes its first
# candidate with data/bundle_meta.json and data/neon_issue_log.json. Once that
# candidate lands on main, flip REQUIRE_PROVENANCE to TRUE so absence fails.
SECTION <- "7 provenance"
REQUIRE_PROVENANCE <- FALSE
absent <- function(msg) if (REQUIRE_PROVENANCE) note(msg) else info(msg)
META_KEYS <- c("product", "release", "doi", "release_generated", "bundle_schema",
               "data_from", "data_through", "sites", "previous_data_through", "advanced")
if (file.exists("data/bundle_meta.json")) {
  meta <- tryCatch(jsonlite::fromJSON("data/bundle_meta.json", simplifyVector = FALSE),
                   error = function(e) e)
  if (inherits(meta, "error") || !is.list(meta)) {
    note("data/bundle_meta.json is not valid JSON")
  } else {
    miss <- setdiff(META_KEYS, names(meta))
    expect(!length(miss), sprintf("bundle_meta.json carries the required keys%s",
                                  if (length(miss)) paste0(" (missing: ", paste(miss, collapse = ","), ")") else ""))
    rel <- as.character(meta$release %||% "")
    expect(length(rel) == 1L && grepl("^RELEASE-[0-9]{4}$", rel),
           sprintf("bundle_meta release is RELEASE-YYYY (got %s)", rel))
    doi <- as.character(meta$doi %||% "")
    expect(length(doi) == 1L && startsWith(doi, "https://doi.org/10.48443/"),
           sprintf("bundle_meta doi is a NEON DOI (got %s)", doi))
    want_through <- if (length(bundle_max_date)) max(bundle_max_date, na.rm = TRUE) else NA_character_
    got_through <- substr(as.character(meta$data_through %||% ""), 1L, 10L)
    expect(identical(got_through, want_through),
           sprintf("bundle_meta data_through %s equals the max collectDate across bundles %s",
                   got_through, want_through))
    expect(identical(as.integer(meta$sites %||% NA), length(site_files)),
           sprintf("bundle_meta sites %s equals the bundle count %d",
                   format(meta$sites %||% NA), length(site_files)))
  }
} else absent("data/bundle_meta.json absent (tolerated until the first bot candidate)")

ISSUE_KEYS <- c("id", "issueDate", "resolvedDate", "dateRangeStart", "dateRangeEnd",
                "locationAffected", "issue", "resolution")
if (file.exists("data/neon_issue_log.json")) {
  log <- tryCatch(jsonlite::fromJSON("data/neon_issue_log.json", simplifyVector = FALSE),
                  error = function(e) e)
  if (inherits(log, "error")) {
    note("data/neon_issue_log.json is not valid JSON")
  } else {
    is_array <- is.list(log) && is.null(names(log))
    bad <- if (is_array) sum(!vapply(log, function(e) is.list(e) && all(ISSUE_KEYS %in% names(e)), logical(1))) else NA
    expect(is_array && bad == 0L,
           sprintf("neon_issue_log.json is an array of %d entries with the eight keys%s",
                   if (is_array) length(log) else 0L,
                   if (!is_array) " (not an array)" else if (bad) sprintf(" (%d entries lack keys)", bad) else ""))
  }
} else absent("data/neon_issue_log.json absent (tolerated until the first bot candidate)")

if (length(bundle_schema2)) {
  n2 <- sum(bundle_schema2 == length(SCHEMA2_COLS))
  partial <- names(bundle_schema2)[bundle_schema2 > 0L & bundle_schema2 < length(SCHEMA2_COLS)]
  if (n2 == 0L && !length(partial)) {
    absent("no bundle carries the bundle_schema 2 columns yet")
  } else {
    expect(n2 == length(bundle_schema2) && !length(partial),
           sprintf("bundle_schema 2 columns in %d of %d bundles%s", n2, length(bundle_schema2),
                   if (length(partial)) paste0(" (partial: ", paste(partial, collapse = ","), ")") else ""))
  }
}

# ---- summary and verdict -----------------------------------------------------
cat("\n==== Bundle verification summary ====\n")
for (i in seq_len(nrow(RESULTS)))
  cat(sprintf("%-4s [%s] %s\n", RESULTS$status[i], RESULTS$section[i], RESULTS$detail[i]))
counts <- table(factor(RESULTS$status, levels = c("PASS", "FAIL", "NOTE")))
cat(sprintf("---- %d passed, %d failed, %d notes ----\n",
            counts[["PASS"]], counts[["FAIL"]], counts[["NOTE"]]))
failed <- RESULTS$detail[RESULTS$status == "FAIL"]
if (length(failed)) {
  for (p in failed) gha_error(p)
  stop(sprintf("Bundle verification FAILED with %d problem(s); refusing to deploy. See ::error:: annotations above.",
               length(failed)), call. = FALSE)
}
cat("\nBundle verification PASSED: safe to deploy.\n")
