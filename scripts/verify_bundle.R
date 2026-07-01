#!/usr/bin/env Rscript
# verify_bundle.R — pre-deploy integrity gate for the monthly refresh loop.
#
# THE VERIFICATION BOX. The refresh workflow rebuilds the per-site bundles, the
# indexes, and the manifest, then pushes to main — and that push IS the deploy
# (Connect Cloud auto-republishes). Nothing used to check the rebuilt artifacts
# before that push, so a truncated NEON pull or a stale index could ship silently
# and Connect Cloud would serve it. This script runs AFTER the rebuild and BEFORE
# the push: if it stop()s, the job fails, the push never happens, and Connect Cloud
# keeps serving the last known-good bundle.
#
# Checks (all non-network, run on both the full-pull and skip-download paths):
#   1. data/sites/*.rds  — at least one exists, every one loads without error and
#      is a data.frame; at least one has rows (not every site is empty).
#   2. data/site_index.rds, data/search_index.rds, data/species_ranges.rds — each
#      loads and has > 0 rows (these drive the map, the search tab, and ranges).
#   3. manifest.json — parses as JSON and every runtime file it lists exists on
#      disk. This is the exact Connect Cloud failure mode DEPLOY.md warns about:
#      a manifest whose file list/checksums drift from the bundles makes Connect
#      serve the OLD snapshot.
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

# ---- 1. per-site bundles -----------------------------------------------------
site_files <- list.files("data/sites", pattern = "\\.rds$", full.names = TRUE)
if (length(site_files) == 0L) {
  note("data/sites/ contains no .rds bundles — refresh produced nothing to deploy.")
} else {
  loaded_ok <- 0L; with_rows <- 0L
  for (f in site_files) {
    obj <- tryCatch(readRDS(f), error = function(e) e)
    if (inherits(obj, "error")) {
      note(sprintf("bundle failed to load: %s (%s)", f, conditionMessage(obj)))
      next
    }
    loaded_ok <- loaded_ok + 1L
    n <- rows_of(obj)
    if (!is.na(n) && n > 0L) with_rows <- with_rows + 1L
  }
  cat(sprintf("sites: %d file(s), %d loaded, %d with rows\n",
              length(site_files), loaded_ok, with_rows))
  if (loaded_ok < length(site_files))
    note(sprintf("%d of %d site bundle(s) failed to load — likely corrupt/truncated.",
                 length(site_files) - loaded_ok, length(site_files)))
  if (with_rows == 0L)
    note("every site bundle is empty (0 rows) — a refresh this empty is almost certainly a bad NEON pull.")
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
