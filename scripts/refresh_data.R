# ===========================================================================
# refresh_data.R — build the bundled per-site "database"
#
# Downloads each NEON site's full small-mammal record (DP1.10072.001), trims it
# to the columns the app uses, and xz-compresses one .rds per site into the
# directory selected by SMT_SITE_OUT_DIR (data/sites by default). CI points that
# variable at an empty staging directory and swaps the result into data/sites
# only after this script proves the exact expected site set was built.
#
# RESUMABLE: skips sites whose .rds already exists. Delete a file to re-pull it.
#
# !! A LOCAL REBUILD IS NOT A RELEASE !! Connect serves the reviewed `main`
# snapshot, and manifest.json pins a checksum per bundled file. The supported flow
# is the producer/validator/publisher workflow in refresh-data.yml:
#   1. build all expected sites in an empty stage with THIS script
#   2. rebuild indexes and generate the manifest under pinned R/package inputs
#   3. verify exact sites, schema, indexes, checksums, helpers, and offline source
#   4. publish only the immutable candidate to a review branch and open/update a PR
#   5. intentionally merge the reviewed PR; Connect republishes watched `main`
# Never hand-push a local partial refresh to production.
#
# Run from the project root:
#   Rscript scripts/refresh_data.R
# ===========================================================================

options(timeout = 1800)
suppressMessages({
  library(neonUtilities)
  library(dplyr)
  library(tibble)
  library(jsonlite)   # freshness-state marker (data/.refresh_state.json)
})

# NEON API token — set env var NEON_TOKEN to raise the anonymous rate limit.
.neon_token <- Sys.getenv("NEON_TOKEN", unset = NA_character_)
if (!is.na(.neon_token) && nchar(.neon_token) > 10) {
  cat("Using NEON API token (higher rate limits).\n")
} else {
  .neon_token <- NA_character_
}

source("R/site_metadata.R")  # for the canonical site list

keep <- c("tagID","individualCode","taxonID","scientificName","taxonRank",
  "identificationQualifier","nativeStatusCode","plotID","trapCoordinate",
  "decimalLatitude","decimalLongitude","elevation","nlcdClass","namedLocation",
  "collectDate","nightuid","trapStatus","recapture","fate","hindfootLength",
  "earLength","tailLength","totalLength","weight","lifeStage","sex","testes",
  "nipples","pregnancyStatus","vagina","domainID","siteID","remarks")

out_dir <- Sys.getenv("SMT_SITE_OUT_DIR", unset = "data/sites")
if (!nzchar(out_dir)) stop("SMT_SITE_OUT_DIR must not be empty", call. = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

start_d <- "2013-01"
end_d   <- format(Sys.Date(), "%Y-%m")

sites <- neon_sites$site
cat(sprintf("Refreshing %d sites (%s → %s) into %s/\n\n", length(sites), start_d, end_d, out_dir))

summary_rows <- list()
for (s in sites) {
  out <- file.path(out_dir, paste0(s, ".rds"))
  if (file.exists(out)) { cat(sprintf("• %-5s skip (exists, %.2f MB)\n", s, file.size(out)/1e6)); next }

  cat(sprintf("• %-5s downloading…\n", s))
  raw <- tryCatch(
    {
      args <- list(dpID = "DP1.10072.001", site = s, startdate = start_d, enddate = end_d,
                   package = "basic", check.size = "F")
      if (!is.na(.neon_token)) args$token <- .neon_token
      do.call(loadByProduct, args)
    },
    error = function(e) { cat(sprintf("    ERROR %s: %s\n", s, conditionMessage(e))); NULL })
  if (is.null(raw) || is.null(raw$mam_pertrapnight) || nrow(raw$mam_pertrapnight) == 0) {
    cat(sprintf("    no data for %s\n", s)); next
  }
  d <- raw$mam_pertrapnight
  d <- d[, intersect(keep, colnames(d)), drop = FALSE]
  d <- tibble::as_tibble(d)
  # Materialize any ALTREP columns (neonUtilities returns arrow-backed strings) to
  # plain base vectors BEFORE saving — otherwise the .rds carries an arrow ALTVEC
  # that reads back length-zero anywhere arrow can't unserialize it (older R, no
  # arrow loaded), silently emptying every character column. See fix_bundles_altrep.R.
  for (nm in names(d)) {
    col <- d[[nm]]
    d[[nm]] <- if (inherits(col, "Date")) structure(as.numeric(col[seq_along(col)]), class = "Date")
               else if (is.factor(col)) factor(as.character(col[seq_along(col)]))
               else col[seq_along(col)]
  }
  saveRDS(d, out, compress = "xz")
  caps <- sum(!is.na(d$tagID))
  cat(sprintf("    saved %s: %d rows, %d captures, %.2f MB\n", s, nrow(d), caps, file.size(out)/1e6))
  summary_rows[[s]] <- data.frame(site = s, rows = nrow(d), captures = caps,
                                  mb = round(file.size(out)/1e6, 2))
}

if (length(summary_rows)) {
  res <- do.call(rbind, summary_rows)
  cat(sprintf("\nDone. %d new sites, total %.1f MB.\n", nrow(res), sum(res$mb)))
}
n_ok <- length(list.files(out_dir, pattern = "\\.rds$"))
cat(sprintf("Bundle now has %d/%d sites.\n", n_ok, length(sites)))

# ---- freshness assertion (did the data actually advance?) ------------------
# A monthly re-pull that "succeeds" but brings back NO newer records is a SILENT
# stall — NEON didn't publish, the token rate-limited, or the date window was
# wrong — and we'd cheerfully propose the same data forever. So we record the
# freshest collectDate seen across ALL bundles and compare it to the last run's
# value (committed in data/.refresh_state.json). If it did NOT advance, we log
# LOUDLY (and the marker is committed so the bot's commit message can say so).
freshest_collect_date <- function(dir) {
  fs <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  mx <- NA_character_
  for (f in fs) {
    d <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.null(d) || !"collectDate" %in% names(d)) next
    cd <- substr(as.character(d$collectDate), 1, 10)
    cd <- max(cd[!is.na(cd) & nzchar(cd)], na.rm = TRUE)
    if (length(cd) && (is.na(mx) || cd > mx)) mx <- cd
  }
  mx
}

state_path <- Sys.getenv("SMT_REFRESH_STATE_PATH", unset = "data/.refresh_state.json")
prev_max <- NA_character_
if (file.exists(state_path)) {
  prev <- tryCatch(jsonlite::fromJSON(state_path), error = function(e) NULL)
  if (!is.null(prev) && !is.null(prev$freshest_collectDate))
    prev_max <- as.character(prev$freshest_collectDate)
}

new_max <- freshest_collect_date(out_dir)
advanced <- !is.na(new_max) && (is.na(prev_max) || new_max > prev_max)

if (is.na(new_max)) {
  cat("!! FRESHNESS: could not read any collectDate from the rebuilt bundle.\n")
} else if (advanced) {
  cat(sprintf("FRESHNESS OK: freshest record advanced %s -> %s.\n",
              ifelse(is.na(prev_max), "(none)", prev_max), new_max))
} else {
  cat(sprintf("!! FRESHNESS WARNING: freshest record did NOT advance (still %s). NEON may not have published new data, the API may have rate-limited, or the window is wrong. Investigate before accepting this candidate.\n",
              new_max))
}

# Persist the marker (committed by the workflow) so the next run can compare AND
# so the commit step can report what actually changed.
tryCatch(
  jsonlite::write_json(
    list(freshest_collectDate = ifelse(is.na(new_max), NULL, new_max),
         previous_freshest    = ifelse(is.na(prev_max), NULL, prev_max),
         advanced             = isTRUE(advanced),
         sites_built          = n_ok,
         refreshed_at_utc     = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
    state_path, auto_unbox = TRUE, pretty = TRUE),
  error = function(e) cat(sprintf("(could not write %s: %s)\n", state_path, conditionMessage(e))))

# Exact site-set gate. A partial bundle is not a degraded success: downstream
# comparisons and denominators assume the canonical network opportunity set.
# Staging means this can fail without touching the committed known-good bundle.
built_sites <- sort(sub("\\.rds$", "", list.files(out_dir, pattern = "\\.rds$")))
expected_sites <- sort(unique(as.character(sites)))
missing_sites <- setdiff(expected_sites, built_sites)
extra_sites <- setdiff(built_sites, expected_sites)
if (!identical(built_sites, expected_sites))
  stop(sprintf(
    "Exact site-set gate failed: built %d/%d; missing=[%s]; extra=[%s]. Known-good data/sites was not replaced.",
    length(built_sites), length(expected_sites), paste(missing_sites, collapse = ","),
    paste(extra_sites, collapse = ",")), call. = FALSE)
cat(sprintf("EXACT SITE SET OK: %d/%d canonical mammal sites built.\n",
            length(built_sites), length(expected_sites)))
